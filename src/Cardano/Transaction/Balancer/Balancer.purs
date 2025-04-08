module Cardano.Transaction.Balancer
  ( getCertsBalance
  , getProposalsBalance
  , runBalancer
  ) where

import Prelude

import Cardano.Provider (Provider)
import Cardano.Transaction.Balancer.CoinSelection
  ( SelectionState
  , SelectionStrategy
  , _leftoverUtxos
  , performMultiAssetSelection
  , selectedInputs
  )
import Cardano.Transaction.Balancer.CoinSelection.UtxoIndex (UtxoIndex, buildUtxoIndex)
import Cardano.Transaction.Balancer.Collateral (setTransactionCollateral)
import Cardano.Transaction.Balancer.Constraints
  ( BalancerConfig
  , _additionalUtxos
  , _changeDatum
  , _collateralUtxos
  , _maxChangeOutputTokenQuantity
  , _nonSpendableInputs
  )
import Cardano.Transaction.Balancer.Error
  ( BalanceTxError(CouldNotGetUtxos, NumericOverflowError, UtxoLookupFailedFor)
  )
import Cardano.Transaction.Balancer.ExUnitsAndMinFee
  ( evalExUnitsAndMinFee
  , finalizeTransaction
  )
import Cardano.Transaction.Balancer.Helpers (liftEither, unsafeFromJust, (??))
import Cardano.Transaction.Balancer.Partition
  ( equipartition
  , equipartitionValueWithTokenQuantityUpperBound
  , partition
  )
import Cardano.Transaction.Balancer.Types (BalanceTxM, logWithLevelAndTags)
import Cardano.Transaction.Balancer.Types.Val (Val(Val), pprintVal)
import Cardano.Transaction.Balancer.Types.Val as Val
import Cardano.Transaction.Balancer.UtxoMinAda (utxoMinAdaValue)
import Cardano.Transaction.Edit (editTransaction)
import Cardano.Types
  ( Address
  , AssetClass(AssetClass)
  , Certificate
      ( UnregDrepCert
      , RegDrepCert
      , StakeVoteRegDelegCert
      , VoteRegDelegCert
      , StakeRegDelegCert
      , StakeDeregistration
      , StakeRegistration
      )
  , Coin(Coin)
  , Language(PlutusV1)
  , NetworkId
  , PlutusScript(PlutusScript)
  , ProtocolParameters(ProtocolParameters)
  , Transaction
  , TransactionBody
  , TransactionOutput
  , TransactionUnspentOutput
  , UtxoMap
  , Value(Value)
  , _amount
  , _body
  , _certs
  , _fee
  , _inputs
  , _mint
  , _networkId
  , _outputs
  , _withdrawals
  )
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Coin as Coin
import Cardano.Types.OutputDatum (OutputDatum(OutputDatum))
import Cardano.Types.TransactionBody (_votingProposals)
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.Value (getMultiAsset, mkValue, pprintValue)
import Cardano.Types.Value as Value
import Control.Monad.Except (class MonadError, ExceptT(ExceptT))
import Control.Monad.Except.Trans (except)
import Control.Parallel (parTraverse)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty
  ( fromArray
  , replicate
  , singleton
  , sortWith
  , toArray
  , uncons
  , zip
  , zipWith
  ) as NEArray
import Data.Array.NonEmpty as NEA
import Data.Bitraversable (ltraverse)
import Data.Either (Either, hush, note)
import Data.Foldable (fold, foldMap, foldr, length, null, sum)
import Data.Lens.Getter (view, (^.))
import Data.Lens.Setter ((%~), (.~), (?~))
import Data.Log.Level (LogLevel(Info))
import Data.Log.Tag (TagSet, tag, tagSetTag)
import Data.Log.Tag (fromArray) as TagSet
import Data.Map (empty, filter, insert, isEmpty, lookup, singleton, toUnfoldable, union) as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, isJust, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Set (Set)
import Data.Set (fromFoldable, member, toUnfoldable) as Set
import Data.Traversable (for, traverse)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import JS.BigInt (BigInt)
import Partial.Unsafe (unsafePartial)

type BalancerContext =
  { balancerConstraints :: BalancerConfig
  , provider :: Provider
  , pparams :: ProtocolParameters
  , network :: NetworkId
  , walletInterface :: BalancerWalletInterface
  , extraUtxos :: UtxoMap
  }

type BalancerWalletInterface =
  { isCip30Wallet :: Boolean
  , ownAddresses :: Array Address
  , getWalletUtxos :: Aff (Maybe UtxoMap)
  , filterLockedUtxos :: UtxoMap -> Aff UtxoMap
  , getChangeAddress :: Aff Address
  , getWalletCollateral :: Aff (Maybe (Array TransactionUnspentOutput))
  }

type BalancerState =
  { transaction :: Transaction
  , leftoverUtxos :: UtxoIndex
  , changeOutputs :: Array TransactionOutput
  , minFee :: Coin
  }

initBalancerState :: UtxoMap -> Transaction -> BalancerState
initBalancerState utxos transaction =
  { transaction
  , leftoverUtxos: buildUtxoIndex utxos
  , changeOutputs: mempty
  , minFee: Coin.zero
  }

data BalancerStep
  = PrebalanceTx BalancerState
  | BalanceChangeAndMinFee BalancerState

runBalancer :: Transaction -> BalancerContext -> BalanceTxM Transaction
runBalancer unbalancedTx ctx = do
  changeAddress <-
    maybe (liftAff ctx.walletInterface.getChangeAddress) pure
      (unwrap ctx.balancerConstraints).changeAddress
  let txWithNetwork = setTxNetwork ctx.network unbalancedTx
  txWithCollateral <-
    case (unwrap (unwrap txWithNetwork).witnessSet).redeemers of
      -- Don't set collateral if tx doesn't contain phase-2 scripts:
      [] -> pure txWithNetwork
      _ -> setTransactionCollateral ctx.walletInterface.getWalletCollateral
        ctx.balancerConstraints
        ctx.pparams
        changeAddress
        txWithNetwork
  nonSpendableCollateralInputs <-
    liftAff
      if ctx.walletInterface.isCip30Wallet then
        ctx.walletInterface.getWalletCollateral <#>
          fold >>> map (unwrap >>> _.input) >>> Set.fromFoldable
      else mempty
  allUtxos <- getAllUtxos
  availableUtxos <- liftAff $ ctx.walletInterface.filterLockedUtxos allUtxos
  let
    txWithMinAda = addLovelacesToTransactionOutputs ctx.pparams txWithCollateral
    spendableUtxos = getSpendableUtxos availableUtxos nonSpendableCollateralInputs
  mainLoop allUtxos changeAddress $ initBalancerState spendableUtxos txWithMinAda
  where
  getAllUtxos :: BalanceTxM UtxoMap
  getAllUtxos = do
    utxos <- ExceptT case (unwrap ctx.balancerConstraints).srcAddresses of
      -- Use wallet utxos.
      Nothing ->
        note CouldNotGetUtxos <$> liftAff ctx.walletInterface.getWalletUtxos
      -- Use utxos from the specified source addresses.
      Just srcAddresses -> do
        -- Even though some of the addresses may be controlled by the wallet,
        -- we can't query the wallet for available UTxOs, because there's no
        -- way to tell it to return UTxOs only from specific subset of the
        -- addresses controlled by a CIP-30 wallet.
        -- `utxosAt` calls are expensive when there are a lot of addresses to
        -- check.
        parTraverse (ctx.provider.utxosAt >>> liftAff >>> map hush) srcAddresses
          <#> traverse (note CouldNotGetUtxos)
            >>> map (foldr Map.union Map.empty) -- merge all utxos into one map
    pure $ utxos `Map.union` ctx.extraUtxos

  coinSelectionStrategy :: SelectionStrategy
  coinSelectionStrategy = (unwrap ctx.balancerConstraints).selectionStrategy

  referenceInputSet :: Set TransactionInput
  referenceInputSet = Set.fromFoldable $ (unwrap (unwrap unbalancedTx).body).referenceInputs

  miscFee :: BigInt
  miscFee = getCertsBalance unbalancedTx ctx.pparams + getProposalsBalance unbalancedTx

  -- We check if the transaction uses a plutusv1 script, so that we can filter
  -- out utxos which use plutusv2 features if so.
  txHasPlutusV1 :: Boolean
  txHasPlutusV1 =
    case (unwrap (unwrap unbalancedTx).witnessSet).plutusScripts of
      [] -> false
      scripts -> flip Array.any scripts case _ of
        PlutusScript (_ /\ PlutusV1) -> true
        _ -> false

  getSpendableUtxos :: UtxoMap -> Set TransactionInput -> UtxoMap
  getSpendableUtxos availableUtxos nonSpendableCollateralInputs =
    -- Get collateral inputs to mark them as unspendable.
    -- Some CIP-30 wallets don't allow to sign Txs that spend it.
    let
      nonSpendableInputs =
        (ctx.balancerConstraints ^. _nonSpendableInputs)
          <> nonSpendableCollateralInputs
    in
      _.spendable $ foldr
        ( \(oref /\ output) acc ->
            let
              hasInlineDatum :: Boolean
              hasInlineDatum = case (unwrap output).datum of
                Just (OutputDatum _) -> true
                _ -> false

              hasScriptRef :: Boolean
              hasScriptRef = isJust (unwrap output).scriptRef

              spendable :: Boolean
              spendable = not $ Set.member oref nonSpendableInputs ||
                Set.member oref referenceInputSet

              validInContext :: Boolean
              validInContext = not $ txHasPlutusV1 &&
                (hasInlineDatum || hasScriptRef)
            in
              case spendable, validInContext of
                true, true -> acc
                  { spendable = Map.insert oref output acc.spendable }
                true, false -> acc
                  { invalidInContext = Map.insert oref output
                      acc.invalidInContext
                  }
                _, _ -> acc
        )
        { spendable: Map.empty
        , invalidInContext: Map.empty
        }
        (Map.toUnfoldable availableUtxos :: Array _)

  mainLoop :: UtxoMap -> Address -> BalancerState -> BalanceTxM Transaction
  mainLoop allUtxos changeAddress = worker <<< PrebalanceTx
    where
    worker :: BalancerStep -> BalanceTxM Transaction
    worker (PrebalanceTx state) = do
      logBalancerState "Pre-balancing (Stage 1)" allUtxos state
      prebalanceTx state >>= runNextBalancerStep
    worker (BalanceChangeAndMinFee state@{ transaction, minFee, leftoverUtxos }) =
      do
        logBalancerState "Balancing change and fees (Stage 2)" allUtxos state
        { transaction: evaluatedTx, minFee: newMinFee } <- evaluateTx state
        case newMinFee <= minFee of
          true -> do
            logTransaction "Balanced transaction (Done)" allUtxos
              evaluatedTx
            if Array.null $ evaluatedTx ^. _body <<< _inputs then
              do
                selectionState <-
                  performMultiAssetSelection coinSelectionStrategy leftoverUtxos
                    (Val one Map.empty)
                runNextBalancerStep $ state
                  { transaction = flip editTransaction transaction $
                      _body <<< _inputs %~ appendInputs
                        (Array.fromFoldable $ selectedInputs selectionState)
                  , leftoverUtxos =
                      selectionState ^. _leftoverUtxos
                  }
            else do
              finalizeTransaction evaluatedTx allUtxos ctx.pparams
          false ->
            runNextBalancerStep $ state
              { transaction = transaction # _body <<< _fee .~ newMinFee
              , minFee = newMinFee
              }

    -- | Determines which balancing step will be performed next.
    -- |
    -- | If the transaction remains unbalanced (i.e. `requiredValue != mempty`)
    -- | after generation of change, the first balancing step `PrebalanceTx`
    -- | is performed, otherwise we proceed to `BalanceChangeAndMinFee`.
    runNextBalancerStep :: BalancerState -> BalanceTxM Transaction
    runNextBalancerStep state@{ transaction } = do
      let txBody = transaction ^. _body
      inputValue <- except $ getInputVal allUtxos txBody
      inputValue' <- liftValue inputValue
      let
        ownWalletAddresses = Set.fromFoldable ctx.walletInterface.ownAddresses
        coinsPerUtxoByte = (unwrap ctx.pparams).coinsPerUtxoByte
      changeOutputs <-
        makeChange ctx.balancerConstraints ownWalletAddresses changeAddress
          inputValue'
          miscFee
          coinsPerUtxoByte
          txBody

      requiredValue <-
        except $ getRequiredValue miscFee allUtxos
          $ setTxChangeOutputs changeOutputs transaction ^. _body

      worker $
        if requiredValue == mempty then BalanceChangeAndMinFee $ state
          { changeOutputs = changeOutputs, transaction = transaction }
        else PrebalanceTx $ state { changeOutputs = changeOutputs }

    -- | Selects a combination of unspent transaction outputs from the wallet's
    -- | utxo set so that the total input value is sufficient to cover all
    -- | transaction outputs, including generated change and min fee.
    prebalanceTx :: BalancerState -> BalanceTxM BalancerState
    prebalanceTx state@{ transaction, changeOutputs, leftoverUtxos } =
      performCoinSelection <#> \selectionState -> state
        { transaction =
            ( flip editTransaction transaction $
                _body <<< _inputs %~
                  appendInputs
                    (Array.fromFoldable $ selectedInputs selectionState)
            )
        , leftoverUtxos =
            selectionState ^. _leftoverUtxos
        }
      where
      performCoinSelection :: BalanceTxM SelectionState
      performCoinSelection = do
        let
          txBody :: TransactionBody
          txBody = setTxChangeOutputs changeOutputs transaction ^. _body
        except (getRequiredValue miscFee allUtxos txBody)
          >>= performMultiAssetSelection coinSelectionStrategy leftoverUtxos

    -- | Calculates execution units for each script in the transaction and sets
    -- | min fee.
    -- |
    -- | The transaction must be pre-balanced before evaluating execution units,
    -- | since this pre-condition is sometimes required for successfull script
    -- | execution during transaction evaluation.
    evaluateTx :: BalancerState -> BalanceTxM BalancerState
    evaluateTx state@{ transaction, changeOutputs } = do
      let
        prebalancedTx :: Transaction
        prebalancedTx = setTxChangeOutputs changeOutputs transaction
      evaluatedTx /\ minFee <- evalExUnitsAndMinFee ctx.provider ctx.pparams prebalancedTx
        ctx.walletInterface.ownAddresses
        { allUtxos: allUtxos
        , collateralUtxos: fromMaybe Map.empty $ ctx.balancerConstraints ^. _collateralUtxos
        , additionalUtxos: ctx.balancerConstraints ^. _additionalUtxos
        }
      pure $ state
        { transaction = evaluatedTx
        , minFee = minFee
        }

setTxNetwork :: NetworkId -> Transaction -> Transaction
setTxNetwork network tx =
  maybe (tx # _body <<< _networkId ?~ network) (const tx)
    (unwrap (unwrap tx).body).networkId

-- | For each transaction output, if necessary, adds some number of lovelaces
-- | to cover the utxo min-ada-value requirement.
addLovelacesToTransactionOutputs :: ProtocolParameters -> Transaction -> Transaction
addLovelacesToTransactionOutputs pparams transaction =
  transaction # _body <<< _outputs .~
    ( addLovelacesToTransactionOutput pparams <$>
        (unwrap (unwrap transaction).body).outputs
    )

addLovelacesToTransactionOutput :: ProtocolParameters -> TransactionOutput -> TransactionOutput
addLovelacesToTransactionOutput pparams txOutput =
  let
    coinsPerUtxoUnit :: Coin
    coinsPerUtxoUnit = (unwrap pparams).coinsPerUtxoByte

    txOutputMinAda = Coin $ utxoMinAdaValue coinsPerUtxoUnit txOutput
    txOutputRec = unwrap txOutput

    txOutputValue :: Value
    txOutputValue = txOutputRec.amount

    newCoin :: Coin
    newCoin = max (Value.getCoin txOutputValue) txOutputMinAda
  in
    wrap txOutputRec
      { amount = mkValue newCoin (getMultiAsset txOutputValue)
      }

-- removes duplicates
appendInputs
  :: Array TransactionInput
  -> Array TransactionInput
  -> Array TransactionInput
appendInputs a b = Set.toUnfoldable (Set.fromFoldable a <> Set.fromFoldable b)

setTxChangeOutputs
  :: Array TransactionOutput -> Transaction -> Transaction
setTxChangeOutputs outputs tx =
  tx # _body <<< _outputs %~ flip append outputs

--------------------------------------------------------------------------------
-- Making change
--------------------------------------------------------------------------------

-- | Constructs change outputs to return all excess `Value` back to the owner's
-- | address.
-- |
-- | Returns an array of change outputs even if the transaction becomes
-- | unbalanced after attaching them (which can be the case if the specified
-- | inputs do not provide enough ada to satisfy minimum ada quantites of the
-- | change outputs generated).
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/4c2eb651d79212157a749d8e69a48fff30862e93/lib/wallet/src/Cardano/Wallet/CoinSelection/Internal/Balance.hs#L1396
-- |
-- | Differences from cardano-wallet:
-- |
-- | - We only consider outputs that go back to our wallet when deciding on
-- | the number of desired outputs for change generation. See
-- | https://github.com/Plutonomicon/cardano-transaction-lib/issues/1530
makeChange
  :: BalancerConfig
  -> Set Address
  -> Address
  -> Value
  -> BigInt
  -> Coin
  -> TransactionBody
  -> BalanceTxM (Array TransactionOutput)
makeChange
  balancerConstraints
  ownWalletAddresses
  changeAddress
  inputValue'
  miscFee
  coinsPerUtxoByte
  txBody =
  -- Always generate change when a transaction has no outputs to avoid issues
  -- with transaction confirmation:
  -- FIXME: https://github.com/Plutonomicon/cardano-transaction-lib/issues/1293
  if excessValue == mempty && (txBody ^. _outputs) /= mempty then pure mempty
  else do
    res <- traverse (ltraverse liftValue) changeValueOutputCoinPairs
      >>= splitOversizedValues
        >>> map (assignCoinsToChangeValues coinsPerUtxoByte changeAddress excessCoin)
    let changeDatum = balancerConstraints ^. _changeDatum
    pure $ mkChangeOutput changeAddress changeDatum <$> res
  where
  inputValue = Val.fromValue inputValue'

  -- | Change `Value`s for all assets, where each change map is paired with a
  -- | corresponding coin from the original outputs.
  -- |
  -- | This array is sorted into ascending order of asset count, where empty
  -- | change `Value`s are all located at the start of the list.
  -- |
  -- | Taken from cardano-wallet:
  -- | https://github.com/input-output-hk/cardano-wallet/blob/4c2eb651d79212157a749d8e69a48fff30862e93/lib/wallet/src/Cardano/Wallet/CoinSelection/Internal/Balance.hs#L1447
  changeValueOutputCoinPairs :: NonEmptyArray (Val /\ BigInt)
  changeValueOutputCoinPairs = outputCoins
    # NEArray.zip changeForAssets
    # NEArray.sortWith (Array.length <<< Val.valueAssets <<< fst)
    where
    outputCoins :: NonEmptyArray BigInt
    outputCoins =
      NEArray.fromArray
        ( BigNum.toBigInt <<< unwrap <<< Value.getCoin <<< _.amount <<< unwrap
            <$> ownAddressOutputs
        )
        ?? NEArray.singleton zero

  splitOversizedValues
    :: NonEmptyArray (Value /\ BigInt)
    -> BalanceTxM (NonEmptyArray (Value /\ BigInt))
  splitOversizedValues pairs = do
    case balancerConstraints ^. _maxChangeOutputTokenQuantity of
      Nothing -> pure pairs
      Just maxTokenQuantity -> do
        traverse bundle pairs <#> \bundled ->
          unbundle <$>
            ( equipartitionValueWithTokenQuantityUpperBound maxTokenQuantity =<<
                bundled
            )
    where
    bundle :: Value /\ BigInt -> BalanceTxM Value
    bundle (Value _ assets /\ coin) = do
      coin' <- liftEither
        (note (NumericOverflowError Nothing) $ BigNum.fromBigInt coin)
      pure $ mkValue (wrap coin') assets

    unbundle :: Value -> Value /\ BigInt
    unbundle (Value coin assets) = mkValue mempty assets /\ BigNum.toBigInt
      (unwrap coin)

  -- outputs belonging to one of the wallet's addresses.
  ownAddressOutputs :: Array TransactionOutput
  ownAddressOutputs = Array.filter isOwnWalletAddress $ (unwrap txBody).outputs
    where
    isOwnWalletAddress = unwrap >>> _.address >>> flip Set.member
      ownWalletAddresses

  changeForAssets :: NonEmptyArray Val
  changeForAssets = foldr
    (NEArray.zipWith (<>) <<< makeChangeForAsset ownAddressOutputs)
    (NEArray.replicate (length ownAddressOutputs) mempty)
    excessAssets

  excessAssets :: Array (AssetClass /\ BigInt)
  excessAssets = Val.valueAssets excessValue

  excessCoin :: BigInt
  excessCoin = case excessValue of
    Val c _ -> c

  excessValue :: Val
  excessValue = posVal $
    (inputValue <> mintValue txBody) `Val.minus`
      (outputValue txBody <> minFeeValue txBody <> Val miscFee Map.empty)

  posVal :: Val -> Val
  posVal (Val coin nonAdaAsset) =
    Val (max coin zero)
      $ Map.filter (not <<< Map.isEmpty)
      $ map (Map.filter (\x -> x > zero)) nonAdaAsset

-- | Constructs change outputs for an asset.
-- |
-- | The given asset quantity is partitioned into a list of `Value`s that are
-- | proportional to the weights withing the given distribution. If the given
-- | asset quantity does not appear in the distribution, then it is equally
-- | partitioned into a list of the same length.
-- |
-- | The length of the output list is always the same as the length of the input
-- | list, and the sum of quantities is exactly equal to the asset quantity in
-- | the second argument.
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/4c2eb651d79212157a749d8e69a48fff30862e93/lib/wallet/src/Cardano/Wallet/CoinSelection/Internal/Balance.hs#L1729
makeChangeForAsset
  :: Array TransactionOutput
  -> (AssetClass /\ BigInt)
  -> NonEmptyArray Val
makeChangeForAsset
  ownAddressOutputs
  (assetClass@(AssetClass scriptHash assetName) /\ excess) =
  mkVal <$>
    partition excess weights ?? equipartition excess (length weights)
  where
  mkVal n = Val zero (Map.singleton scriptHash $ Map.singleton assetName n)

  weights :: NonEmptyArray BigInt
  weights = NEArray.fromArray assetQuantities ?? NEArray.singleton one

  assetQuantities :: Array BigInt
  assetQuantities =
    ownAddressOutputs <#> BigNum.toBigInt <<< Value.getAssetQuantity assetClass
      <<< _.amount
      <<<
        unwrap

-- | Constructs an array of ada change outputs based on the given distribution.
-- |
-- | The given ada amount is partitioned into a list of `Value`s that are
-- | proportional to the weights withing the given distribution. If the sum of
-- | weights in the given distribution is equal to zero, then the given excess
-- | coin is equally partitioned into a list of the same length.
-- |
-- | The length of the output list is always the same as the length of the input
-- | list, and the sum of its quantities is always exactly equal to the excess
-- | ada amount given as the second argument.
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/4c2eb651d79212157a749d8e69a48fff30862e93/lib/wallet/src/Cardano/Wallet/CoinSelection/Internal/Balance.hs#L1799
makeChangeForCoin :: NonEmptyArray BigInt -> BigInt -> NonEmptyArray Val
makeChangeForCoin weights excess =
  flip Val Map.empty <$>
    partition excess weights ?? equipartition excess (length weights)

-- | Assigns coin quantities to a list of pre-computed change `Value`s.
-- |
-- | Each pre-computed change `Value` must be paired with the original coin
-- | value of its corresponding output.
-- |
-- | This function:
-- |   - expects the list of pre-computed change `Value`s to be sorted in an
-- |     order that ensures all empty `Value`s are at the start of the list.
-- |
-- |   - attempts to assign a minimum ada quantity to every change `Value`, but
-- |     iteratively drops empty change `Value`s from the start of the list if
-- |     the amount of ada is insufficient to cover them all.
-- |
-- |   - continues dropping empty change maps from the start of the list until
-- |     it is possible to assign a minimum ada value to all remaining entries,
-- |     or until only one entry remains (in which case it assigns a minimum
-- |     ada value, even if the amount of ada is insufficient to cover it).
-- |
-- |   - assigns the minimum ada quantity to all non-empty change `Value`s, even
-- |     if `adaAvailable` is insufficient, does not fail.
-- |
-- | Taken from cardano-wallet:
-- | https://github.com/input-output-hk/cardano-wallet/blob/4c2eb651d79212157a749d8e69a48fff30862e93/lib/wallet/src/Cardano/Wallet/CoinSelection/Internal/Balance.hs#L1631
assignCoinsToChangeValues
  :: Coin
  -> Address
  -> BigInt
  -> NonEmptyArray (Value /\ BigInt)
  -> Array Value
assignCoinsToChangeValues coinsPerUtxoByte changeAddress adaAvailable pairsAtStart =
  unsafeFromJust "assignCoinsToChangeValues" <<< Val.toValue <$> worker
    (adaRequiredAtStart changeValuesAtStart)
    changeValuesAtStart
  where
  worker :: BigInt -> NonEmptyArray ChangeValue -> Array Val
  worker adaRequired changeValues = changeValues # NEArray.uncons >>> case _ of
    { head: x, tail }
      | Just xs <- NEA.fromArray tail
      , adaAvailable < adaRequired && noTokens x ->
          worker (adaRequired - x.minCoin) xs
    _ ->
      let
        adaRemaining :: BigInt
        adaRemaining = max zero (adaAvailable - adaRequired)

        changeValuesForOutputCoins :: NonEmptyArray Val
        changeValuesForOutputCoins =
          let
            weights = _.outputAda <$> changeValues
          in
            makeChangeForCoin weights adaRemaining

        changeValuesWithMinCoins :: NonEmptyArray Val
        changeValuesWithMinCoins = assignMinCoin <$> changeValues
      in
        NEArray.toArray $
          NEArray.zipWith append changeValuesWithMinCoins
            changeValuesForOutputCoins
    where
    noTokens :: ChangeValue -> Boolean
    noTokens = null <<< Val.getAssets <<< _.value

    assignMinCoin :: ChangeValue -> Val
    assignMinCoin { value: (Val _ assets), minCoin } =
      Val minCoin assets

  adaRequiredAtStart :: NonEmptyArray ChangeValue -> BigInt
  adaRequiredAtStart = sum <<< map _.minCoin

  changeValuesAtStart :: NonEmptyArray ChangeValue
  changeValuesAtStart =
    pairsAtStart <#> \(value /\ outputAda) ->
      { value: Val.fromValue value
      , outputAda
      , minCoin: minCoinFor value
      }

  minCoinFor :: Value -> BigInt
  minCoinFor =
    BigNum.toBigInt
      <<< utxoMinAdaValue coinsPerUtxoByte
      -- NOTE: Datum here doesn't matter, we deconstruct UTxO immediately anyway
      <<< mkChangeOutput changeAddress Nothing

type ChangeValue = { value :: Val, outputAda :: BigInt, minCoin :: BigInt }

mkChangeOutput :: Address -> Maybe OutputDatum -> Value -> TransactionOutput
mkChangeOutput changeAddress datum amount = wrap
  { address: changeAddress, amount, datum, scriptRef: Nothing }

--------------------------------------------------------------------------------
-- Getters for various `Value`s
--------------------------------------------------------------------------------

getRequiredValue
  :: BigInt -> UtxoMap -> TransactionBody -> Either BalanceTxError Val
getRequiredValue miscFee utxos txBody = do
  getInputVal utxos txBody <#> \inputValue ->
    ( outputValue txBody <> minFeeValue txBody <> Val miscFee Map.empty
    )
      `Val.minus` (inputValue <> mintValue txBody)

getAmount :: TransactionOutput -> Value
getAmount = _.amount <<< unwrap

getInputVal :: UtxoMap -> TransactionBody -> Either BalanceTxError Val
getInputVal utxos txBody =
  foldMap (view _amount >>> Val.fromValue) <$>
    for (Array.fromFoldable $ txBody ^. _inputs) \oref ->
      note (UtxoLookupFailedFor oref utxos) (Map.lookup oref utxos)

outputValue :: TransactionBody -> Val
outputValue txBody = foldMap (view _amount >>> Val.fromValue)
  (txBody ^. _outputs)

minFeeValue :: TransactionBody -> Val
minFeeValue txBody = Val.fromCoin $ txBody ^. _fee

mintValue :: TransactionBody -> Val
mintValue txBody = maybe mempty Val.fromMint (txBody ^. _mint)

getProposalsBalance :: Transaction -> BigInt
getProposalsBalance tx =
  let
    deposits :: BigInt
    deposits =
      sum $ map (BigNum.toBigInt <<< _.deposit <<< unwrap)
        (tx ^. _body <<< _votingProposals)
  in
    deposits

getCertsBalance :: Transaction -> ProtocolParameters -> BigInt
getCertsBalance tx (ProtocolParameters pparams) =
  let
    stakeAddressDeposit :: BigInt
    stakeAddressDeposit = BigNum.toBigInt $ unwrap pparams.stakeAddressDeposit

    toBi :: Coin -> BigInt
    toBi = BigNum.toBigInt <<< unwrap

    deposits :: BigInt
    deposits =
      (tx ^. _body <<< _certs) #
        map
          ( case _ of
              StakeRegistration _ ->
                stakeAddressDeposit

              StakeDeregistration _ ->
                negate $ stakeAddressDeposit

              StakeRegDelegCert _ _ stakeCredDeposit ->
                toBi stakeCredDeposit

              VoteRegDelegCert _ _ stakeCredDeposit ->
                toBi stakeCredDeposit

              StakeVoteRegDelegCert _ _ _ stakeCredDeposit ->
                toBi stakeCredDeposit

              RegDrepCert _ drepDeposit _ ->
                toBi drepDeposit

              UnregDrepCert _ drepDeposit ->
                negate $ toBi drepDeposit

              _ -> zero
          )
          >>> sum

    withdrawals :: BigInt
    withdrawals =
      sum $ map (BigNum.toBigInt <<< unwrap) $ tx ^. _body <<<
        _withdrawals
  in
    deposits - withdrawals

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

logBalancerState :: String -> UtxoMap -> BalancerState -> BalanceTxM Unit
logBalancerState message utxos { transaction, changeOutputs } =
  logTransactionWithChange message utxos (Just changeOutputs) transaction

logTransaction :: String -> UtxoMap -> Transaction -> BalanceTxM Unit
logTransaction message utxos =
  logTransactionWithChange message utxos Nothing

logTransactionWithChange
  :: String
  -> UtxoMap
  -> Maybe (Array TransactionOutput)
  -> Transaction
  -> BalanceTxM Unit
logTransactionWithChange message utxos mChangeOutputs tx =
  let
    txBody :: TransactionBody
    txBody = tx ^. _body

    outputValuesTagSet :: Maybe (Array TransactionOutput) -> Array TagSet
    outputValuesTagSet Nothing =
      [ "Output Value" `tagSetTag` pprintVal (outputValue txBody) ]
    outputValuesTagSet (Just changeOutputs) =
      [ "Output Value without change" `tagSetTag` pprintVal
          (outputValue txBody)
      , "Change Value" `tagSetTag` pprintValue
          (unsafePartial $ foldMap (getAmount) changeOutputs)
      ]

    transactionInfo :: Val -> TagSet
    transactionInfo inputValue =
      TagSet.fromArray $
        [ "Input Value" `tagSetTag` pprintVal inputValue
        , "Mint Value" `tagSetTag` pprintVal (mintValue txBody)
        , "Fees" `tag` BigNum.toString (unwrap (txBody ^. _fee))
        ] <> outputValuesTagSet mChangeOutputs
  in
    do
      except (getInputVal utxos txBody)
        >>=
          (flip (logWithLevelAndTags Info) (message <> ":") <<< transactionInfo)

liftValue :: forall a. MonadError BalanceTxError a => Val -> a Value
liftValue val = liftEither $ note (NumericOverflowError $ Just val) $
  Val.toValue val

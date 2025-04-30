module Cardano.Transaction.Balancer.Collateral
  ( setTransactionCollateral
  ) where

import Prelude

import Cardano.Transaction.Balancer.Collateral.Select (minRequiredCollateral, selectCollateral) as Collateral
import Cardano.Transaction.Balancer.Constraints (BalancerConfig)
import Cardano.Transaction.Balancer.Error
  ( BalanceTxError
      ( CouldNotGetCollateral
      , CollateralReturnError
      , InsufficientCollateralUtxos
      , NumericOverflowError
      )
  )
import Cardano.Transaction.Balancer.Helpers (pprintTagSet)
import Cardano.Transaction.Balancer.Types (BalanceTxM, logWithLevel)
import Cardano.Transaction.Balancer.Types.ProtocolParameters (BalancerProtocolParameters)
import Cardano.Transaction.Balancer.Types.Val (fromValue, getCoin) as Val
import Cardano.Transaction.Balancer.UtxoMinAda (utxoMinAdaValue)
import Cardano.Types
  ( Address
  , BigNum
  , Coin
  , MultiAsset
  , Transaction
  , TransactionOutput
  , TransactionUnspentOutput(TransactionUnspentOutput)
  , UtxoMap
  , _body
  , _collateral
  , _collateralReturn
  , _totalCollateral
  )
import Cardano.Types.Address (getPaymentCredential) as Address
import Cardano.Types.BigNum (add, max, maxValue, sub, toBigInt, zero) as BigNum
import Cardano.Types.Credential (asPubKeyHash) as Credential
import Cardano.Types.MultiAsset as MultiAsset
import Cardano.Types.TransactionUnspentOutput (fromUtxoMap, toUtxoMap)
import Cardano.Types.UtxoMap (pprintUtxoMap)
import Cardano.Types.Value (getMultiAsset, mkValue, valueToCoin) as Value
import Control.Monad.Error.Class (liftEither, throwError)
import Control.Monad.Except (ExceptT(ExceptT))
import Control.Monad.Except.Trans (except)
import Data.Array (fromFoldable, null, partition) as Array
import Data.Either (Either(Left, Right), note)
import Data.Foldable (foldMap, foldl)
import Data.Lens ((.~))
import Data.Lens.Setter ((?~))
import Data.Log.Level (LogLevel(Warn))
import Data.Map (filter, member) as Map
import Data.Maybe (Maybe(Just, Nothing), isJust)
import Data.Newtype (unwrap, wrap)
import Data.UInt (toInt) as UInt
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)

setTransactionCollateral
  :: forall (r :: Row Type)
   . Aff (Maybe (Array TransactionUnspentOutput))
  -> BalancerConfig
  -> Record (BalancerProtocolParameters r)
  -> Address
  -> Transaction
  -> UtxoMap
  -> BalanceTxM Transaction
setTransactionCollateral
  getWalletCollateralAff
  balancerConstraints
  pparams
  changeAddr
  transaction
  spendableUtxos = do
  collateral <-
    case (unwrap balancerConstraints).collateralUtxos of
      -- if no collateral utxos are specified, use the wallet, but filter
      -- the unspendable ones
      Nothing -> do
        (walletCollateral :: Array TransactionUnspentOutput) <-
          ExceptT $ liftAff $ note CouldNotGetCollateral <$>
            getWalletCollateralAff
        let
          { yes: spendableCollUtxos, no: filteredUtxos } = Array.partition
            isSpendable
            walletCollateral
        when (not $ Array.null filteredUtxos) do
          logWithLevel Warn $ pprintTagSet
            "Some of the collateral UTxOs returned by the wallet were marked as non-spendable and ignored"
            (pprintUtxoMap $ toUtxoMap filteredUtxos)
        let
          collVal =
            foldMap (Val.fromValue <<< _.amount <<< unwrap <<< _.output <<< unwrap)
              spendableCollUtxos
          minRequiredCollateral = BigNum.toBigInt $ unwrap Collateral.minRequiredCollateral
        if (Val.getCoin collVal < minRequiredCollateral) then do
          logWithLevel Warn $ pprintTagSet
            "Filtered collateral UTxOs do not cover the minimum required \
            \collateral, reselecting collateral using the internal CTL algorithm"
            (pprintUtxoMap $ toUtxoMap spendableCollUtxos)
          selectCollateralFromUtxos pparams $ Map.filter isPkhUtxo spendableUtxos
        else pure spendableCollUtxos
      -- otherwise, select collateral from the spendable utxos using
      -- the KeyWallet algorithm
      Just utxoMap -> do
        let
          { yes: spendableCollUtxos, no: filteredUtxos } = Array.partition
            isSpendable
            (fromUtxoMap utxoMap)
        when (not $ Array.null filteredUtxos) do
          logWithLevel Warn $ pprintTagSet
            "Some of the collateral UTxOs specified via the `mustUseCollateralUtxos` constraint \
            \were marked as non-spendable and ignored"
            (pprintUtxoMap $ toUtxoMap filteredUtxos)
        selectCollateralFromUtxos pparams $ toUtxoMap spendableCollUtxos
  addTxCollateralReturn collateral (addTxCollateral collateral transaction) changeAddr
    pparams.coinsPerUtxoByte
  where
  -- Utxos specified as reference (read-only) inputs or marked as unspendable in the balancer
  -- constraints must be excluded from collateral selection.
  isSpendable :: TransactionUnspentOutput -> Boolean
  isSpendable (TransactionUnspentOutput { input, output }) =
    Map.member input spendableUtxos
      && isPkhUtxo output

  isPkhUtxo :: TransactionOutput -> Boolean
  isPkhUtxo txOut =
    isJust do
      cred <- Address.getPaymentCredential $ (unwrap txOut).address
      Credential.asPubKeyHash $ unwrap cred

-- | Select collateral from the provided utxos using the internal
-- | collateral selection algorithm.
selectCollateralFromUtxos
  :: forall (r :: Row Type)
   . Record (BalancerProtocolParameters r)
  -> UtxoMap
  -> BalanceTxM (Array TransactionUnspentOutput)
selectCollateralFromUtxos pparams utxos = do
  let
    maxCollateralInputs = UInt.toInt $ pparams.maxCollateralInputs
    mbCollateral =
      Array.fromFoldable <$> Collateral.selectCollateral
        pparams.coinsPerUtxoByte
        maxCollateralInputs
        utxos
  liftEither $ note (InsufficientCollateralUtxos utxos)
    mbCollateral

addTxCollateral :: Array TransactionUnspentOutput -> Transaction -> Transaction
addTxCollateral collateral transaction =
  transaction # _body <<< _collateral .~
    map (_.input <<< unwrap) collateral

--------------------------------------------------------------------------------
-- Collateral Return, Total Collateral
--------------------------------------------------------------------------------

-- | Sets `collateral return` and `total collateral` fields of the transaction.
-- | In the special case with an Ada-only collateral that is less than or equal
-- | to `minRequiredCollateral`, returns unmodified transaction (see NOTE).
-- |
-- | NOTE: Collateral cannot be less than `minRequiredCollateral` when
-- | selected using `selectCollateral` function
addTxCollateralReturn
  :: Array TransactionUnspentOutput
  -> Transaction
  -> Address
  -> Coin
  -> BalanceTxM Transaction
addTxCollateralReturn collateral transaction ownAddress coinsPerUtxoByte = do
  let maybeAdd acc n = BigNum.add (unwrap $ adaValue n) =<< acc
  collAdaValue <- throwOnOverflow $ foldl maybeAdd (Just BigNum.zero) collateral
  collMultiAsset <- throwOnOverflow $ MultiAsset.sum $ nonAdaAsset <$>
    collateral
  case
    collAdaValue <= unwrap Collateral.minRequiredCollateral && collMultiAsset ==
      MultiAsset.empty
    of
    true ->
      pure transaction
    false ->
      setTxCollateralReturn collAdaValue collMultiAsset
  where
  setTxCollateralReturn
    :: BigNum
    -> MultiAsset
    -> BalanceTxM Transaction
  setTxCollateralReturn collAdaValue collMultiAsset = do
    let
      maxBigNumAdaValue :: Coin
      maxBigNumAdaValue = wrap BigNum.maxValue

      collReturnOutputRec =
        { address: ownAddress
        , amount: Value.mkValue maxBigNumAdaValue collMultiAsset
        , datum: Nothing
        , scriptRef: Nothing
        }

    -- Calculate the required min ada value for the collateral return output:
    let
      returnAsTxOut = wrap collReturnOutputRec
      minAdaValue = utxoMinAdaValue coinsPerUtxoByte returnAsTxOut
    -- Determine the actual ada value of the collateral return output:
    collReturnAda <- throwOnOverflow do
      remaining <- BigNum.sub collAdaValue (unwrap Collateral.minRequiredCollateral)
      pure $ BigNum.max remaining minAdaValue
    let
      -- Build the final collateral return output:
      collReturnOutput :: TransactionOutput
      collReturnOutput = wrap $
        collReturnOutputRec
          { amount = Value.mkValue (wrap collReturnAda) collMultiAsset }

    totalCollateral <- throwOnOverflow $ BigNum.sub collAdaValue collReturnAda

    except $
      case totalCollateral > BigNum.zero of
        true ->
          -- Set collateral return and total collateral:
          Right $
            transaction # _body <<< _collateralReturn ?~ collReturnOutput
              # _body <<< _totalCollateral ?~ wrap totalCollateral
        false -> Left CollateralReturnError

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

adaValue :: TransactionUnspentOutput -> Coin
adaValue =
  Value.valueToCoin <<< _.amount <<< unwrap <<< _.output <<< unwrap

nonAdaAsset :: TransactionUnspentOutput -> MultiAsset
nonAdaAsset =
  Value.getMultiAsset <<< _.amount <<< unwrap <<< _.output <<< unwrap

throwOnOverflow :: forall a. Maybe a -> BalanceTxM a
throwOnOverflow = case _ of
  Nothing -> throwError (NumericOverflowError Nothing)
  Just a -> pure a

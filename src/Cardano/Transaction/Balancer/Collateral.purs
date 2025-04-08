module Cardano.Transaction.Balancer.Collateral
  ( setTransactionCollateral
  ) where

import Prelude

import Cardano.Transaction.Balancer.Collateral.Select (minRequiredCollateral, selectCollateral)
import Cardano.Transaction.Balancer.Constraints
  ( BalancerConfig
  , _collateralUtxos
  , _nonSpendableInputs
  )
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
import Cardano.Transaction.Balancer.UtxoMinAda (utxoMinAdaValue)
import Cardano.Types
  ( Address
  , BigNum
  , Coin
  , MultiAsset
  , Transaction
  , TransactionOutput
  , TransactionUnspentOutput
  , _body
  , _collateral
  , _collateralReturn
  , _totalCollateral
  )
import Cardano.Types.BigNum (add, max, maxValue, sub, zero) as BigNum
import Cardano.Types.MultiAsset as MultiAsset
import Cardano.Types.TransactionUnspentOutput (toUtxoMap) as TransactionUnspentOutputs
import Cardano.Types.UtxoMap (pprintUtxoMap)
import Cardano.Types.Value (getMultiAsset, mkValue, valueToCoin) as Value
import Control.Monad.Error.Class (liftEither, throwError)
import Control.Monad.Except (ExceptT(ExceptT))
import Control.Monad.Except.Trans (except)
import Data.Array (fromFoldable, null, partition) as Array
import Data.Either (Either(Left, Right), note)
import Data.Foldable (foldl)
import Data.Lens ((^.), (.~))
import Data.Lens.Setter ((?~))
import Data.Log.Level (LogLevel(Warn))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap, wrap)
import Data.Set (member) as Set
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
  -> BalanceTxM Transaction
setTransactionCollateral
  getWalletCollateralAff
  balancerConstraints
  pparams
  changeAddr
  transaction = do
  let
    nonSpendableSet = balancerConstraints ^. _nonSpendableInputs
    mbCollateralUtxos = balancerConstraints ^. _collateralUtxos
  -- We must filter out UTxOs that are set as non-spendable in the balancer
  -- constraints
  let
    isSpendable = not <<< flip Set.member nonSpendableSet
    coinsPerUtxoByte = pparams.coinsPerUtxoByte
  collateral <- case mbCollateralUtxos of
    -- if no collateral utxos are specified, use the wallet, but filter
    -- the unspendable ones
    Nothing -> do
      let isSpendableUtxo = isSpendable <<< _.input <<< unwrap
      (walletCollateral :: Array TransactionUnspentOutput) <- ExceptT $ liftAff
        $
          note CouldNotGetCollateral <$>
            getWalletCollateralAff
      let
        { yes: spendableUtxos, no: filteredUtxos } = Array.partition
          isSpendableUtxo
          walletCollateral
      when (not $ Array.null filteredUtxos) do
        logWithLevel Warn $ pprintTagSet
          "Some of the collateral UTxOs returned by the wallet were marked as non-spendable and ignored"
          (pprintUtxoMap (TransactionUnspentOutputs.toUtxoMap filteredUtxos))
      pure spendableUtxos
    -- otherwise, get all the utxos, filter out unspendable, and select
    -- collateral using internal algo, that is also used in KeyWallet
    Just utxoMap -> do
      let
        maxCollateralInputs = UInt.toInt pparams.maxCollateralInputs
        mbCollateral =
          Array.fromFoldable <$>
            selectCollateral coinsPerUtxoByte maxCollateralInputs utxoMap
      liftEither $ note (InsufficientCollateralUtxos utxoMap) mbCollateral
  addTxCollateralReturn collateral (addTxCollateral collateral transaction) changeAddr
    coinsPerUtxoByte

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
    collAdaValue <= unwrap minRequiredCollateral && collMultiAsset ==
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
      remaining <- BigNum.sub collAdaValue (unwrap minRequiredCollateral)
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

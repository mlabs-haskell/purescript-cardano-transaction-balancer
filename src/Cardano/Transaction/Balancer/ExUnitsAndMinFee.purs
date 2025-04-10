module Cardano.Transaction.Balancer.ExUnitsAndMinFee
  ( evalExUnitsAndMinFee
  , finalizeTransaction
  ) where

import Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.Kupmios.Ogmios.Types (AdditionalUtxoSet) as Ogmios
import Cardano.Provider (Provider)
import Cardano.Provider.TxEvaluation
  ( TxEvaluationFailure(AdditionalUtxoOverlap, UnparsedError)
  , TxEvaluationResult(TxEvaluationResult)
  )
import Cardano.Transaction.Balancer.Error
  ( BalanceTxError(UtxoLookupFailedFor, ExUnitsEvaluationFailed, CouldNotComputeRefScriptsFee)
  )
import Cardano.Transaction.Balancer.Helpers
  ( liftEither
  , setScriptDataHash
  , transactionInputToTxOutRef
  , transactionOutputToOgmiosTxOut
  , unsafeFromJust
  )
import Cardano.Transaction.Balancer.MinFee (calculateMinFee) as Contract.MinFee
import Cardano.Transaction.Balancer.Types (BalanceTxM)
import Cardano.Transaction.Balancer.Types.ProtocolParameters (BalancerProtocolParameters)
import Cardano.Types
  ( Address
  , Coin
  , CostModel
  , ExUnits(ExUnits)
  , Language
  , PlutusData
  , PlutusScript
  , Redeemer(Redeemer)
  , Transaction
  , TransactionBody(TransactionBody)
  , TransactionOutput(TransactionOutput)
  , TransactionWitnessSet
  , UtxoMap
  , _body
  , _inputs
  , _isValid
  , _referenceInputs
  , _witnessSet
  )
import Cardano.Types.BigNum as BigNum
import Cardano.Types.ScriptRef as ScriptRef
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.TransactionWitnessSet (_redeemers)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (except)
import Data.Array (catMaybes)
import Data.Array (fromFoldable, notElem) as Array
import Data.Bifunctor (bimap, lmap)
import Data.ByteArray as ByteArray
import Data.Either (Either(Left, Right), note)
import Data.Foldable (foldMap)
import Data.Lens ((.~))
import Data.Lens.Getter ((^.))
import Data.Map (Map)
import Data.Map (empty, filterKeys, fromFoldable, lookup, toUnfoldable, union) as Map
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for, sum)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt as UInt
import Effect.Aff (attempt)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)

evalTxExecutionUnits
  :: Provider
  -> Transaction
  -> UtxoMap
  -> BalanceTxM TxEvaluationResult
evalTxExecutionUnits provider tx = worker <<< toOgmiosAdditionalUtxos
  where
  toOgmiosAdditionalUtxos :: UtxoMap -> Ogmios.AdditionalUtxoSet
  toOgmiosAdditionalUtxos additionalUtxos =
    wrap $ Map.fromFoldable
      ( bimap transactionInputToTxOutRef transactionOutputToOgmiosTxOut
          <$> (Map.toUnfoldable :: _ -> Array _) additionalUtxos
      )

  worker :: Ogmios.AdditionalUtxoSet -> BalanceTxM TxEvaluationResult
  worker additionalUtxos = do
    evalResult' <-
      map unwrap <$>
        (liftAff $ attempt $ provider.evaluateTx tx (unwrap additionalUtxos))
    case evalResult' of
      Left err | tx ^. _isValid ->
        liftAff $ throwError err
      Left _ ->
        pure $ wrap Map.empty
      Right evalResult ->
        case evalResult of
          Right a -> pure a
          Left (AdditionalUtxoOverlap overlappingUtxos) ->
            -- Remove overlapping additional utxos and retry evaluation:
            worker $ wrap $ Map.filterKeys (flip Array.notElem overlappingUtxos)
              (unwrap additionalUtxos)
          Left evalFailure | tx ^. _isValid -> do
            throwError $ ExUnitsEvaluationFailed tx evalFailure
          Left _ -> do
            pure $ wrap Map.empty

-- Calculates the execution units needed for each script in the transaction
-- and the minimum fee, including the script fees.
-- Returns a tuple consisting of updated `UnbalancedTx` and the minimum fee.
evalExUnitsAndMinFee
  :: forall (r :: Row Type)
   . Provider
  -> Record (BalancerProtocolParameters r)
  -> Transaction
  -> Array Address
  -> { allUtxos :: UtxoMap
     , collateralUtxos :: UtxoMap
     , additionalUtxos :: UtxoMap
     }
  -> BalanceTxM (Transaction /\ Coin)
evalExUnitsAndMinFee provider pparams transaction ownAddresses utxos = do
  -- Evaluate transaction ex units:
  exUnits <- evalTxExecutionUnits provider transaction utxos.additionalUtxos
  -- Set execution units received from the server:
  txWithExUnits <-
    case updateTxExecutionUnits transaction exUnits of
      Just res -> pure res
      Nothing
        | not (transaction ^. _isValid) -> pure transaction
      _ -> throwError $ ExUnitsEvaluationFailed transaction
        (UnparsedError "Unable to extract ExUnits from Ogmios response")
  -- Attach datums and redeemers, set the script integrity hash:
  finalizedTx <- finalizeTransaction txWithExUnits utxos.allUtxos pparams
  -- Calculate the minimum fee for a transaction:
  refScriptsTotalSize <- liftEither $ lmap CouldNotComputeRefScriptsFee $
    calculateRefScriptsTotalSize finalizedTx utxos.allUtxos

  minFee <- liftAff $ Contract.MinFee.calculateMinFee
    { ownAddrs: ownAddresses, provider, protocolParameters: pparams }
    finalizedTx
    (Map.union utxos.additionalUtxos utxos.collateralUtxos)
    (UInt.fromInt refScriptsTotalSize)
  pure $ txWithExUnits /\ minFee

calculateRefScriptsTotalSize
  :: Transaction -> UtxoMap -> Either TransactionInput Int
calculateRefScriptsTotalSize tx utxoMap = do
  let
    refInputs = tx ^. _body <<< _referenceInputs
    inputs = tx ^. _body <<< _inputs
    allInputs = refInputs <> inputs
  outputs <- for allInputs \input ->
    note input $ Map.lookup input utxoMap
  let
    refScriptSizes = outputs <#> \(TransactionOutput { scriptRef }) ->
      maybe zero (ByteArray.byteLength <<< unwrap <<< encodeCbor) scriptRef
  pure $ sum refScriptSizes

-- | Attaches datums and redeemers, sets the script integrity hash,
-- | for use after reindexing.
finalizeTransaction
  :: forall (r :: Row Type)
   . Transaction
  -> UtxoMap
  -> Record (BalancerProtocolParameters r)
  -> BalanceTxM Transaction
finalizeTransaction tx utxos pparams = do
  let
    txBody :: TransactionBody
    txBody = tx ^. _body

    ws :: TransactionWitnessSet
    ws = tx ^. _witnessSet

    redeemers :: Array Redeemer
    redeemers = (_.redeemers $ unwrap ws)

    datums :: Array PlutusData
    datums = _.plutusData (unwrap ws)

  refPlutusScripts <- except $ getRefPlutusScripts txBody

  let
    scripts :: Array PlutusScript
    scripts = (_.plutusScripts $ unwrap ws) <> refPlutusScripts

    languages :: Set Language
    languages = foldMap (Set.singleton <<< snd <<< unwrap) scripts

    costModels :: Map Language CostModel
    costModels = Map.filterKeys (flip Set.member languages) pparams.costModels

  liftEffect $ setScriptDataHash costModels redeemers datums tx
  where
  getRefPlutusScripts
    :: TransactionBody -> Either BalanceTxError (Array PlutusScript)
  getRefPlutusScripts (TransactionBody txBody) =
    let
      spendAndRefInputs :: Array TransactionInput
      spendAndRefInputs =
        Array.fromFoldable (txBody.inputs <> txBody.referenceInputs)
    in
      catMaybes <<< map getPlutusScript <$>
        for spendAndRefInputs \oref ->
          note (UtxoLookupFailedFor oref utxos) (Map.lookup oref utxos)

  getPlutusScript :: TransactionOutput -> Maybe PlutusScript
  getPlutusScript (TransactionOutput { scriptRef }) =
    ScriptRef.getPlutusScript =<< scriptRef

updateTxExecutionUnits
  :: Transaction
  -> TxEvaluationResult
  -> Maybe Transaction
updateTxExecutionUnits tx result =
  getRedeemersExUnits result (tx ^. _witnessSet <<< _redeemers) <#>
    \redeemers' ->
      tx # _witnessSet <<< _redeemers .~ redeemers'

getRedeemersExUnits
  :: TxEvaluationResult
  -> Array Redeemer
  -> Maybe (Array Redeemer)
getRedeemersExUnits (TxEvaluationResult result) redeemers = do
  for redeemers \indexedRedeemer -> do
    { memory, steps } <- Map.lookup
      { redeemerTag: (unwrap indexedRedeemer).tag
      , redeemerIndex: UInt.fromInt $ unsafeFromJust "getRedeemersExUnits"
          $ BigNum.toInt
          $ (unwrap indexedRedeemer).index
      }
      result
    pure $ Redeemer $ (unwrap indexedRedeemer)
      { exUnits = ExUnits
          { mem: memory
          , steps: steps
          }
      }

module Cardano.Transaction.Balancer.Helpers
  ( bugTrackerLink
  , pprintTagSet
  , liftEither
  , liftM
  , liftedM
  , unsafeFromJust
  , fromMaybeFlipped
  , setScriptDataHash
  , (??)
  , transactionInputToTxOutRef
  , transactionOutputToOgmiosTxOut
  ) where

import Prelude

import Ansi.Codes (Color(Yellow))
import Ansi.Output (bold, foreground, withGraphics)
import Cardano.AsCbor (encodeCbor)
import Cardano.Data.Lite
  ( hashScriptData
  , packListContainer
  , packMapContainer
  , toBytes
  )
import Cardano.Provider (OgmiosTxOut, OgmiosTxOutRef)
import Cardano.Types (DataHash, TransactionInput(TransactionInput))
import Cardano.Types.Address as Address
import Cardano.Types.CostModel (CostModel)
import Cardano.Types.CostModel as CostModel
import Cardano.Types.Language (Language)
import Cardano.Types.Language as Language
import Cardano.Types.OutputDatum (outputDatumDataHash, outputDatumDatum)
import Cardano.Types.PlutusData (PlutusData)
import Cardano.Types.PlutusData as PlutusData
import Cardano.Types.Redeemer (Redeemer)
import Cardano.Types.Redeemer as Redeemer
import Cardano.Types.ScriptDataHash (ScriptDataHash(ScriptDataHash))
import Cardano.Types.Transaction (Transaction(Transaction))
import Cardano.Types.TransactionBody (TransactionBody(TransactionBody))
import Cardano.Types.TransactionOutput (TransactionOutput(TransactionOutput))
import Control.Monad.Error.Class
  ( class MonadError
  , class MonadThrow
  , throwError
  )
import Data.Array (concat, cons, singleton)
import Data.Array as Array
import Data.ByteArray (byteArrayToHex)
import Data.Either (Either(Right), either)
import Data.Foldable (null)
import Data.JSDate (JSDate, toISOString)
import Data.Log.Tag
  ( Tag(StringTag, NumberTag, IntTag, BooleanTag, JSDateTag, TagSetTag)
  , TagSet
  )
import Data.Map (Map, isEmpty, toUnfoldable)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), fromMaybe, maybe)
import Data.Newtype (over, unwrap)
import Data.Profunctor.Strong ((***))
import Data.String (joinWith)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import Literals.Undefined (undefined)
import Unsafe.Coerce (unsafeCoerce)

-- | Given an error and a lifted `Maybe` value.
liftedM
  :: forall (e :: Type) (m :: Type -> Type) (a :: Type)
   . MonadError e m
  => e
  -> m (Maybe a)
  -> m a
liftedM err mma = mma >>= maybe (throwError err) Right >>> liftEither

-- | Given an error and a `Maybe` value, lift the context via `liftEither`.
liftM
  :: forall (e :: Type) (m :: Type -> Type) (a :: Type)
   . MonadThrow e m
  => e
  -> Maybe a
  -> m a
liftM err = liftEither <<< maybe (throwError err) Right

liftEither
  :: forall (a :: Type) (e :: Type) (m :: Type -> Type)
   . MonadThrow e m
  => Either e a
  -> m a
liftEither = either throwError pure

unsafeFromJust :: forall a. String -> Maybe a -> a
unsafeFromJust e a = case a of
  Nothing ->
    unsafePerformEffect $ throw $ "unsafeFromJust: impossible happened: "
      <> e
      <> " (please report as bug at "
      <> bugTrackerLink
      <> " )"
  Just v -> v

fromMaybeFlipped :: forall (a :: Type). Maybe a -> a -> a
fromMaybeFlipped = flip fromMaybe

infixl 5 fromMaybeFlipped as ??

bugTrackerLink :: String
bugTrackerLink =
  "https://github.com/Plutonomicon/cardano-transaction-lib/issues"

pprintTagSet :: String -> TagSet -> String
pprintTagSet message tags =
  message <> " " <> showTags tags

showTags :: TagSet -> String
showTags = tagLines >>> case _ of
  Nothing -> ""
  Just lines -> append "\n" (joinWith "\n" lines)

tagLines :: TagSet -> Maybe (Array String)
tagLines tags_
  | isEmpty tags_ = Nothing
  | otherwise = Just $ indentEachLine <$> concat (lineify tags_)

lineify :: TagSet -> Array (Array String)
lineify tags_ = showField <$> toUnfoldable tags_

showField :: Tuple String Tag -> Array String
showField (Tuple name value) = showTag value $ bold' name <> bold' ": "

showTag :: Tag -> String -> Array String
showTag (StringTag value) = showBasic value
showTag (IntTag value) = showSpecial $ show value
showTag (NumberTag value) = showSpecial $ show value
showTag (BooleanTag value) = showSpecial $ show value
showTag (TagSetTag value) = showSubTags value
showTag (JSDateTag value) = showJsDate value

showSubTags :: TagSet -> String -> Array String
showSubTags value label = cons label $ fromMaybe [] (tagLines value)

showJsDate :: JSDate -> String -> Array String
showJsDate value label =
  showSpecial (unsafePerformEffect (toISOString value)) label

showBasic :: String -> String -> Array String
showBasic value label = singleton $ label <> value

showSpecial :: String -> String -> Array String
showSpecial = color Yellow >>> showBasic

indentEachLine :: String -> String
indentEachLine = append "   "

color :: Color -> String -> String
color = foreground >>> withGraphics

bold' :: String -> String
bold' = withGraphics bold

-- | Set the `Transaction` body's script data hash. NOTE: Must include *all* of
-- | the datums and redeemers for the given transaction
setScriptDataHash
  :: Map Language CostModel
  -> Array Redeemer
  -> Array PlutusData
  -> Transaction
  -> Effect Transaction
setScriptDataHash costModels rs ds tx@(Transaction { body, witnessSet })
  -- No hash should be set if *all* of the following hold:
  --
  --   * there are no scripts
  --   * there are no redeemers
  --   * there are no datums
  --
  | null (unwrap witnessSet).plutusScripts
  , null (unwrap witnessSet).plutusData
  , null rs
  , null ds = pure tx
  | otherwise = do
      let
        costMdlsCdl =
          packMapContainer $ map (Language.toCdl *** CostModel.toCdl) $
            Map.toUnfoldable costModels
        redeemersCdl =
          packListContainer $ Redeemer.toCdl <$> rs
        datumsCdl =
          if Array.null ds
          -- This is a hack. The datums argument is optional and is
          -- supposed to not be provided if there are no datums.
          -- TODO: fix upstream
          then unsafeCoerce undefined
          else packListContainer $ PlutusData.toCdl <$> ds
        scriptDataHash =
          ScriptDataHash $ hashScriptData redeemersCdl costMdlsCdl datumsCdl
      pure $ over Transaction
        _
          { body = over TransactionBody
              _ { scriptDataHash = Just scriptDataHash }
              body
          }
        tx

-- | Converts an (internal) `TransactionInput` to an Ogmios transaction input
transactionInputToTxOutRef
  :: TransactionInput -> OgmiosTxOutRef
transactionInputToTxOutRef
  (TransactionInput { transactionId, index }) =
  { txId: byteArrayToHex (toBytes $ unwrap transactionId)
  , index
  }

-- | Converts an internal transaction output to the Ogmios transaction output.
transactionOutputToOgmiosTxOut
  :: TransactionOutput -> OgmiosTxOut
transactionOutputToOgmiosTxOut
  (TransactionOutput { address, amount: value, datum, scriptRef }) =
  { address: Address.toBech32 address
  , value
  , datumHash: datumHashToOgmiosDatumHash <$> (outputDatumDataHash =<< datum)
  , datum: datumToOgmiosDatum <$> (outputDatumDatum =<< datum)
  , script: scriptRef
  }

--------------------------------------------------------------------------------
-- Conversion between transaction datum hash types
--------------------------------------------------------------------------------

-- | Converts an internal `DataHash` to an Ogmios datumhash `String`
datumHashToOgmiosDatumHash :: DataHash -> String
datumHashToOgmiosDatumHash = byteArrayToHex <<< unwrap <<< encodeCbor

-- | Converts an internal `Datum` to an Ogmios datum `String`
datumToOgmiosDatum :: PlutusData -> String
datumToOgmiosDatum =
  encodeCbor >>> unwrap >>> byteArrayToHex


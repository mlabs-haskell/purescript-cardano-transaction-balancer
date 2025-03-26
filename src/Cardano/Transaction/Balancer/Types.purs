module Cardano.Transaction.Balancer.Types
  ( BalanceTxM
  , BalanceTxMContext
  , askCoinsPerUtxoUnit
  , askCostModelsForLanguages
  , askNetworkId
  , asksConstraints
  , log
  , logWithLevel
  , logWithLevelAndTags
  ) where

import Prelude

import Cardano.Provider (Provider)
import Cardano.Types
  ( Address
  , Coin
  , CostModel
  , Language
  , NetworkId
  , ProtocolParameters
  )
import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Reader.Class (ask, asks)
import Control.Monad.Reader.Trans (ReaderT)
import Cardano.Transaction.Balancer.Constraints (BalancerConfig)
import Cardano.Transaction.Balancer.Error (BalanceTxError)
import Data.JSDate (now)
import Data.Lens (Lens')
import Data.Lens.Getter (view)
import Data.Log.Formatter.Pretty (prettyFormatter)
import Data.Log.Level (LogLevel)
import Data.Log.Message (Message)
import Data.Log.Tag (TagSet)
import Data.Map (Map)
import Data.Map (empty, filterKeys) as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.Set (member) as Set
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log) as Effect

type BalanceTxMContext =
  { constraints :: BalancerConfig
  , ownAddresses :: Array Address
  , networkId :: NetworkId
  , protocolParameters :: ProtocolParameters
  , provider :: Provider
  , logLevel :: LogLevel
  , customLogger :: Maybe (LogLevel -> Message -> Aff Unit)
  }

type BalanceTxM (a :: Type) =
  ExceptT BalanceTxError (ReaderT BalanceTxMContext Aff) a

log :: String -> BalanceTxM Unit
log str = do
  config <- ask
  logWithLevel config.logLevel str

logWithLevel :: LogLevel -> String -> BalanceTxM Unit
logWithLevel logLevel str = logWithLevelAndTags logLevel Map.empty str

logWithLevelAndTags :: LogLevel -> TagSet -> String -> BalanceTxM Unit
logWithLevelAndTags logLevel tags str = do
  config <- ask
  n <- liftEffect now
  let
    logFunction = fromMaybe log_ config.customLogger
    msg =
      { level: logLevel, message: str, tags, timestamp: n }
  liftAff $ logFunction logLevel msg
  where
  log_ lvl msg = when (msg.level >= lvl) $ Effect.log =<< prettyFormatter msg

asksConstraints
  :: forall (a :: Type). Lens' BalancerConfig a -> BalanceTxM a
asksConstraints l = asks (view l <<< _.constraints)

askCoinsPerUtxoUnit :: BalanceTxM Coin
askCoinsPerUtxoUnit =
  asks
    (_.coinsPerUtxoByte <<< unwrap <<< _.protocolParameters)

askNetworkId :: BalanceTxM NetworkId
askNetworkId = asks _.networkId

askCostModelsForLanguages :: Set Language -> BalanceTxM (Map Language CostModel)
askCostModelsForLanguages languages =
  asks (_.costModels <<< unwrap <<< _.protocolParameters)
    <#> Map.filterKeys (flip Set.member languages)

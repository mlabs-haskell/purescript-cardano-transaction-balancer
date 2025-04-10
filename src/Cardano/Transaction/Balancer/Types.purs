module Cardano.Transaction.Balancer.Types
  ( BalanceTxM
  , CustomLogger
  , Logging
  , log
  , logWithLevel
  , logWithLevelAndTags
  ) where

import Prelude

import Control.Monad.Except.Trans (ExceptT)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Reader.Trans (ReaderT)
import Cardano.Transaction.Balancer.Error (BalanceTxError)
import Data.JSDate (now)
import Data.Log.Formatter.Pretty (prettyFormatter)
import Data.Log.Level (LogLevel)
import Data.Log.Message (Message)
import Data.Log.Tag (TagSet)
import Data.Map (empty) as Map
import Data.Maybe (Maybe, fromMaybe)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log) as Effect

type CustomLogger = Maybe (LogLevel -> Message -> Aff Unit)

type Logging =
  { logLevel :: LogLevel
  , customLogger :: CustomLogger
  }

type BalanceTxM (a :: Type) = ExceptT BalanceTxError (ReaderT Logging Aff) a

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

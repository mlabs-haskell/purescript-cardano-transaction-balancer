module Cardano.Transaction.Balancer.Contract
  ( withBalancerConstraints
  ) where

import Prelude

import Cardano.Provider (Provider)
import Cardano.Types (Address, NetworkId, ProtocolParameters)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Cardano.Transaction.Balancer.Constraints
  ( BalancerConfig
  , BalancerConstraints
  , buildBalancerConfig
  )
import Cardano.Transaction.Balancer.Types (BalanceTxMContext)
import Data.Log.Level (LogLevel)
import Data.Log.Message (Message)
import Data.Maybe (Maybe)
import Effect.Aff (Aff)

withBalancerConstraints
  :: forall (a :: Type)
   . Array Address
  -> NetworkId
  -> ProtocolParameters
  -> Provider
  -> LogLevel
  -> Maybe (LogLevel -> Message -> Aff Unit)
  -> BalancerConstraints
  -> ReaderT BalanceTxMContext Aff a
  -> Aff a
withBalancerConstraints
  ownAddresses
  networkId
  protocolParameters
  provider
  logLevel
  customLogger
  constraintsBuilder
  m = do
  -- we can ignore failures due to reward addresses because reward addresses
  -- do not receive transaction outputs from dApps
  flip runReaderT
    { constraints
    , ownAddresses
    , networkId
    , protocolParameters
    , provider
    , logLevel
    , customLogger
    }
    m
  where
  constraints :: BalancerConfig
  constraints = buildBalancerConfig constraintsBuilder

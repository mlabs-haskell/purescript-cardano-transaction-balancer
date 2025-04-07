module Cardano.Transaction.Balancer.API
  ( balanceTransaction
  ) where

import Prelude

import Cardano.Provider (Provider)
import Cardano.Transaction.Balancer (BalancerParams)
import Cardano.Transaction.Balancer.Constraints (BalancerConstraints)
import Cardano.Transaction.Balancer.Contract (withBalancerConstraints)
import Cardano.Transaction.Balancer.Error (BalanceTxError)
import Cardano.Transaction.Balancer.Strategy (BalancerStrategy, defaultBalancer, runTransactionBalancing)
import Cardano.Types.Address (Address)
import Cardano.Types.NetworkId (NetworkId)
import Cardano.Types.ProtocolParameters (ProtocolParameters)
import Cardano.Types.Transaction (Transaction)
import Control.Monad.Except.Trans (runExceptT)
import Data.Either (Either)
import Data.Log.Level (LogLevel)
import Data.Log.Message (Message)
import Data.Maybe (Maybe, fromMaybe)
import Effect.Aff (Aff)

balanceTransaction
  :: Array Address
  -> NetworkId
  -> ProtocolParameters
  -> Provider
  -> LogLevel
  -> Maybe (LogLevel -> Message -> Aff Unit)
  -> BalancerConstraints
  -> BalancerParams
  -> Maybe BalancerStrategy
  -> Aff (Either BalanceTxError Transaction)
balanceTransaction ownAddrs nid pparams prov lvl customLog constr params mStrat =
  let
    strat = fromMaybe defaultBalancer mStrat
  in
    withBalancerConstraints ownAddrs nid pparams prov lvl customLog constr
      $ runExceptT
      $ runTransactionBalancing strat params

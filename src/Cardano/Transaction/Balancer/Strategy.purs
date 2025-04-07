module Cardano.Transaction.Balancer.Strategy
  ( module X
  , BalancerStrategy
  , defaultBalancer
  , runTransactionBalancing
  ) where

import Cardano.Transaction.Balancer (BalancerParams)
import Cardano.Transaction.Balancer (BalancerParams) as X
import Cardano.Transaction.Balancer as Internal
import Cardano.Transaction.Balancer.Types (BalanceTxM)
import Cardano.Types.Transaction (Transaction)

type BalancerStrategy =
  { balanceTransaction ::
      BalancerParams
      -> BalanceTxM Transaction
  }

--------------------------------------------------------------------------------
--  Default strategy
--------------------------------------------------------------------------------
defaultBalancer :: BalancerStrategy
defaultBalancer =
  { balanceTransaction:
      Internal.runDefaultBalancer
  }

runTransactionBalancing
  :: BalancerStrategy
  -> BalancerParams
  -> BalanceTxM Transaction
runTransactionBalancing strat = strat.balanceTransaction

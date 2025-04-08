module Cardano.Transaction.Balancer.Types.ProtocolParameters
  ( BalancerProtocolParameters
  ) where

import Cardano.Types (Coin, CostModel, ExUnitPrices, Language, Rational)
import Data.Map (Map)
import Data.UInt (UInt)

-- | A subset of protocol parameters used by the balancing algorithm.
-- | Compatible with `Cardano.Types.ProtocolParameters`.
type BalancerProtocolParameters (r :: Row Type) =
  ( txFeeFixed :: Coin
  , txFeePerByte :: UInt
  , stakeAddressDeposit :: Coin
  , coinsPerUtxoByte :: Coin
  , costModels :: Map Language CostModel
  , prices :: ExUnitPrices
  , maxCollateralInputs :: UInt
  , refScriptCoinsPerByte :: Rational
  | r
  )

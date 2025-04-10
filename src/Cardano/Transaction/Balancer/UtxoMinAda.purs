module Cardano.Transaction.Balancer.UtxoMinAda
  ( adaOnlyUtxoMinAdaValue
  , utxoMinAdaValue
  ) where

import Prelude

import Cardano.Types.BigNum (BigNum)
import Cardano.Types.BigNum (maxValue) as BigNum
import Cardano.Types.Coin (Coin)
import Cardano.Types.TransactionOutput (TransactionOutput, minAdaForOutput)
import Cardano.Types.Value (lovelaceValueOf)
import Cardano.Transaction.Balancer.FakeOutput (fakeOutputWithValue)
import Data.Newtype (unwrap)

utxoMinAdaValue
  :: Coin -> TransactionOutput -> BigNum
utxoMinAdaValue coinsPerUtxoByte txOutput =
  unwrap $ minAdaForOutput txOutput (unwrap coinsPerUtxoByte)

adaOnlyUtxoMinAdaValue :: Coin -> BigNum
adaOnlyUtxoMinAdaValue coinsPerUtxoByte =
  utxoMinAdaValue coinsPerUtxoByte <<<
    fakeOutputWithValue
    $ lovelaceValueOf BigNum.maxValue

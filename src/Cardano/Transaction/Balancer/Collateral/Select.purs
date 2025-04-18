module Cardano.Transaction.Balancer.Collateral.Select
  ( maxCandidateUtxos
  , minRequiredCollateral
  , selectCollateral
  ) where

import Prelude

import Cardano.Types.BigNum (BigNum)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Coin (Coin)
import Cardano.Types.Coin as Coin
import Cardano.Types.MultiAsset (MultiAsset)
import Cardano.Types.MultiAsset as MultiAsset
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.TransactionOutput (TransactionOutput)
import Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  )
import Cardano.Types.UtxoMap (UtxoMap)
import Cardano.Types.Value as Value
import Cardano.Transaction.Balancer.FakeOutput (fakeOutputWithMultiAssets)
import Cardano.Transaction.Balancer.Helpers (unsafeFromJust)
import Cardano.Transaction.Balancer.UtxoMinAda (utxoMinAdaValue)
import Data.Array as Array
import Data.Foldable (foldl)
import Data.Function (on)
import Data.List (List(Nil, Cons))
import Data.List as List
import Data.Map (toUnfoldable) as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Ordering (invert) as Ordering
import Data.Tuple (Tuple(Tuple))
import Data.Tuple (fst, snd) as Tuple
import Data.Tuple.Nested (type (/\), (/\))

minRequiredCollateral :: Coin
minRequiredCollateral = wrap $ BigNum.fromInt 5_000_000

-- | A constant that limits the number of candidate utxos for collateral
-- | selection, thus maintaining acceptable time complexity.
maxCandidateUtxos :: Int
maxCandidateUtxos = 10

--------------------------------------------------------------------------------
-- Select Collateral
--------------------------------------------------------------------------------

collateralReturnMinAdaValue
  :: Coin -> List TransactionUnspentOutput -> Maybe BigNum
collateralReturnMinAdaValue coinsPerUtxoByte =
  pure <<< utxoMinAdaValue coinsPerUtxoByte <<< fakeOutputWithMultiAssets <=<
    MultiAsset.sum <<< Array.fromFoldable <<< map nonAdaAsset

type ReturnOutMinAdaValue = BigNum

newtype CollateralCandidate =
  CollateralCandidate (List TransactionUnspentOutput /\ ReturnOutMinAdaValue)

derive instance Newtype CollateralCandidate _

instance Eq CollateralCandidate where
  eq = eq `on` (Tuple.snd <<< unwrap)

instance Ord CollateralCandidate where
  compare lhs rhs =
    caseEq (on compare byReturnOutMinAda lhs rhs) $
      -- If two candidate utxo combinations correspond to return outputs with
      -- the same utxo min ada value, order them by the number of
      -- collateral inputs:
      caseEq (on compare byNumOfInputs lhs rhs)
        -- If two candidate utxo combinations have the same number of inputs,
        -- order them by ada value:
        (on compare byAdaValue lhs rhs)
    where
    caseEq :: Ordering -> Ordering -> Ordering
    caseEq EQ ordering = ordering
    caseEq ordering _ = ordering

    byReturnOutMinAda :: CollateralCandidate -> ReturnOutMinAdaValue
    byReturnOutMinAda = Tuple.snd <<< unwrap

    byNumOfInputs :: CollateralCandidate -> Int
    byNumOfInputs = List.length <<< Tuple.fst <<< unwrap

    byAdaValue :: CollateralCandidate -> Coin
    byAdaValue = foldl consumeUtxoAdaValue Coin.zero <<< Tuple.fst <<< unwrap

mkCollateralCandidate
  :: List TransactionUnspentOutput /\ Maybe ReturnOutMinAdaValue
  -> Maybe CollateralCandidate
mkCollateralCandidate (unspentOutputs /\ returnOutMinAdaValue) =
  CollateralCandidate <<< Tuple unspentOutputs <$> returnOutMinAdaValue

-- | Selects an utxo combination to use as collateral by generating all possible
-- | utxo combinations and then applying the following constraints:
-- |
-- |   1. `maxCollateralInputs` protocol parameter limits the maximum
-- |   cardinality of a single utxo combination.
-- |
-- |   2. Collateral inputs must have a total value of at least 5 Ada
-- |   (`minRequiredCollateral`).
-- |
-- |   3. We prefer utxo combinations that require the lowest utxo min ada
-- |   value for the corresponding collateral output, thus maintaining a
-- |   sufficient `totalCollateral`.
-- |
-- |   4. If two utxo combinations correspond to return outputs with the same
-- |   utxo min ada value, we prefer the one with fewer inputs.
-- |
selectCollateral
  :: Coin
  -> Int
  -> UtxoMap
  -> Maybe (List TransactionUnspentOutput)
selectCollateral coinsPerUtxoUnit maxCollateralInputs =
  -- Sort candidate utxo combinations in ascending order by utxo min ada value
  -- of return output, then select the first utxo combination:
  map (Tuple.fst <<< unwrap) <<< List.head <<< List.sort
    -- For each candidate utxo combination calculate
    -- the min Ada value of the corresponding collateral return output:
    <<< List.mapMaybe mkCollateralCandidate
    <<< map (\x -> Tuple x $ collateralReturnMinAdaValue coinsPerUtxoUnit x)
    -- Filter out all utxo combinations
    -- with total Ada value < `minRequiredCollateral`:
    <<< List.filter
      (\x -> foldl consumeUtxoAdaValue (Coin.zero) x >= minRequiredCollateral)
    -- Get all possible non-empty utxo combinations
    -- with the number of utxos <= `maxCollateralInputs`:
    <<< combinations maxCollateralInputs
    -- Limit the number of candidate utxos for collateral selection to
    -- maintain acceptable time complexity:
    <<< List.take maxCandidateUtxos
    <<< map unwrap
    -- Sort utxos by ada value in decreasing order:
    <<< List.sortBy (\lhs -> Ordering.invert <<< compare lhs)
    <<< map (AdaOut <<< asTxUnspentOutput)
    <<< Map.toUnfoldable

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | A wrapper around an utxo with ordering by ada value.
newtype AdaOut = AdaOut TransactionUnspentOutput

derive instance Newtype AdaOut _

instance Eq AdaOut where
  eq = eq `on` (adaValue <<< unwrap)

instance Ord AdaOut where
  compare = compare `on` (adaValue <<< unwrap)

asTxUnspentOutput
  :: TransactionInput /\ TransactionOutput -> TransactionUnspentOutput
asTxUnspentOutput (input /\ output) = wrap { input, output }

adaValue :: TransactionUnspentOutput -> Coin
adaValue =
  Value.getCoin <<< _.amount <<< unwrap <<< _.output <<< unwrap

consumeUtxoAdaValue :: Coin -> TransactionUnspentOutput -> Coin
consumeUtxoAdaValue acc = unsafeFromJust "consumeUtxoAdaValue" <<< Coin.add acc
  <<< adaValue

nonAdaAsset :: TransactionUnspentOutput -> MultiAsset
nonAdaAsset =
  Value.getMultiAsset <<< _.amount <<< unwrap <<< _.output <<< unwrap

-- | Returns a list of all subsequences of the given list.
subsequences :: forall (a :: Type). List a -> List (List a)
subsequences Nil = Cons Nil Nil
subsequences (Cons x xs) =
  let subs = subsequences xs in map (Cons x) subs <> subs

-- | Generates all possible combinations of list elements with the number of
-- | elements in each combination not exceeding `k` (no repetitions, no order).
combinations :: forall (a :: Type). Int -> List a -> List (List a)
combinations k =
  List.filter (\x -> List.length x <= k && not (List.null x))
    <<< subsequences

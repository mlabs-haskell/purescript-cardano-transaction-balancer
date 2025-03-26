module Cardano.Transaction.Balancer.MinFee
  ( calculateMinFee
  , CalculateMinFeeData
  , getMaximumSigners
  ) where

import Prelude

import Cardano.Data.Lite
  ( linearFee_new
  , minFee
  , minRefScriptFee
  , minScriptFee
  )
import Cardano.Provider (Provider)
import Cardano.Types
  ( Certificate
      ( StakeRegistration
      , StakeDeregistration
      , StakeDelegation
      , PoolRegistration
      , PoolRetirement
      , VoteDelegCert
      , StakeVoteDelegCert
      , StakeRegDelegCert
      , VoteRegDelegCert
      , StakeVoteRegDelegCert
      , AuthCommitteeHotCert
      , ResignCommitteeColdCert
      , RegDrepCert
      , UnregDrepCert
      , UpdateDrepCert
      )
  , Coin
  , Credential
  , Ed25519KeyHash
  , RewardAddress
  , Transaction
  , UtxoMap
  , Vkey(Vkey)
  , Vkeywitness(Vkeywitness)
  , Voter(Cc, Drep, Spo)
  , _body
  , _certs
  , _collateral
  , _inputs
  , _vkeys
  , _withdrawals
  , _witnessSet
  )
import Cardano.Types.Address (Address, getPaymentCredential, getStakeCredential)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Credential (asPubKeyHash)
import Cardano.Types.Credential (asPubKeyHash) as Credential
import Cardano.Types.Ed25519Signature as Ed25519Signature
import Cardano.Types.ExUnitPrices as ExUnitPrices
import Cardano.Types.NativeScript
  ( NativeScript
      ( ScriptPubkey
      , ScriptAll
      , ScriptAny
      , ScriptNOfK
      , TimelockStart
      , TimelockExpiry
      )
  )
import Cardano.Types.ProtocolParameters (ProtocolParameters(ProtocolParameters))
import Cardano.Types.PublicKey as PublicKey
import Cardano.Types.Rational as Rational
import Cardano.Types.Transaction as Transaction
import Cardano.Types.TransactionBody (_votingProcedures)
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.UnitInterval as UnitInterval
import Control.Monad.Error.Class (class MonadThrow)
import Cardano.Transaction.Balancer.Helpers (liftM, liftedM, unsafeFromJust)
import Data.Array (fromFoldable, mapMaybe)
import Data.Array (fromFoldable, nub, range, replicate) as Array
import Data.ByteArray as BytesArray
import Data.Either (hush)
import Data.Foldable (fold, foldl, foldr, maximumBy)
import Data.Function (on)
import Data.Int (hexadecimal) as Radix
import Data.Int (toNumber, toStringAs) as Int
import Data.Lens (view, (.~))
import Data.Lens.Getter ((^.))
import Data.List (List(Cons, Nil), (:))
import Data.List as List
import Data.Map (keys, lookup, values) as Map
import Data.Maybe (Maybe(Just, Nothing), fromJust, fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Set (Set)
import Data.Set
  ( difference
  , empty
  , fromFoldable
  , insert
  , intersection
  , isEmpty
  , mapMaybe
  , member
  , singleton
  , size
  , union
  ) as Set
import Data.String (length) as String
import Data.Traversable (for)
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect.Aff (Aff, error)
import Effect.Class (class MonadEffect)
import Effect.Exception (Error)
import Partial.Unsafe (unsafePartial)

type CalculateMinFeeData =
  { ownAddrs :: Array Address
  , provider :: Provider
  , protocolParameters :: ProtocolParameters
  }

-- | Calculate the minimum transaction fee.
calculateMinFee
  :: CalculateMinFeeData
  -> Transaction
  -> UtxoMap
  -> UInt
  -> Aff Coin
calculateMinFee mfData tx additionalUtxos refScriptsSize =
  do
    selfSigners <- getSelfSigners mfData tx
      additionalUtxos
    calculateMinFeeCdl mfData.protocolParameters selfSigners tx refScriptsSize

-- | This function estimates the set of keys that must be used
-- | for signing to make the transaction valid for the network.

getSelfSigners
  :: CalculateMinFeeData
  -> Transaction
  -> UtxoMap
  -> Aff (Set Ed25519KeyHash)
getSelfSigners mfData tx additionalUtxos = do
  -- Get all tx inputs and remove the additional ones.
  let
    txInputs :: Set TransactionInput
    txInputs =
      Set.difference
        (Set.fromFoldable $ tx ^. _body <<< _inputs)
        (Map.keys additionalUtxos)

    additionalUtxosAddrs :: Set Address
    additionalUtxosAddrs = Set.fromFoldable $
      (_.address <<< unwrap) <$> Map.values additionalUtxos

  (inUtxosAddrs :: Set Address) <- setFor txInputs $ \txInput ->
    liftedM (error $ "Couldn't get tx output for " <> show txInput)
      $ (map <<< map) (_.address <<< unwrap)
      $
        case Map.lookup txInput additionalUtxos of -- AffE (Maybe TransactionOutput)
          Nothing ->
            (mfData.provider.getUtxoByOref txInput <#> hush >>> join)
          Just utxo -> pure $ Just utxo

  let
    collateralInputs = tx ^. _body <<< _collateral
    (ownAddrs :: Set Address) = Set.fromFoldable mfData.ownAddrs

  (collateralAddresses :: Set Address) <-
    setFor (Set.fromFoldable collateralInputs) $ \txInput ->
      liftedM (error $ "Couldn't get tx output for " <> show txInput)
        $ (map <<< map) (_.address <<< unwrap)
        $ case Map.lookup txInput additionalUtxos of
            Nothing -> mfData.provider.getUtxoByOref txInput <#> hush >>> join
            Just utxo -> pure $ Just utxo

  -- Combine to get all self tx input addresses
  let
    txOwnAddrs =
      (additionalUtxosAddrs `Set.union` ownAddrs) `Set.intersection`
        (inUtxosAddrs `Set.union` collateralAddresses)

  -- Extract payment pub key hashes from addresses.
  paymentPkhs <- map (Set.mapMaybe identity) $ setFor txOwnAddrs $ \addr -> do
    paymentCred <-
      liftM
        ( error $ "Could not extract payment credential from Address: " <> show
            addr
        ) $ getPaymentCredential addr
    pure $ asPubKeyHash $ unwrap paymentCred

  -- Extract stake pub key hashes from addresses
  let
    stakePkhs = Set.fromFoldable $
      (asPubKeyHash <<< unwrap <=< getStakeCredential) `mapMaybe`
        Array.fromFoldable txOwnAddrs

  -- Extract signers for certificates, withdrawals, and voting procedures
  let
    certsPkhs = getSignersForCerts tx
    withdrawalsPkhs = getSignersForWithdrawals tx
    votingProceduresPkhs = getSignersForVotingProcedures tx

  pure $ paymentPkhs <> stakePkhs <> certsPkhs <> withdrawalsPkhs
    <> votingProceduresPkhs
  where
  setFor
    :: forall (a :: Type) (b :: Type)
     . Ord a
    => Ord b
    => Set a
    -> (a -> Aff b)
    -> Aff (Set b)
  setFor txIns f = do
    arr <- for (fromFoldable txIns) f
    pure $ Set.fromFoldable arr

getSignersForCerts :: Transaction -> Set Ed25519KeyHash
getSignersForCerts = foldl worker Set.empty <<< view (_body <<< _certs)
  where
  worker :: Set Ed25519KeyHash -> Certificate -> Set Ed25519KeyHash
  worker acc =
    case _ of
      StakeRegistration _ -> acc
      StakeDeregistration cred -> addSigner $ unwrap cred
      StakeDelegation cred _ -> addSigner $ unwrap cred
      PoolRegistration poolParams -> Set.insert
        (unwrap (unwrap poolParams).operator)
        acc
      PoolRetirement { poolKeyHash } -> Set.insert (unwrap poolKeyHash) acc
      VoteDelegCert cred _ -> addSigner $ unwrap cred
      StakeVoteDelegCert cred _ _ -> addSigner $ unwrap cred
      StakeRegDelegCert cred _ _ -> addSigner $ unwrap cred
      VoteRegDelegCert cred _ _ -> addSigner $ unwrap cred
      StakeVoteRegDelegCert cred _ _ _ -> addSigner $ unwrap cred
      AuthCommitteeHotCert { coldCred } -> addSigner coldCred
      ResignCommitteeColdCert cred _ -> addSigner cred
      RegDrepCert cred _ _ -> addSigner cred
      UnregDrepCert cred _ -> addSigner cred
      UpdateDrepCert cred _ -> addSigner cred
    where
    addSigner :: Credential -> Set Ed25519KeyHash
    addSigner = maybe acc (flip Set.insert acc) <<< Credential.asPubKeyHash

getSignersForWithdrawals :: Transaction -> Set Ed25519KeyHash
getSignersForWithdrawals =
  foldl worker Set.empty <<< Map.keys <<< view (_body <<< _withdrawals)
  where
  worker :: Set Ed25519KeyHash -> RewardAddress -> Set Ed25519KeyHash
  worker acc =
    maybe acc (flip Set.insert acc) <<< Credential.asPubKeyHash <<< unwrap
      <<< _.stakeCredential

getSignersForVotingProcedures :: Transaction -> Set Ed25519KeyHash
getSignersForVotingProcedures =
  foldl worker Set.empty <<< Map.keys <<< unwrap
    <<< view (_body <<< _votingProcedures)
  where
  worker :: Set Ed25519KeyHash -> Voter -> Set Ed25519KeyHash
  worker acc =
    case _ of
      Cc cred -> addSigner cred
      Drep cred -> addSigner cred
      Spo poolKeyHash -> Set.insert poolKeyHash acc
    where
    addSigner :: Credential -> Set Ed25519KeyHash
    addSigner = maybe acc (flip Set.insert acc) <<< Credential.asPubKeyHash

-- | `min_fee` calculation using CDL.

calculateMinFeeCdl
  :: forall (m :: Type -> Type)
   . MonadEffect m
  => MonadThrow Error m
  => ProtocolParameters
  -> Set Ed25519KeyHash
  -> Transaction
  -> UInt
  -> m Coin
calculateMinFeeCdl
  (ProtocolParameters pparams)
  selfSigners
  txNoSigs
  refScriptsSize = do
  let
    tx = addFakeSignatures selfSigners txNoSigs
    cslTx = Transaction.toCdl tx
    cslLinearFee = linearFee_new
      (unwrap $ BigNum.fromUInt pparams.txFeePerByte)
      (unwrap $ unwrap pparams.txFeeFixed)
    fee = minFee cslTx cslLinearFee
    exUnitPrices = pparams.prices
    exUnitPricesCsl = ExUnitPrices.toCdl exUnitPrices
    scriptFee = minScriptFee cslTx exUnitPricesCsl
    refScriptFee =
      minRefScriptFee
        (Int.toNumber $ UInt.toInt refScriptsSize)
        ( UnitInterval.toCdl
            $ unsafeFromJust "calculateMinFeeCdl: refScriptCoinsPerByte"
            $ Rational.toUnitInterval pparams.refScriptCoinsPerByte
        )
  -- Ignore the overflow here: fees are much lower
  pure $ wrap $ unsafeFromJust "calculateMinFeeCdl" $
    BigNum.add (wrap fee)
      (wrap scriptFee) >>= BigNum.add (wrap refScriptFee)

-- | Adds fake signatures for each expected signature of a transaction.
addFakeSignatures :: Set Ed25519KeyHash -> Transaction -> Transaction
addFakeSignatures selfSigners tx =
  let
    -- `requiredSigners` field of the transaction
    requiredSigners :: Set Ed25519KeyHash
    requiredSigners =
      tx # unwrap >>> _.body >>> unwrap >>> _.requiredSigners
        >>> Set.fromFoldable

    requiredAndSelfSigners :: Set Ed25519KeyHash
    requiredAndSelfSigners = requiredSigners <> selfSigners

    -- All possible signers from native scripts.
    numNativeScriptSigners :: Int
    numNativeScriptSigners =
      getMaximumSigners requiredAndSelfSigners $
        ScriptAll
          ( tx # unwrap >>> _.witnessSet >>> unwrap >>> _.nativeScripts
          )

    numFakeSigs :: Int
    numFakeSigs =
      Set.size requiredAndSelfSigners
        + numNativeScriptSigners
        -- We want to add space for required signatures
        -- (at least one, if none specified).
        + if Set.isEmpty selfSigners then one else zero
  in
    -- Generate unique vkeys because Vkeywitnesses now has Set
    -- semantics.
    tx # _witnessSet <<< _vkeys .~ map mkFakeVkeyWitness
      (Array.range one numFakeSigs)

mkFakeVkeyWitness :: Int -> Vkeywitness
mkFakeVkeyWitness n = Vkeywitness
  { vkey:
      Vkey
        ( let
            nHex = Int.toStringAs Radix.hexadecimal n
          in
            unsafeFromJust "Ctl.Internal.Serialization.MinFee.mkFakeVkeyWitness"
              ( fold (Array.replicate (64 - String.length nHex) "0") <> nHex #
                  ( PublicKey.fromRawBytes
                      <=< (map wrap <<< BytesArray.hexToByteArray)
                  )
              )
        )
  , signature:
      ( unsafePartial $ fromJust $ Ed25519Signature.fromBech32
          "ed25519_sig1mr6pm5kanam2wkmae70jx7fjkzepghefj0lmnczu6fra\
          \6auf2urgrte5axxhunw4x34l3l8tj9c0t4le39tj8lpjdgxmqnujw07t\
          \kzs9m6t6x"
      )
  }

-- | `SetChoice` is an internal type representing internal state of
-- | `getMaximumSigners` algorithm.
type SetChoice (a :: Type) = Array (Set a)

-- | Used for fee calculation.
-- | We try to calculate maximum number of signers from the script itself,
-- | following its logic.
-- | But we must not count `requiredSigners` and `selfSigners` as signers from
-- | native scripts twice, because that would lead to excessive fees. Hence we
-- | accept a set of already known signers to be ignored in this function.
getMaximumSigners :: Set Ed25519KeyHash -> NativeScript -> Int
getMaximumSigners alreadyCounted =
  sizes >>> maximumBy (compare `on` Set.size) >>> map Set.size >>> fromMaybe 0
  where
  sizes :: NativeScript -> SetChoice Ed25519KeyHash
  sizes = case _ of
    ScriptPubkey kh
      | Set.member kh alreadyCounted -> emptySetChoice
      | otherwise -> [ Set.singleton kh ]
    ScriptAll nss -> foldr allChoice emptySetChoice
      (sizes <$> nss)
    ScriptAny nss -> foldr anyChoice emptySetChoice
      (sizes <$> nss)
    ScriptNOfK n nss -> sizes
      (ScriptAny $ map ScriptAll (subsetsOfLength n nss))
    TimelockStart _ -> emptySetChoice
    TimelockExpiry _ -> emptySetChoice

  emptySetChoice :: forall (a :: Type). SetChoice a
  emptySetChoice = [ Set.empty ]

  anyChoice
    :: forall (a :: Type). Ord a => SetChoice a -> SetChoice a -> SetChoice a
  anyChoice as bs = Array.nub $ as <> bs

  allChoice
    :: forall (a :: Type). Ord a => SetChoice a -> SetChoice a -> SetChoice a
  allChoice as bs = (<>) <$> as <*> bs

  subsetsOfLength :: forall (a :: Type). Int -> Array a -> Array (Array a)
  subsetsOfLength n =
    List.fromFoldable >>> sublists n >>> List.toUnfoldable >>> map
      List.toUnfoldable

  sublists :: forall (a :: Type). Int -> List a -> List (List a)
  sublists n xs = List.take (List.length xs - n + 1) $ sublists' n xs
    where
    sublists' :: Int -> List a -> List (List a)
    sublists' _ Nil = Cons Nil Nil
    sublists' n' xs'@(Cons _ rest) = List.take n' xs' : sublists' n' rest

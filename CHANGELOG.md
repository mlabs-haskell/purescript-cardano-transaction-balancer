# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and we follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

# [Unreleased]

## Changed

- Van Rossem (PV11) support: reference-input UTxOs are excluded from spendable
  selection only when the tx executes a Plutus V3 script. At PV11+ the ledger
  allows input/reference-input overlap for non-V3 transactions, so the previous
  unconditional exclusion was overly conservative.
  ([#5](https://github.com/mlabs-haskell/purescript-cardano-transaction-balancer/pull/5))

- `txHasPlutus` (replacing `txHasPlutusV1`) now also scans reference scripts
  attached to the tx's inputs. For V2/V3, both spending and reference inputs
  are scanned. For V1, only reference inputs are scanned - a V1 script on a
  spending input's `scriptRef` cannot be invoked, since V1 execution fails
  in the presence of reference scripts on spent inputs.
  ([#5](https://github.com/mlabs-haskell/purescript-cardano-transaction-balancer/pull/5))

## Fixed

- `setScriptDataHash` no longer emits a script integrity hash for transactions
  that carry an unused Plutus script in the witness set but have no redeemers.
  The guard now mirrors the ledger rule (`null redeemers && null datums`),
  preventing spurious `PPViewHashesDontMatch` / `ScriptIntegrityHashMismatch`
  failures. ([#5](https://github.com/mlabs-haskell/purescript-cardano-transaction-balancer/pull/5))

# [v1.1.0]

## Added

- New balancer constraint `mustNotSpendUtxosWhere`, which allows marking utxos as non-spendable
using arbitrary predicates ([#3](https://github.com/mlabs-haskell/purescript-cardano-transaction-balancer/pull/3))

##  Changed

- Improved handling of non-spendable utxos in collateral selection ([#4](https://github.com/mlabs-haskell/purescript-cardano-transaction-balancer/pull/4))
  - Now, utxos for collateral are always selected from the set of spendable, non-script utxos
  - In case no collateral is specified via the `mustUseCollateralUtxos` constraint, and the
    spendable wallet-provided collateral fails to cover the minimum required amount, the
    balancer now falls back to the internal collateral selection algorithm.
    See CTL issue [#1581](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1581).

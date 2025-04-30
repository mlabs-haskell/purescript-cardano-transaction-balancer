# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and we follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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

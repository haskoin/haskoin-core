# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## 0.9.1
### Added
- Adds a function to produce a structured signature over a transaction

## 0.9.0
### Changed
- Address conversion to string now defined for all inputs.

## 0.8.4
### Added
- Add reward computation to block functions.
- Add PSBT [BIP-174](https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki) types and functions

## 0.8.3
### Added
- Add reward halving interval parameter to network constants.

## 0.8.2
### Added
- Recognize `OP_CHECKDATASIG` and `OP_CHECKDATASIGVERIFY` opcodes.

## 0.8.1
### Added
- Add instances of `Hashable` and `Generic` where possible.

## 0.8.0
### Removed
- Remove `deepseq` dependency.
- Remove network constant reference from address and extended keys.

## 0.7.0
### Added
- Add `Serialize` instance for network constants.
- Add `Serialize` instance for addresses that includes network constants.

### Changed
- Move functions related to addresses from `Script` to `Address` module.

## 0.6.1
### Added
- Compatibility with latest GHC and base.

### Changed
- Update minimum base to 4.9.

## 0.6.0
### Changed
- Force initialization of addresses through smart constructor.
- Assume addresses are always valid when instantiated in code.
- Allow to provide unwrapped private keys to transaction signing functions.

## 0.5.2
### Changed
- Make dependencies more specific.

## 0.5.1
### Changed
- Remove some unneeded dependencies from `stack.yaml`.
- Change `secp256k1` dependency to `secp256k1-haskell`.

## 0.5.0
### Added
- Support for Bitcoin Cash network block sychronization.
- Support for Bitcoin Cash signatures.
- Initial work on SegWit support.
- New version of `secp256k1` bindings.
- Block header validation.
- Support for RegTest networks on Bitcoin and Bitcoin Cash.
- Support for Bitcoin Cash Testnet3 Network.
- Support for new Haskoin Wallet.
- Minikey decoding for Casascius coins.
- New tests for various networks and new features.
- Added `CHANGELOG.md` file.
- Support for SegWit addresses.
- Support for CashAddr addresses.

### Changed
- Use of hpack `package.yaml` file to auto-generate Cabal file.
- Removal of dependency version limits, relying on `stack.yaml` instead.
- Tests moved to `hspec`.
- New documentation.
- Updated `.gitignore`.
- Renamed network constants to use same style for BTC and BCH.
- Network constants must be passed explicitly.
- Target LTS Haskell 12.9.

### Removed
- Removed `.stylish-haskell.yaml` files.
- Removed old `haskoin-node` and `haskoin-wallet` packages from main repository.
- Removed support for non-strict signatures and related tests.
- Removed script evaluator and related tests.

# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

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

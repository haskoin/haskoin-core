# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## 0.21.0
### Changed
- Use a newtype for Fingerprint, which uses an 8 digit hex string for various
  instances.  This fixes inconsistent (de)serialization across the package.

### Fixed
- Makes `finalScriptWitness` field encoding conform to bitcoin core.
- Fixes bug in `finalizeTransaction`

### Added
- Signing support for PSBTs
- Helper function for merging PSBTs
- More PSBT tests
- Partial support for taproot

## 0.20.5
### Added
- Support Bech32m address format for Taproot.

## 0.20.4
### Fixed
- Add missing case for witness version.

## 0.20.3
### Fixed
- Allow unknown inv types.

## 0.20.2
### Fixed
- Allow unknown messages of zero length.

## 0.20.1
### Fixed
- Correct case where binary search returned the wrong element.

## 0.20.0
### Chaged
- Use bytes instead of binary or cereal.

## 0.19.0
### Added
- Hashable instances for extended keys.

### Changed
- Mnemonic passphrases now `Text` instead of `ByteString`.

### Fixed
- Tests now pass for witness addresses.

## 0.18.0
### Added
- Support SegWit addresses with version other than 0.

## 0.17.6
### Added
- Serialize instances for `XPubKey` and `XPrvKey`.

## 0.17.5
### Fixed
- Handle special case in block header binary search function.

## 0.17.4
### Fixed
- Bounds check too restrictive in block header binary search function.

## 0.17.3
### Changed
- Reduce minimum version of text package dependency.

## 0.17.2
### Changed
- Update lists of seeds for all networks.

## 0.17.1
### Changed
- Use the C-preprocessor to handle versions of `base16-bytestring` including 1.0
  (with a breaking API change)

## 0.17.0
### Added
- Support for Bitcoin Cash November 2020 hard fork.
- Functions to find block headers matching arbitrary sorted attributes.

### Removed
- GenesisNode constructor for BlockNode type.

## 0.15.0
### Added
- Add more test vectors

### Changed
- stringToAddr renamed to textToAddr
- Move ScriptOutput to Standard.hs
- Move WIF encoding/decoding to Keys.hs
- (breaking) rename `OP_NOP2` and `OP_NOP3` to `OP_CHECKLOCKTIMEVERIFY` and
  `OP_CHECKSEQUENCEVERIFY` resp.
- Update to latest secp256k1 bindings.

## 0.14.1
### Fixed
- Correct some Bitcoin Cash Testnet3 seeds.
- Add helpers for writing Data.Serialize and Data.Aeson identity tests

## 0.14.0
### Changed
- Expose all modules for tests.
- Tests depend on library instead of having access to its source code.
- Use MIT license.
- Update seeds.
- Bump secp256k1-haskell dependency.

## 0.13.6
### Changed
- Expose the Arbitrary test instances under Haskoin.Util.Arbitrary

## 0.13.5
### Changed
- Provide meaningful JSON instances for most types.

## 0.13.4
### Added
- Support for Bitcoin Cash May 2020 hard fork.

## 0.13.3
### Changed
- Improve code and documentation organisation.

## 0.13.2
### Changed
- Move all packages from Network.Haskoin namespace to Haskoin namespace.
- Expose all top-level modules directly.

## 0.13.1
### Changed
- Faster JSON serialization.

## 0.13.0
### Changed
- Consolidate all modules in Haskoin module.

### Removed
- Deprecate Network.Haskoin namespace.
- Hide QuickCheck generators in test suite.

## 0.12.0
### Added
- Support for signing segwit transactions.

## 0.11.0
### Added
- High-level representation of segwit v0 data and auxilliary functions.

### Changed
- Adds handling of segwit signing parameters to transaction signing code.

## 0.10.1
### Added
- Lower bound versions for some dependencies.

## 0.10.0
### Added
- DeepSeq instances for all data types.

### Changed
- There is no `SockAddr` inside `NetworkAddress` anymore.

## 0.9.8
### Added
- Ord instance for `DerivPathI`

## 0.9.7
### Added
- JSON encoding/decoding for blocks.

### Fixed
- Fix lowercase HRP test for Bech32.

## 0.9.6
### Added
- `bloomRelevantUpdate` implementation for Bloom filters (thanks to @IlyasRidhuan).

### Fixed
- Fix for Bech32 encoding (thanks to @pavel-main).

## 0.9.5
### Added
- Expose functions added in 0.9.4.

## 0.9.4
### Added
- Support for (P2SH-)P2WPKH addresses derived from extended keys.

### Changed
- Change names of backwards-compatible P2SH-P2WPKH functions from 0.9.3.

## 0.9.3
### Added
- Some support for P2WPKH-over-P2SH addresses.

## 0.9.2
### Removed
- Disable unnecessary `-O2` optimisation added in previous version.

### Added
- Allow decoding unknown P2P messages.

## 0.9.1
### Added
- Add a function to produce a structured signature over a transaction.
- Enable `-O2` optimisations.

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

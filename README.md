# Haskoin

[![Build Status](https://travis-ci.org/haskoin/haskoin.svg?branch=master)](https://travis-ci.org/haskoin/haskoin)

Haskoin is an implementation of the Bitcoin protocol in Haskell.

## Features
Haskoin is a package implementing the core functionalities of the Bitcoin
protocol specifications. The following features are provided:

- Hashing functions (sha-256, ripemd-160)
- Base58 encoding
- BIP32 extended key derivation and parsing (m/1'/2/3)
- BIP39 mnemonic keys
- ECDSA cryptographic primitives (using the C library libsecp256k1)
- Script parsing and evaluation
- Building and signing of standard transactions (regular, multisig, p2sh)
- Parsing and manipulation of all Bitcoin protocol types
- Bloom filters and partial merkle tree library (used in SPV wallets)
- Comprehensive test suite

A wallet implementation is available in haskoin-wallet which uses both this
package and the node implementation in haskoin-node.

## Documentation

http://hackage.haskell.org/package/haskoin

You may be interested in getting the [Haskoin Wallet](https://github.com/haskoin/haskoin-wallet)
instead.

## Contributing

Contribute via GitHub pull requests.

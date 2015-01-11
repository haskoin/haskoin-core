# Haskoin

Haskoin is an implementation of the Bitcoin protocol in Haskell.

## Features

Haskoin is a package implementing the Bitcoin protocol specifications. It
is written in pure Haskell and the library is implemented mostly with pure
functions (no IO monad). It provides the following features:

- ECDSA cryptographic primitives (secp256k1)
- Hashing functions (sha-256, ripemd-160)
- Base58 encoding
- BIP32 extended key derivations
- BIP39 mnemonic key
- Script parsing and evaluation
- Building and signing of standard transactions (regular, multisig, p2sh)
- Deterministic signing (rfc-6979)
- Network protocol type parsing
- Headerchain implementation (Blockchain with headers only)
- Bloom filters and partial merkle tree library
- Headers-first SPV node implementation (network-only, no wallet)
- JSON-RPC/Stratum client library

A wallet implementation using the SPV node library is available in the
haskoin-wallet package.

## Documentation

http://hackage.haskell.org/package/haskoin

## Installing from Cabal

You can install the latest stable version of the haskoin package automatically
through the cabal package manager:

```sh
# Compile for prodnet 
cabal install haskoin

# Compile for testnet
cabal install haskoin --flags=testnet
```

## Contributing

Commits are done through GitHub pull requests.

We do a lot of our technical discussions in the IRC channel #haskoin on
chat.freenode.net.

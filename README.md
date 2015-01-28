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

## Installation

### Dependencies

You'll need Haskell Platform 2013 or 2014, pkg-config, as well as development
libraries for LevelDB, Snappy, and zlib.  On Debian/Ubuntu, use following
commands:

```sh
sudo apt-get install git haskell-platform libleveldb-dev \
    libzmq3-dev libsnappy-dev pkg-config
```

### Git

Install from Git for the latest development snapshot.

```sh
git clone https://github.com/haskoin/haskoin.git
cd haskoin
cabal install
```

### Cabal

Use cabal-install to get the latest stable version from Hackage.  On
Debian/Ubuntu operating system:

```sh
cabal install haskoin
```

## Contributing

Contribute via GitHub pull requests.

We do a lot of our technical discussions in the IRC channel #haskoin on
chat.freenode.net.

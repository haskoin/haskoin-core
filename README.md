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

A wallet implementation using the SPV node library is available in the
haskoin-wallet package.

## Documentation

http://hackage.haskell.org/package/haskoin

## Installation

### Dependencies

Get Haskell from [Stackage](http://stackage.org/).

Get pkg-config, wget and development libraries for LevelDB,
Snappy, and zlib. On Debian/Ubuntu, use following command:

```sh
sudo apt-get install git wget libleveldb-dev \
    libzmq3-dev libsnappy-dev pkg-config zlib1g-dev
```

You may install for Git or Hackage.

### Instructions for Git development version

Install from Git:

```sh
git clone https://github.com/haskoin/haskoin.git
cd haskoin
wget https://www.stackage.org/lts-1.0/cabal.config
cabal update
cabal install --force-reinstalls
```

*Note: `--force-reinstalls` needed because of newer QuickCheck version
after having installed `alex` and `happy` according to Stackage
instructions.

### Instructions for Hackage

Use cabal-install to get the latest stable version from Hackage.  On
Debian/Ubuntu operating system:

```sh
cabal update
cabal install haskoin
```

## Contributing

Contribute via GitHub pull requests.

We do a lot of our technical discussions in the IRC channel #haskoin on
chat.freenode.net.

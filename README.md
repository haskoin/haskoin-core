# Haskoin Wallet

Implementation of a Bitcoin HD (BIP32) multisig SPV Wallet in Haskell.

## Features

This package provides a Haskell SPV (simple payment verification) wallet
implementation.  It features BIP32 hierarchical-deterministic key management,
deterministic signatures (RFC-6979) and first order support for multi-signature
transactions. You can communicate with the wallet process using JSON
serialization over ØMQ socket or the supplied “hw” tool.

## Library Documentation

http://hackage.haskell.org/package/haskoin-wallet

## Installing

Get pkg-config, LevelDB, Snappy, zlib and ØMQ.
On Debian/Ubuntu systems, use these command:

```sh
sudo apt-get install git wget libleveldb-dev \
    libzmq3-dev libsnappy-dev pkg-config zlib1g-dev
```

Get [Stack](https://github.com/commercialhaskell/stack).

Clone this repository, and then install using Stack.

```sh
git clone https://github.com/haskoin/haskoin-wallet.git
cd haskoin-wallet
stack install
```

Executable `hw` will be installed in `~/.local/bin`.

## Contributing

Commits are done through GitHub pull requests.

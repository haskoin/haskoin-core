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

### Dependencies

To compile haskoin-wallet, install [haskoin](https://github.com/haskoin/haskoin) first.
Then get development libraries for LevelDB, Snappy, zlib and ØMQ.
On Debian/Ubuntu systems, use these command:

```sh
sudo apt-get install wget git libleveldb-dev libsnappy-dev \
    zlib1g-dev libzmq3-dev
```

Follow instructions below to install via Git or Hackage. Executabe
should end up in `~/.cabal/bin/hw`. You may want to add `~/.cabal/bin`
to your `PATH` or copy the file to `/usr/local/bin/hw`.

### Instructions for Git development version

Install from Git to get the latest development code:

```sh
git clone https://github.com/haskoin/haskoin-wallet
cd haskoin-wallet
cabal update
cabal install
```

### Instructions for Hackage

You can install the latest stable version of the haskoin-wallet package
automatically through the cabal package manager:

```sh
cabal update
cabal install haskoin-wallet
```

## Contributing

Commits are done through GitHub pull requests.

We do a lot of our technical discussions in the IRC channel #haskoin on
chat.freenode.net.

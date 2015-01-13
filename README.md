# Haskoin Wallet

Implementation of a Bitcoin SPV Wallet with BIP32 and multisig support.

## Features

This package provides a SPV (simple payment verification) wallet implementation.
It features BIP32 key management, deterministic signatures (RFC-6979) and first
order support for multi-signature transactions. You can communicate with the
wallet process through a ZeroMQ API or through a command-line tool called "hw"
which is also provided in this package. 

## Library Documentation

http://hackage.haskell.org/package/haskoin-wallet

## Installing

To compile haskoin-wallet, it is necessary to install GHC, the ‘cabal’ command,
and development libraries from LevelDB and Snappy. You will also need the
ZeroMQ development libraries.

```sh
# Debian/Ubuntu installation
sudo apt-get update
sudo apt-get install cabal-install libleveldb-dev libsnappy-dev zlib1g-dev
sudo apt-get install libzmq3-dev
```

You can install the latest stable version of the haskoin-wallet package
automatically through the cabal package manager:

```sh
# Update cabal package list
cabal update

# Install
cabal install haskoin-wallet
```

## Contributing

Commits are done through GitHub pull requests.

We do a lot of our technical discussions in the IRC channel #haskoin on
chat.freenode.net.

# haskoin-wallet

This package provides a command lines application called hw (haskoin wallet).
It is a lightweight bitcoin wallet featuring BIP32 key management,
deterministic signatures (RFC-6979) and first order support for multisignature
transactions. Both a Haskell and a REST api are exposed for the wallet.

This package also provides an SPV (simple payment verification) node.

## Library Documentation

http://hackage.haskell.org/package/haskoin-wallet

## REST API Documentation

https://github.com/haskoin/haskoin-wallet/blob/master/docs/haskoin-api.md

## Installing from Cabal

You can install the latest stable version of the haskoin-wallet package
automatically through the cabal package manager:

```sh
    # You may have to update your cabal package list:
    cabal update

    # Install
    cabal install haskoin-wallet

    # Run test suites and install:
    cabal install haskoin-wallet --enable-tests
```

## Contributing

Commits are done through GitHub pull requests.

We do a lot of our technical discussions in the IRC channel #haskoin on chat.freenode.net.

Code guidelines:

- 80 columns.
- 4 space indentation. No tabs.
- Follow the general style of the code, whenever it makes sense.
- Purity and type-safety are important.

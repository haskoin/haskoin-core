# haskoin-wallet

This package provides a command lines application called hw (haskoin wallet).
It is a lightweight bitcoin wallet featuring BIP32 key management,
deterministic signatures (RFC-6979) and first order support for multisignature
transactions. A library API for hw is also exposed.

## Documentation

http://hackage.haskell.org/package/haskoin-wallet

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

We're glad you want to contribute! It's simple:

- Fork haskoin-wallet
- Create a branch `git checkout -b my_branch`
- Commit your changes `git commit -am 'comments'`
- Push the branch `git push origin my_branch`
- Open a pull request

Code guidelines:

- 80 columns.
- 4 space indentation. No tabs.
- Follow the general style of the code, whenever it makes sense.

## Supporting

You can support the project by donating in [Bitcoins](http://www.bitcoin.org)
to: 176CwMCWMq1y9CxFZWk7Vfoka5PoaNzxRq


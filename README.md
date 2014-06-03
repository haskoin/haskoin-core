# Haskoin

Haskoin is a Haskell implementation of the Bitcoin protocol. It provides the
cryptographic ECDSA and hashing primitives, functions for handling BIP32
extended keys, functions for building and signing various types of regular and
multisig transactions and a definition of the network protocol messages.

## Documentation

http://hackage.haskell.org/package/haskoin

## Installing from Cabal

You can install the latest stable version of the haskoin package automatically
through the cabal package manager:

```sh
    # You need the ICU library for Unicode NFKD support
    sudo apt-get install libicu-dev

    # You may have to update your cabal package list:
    cabal update

    # Install
    cabal install haskoin

    # Run test suites and install:
    cabal install haskoin --enable-tests
```

If you have problems compiling haskoin because of issues with the text-icu
library, you may need to uninstall text-icu from your Haskell libraries to have
cabal recompile it.

```sh
    # Unregister the package from Cabal
    # If you see warnings regarding packages that will break, unregister them
    ghc-pkg unregister haskoin
    ghc-pkg unregister text-icu

    # Attempt installation again
    cabal install haskoin
```

## Benchmarks

Here are the results of some crypto benchmarks running on a single core I7:

```
    BigWord multiplication (mod n) (10000000 samples)
    Total time: 1.868806s
    Op/sec    : 5351010.217218908757s
    ----------------------------
    BigWord inversion (mod n) (100000 samples)
    Total time: 7.244239s
    Op/sec    : 13804.072449846008s
    ----------------------------
    Point multiplications (2000 samples)
    Total time: 4.372304s
    Op/sec    : 457.424735334048s
    ----------------------------
    Point additions (100000 samples)
    Total time: 0.684761s
    Op/sec    : 146036.354290036961s
    ----------------------------
    Point doubling (100000 samples)
    Total time: 0.455613s
    Op/sec    : 219484.518659476353s
    ----------------------------
    Shamirs trick (2000 samples)
    Total time: 4.366215s
    Op/sec    : 458.062646937908s
    ----------------------------
    Signature creations (2000 samples)
    Total time: 4.786426s
    Op/sec    : 417.848306857768s
    ----------------------------
    Signature verifications (2000 samples)
    Total time: 5.70411s
    Op/sec    : 350.624374354632s
```

## Contributing

We're glad you want to contribute! It's simple:

- Fork haskoin
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


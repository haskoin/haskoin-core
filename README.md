# haskoin-node

Haskoin node is an implementation of a Bitcoin node in Haskell.

## Features

Haskoin-node provides an implementation of the Bitcoin network protocol in
Haskell that allows you to synchronize headers (with SPV validation) and
download merkle blocks and full blocks. This package can be used to implement
wallets or other Bitcoin components that require talking to the Bitcoin
network. It provides the following features:

- Implementation of the Bitcoin network protocol
- Headertree implementation with SPV verification
- Headers-first synchronization
- Merkle block download from peers with bloom filters
- Full block download from peers

A wallet implementation using this package is available in haskoin-wallet.

## Documentation

http://hackage.haskell.org/package/haskoin-node

## Dependencies

* LevelDB

On Debian/Ubuntu, use following command:

```sh
sudo apt-get install libleveldb-dev
```

You may be interested in getting the [Haskoin Wallet](https://github.com/haskoin/haskoin-wallet)
instead.

## Contributing

Contribute via GitHub pull requests.

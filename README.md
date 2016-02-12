# Haskoin

[![Build Status](https://travis-ci.org/haskoin/haskoin.svg?branch=master)](https://travis-ci.org/haskoin/haskoin)

Haskoin is an implementation of the Bitcoin protocol in Haskell. There are
currently 3 main packages in Haskoin, namely haskoin-core, haskoin-node and
haskoin-wallet.

## haskoin-core

haskoin-core is a package implementing the core functionalities of the Bitcoin
protocol specifications. The following features are provided:

- Hashing functions (sha-256, ripemd-160)
- Base58 encoding
- BIP32 extended key derivation and parsing (m/1'/2/3)
- BIP39 mnemonic keys
- ECDSA cryptographic primitives (using the C library libsecp256k1)
- Script parsing and evaluation
- Building and signing of standard transactions (regular, multisig, p2sh)
- Parsing and manipulation of all Bitcoin protocol types
- Bloom filters and partial merkle tree library (used in SPV wallets)
- Comprehensive test suite

A wallet implementation is available in haskoin-wallet which uses both this
package and the node implementation in haskoin-node.

[haskoin-core hackage documentation](http://hackage.haskell.org/package/haskoin-core)

## haskoin-node

haskoin-node is essentially an SPV (simple payment verification) server node.
It implements the Bitcoin network protocol in Haskell and allows the
synchronization of headers and the download of merkle blocks. haskoin-node is
not a full node (yet) as it only support SPV verification of headers rather
than full block validation. The following features are supported:

- Implementation of the Bitcoin network protocol
- Headertree implementation with SPV verification
- Headers-first synchronization
- Merkle block download from peers with bloom filters
- Full block download from peers (without verification)

[haskoin-node hackage documentation](http://hackage.haskell.org/package/haskoin-node)

### haskoin-node dependencies

* LevelDB

On Debian/Ubuntu, use following command:

```sh
sudo apt-get install libleveldb-dev
```

## haskoin-wallet

haskoin-wallet is an SPV (simple payment verification) wallet implementation in
Haskell.  It features BIP32 hierarchical-deterministic key management,
deterministic signatures (RFC-6979) and first order support for multi-signature
transactions. You can communicate with the wallet process using JSON
serialization over ØMQ socket or the supplied `hw` tool.

[haskoin-wallet hackage documentation](http://hackage.haskell.org/package/haskoin-wallet)

### Installing haskoin-wallet

Get pkg-config, LevelDB, Snappy, zlib and ØMQ.
On Debian/Ubuntu systems, use these command:

```sh
sudo apt-get install -y git libleveldb-dev libzmq3-dev \
    libsnappy-dev pkg-config zlib1g-dev
```

Get [Stack](https://github.com/commercialhaskell/stack).

Clone this repository, and then install using Stack.

```sh
git clone https://github.com/haskoin/haskoin.git
cd haskoin
stack install
```

Executable `hw` will be installed in `~/.local/bin`.

## Contributing

Contribute via GitHub pull requests.

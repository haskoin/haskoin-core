## Haskoin RESTful API

The Haskoin API is designed to help you manage your Haskoin wallet through a web friendly REST api. It is accessible from the haskoin daemon when you start it with `hw start`.

### Overview

| Resource                                     | Verbs     | Description                                    |
| -------------------------------------------- | --------- | ---------------------------------------------- |
| [/api/wallets](#get-apiwallets)              | GET, POST | List all wallets, Create a new wallet          |
| /api/wallets/{name}                          | GET       | Get a wallet by name                           |
| /api/accounts                                | GET, POST | List all accounts, Create a new account        |
| /api/accounts/{name}                         | GET       | Get an account by name                         |
| /api/accounts/{name}/keys                    | POST      | Add keys to a multisig account                 |
| /api/accounts/{name}/addrs                   | GET, POST | List addresses, Get a new unused address       |
| /api/accounts/{name}/addrs/{key}             | GET, PUT  | Get an address by key, Update an address label |
| /api/accounts/{name}/acctxs                  | GET, POST | List txs, Send coins, Sign txs/sigblobs        |
| /api/accounts/{name}/acctxs/{txhash}         | GET       | Get a tx by account and transaction id         |
| /api/accounts/{name}/acctxs/{txhash}/sigblob | GET       | Get data to sign a transaction offline         |
| /api/accounts/{name}/balance                 | GET       | Get an account balance in satoshi              |
| /api/txs/{txhash}                            | GET       | Get a full transaction by transaction id       |
| /api/node                                    | POST      | Rescan the wallet from a given timestamp       |

### API Specification

#### GET /api/wallets

* **Output**: Returns a list of all available wallets in the following format:

```json
[
  {
    "name": "wallet1",
    "master": "xprv..."
  },
  {
    "name": "wallet2",
    "master": "xprv..."
  }
]
```

#### POST /api/wallets

* **Input**: A JSON object representing a new wallet containing a wallet name,
a passphrase and an optional BIP39 mnemonic. If no mnemonic is provided, a new
random mnemonic for this wallet will be generated. You can provide a passphrase
to protect your mnemonic sentence.

```json
{
  "walletname": "wallet1",
  "passphrase": "correct horse battery staple",
  "mnemonic": "film pig subway ..."
}
```

* **Output**: Returns the mnemonic sentence of your new wallet:

```json
{ "mnemonic": "film pig subway ..." }
```
#### GET /api/wallets/{name}

* **Input**: A wallet resource is identified by its name. For example:

```
/api/wallets/wallet1
```

* **Output**: A JSON object representing a wallet in the following format:

```json
{
  "name": "wallet1",
  "master": "xprv..."
}
```

It contains both the wallet name and the extended private key (master key) for
that wallet.



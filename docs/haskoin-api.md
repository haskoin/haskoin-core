## Haskoin RESTful API

The Haskoin API is designed to help you manage your Haskoin wallet through a web friendly REST api. It is accessible from the haskoin daemon when you start it with `hw start`.

### Overview

| Resource                                     | Verbs     | Description                                    |
| -------------------------------------------- | --------- | ---------------------------------------------- |
| /api/wallets                                 | GET, POST | List all wallets, Create a new wallet          |
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

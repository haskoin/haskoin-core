## Haskoin RESTful API

The Haskoin API is designed to help you manage your Haskoin wallet through a web friendly REST api. It is accessible from the haskoin daemon when you start it with `hw start`.

### Overview

| Resource                                     | Verbs     | Description                                    |
| -------------------------------------------- | --------- | ---------------------------------------------- |
| [/api/wallets](#get-apiwallets)              | GET, POST | List all wallets, Create a new wallet          |
| [/api/wallets/{name}](#get-apiwalletsname)   | GET       | Get a wallet by name                           |
| [/api/accounts](#get-apiaccounts)            | GET, POST | List all accounts, Create a new account        |
| [/api/accounts/{name}](#get-apiaccountsname) | GET       | Get an account by name                         |
| [/api/accounts/{name}/keys](#post-apiaccountsnamekeys) | POST | Add keys to a multisig account |
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

#### GET /api/accounts

* **Output**: Returns a list of all available accounts. There are 4 types of accounts
having each a different JSON representation:

  * **Regular account**
  
  A regular account creates pay-to-pubkey-hash addresses which are derived from the xpub key.

  ```json
  {
    "type": "regular",
    "name": "account1",
    "wallet": "wallet1",
    "index": 4,
    "key": "xpub..."
  }
  ```
  * **Multisig account**
  
  A multisig accounts creates pay-to-script-hash (p2sh) addresses which are derived from the keys.

  ```json
  {
    "type": "multisig",
    "name": "account2",
    "wallet": "wallet1",
    "index": 5,
    "required": 2,
    "total": 3,
    "keys": [ "xpub1...", "xpub2...", "xpub3..." ]
  }
  ```
  * **Regular read-only account**
  
  A regular read-only account is similar to a regular account. It can derive pay-to-pubkey-hash addresses
  from the key, but it can not derive the associated private keys. It is useful for monitoring payments
  in an environment where security is not optimal, such as on a web server.

  ```json
  {
    "type": "read",
    "name": "account3",
    "key": "xpub..."
  }
  ```
  
  * **Multisig read-only account**

  A multisig read-only account is similar to a multisig account. It can derive pay-to-script-hash (p2sh)
  addresses from the keys, but it can not derive the associated private keys. It is useful for monitoring
  payments in an environment where security is not optimal, such as on a web server.

  ```json
  {
    "type": "readmultisig",
    "name": "account4",
    "required": 2,
    "total": 3,
    "keys": [ "xpub1...", "xpub2...", "xpub3..." ]
  }
  ```
#### POST /api/accounts

* **Input**: A JSON object representing the type of account you want to create. 

  * **Regular account**
  
  A regular account is a prime derivation of a wallets extended private key (see bip32). The wallet must
  already exist in order to create a regular account.

  ```json
  {
    "type": "regular",
    "walletname": "wallet1",
    "accountname": "account1"
  }
  ```
  
  * **Multisig account**
  
  A multisig account is a prime derivation of a wallets extended private key (see bip32). The wallet must
  already exist in order to create a multisig account. The keys field corresponds to the thirdparty keys
  associated to this multisig account. Your key will be generated by the wallet and added as the first
  element on the list. So, if you are creating a m of n account, you can specify an additional (n-1) keys.
  Thirdparty keys can be added at a later time through the resource 
  [/api/accounts/{name}/keys](#post-apiaccountsnamekeys).

  ```json
  {
    "type": "multisig",
    "walletname": "wallet1",
    "accountname": "account2",
    "required": 2,
    "total": 3,
    "keys": [ "xpub1...", ... ]
  }
  ```
  
  * **Regular read-only account**
  
  You can create a regular read-only account to monitor payments without having the ability to
  spend coins. The extended public key is used to generate the pay-to-pubkey-hash addresses
  that you would like to monitor.

  ```json
  {
    "type": "read",
    "accountname": "account3",
    "key": "xpub..."
  }
  ```
  
  * **Multisig read-only account**
  
  You can create a multisig read-only account to monitor payments without having the ability to spend coins.
  The extended public keys are used to generate the pay-to-script-hash addresses that you would like to monitor.
  Keys can be added at a later time through the resource [/api/accounts/{name}/keys](#post-apiaccountsnamekeys).

  ```json
  {
    "type": "readmultisig",
    "accountname": "account2",
    "required": 2,
    "total": 3,
    "keys": [ "xpub1...", ... ]
  }
  ```
  
* **Output**: A JSON representation of your new account as defined [here](#get-apiaccounts).

#### GET /api/accounts/{name}

* **Input**: An account resource is identified by its name. For example:

```
/api/accounts/account1
```

* **Output**: A JSON representation of the given account as defined [here](#get-apiaccounts).

#### POST /api/accounts/{name}/keys

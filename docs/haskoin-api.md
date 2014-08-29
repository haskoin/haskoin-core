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
| [/api/accounts/{name}/addrs](#get-apiaccountsnameaddrs) | GET, POST | List addresses, Get a new unused address |
| [/api/accounts/{name}/addrs/{key}](#get-apiaccountsnameaddrskey)|GET, PUT | Get an address by key, Update an address label |
| [/api/accounts/{name}/acctxs](#get-apiaccountsnameacctxs) | GET, POST | List txs, Send coins, Sign txs/sigblobs|
| [/api/accounts/{name}/acctxs/{txhash}](#get-apiaccountsnameacctxstxhash) | GET| Get a tx by account and transaction id |
| [/api/accounts/{name}/acctxs/{txhash}/sigblob](#get-apiaccountsnameacctxstxhashsigblob) | GET | Get data to sign a transaction offline         |
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

When creating a multisignature account, you can add the keys at a later time by using this resource.
Adding too many keys will result in a failure.

* **Input**: A JSON list of extended public keys:

```json
  [ "xpub1...", "xpub2", ... ]
```

* **Output**: A JSON representation of the given account as defined [here](#get-apiaccounts).

#### GET /api/accounts/{name}/addrs

This resource will return a list of addresses pertaining to an account.

* **Input**: This resource accepts optional query parameters to receive paged results. If no paging parameters
  are set, the entire list is returned. Asking for page 0 will return the last page.
  Here are some valid example queries:

```
GET /api/accounts/account1/addrs
GET /api/accounts/account1/addrs?page=1
GET /api/accounts/account1/addrs?page=2&elemperpage=50
```

* **Output**: When requesting the entire address list (no paging parameters), the following JSON result is returned:

```json
[
  {
    "address": "n3uypZFbRqscWNqRvR64cgoMnygDHfQajD",
    "index": 0,
    "label": "Label 1"
  },
  {
    "address": "mymqB9Bk5s5KaYDRAgjznEbnLVyrajxtjm",
    "index": 1,
    "label": "Label 2"
  }
]
```

When requesting a page, you also get the maximum page number:

```json
{
  "maxpage": 6,
  "addresspage": [
    {
      "address": "msKuXqKp9MZdmR4V86YTWZewjSJMVrmZTu",
      "index": 5,
      "label": ""
    },
    {
      "address": "n1Gh9BbzV3b3MJHsMLCPrFyB4mMK9mRG4x",
      "index": 6,
      "label": ""
    }
  ]
}
```

#### POST /api/accounts/{name}/addrs

You can POST a label to this resource to create a new address. It will find the next
unused and unlabeled address in your wallet and add a label to it.

* **Input**: A JSON object with the label that the new address should have:

```json
{ 
  "label": "my label" 
}
```

* **Output**: A JSON representation of the newly created address:

```json
{
  "address": "mv6hqrDt9qeHjxo3n5dMUfhScoVHVTXyEt",
  "index": 17,
  "label": "my label"
}
```

#### GET /api/accounts/{name}/addrs/{key}

* **Input**: You can retrieve an individual address using its derivation index (key):

```
GET /api/accounts/account1/addrs/17
```

* **Output**: A JSON representation of the given address:
 
```json
{
  "address": "mv6hqrDt9qeHjxo3n5dMUfhScoVHVTXyEt",
  "index": 17,
  "label": "my label"
}
```

#### PUT /api/accounts/{name}/addrs/{key}

You can PUT a label to this resource to update the label of an address.

* **Input**: A JSON object with the new label:

```json
{ 
  "label": "my updated label"
}
```

* **Output**: A JSON representation of the updated address:

```json
{
  "address": "mv6hqrDt9qeHjxo3n5dMUfhScoVHVTXyEt",
  "index": 17,
  "label": "my updated label"
}
```

#### GET /api/accounts/{name}/acctxs

This resource will return a list of transactions pertaining to an account. Account transactions
summarize the results of a transaction for a given account. For instance, the value of an
account transaction is the sum of the input coins + the sum of the output coins for a
specific account. When importing a transaction, it might produce several account transaction for
each account affected by the transaction.

* **Input**: This resource accepts optional query parameters to receive paged results. If no paging parameters
  are set, the entire list is returned. Asking for page 0 will return the last page.
  Here are some valid example queries:

```
GET /api/accounts/account1/acctxs
GET /api/accounts/account1/acctxs?page=1
GET /api/accounts/account1/acctxs?page=2&elemperpage=50
```

* **Output**: When requesting the entire transaction list (no paging parameters), 
  the following JSON result is returned:

```json
[
  {
    "value": 333000,
    "confidence": "building",
    "isCoinbase": false,
    "confirmations": 3343,
    "recipients": [
      "mrinhQTEfn5qepFNHcEo3zD5FpJFNi5AeW"
    ],
    "txid": "9b3fa96547c6aa3f355e2f10ed860f4a36d8369064a7cfe4e65eecbbc03bfe8f"
  },
  {
    "value": -333000,
    "confidence": "pending",
    "isCoinbase": false,
    "confirmations": 0,
    "recipients": [
      "mkwNK8zgNtVxF8CqZCFU83qxTNcwj4cs8q"
    ],
    "txid": "3a4317be696a438fca5a9705786a9d2da6eadcd1d0a6e8be34be8b41b8dff79c"
  }
]
```

When requesting a page, you also get the maximum page number:

```json
{
  "maxpage": 2,
  "txpage": [
    {
      "value": 333000,
      "confidence": "building",
      "isCoinbase": false,
      "confirmations": 3343,
      "recipients": [
        "mrinhQTEfn5qepFNHcEo3zD5FpJFNi5AeW"
      ],
      "txid": "9b3fa96547c6aa3f355e2f10ed860f4a36d8369064a7cfe4e65eecbbc03bfe8f"
    },
    {
      "value": -333000,
      "confidence": "pending",
      "isCoinbase": false,
      "confirmations": 0,
      "recipients": [
        "mkwNK8zgNtVxF8CqZCFU83qxTNcwj4cs8q"
      ],
      "txid": "3a4317be696a438fca5a9705786a9d2da6eadcd1d0a6e8be34be8b41b8dff79c"
    }
  ]
}

```

#### POST /api/accounts/{name}/acctxs

By POSTing to this resource, you can send coins, sign a transactions or sign an offline transaction blob.

##### Send Coins

* **Input**: A JSON object with the recipient addresses, the amount in satoshi and 
  the fee to pay (in satoshi/1000 bytes).

```json
{
  "type": "send",
  "recipients": [
    [ "mrinhQTEfn5qepFNHcEo3zD5FpJFNi5AeW", 333000 ],
    [ "mkwNK8zgNtVxF8CqZCFU83qxTNcwj4cs8q", 336000 ]
  ],
  "fee": 10000
}
```

* **output**: The resulting transaction hash and a status indicating if the transaction is complete.

```json
{
  "txhash": "3a4317be696a438fca5a9705786a9d2da6eadcd1d0a6e8be34be8b41b8dff79c",
  "complete": true
}
```

##### Sign a transaction

You can only sign a transaction with the keys of one account at a time. If a transactions requires
keys from multiple accounts, you have to call this resource for every account. This resource is
also the standard way of importing a transaction into your wallet. If a transaction is already
signed, it will simply be imported as-is in the wallet.

* **Input**: A JSON object containing the raw transaction to sign in base16 (Hex):

```json
{
  "type": "sign",
  "tx": "0100000001a65337..."
}
```

* **output**: The resulting transaction hash and a status indicating if the transaction is complete.

```json
{
  "txhash": "9b3fa96547c6aa3f355e2f10ed860f4a36d8369064a7cfe4e65eecbbc03bfe8f",
  "complete": true
}
```

##### Sign offline transaction

Use this resource to sign an offline sigblob that was produced from the following resource:

GET [/api/accounts/{name}/acctxs/{txhash}/sigblob](#get-apiaccountsnameacctxstxhashsigblob)

You can have an online read-only wallet produce a sigblob that you can sign using an offline wallet
containing the private keys. The sigblob contains a transaction and the data required to sign it
(previous outpoints, previous scripts, redeem scripts).

This call will not import the transaction into the wallet.

You can only sign a sigblob using the keys of one account at a time. 

* **Input**: The sigblob containing the transaction and the signing data. The signing data is a list of the
  following elements:

  * Previous outpoint (hash + position)
  * Previous script output
  * True if the private key is internal. False otherwise.
  * Derivation index for the public (and private) key.

```json
{
  "type": "sigblob",
  "sigblob": {
    "tx": "01000000014db...",
    "data": [
      [
        "8ffe3bc0bbec5ee6e4cfa7649036d8364a0f86ed102f5e353faac64765a93f9b00000000",
        "76a9147ae65d18f30406333d304293af1d783ab83b87d288ac",
        false,
        7
      ]
    ]
  }
}
```

* **output**: The resulting transaction and a status indicating if the transaction is complete.

```json
{
  "tx": "0100000001a65337...",
  "complete": true
}
```

#### GET /api/accounts/{name}/acctxs/{txhash}

You can use this resource to query individual account transactions.

* **Output**: Returns the requested account transaction:

```json
{
  "value": -333000,
  "confidence": "pending",
  "isCoinbase": false,
  "confirmations": 0,
  "recipients": [
    "mkwNK8zgNtVxF8CqZCFU83qxTNcwj4cs8q"
  ],
  "txid": "3a4317be696a438fca5a9705786a9d2da6eadcd1d0a6e8be34be8b41b8dff79c"
  }
```

#### GET /api/accounts/{name}/acctxs/{txhash}/sigblob

Use this resource to compute the offline sigblob for a given transaction. A sigblob can be produced from
any type of account (including read-only accounts). You can then sign the transaction included in the
sigblob by using this resource on another account containing the private keys (ideally on an offline wallet):

POST [/api/accounts/{name}/acctxs](#post-apiaccountsnameacctxs)

* **Output**: The sigblob contaings the transaction and data required for signing the transaction. The data part
  consists of a list of the following elements:

  * Previous outpoint (hash + position)
  * Previous script output
  * True if the private key is internal. False otherwise.
  * Derivation index for the public (and private) key.

```json
{
  "tx": "01000000014db...",
  "data": [
    [
      "8ffe3bc0bbec5ee6e4cfa7649036d8364a0f86ed102f5e353faac64765a93f9b00000000",
      "76a9147ae65d18f30406333d304293af1d783ab83b87d288ac",
      false,
      7
    ]
  ]
}
```

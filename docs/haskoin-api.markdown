# Haskoin RESTful API

The Haskoin API is designed to help you manage your Haskoin wallet through a
web friendly REST api. It is accessible from the haskoin daemon when you start
it with `hw start`.

## Overview

- [/api/wallets](#get-apiwallets) (GET, POST)  
  List all wallets, Create a new wallet
- [/api/wallets/{name}](#get-apiwalletsname) (GET)  
  Get a wallet by name
- [/api/accounts](#get-apiaccounts) (GET, POST)  
  List all accounts, Create a new account
- [/api/accounts/{name}](#get-apiaccountsname) (GET)  
  Get an account by name
- [/api/accounts/{name}/keys](#post-apiaccountsnamekeys) (POST)  
  Add keys to a multisig account
- [/api/accounts/{name}/addrs](#get-apiaccountsnameaddrs) (GET, POST)  
  List addresses, Get a new unused address
- [/api/accounts/{name}/addrs/{key}](#get-apiaccountsnameaddrskey) (GET, PUT)  
  Get an address by key, Update an address label
- [/api/accounts/{name}/acctxs](#get-apiaccountsnameacctxs) (GET, POST)  
  List txs, Send coins, Sign txs/sigblobs
- [/api/accounts/{name}/acctxs/{txhash}](#get-apiaccountsnameacctxstxhash) (GET)  
  Get a tx by account and transaction id
- [/api/accounts/{name}/acctxs/{txhash}/sigblob](#get-apiaccountsnameacctxstxhashsigblob) (GET)  
  Get data to sign a transaction offline
- [/api/accounts/{name}/balance](#get-apiaccountsnamebalance) (GET)  
  Get an account balance in satoshi
- [/api/accounts/{name}/spendablebalance](#get-apiaccountsnamespendablebalance) (GET)  
  Get an account spendable balance in satoshi
- [/api/txs](#put-apitxs) (PUT)  
  Import transactions
- [/api/txs/{txhash}](#get-apitxstxhash) (GET)  
  Get a full transaction by transaction id
- [/api/node](#post-apinode)  (POST)  
  Rescan the wallet from a given timestamp

## API Specification

### GET /api/wallets

**Modes**: online, offline

#### Output

Returns a list of all available wallets in the following format:

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

### POST /api/wallets

**Modes**: online, offline

#### Input

A JSON object representing a new wallet containing a wallet name,
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

#### Output

Returns the mnemonic sentence of your new wallet:

```json
{ "mnemonic": "film pig subway ..." }
```

### GET /api/wallets/{name}

**Modes**: online, offline

#### Input

A wallet resource is identified by its name. For example:

```
/api/wallets/wallet1
```

#### Output

A JSON object representing a wallet in the following format:

```json
{
  "name": "wallet1",
  "master": "xprv..."
}
```

It contains both the wallet name and the extended private key (master key) for
that wallet.

### GET /api/accounts

**Modes**: online, offline, vault

#### Output

Returns a list of all available accounts. There are 4 types of accounts
having each a different JSON representation:

##### Regular account
  
A regular account creates pay-to-pubkey-hash addresses which are derived from
the xpub key.

```json
{
"type": "regular",
"name": "account1",
"wallet": "wallet1",
"index": 4,
"key": "xpub..."
}
```

##### Multisig account
  
A multisig accounts creates pay-to-script-hash (p2sh) addresses which are
derived from the keys.

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

##### Regular read-only account
  
A regular read-only account is similar to a regular account. It can derive
pay-to-pubkey-hash addresses from the key, but it can not derive the associated
private keys. It is useful for monitoring payments in an environment where
security is not optimal, such as on a web server.

```json
{
"type": "read",
"name": "account3",
"key": "xpub..."
}
```
  
##### Multisig read-only account

A multisig read-only account is similar to a multisig account. It can derive
pay-to-script-hash (p2sh) addresses from the keys, but it can not derive the
associated private keys. It is useful for monitoring payments in an environment
where security is not optimal, such as on a web server.

```json
{
"type": "readmultisig",
"name": "account4",
"required": 2,
"total": 3,
"keys": [ "xpub1...", "xpub2...", "xpub3..." ]
}
```
  
### POST /api/accounts

**Modes**: online, offline, vault

#### Input

A JSON object representing the type of account you want to create. 

##### Regular account
  
A regular account is a prime derivation of a wallets extended private key (see
bip32). The wallet must already exist in order to create a regular account.

```json
{
"type": "regular",
"walletname": "wallet1",
"accountname": "account1"
}
```
  
##### Multisig account
  
A multisig account is a prime derivation of a wallets extended private key (see
bip32). The wallet must already exist in order to create a multisig account.
The keys field corresponds to the thirdparty keys associated to this multisig
account. Your key will be generated by the wallet and added as the first
element on the list. So, if you are creating a m of n account, you can specify
an additional (n-1) keys.  Thirdparty keys can be added at a later time through
the resource [/api/accounts/{name}/keys](#post-apiaccountsnamekeys).

```json
{
"type": "multisig",
"walletname": "wallet1",
"accountname": "account2",
"required": 2,
"total": 3,
"keys": [ "xpub1..." ]
}
```
  
##### Regular read-only account
  
You can create a regular read-only account to monitor payments without having
the ability to spend coins. The extended public key is used to generate the
pay-to-pubkey-hash addresses that you would like to monitor.

```json
{
"type": "read",
"accountname": "account3",
"key": "xpub..."
}
```
  
##### Multisig read-only account
  
You can create a multisig read-only account to monitor payments without having
the ability to spend coins.  The extended public keys are used to generate the
pay-to-script-hash addresses that you would like to monitor.  Keys can be added
at a later time through the resource
[/api/accounts/{name}/keys](#post-apiaccountsnamekeys).

```json
{
"type": "readmultisig",
"accountname": "account2",
"required": 2,
"total": 3,
"keys": [ "xpub1..." ]
}
```
  
#### Output

A JSON representation of your new account as defined [here](#get-apiaccounts).

### GET /api/accounts/{name}

**Modes**: online, offline, vault

#### Input

An account resource is identified by its name. For example:

```
/api/accounts/account1
```

#### Output

A JSON representation of the given account as defined [here](#get-apiaccounts).

### POST /api/accounts/{name}/keys

**Modes**: online, offline, vault

When creating a multisignature account, you can add the keys at a later time by
using this resource.  Adding too many keys will result in a failure.


#### Input

A JSON list of extended public keys:

```json
  [ "xpub1...", "xpub2..." ]
```

#### Output

A JSON representation of the given account as defined [here](#get-apiaccounts).

### GET /api/accounts/{name}/addrs

**Modes**: online, offline, vault

This resource will return a list of addresses pertaining to an account.

#### Input

This resource accepts optional query parameters to receive paged results. If no
paging parameters are set, the entire list is returned. Asking for page 0 will
return the last page.

Additionally, you can specify the minimum number of confirmations required for
computing the address balances. If no "minconf" flag is set, the 0-confirmation
balance will be returned. By default, the external addresses are returned. You
can ask for internal addresses by setting the internal flag.

Here are some valid example queries:

```
GET /api/accounts/account1/addrs
GET /api/accounts/account1/addrs?page=1
GET /api/accounts/account1/addrs?page=2&elemperpage=50
GET /api/accounts/account1/addrs?minconf=1
GET /api/accounts/account1/addrs?minconf=2&internal=true
```

#### Output

When requesting the entire address list (no paging parameters), the following
JSON result is returned:

```json
[
  {
    "address": {
      "address": "n3uypZFbRqscWNqRvR64cgoMnygDHfQajD",
      "index": 0,
      "label": "Label 1"
    },
    "finalbalance": {
      "status": "valid",
      "balance": 1000000
    },
    "totalreceived": {
      "status": "valid",
      "balance": 1000000
    },
    "fundingtxs": [
      "9b3fa96547c6aa3f355e2f10ed860f4a36d8369064a7cfe4e65eecbbc03bfe8f"
    ],
    "spendingtxs": [
      "3a4317be696a438fca5a9705786a9d2da6eadcd1d0a6e8be34be8b41b8dff79c"
    ],
    "conflicttxs": []
  }
]
```

When requesting a page, you also get the maximum page number:

```json
{
  "maxpage": 6,
  "addresspage": [
    {
      "address": {
        "address": "msKuXqKp9MZdmR4V86YTWZewjSJMVrmZTu",
        "index": 5,
        "label": "Label 2"
      },
      "finalbalance": {
        "status": "conflict"
      },
      "totalreceived": {
        "status": "valid",
        "balance": 5000000
      },
      "fundingtxs": [
        "9b3fa96547c6aa3f355e2f10ed860f4a36d8369064a7cfe4e65eecbbc03bfe8f"
      ],
      "spendingtxs": [],
      "conflicttxs": [
        "3a4317be696a438fca5a9705786a9d2da6eadcd1d0a6e8be34be8b41b8dff79c",
        "8c2ea96547c6aa3f355e2f10ed860f4a36d8369064a7cfe4e65eecbbc03bf271"
      ]
    }
  ]
}
```

##### Address fields

The `finalbalance` and `totalreceived` fields are balances. 

Balances are objects that have a "status" of "valid" or "conflict". When
"valid", the balance will have a "balance" field with the balance value. In
rare occasions when you have conflicting transactions in your wallet (double
spends, malleability attacks etc.), it is impossible to compute the balance
reliably. In such a case, and to alert the user that something is wrong, the
balance will be in "conflict" status until any conflicts in the wallet are
resolved. The transactions that are in conflict will be given in the
`conflicttxs` field.

Conflicts usually resolve on their own when new blocks are solved. If you have
a balance in conflict status, you should wait for a few extra blocks to sort
out the conflicts.

The `fundingtxs` field contains transactions that fund the given address and
the `spendingtxs` field contains transactions that spend from the given
address.

### POST /api/accounts/{name}/addrs

**Modes**: online, offline, vault

You can POST a label to this resource to create a new address. It will find the
next unused and unlabeled address in your wallet and add a label to it.

#### Input

A JSON object with the label that the new address should have:

```json
{ 
  "label": "my label" 
}
```

#### Output

A JSON representation of the newly created address:

```json
{
  "address": "mv6hqrDt9qeHjxo3n5dMUfhScoVHVTXyEt",
  "index": 17,
  "label": "my label"
}
```

### GET /api/accounts/{name}/addrs/{key}

**Modes**: online, offline, vault

#### Input

You can retrieve an individual address using its derivation index (key). You
can additionally use the "minconf" and "internal" query strings to specify
the minimum number of confirmations to compute the address balance and to
get internal addresses instead of external ones.

```
GET /api/accounts/account1/addrs/17
GET /api/accounts/account1/addrs/25?minconf=1&internal=true
```

#### Output

A JSON representation of the given address:
 
```json
{
  "address": {
    "address": "mv6hqrDt9qeHjxo3n5dMUfhScoVHVTXyEt",
    "index": 17,
    "label": "my label"
  },
  "finalbalance": {
    "status": "valid",
    "balance": 0
  },
  "totalreceived": {
    "status": "valid",
    "balance": 5000000
  },
  "fundingtxs": [
    "9b3fa96547c6aa3f355e2f10ed860f4a36d8369064a7cfe4e65eecbbc03bfe8f"
  ],
  "spendingtxs": [
    "3a4317be696a438fca5a9705786a9d2da6eadcd1d0a6e8be34be8b41b8dff79c"
  ],
  "conflicttxs": []
}
```

Go [here](#address-fields) for a description of the address fields.

### PUT /api/accounts/{name}/addrs/{key}

**Modes**: online, offline, vault

You can PUT a label to this resource to update the label of an address.

#### Input

A JSON object with the new label:

```json
{ 
  "label": "my updated label"
}
```

#### Output

A JSON representation of the updated address:

```json
{
  "address": "mv6hqrDt9qeHjxo3n5dMUfhScoVHVTXyEt",
  "index": 17,
  "label": "my updated label"
}
```

### GET /api/accounts/{name}/acctxs

**Modes**: online, offline, vault

This resource will return a list of transactions pertaining to an account.
Account transactions summarize the results of a transaction for a given
account. For instance, the value of an account transaction is the sum of the
input coins + the sum of the output coins for a specific account. When
importing a transaction, it might produce several account transaction for each
account affected by the transaction.

#### Input

This resource accepts optional query parameters to receive paged results. If no
paging parameters are set, the entire list is returned. Asking for page 0 will
return the last page.  Here are some valid example queries:

```
GET /api/accounts/account1/acctxs
GET /api/accounts/account1/acctxs?page=1
GET /api/accounts/account1/acctxs?page=2&elemperpage=50
```

#### Output

When requesting the entire transaction list (no paging parameters), the
following JSON result is returned:

```json
[
  {
    "value": 333000,
    "confidence": "building",
    "isCoinbase": false,
    "confirmations": 3343,
    "recipients": [
      { 
        "address": "mrinhQTEfn5qepFNHcEo3zD5FpJFNi5AeW",
        "label": "label 3",
        "islocal": true
      },
      { 
        "address": "mkwNK8zgNtVxF8CqZCFU83qxTNcwj4cs8q",
        "label": "label 1",
        "islocal": true
      }
    ],
    "txid": "9b3fa96547c6aa3f355e2f10ed860f4a36d8369064a7cfe4e65eecbbc03bfe8f",
    "receiveddate": 1416005281,
    "confirmationdate": 1416008832
  },
  {
    "value": -333000,
    "confidence": "pending",
    "isCoinbase": false,
    "confirmations": 0,
    "recipients": [
      { 
        "address": "mkwNK8zgNtVxF8CqZCFU83qxTNcwj4cs8q",
        "label": "label 1",
        "islocal": false
      }
    ],
    "txid": "3a4317be696a438fca5a9705786a9d2da6eadcd1d0a6e8be34be8b41b8dff79c",
    "receiveddate": 1416002213
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
        { 
          "address": "mrinhQTEfn5qepFNHcEo3zD5FpJFNi5AeW",
          "label": "label 3",
          "islocal": true
        },
        { 
          "address": "mkwNK8zgNtVxF8CqZCFU83qxTNcwj4cs8q",
          "label": "label 1",
          "islocal": true
        }
      ],
      "txid": "9b3fa96547c6aa3f355e2f10ed860f4a36d8369064a7cfe4e65eecbbc03bfe8f",
      "receiveddate": 1416005281,
      "confirmationdate": 1416008832
    },
    {
      "value": -333000,
      "confidence": "pending",
      "isCoinbase": false,
      "confirmations": 0,
      "recipients": [
        { 
          "address": "mkwNK8zgNtVxF8CqZCFU83qxTNcwj4cs8q",
          "label": "label 1",
          "islocal": false
        }
      ],
      "txid": "3a4317be696a438fca5a9705786a9d2da6eadcd1d0a6e8be34be8b41b8dff79c",
      "receiveddate": 1416002213
    }
  ]
}

```

##### Acctxs fields

The "recipients" fields contains a list of addresses that have a "label" and
a "islocal" field. If the "islocal" field is true, it means that the
recipient address is a member of the addresses in this account. Otherwise it
is an address external to this account.

The "receiveddate" is the date at which the wallet received the transaction.
This date could be after the initial broadcast date if, for example, the wallet
was offline during the broadcast. The "confirmationdate" is the timestamp of
the block that confirmed the transaction. This might be a more reliable way
of knowing when the transaction was created if your wallet is often offline.

### POST /api/accounts/{name}/acctxs

**Modes**: offline, online

By POSTing to this resource, you can send coins, sign transactions or sign an
offline transaction blob.

#### Send Coins

##### Input

A JSON object with the recipient addresses, the amount in satoshi, the fee
to pay (in satoshi/1000 bytes) and the minimum number of confirmations of the
coins that will be spent.

```json
{
  "type": "send",
  "recipients": [
    [ "mrinhQTEfn5qepFNHcEo3zD5FpJFNi5AeW", 333000 ],
    [ "mkwNK8zgNtVxF8CqZCFU83qxTNcwj4cs8q", 336000 ]
  ],
  "fee": 10000,
  "minconf": 1
}
```

##### Output

The resulting transaction hash and a status indicating if the transaction is
complete. If the complete flag is false (the transaction is still partially
signed), an additional proposition field will be included which is the 
original transaction with all input scripts blanked out. You can give the
proposition to the other keys holders in a multisig account for them to sign,
then merge all the signed propositions back into the wallet using the
[import resource](#put-apitxs).

```json
{
  "txhash": "3a4317be696a438fca5a9705786a9d2da6eadcd1d0a6e8be34be8b41b8dff79c",
  "complete": false,
  "proposition": "0100000001a65337..." 
}
```

#### Sign a transaction

You can only sign a transaction with the keys of one account at a time. If a
transactions requires keys from multiple accounts, you have to call this
resource for every account. This resource is also the standard way of importing
a transaction into your wallet. If a transaction is already signed, it will
simply be imported as-is in the wallet.

##### Input

A JSON object containing the raw transaction to sign in base16 (Hex):

```json
{
  "type": "sign",
  "tx": "0100000001a65337..."
}
```

##### Output

The resulting transaction hash and a status indicating if the transaction is
complete. If the complete flag is false (the transaction is still partially
signed), an additional proposition field will be included which is the original
transaction with all input scripts blanked out.

```json
{
  "txhash": "9b3fa96547c6aa3f355e2f10ed860f4a36d8369064a7cfe4e65eecbbc03bfe8f",
  "complete": false,
  "proposition": "0100000001a65337..." 
}
```

#### Sign offline transaction

Use this resource to sign an offline sigblob that was produced from
resource GET
[/api/accounts/{name}/acctxs/{txhash}/sigblob](#get-apiaccountsnameacctxstxhashsigblob).

You can have an online read-only wallet produce a sigblob that you can sign
using an offline wallet containing the private keys. The sigblob contains a
transaction and the data required to sign it (previous outpoints, previous
scripts, redeem scripts).

This call will not import the transaction into the wallet.

You can only sign a sigblob using the keys of one account at a time. 

##### Input

The sigblob containing the transaction and the signing data. The signing data
is a list of the following elements:

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

##### Output

The resulting transaction and a status indicating if the transaction is
complete. If the complete flag is false (the transaction is still partially
signed), an addition proposition field will be included which is the original
transaction with all the input scripts blanked out.

```json
{
  "tx": "0100000001a65337...",
  "complete": false,
  "proposition": "0100000001a65337..." 
}
```

### GET /api/accounts/{name}/acctxs/{txhash}

**Modes**: online, offline, vault

You can use this resource to query individual account transactions.

#### Output

Returns the requested account transaction:

```json
{
  "value": -333000,
  "confidence": "pending",
  "isCoinbase": false,
  "confirmations": 12,
  "recipients": [
    { 
      "address": "mkwNK8zgNtVxF8CqZCFU83qxTNcwj4cs8q",
      "label": "label 1",
      "islocal": false
    }
  ],
  "txid": "3a4317be696a438fca5a9705786a9d2da6eadcd1d0a6e8be34be8b41b8dff79c",
  "receiveddate": 1416005281,
  "confirmationdate": 1416008832
  }
```

Go [here](#acctxs-fields) for a description of the account transaction fields.

### GET /api/accounts/{name}/acctxs/{txhash}/sigblob

**Modes**: online, offline, vault

Use this resource to compute the offline sigblob for a given transaction. A
sigblob can be produced from any type of account (including read-only
accounts). You can then sign the transaction included in the sigblob by using
this resource on another account containing the private keys (ideally on an
offline wallet): POST [/api/accounts/{name}/acctxs](#sign-offline-transaction).

#### Output

The sigblob contaings the transaction and data required for signing the
transaction. The data part consists of a list of the following elements:

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

### GET /api/accounts/{name}/balance

**Modes**: online, offline, vault

Use this resource to request the true balance of an account.

#### Input

You can pass the "minconf" parameter to request the balance of all coins
that have at least a minimum number of confirmations. By default, the 0-conf
balance is returned, which is your unconfirmed balance.

```
GET /api/accounts/account1/balance
GET /api/accounts/account1/balance?minconf=1
```

#### Output

The true balance of an account with the given number of minimum confirmations.
In rare occasions when you have conflicting transactions in your wallet (double
spends, malleability attacks etc.), it is impossible to compute the balance
reliably. In such a case, and to alert the user that something is wrong, the
balance will be in "conflict" status until the conflicts in the wallet are
resolved. The transactions that are in conflict will be given in the
"conflicts" field.

Conflicts usually resolve on their own when new blocks are solved. If you have
a balance in conflict status, you should wait for a few extra blocks to sort
out the conflicts.

Alternatively, you can use the [spendable balance](#get-apiaccountsnamespendablebalance) 
to always return a value, but it will not be accurate in the presence of
conflicts.

```json
{
  "balance": {
    "status": "valid",
    "balance": 3330000 
  },
  "conflicts": []
}
```

```json
{
  "balance": {
    "status": "conflict"
  },
  "conflicts": [
    "3a4317be696a438fca5a9705786a9d2da6eadcd1d0a6e8be34be8b41b8dff79c",
    "9b3fa96547c6aa3f355e2f10ed860f4a36d8369064a7cfe4e65eecbbc03bfe8f"
  ]
}
```

### GET /api/accounts/{name}/spendablebalance

**Modes**: online, offline, vault

Use this resource to request the spendable balance of an account.

#### Input

You can pass the "minconf" parameter to request the spendable balance of all
coins that have at least a minimum number of confirmations. By default, the
0-conf balance is returned, which is your unconfirmed spendable balance.

```
GET /api/accounts/account1/spendablebalance
GET /api/accounts/account1/spendablebalance?minconf=1
```

#### Output

The spendable balance of an account with the given number of minimum
confirmations. The spendable balance will ignore the coins created by
conflicting transactions and ignore coins spent by conflicting transactions. It
will also ignore coinbase transactions with less than 100 confirmations. In
contrast to the [true balance](#get-apiaccountsnamebalance), the spendable
balance will always return a value but it will be innacurate in the presence of
conflicts. However, it represents the balance that a user can spend at any
given point in time.

If you display the spendable balance to a user, it is advisable to also 
display the true balance as this will let the user know something is wrong
if he is a victim of a double spend or malleability attack.

```json
{ "balance": 3330000 }
```

### PUT /api/txs

**Modes**: online, offline

PUT a transaction to this resource to import it into the wallet. The main
use case for this resource is to merge partially signed multisig transactions
with existing ones in your wallet.

#### Input

The transaction to import:

```json
{
  "type": "import",
  "tx": "0100000001a65337..." 
}
```

#### Output

The resource will return the transaction hash and a completed flag. If the
transaction is not complete (partially signed), an additional proposition
field will be included which contains the original transaction with all
input script blanked out.

```json
{ 
  "txhash": "9b3fa96547c6aa3f355e2f10ed860f4a36d8369064a7cfe4e65eecbbc03bfe8f",
  "complete": false,
  "proposition": "0100000001a65337..." 
}
```

### GET /api/txs/{txhash}

**Modes**: online, offline, vault

Use this resource to query a whole transaction by transaction id. Only
transactions that exist in your wallet can be queried.

#### Output

The requested transaction:

```json
{ 
  "tx": "0100000001a65337..." 
}
```

### POST /api/node

**Modes**: online, offline

POST to this resource to call actions on the p2p node.

#### Input

The requested action for the p2p node. Only rescan is supported at this time.
The rescan action will trigger a re-download and importing of all the
transactions in merkle blocks after the given timestamp. If the timestamp is 0,
all transactions from the genesis will be re-imported. The timestamp is
optional: if the timestamp is omitted, the creation time of the first key in
your wallet will be used.

```json
{
  "type": "rescan",
  "timestamp": 1408222895
}
```

#### Output

The timestamp from which the rescan will be started:

```json
{
  "timestamp": 1408222895
}
```

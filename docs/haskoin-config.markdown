# Configuration File

The Haskoin Wallet configuration file allows to manage the behaviour of the
Haskoin Wallet daemon.  This file is formatted in [YAML](http://www.yaml.org).
The default location for the configuration file is:

    $HOME/.haskoin/config.yaml



## Server Directives

### detach

Detach and continue running on background when running daemon.

Default: false

### user

Username to connect to the API via HTTP basic authentication.

### password

Password to connect to the API via HTTP basic authentication.

### bind

Bind the RESTful API server to this network interface.  Defaults to IPv4
localhost. For possible values look at [HostPreference](http://goo.gl/UA5dX5).

Default: '127.0.0.1'

### port

Bind the RESTful API server to this port.

Default: 18555

### bitcoin-hosts

List of full Bitcoin nodes to whom this client will establish connections.
Currently Haskoin does not implement peer discovery, and you must specify to
which nodes it should establish connections.

Default:

    - host: '127.0.0.1'
      port: 18333

### operation-mode

There are three modes of operation: online, offline and vault. 

- Online mode launches an SPV client and connects to the Bitcoin nodes set in
  the configuration file.  

- Offline mode does not attempt to connect to the Bitcoin network or keep track
  of transactions. Most API calls are otherwise available.

- Vault mode is an offline mode with restrictions on the available API calls.
  It is meant for signing transactions in an offline environment. Functions
  like creating new wallets or printing wallet private keys are not available.

Default: online

### batch

How many block should be batched together for download/import.

Default: 100

### passphrase

Passphrase for mnemonic.  Refer to [BIP-39](http://goo.gl/3JvOdI) for more
information.

Default: ''

### count

How many elements should be returned by commands who require an element count

Default: 5

### fee

Fee to pay the network per kilobyte of transaction data.

Default: 10000

### minconf

Minimum number of confirmations to use in various commands.

Default: 0

### internal

Display internal addresses instead of external ones.

Default: false

### false-positive

False positive rate in bloom filter. See [BIP-37](http://goo.gl/hzXCtC) for
more information.

Default: 1.0e-5

### gap

Number of addresses to generate in account by default.  The wallet will always
have this many unused addresses generated.

Default: 10



## Client Directives

### provider

URL of the RESTful haskoin API server.

Default: http://localhost:8555

### json

Output of commands in [JSON](http://www.json.org) format.

Default: false

### yaml

Output of commands in [YAML](http://www.yaml.org) format.

Default: false


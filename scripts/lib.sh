#!/usr/bin/env bash

# Use Bitcoin Core 22.0 to create a complex PSBT test vector

if ! command -v bitcoind &> /dev/null; then
    echo "Please install bitcoind"
    exit 1
fi

if ! command -v jq &> /dev/null; then
    echo "Please install jq"
    exit 1
fi

export datadir=$(mktemp -d "/tmp/bitcoind-regtest-XXXXXX")

btc () {
    bitcoin-cli -regtest -datadir=$datadir "$@"
}

start_bitcoind () {
    bitcoind -regtest -datadir=$datadir -fallbackfee=0.0001000 -txindex=1 -daemon
    while ! btc getblockchaininfo &> /dev/null; do
        echo "Waiting for bitcoind"
        sleep 5
    done
}

stop_bitcoind () {
    btc stop
    rm -r $datadir
}

create_wallet () {
    passphrase="password"
    btc -named createwallet wallet_name=$1 passphrase=$passphrase &> /dev/null
    btc -rpcwallet=$1 walletpassphrase $passphrase 3600
}

get_address () {
    btc -rpcwallet="miner" getnewaddress
}

get_public_descriptor () {
    btc getdescriptorinfo $1 | jq -r .descriptor
}

get_descriptor_address () {
    btc deriveaddresses $1 | jq -r ".[0]"
}

import_descriptor () {
    request=$(jq --null-input --arg "desc" $2 '[ { desc: $desc, timestamp: "now" } ]')
    btc -rpcwallet=$1 importmulti "$request"
}

generate_blocks () {
    btc generatetoaddress $1 $(btc -rpcwallet="miner" getnewaddress) &> /dev/null
}

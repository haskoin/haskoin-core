#!/usr/bin/env bash

. ./scripts/lib.sh

cabal build
start_bitcoind

create_wallet "miner"
create_wallet "payer"
create_wallet "signer"

generate_blocks 200


address1=$(get_address)
privkey1=$(btc -rpcwallet="miner" dumpprivkey $address1)
pubkey1=$(btc -rpcwallet="miner" getaddressinfo $address1 | jq -r .pubkey)

address2=$(get_address)
privkey2=$(btc -rpcwallet="miner" dumpprivkey $address2)
pubkey2=$(btc -rpcwallet="miner" getaddressinfo $address2 | jq -r .pubkey)

address3=$(get_address)
pubkey3=$(btc -rpcwallet="miner" getaddressinfo $address3 | jq -r .pubkey)

sign () {
    base64 -d |
        cabal exec runghc -- ./scripts/PsbtSignTest.hs "$1" |
        base64 -w0
}

build () {

    descriptor=$1

    signer_descriptor=$(get_public_descriptor $descriptor)
    signer_addr=$(get_descriptor_address $signer_descriptor)

    suitable_outputs=$(btc -rpcwallet=miner listunspent | jq ". | map(select(.spendable) | select(.amount > 1))")

    txid=$(jq -r ".[0].txid" <<< $suitable_outputs)
    vout=$(jq -r ".[0].vout" <<< $suitable_outputs)

    funding_inputs=$(
        jq --null-input \
            --arg txid $txid \
            --arg vout $vout \
            '[{ txid: $txid, vout: $vout | tonumber }]'
    )
    funding_outputs=$(
        jq --null-input \
            --arg addr $signer_addr \
            '{ ($addr): 1 }'
           )
    funding_options=$(jq --null-input "{ changePosition: 1 }")
    funding_psbt=$(
        btc -named -rpcwallet=miner walletcreatefundedpsbt \
            "inputs=$funding_inputs" \
            "outputs=$funding_outputs" \
            "options=$funding_options"|
            jq -r ".psbt"
    )
    signed_funding_psbt=$(
        btc -rpcwallet=miner walletprocesspsbt $funding_psbt true | jq -r .psbt
    )
    funding_tx=$(btc -named finalizepsbt "psbt=$signed_funding_psbt" extract=true | jq -r .hex)
    funding_txid=$(btc sendrawtransaction $funding_tx)

    import_descriptor signer "$signer_descriptor" &> /dev/null
    generate_blocks 10

    signer_inputs=$(
        jq --null-input \
            --arg txid $funding_txid \
            '[{ txid: $txid, vout: 0 }]'
    )
    signer_outputs=$(
        jq --null-input \
            --arg addr $(get_address) \
            '{ ($addr): 0.8 }'
    )
    btc -named -rpcwallet=signer walletcreatefundedpsbt \
        "inputs=$signer_inputs" \
        "outputs=$signer_outputs" |
        jq -r .psbt
}

echo "p2pkh"
{
    signed_psbt=$(build "pkh($pubkey1)" | sign $privkey1)
    tx=$(btc -named finalizepsbt "psbt=$signed_psbt" extract=true | jq -r .hex)
    btc sendrawtransaction $tx 1> /dev/null
}

generate_blocks 10

echo "p2sh-pk"
{
    signed_psbt=$(build "sh(pk($pubkey1))" | sign $privkey1)
    tx=$(btc -named finalizepsbt "psbt=$signed_psbt" extract=true | jq -r .hex)
    btc sendrawtransaction $tx 1> /dev/null
}

generate_blocks 10

echo "p2sh-ms"
{
    descriptor="sh(sortedmulti(2,$pubkey1,$pubkey2,$pubkey3))"
    unsigned_psbt=$(build $descriptor)
    signed_psbt_1=$(sign $privkey1 <<< $unsigned_psbt)
    signed_psbt_2=$(sign $privkey2 <<< $unsigned_psbt)
    psbt_list=$(
        jq --null-input \
            --arg psbt1 $signed_psbt_1 \
            --arg psbt2 $signed_psbt_2 \
            '[ $psbt1, $psbt2 ]'
    )
    merged_psbt=$(btc combinepsbt "$psbt_list")
    tx=$(btc -named finalizepsbt "psbt=$merged_psbt" extract=true | jq -r .hex)
    btc sendrawtransaction $tx 1> /dev/null
}

generate_blocks 10

echo "p2sh-wpkh"
{
    signed_psbt=$(build "sh(wpkh($pubkey1))" | sign $privkey1)
    tx=$(btc -named finalizepsbt "psbt=$signed_psbt" extract=true | jq -r .hex)
    btc sendrawtransaction $tx 1> /dev/null
}

generate_blocks 10

echo "p2sh-p2wsh-ms"
{
    descriptor="sh(wsh(sortedmulti(2,$pubkey1,$pubkey2,$pubkey3)))"
    unsigned_psbt=$(build $descriptor)
    signed_psbt_1=$(sign $privkey1 <<< $unsigned_psbt)
    signed_psbt_2=$(sign $privkey2 <<< $unsigned_psbt)
    psbt_list=$(
        jq --null-input \
            --arg psbt1 $signed_psbt_1 \
            --arg psbt2 $signed_psbt_2 \
            '[ $psbt1, $psbt2 ]'
    )
    merged_psbt=$(btc combinepsbt "$psbt_list")
    tx=$(btc -named finalizepsbt "psbt=$merged_psbt" extract=true | jq -r .hex)
    btc sendrawtransaction $tx 1> /dev/null
}

generate_blocks 10

echo "p2wpkh"
{
    signed_psbt=$(build "wpkh($pubkey1)" | sign $privkey1)
    tx=$(btc -named finalizepsbt "psbt=$signed_psbt" extract=true | jq -r .hex)
    btc sendrawtransaction $tx 1> /dev/null
}

generate_blocks 10

echo "p2wsh-ms"
{
    descriptor="wsh(sortedmulti(2,$pubkey1,$pubkey2,$pubkey3))"
    unsigned_psbt=$(build $descriptor)
    signed_psbt_1=$(sign $privkey1 <<< $unsigned_psbt)
    signed_psbt_2=$(sign $privkey2 <<< $unsigned_psbt)
    psbt_list=$(
        jq --null-input \
            --arg psbt1 $signed_psbt_1 \
            --arg psbt2 $signed_psbt_2 \
            '[ $psbt1, $psbt2 ]'
    )
    merged_psbt=$(btc combinepsbt "$psbt_list")
    tx=$(btc -named finalizepsbt "psbt=$merged_psbt" extract=true | jq -r .hex)
    btc sendrawtransaction $tx 1> /dev/null
}

stop_bitcoind

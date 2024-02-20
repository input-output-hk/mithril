# Create Mithril era keypair and address
ADDR=mithril-era

## Payment address keys
./cardano-cli address key-gen \
    --verification-key-file addresses/${ADDR}.vkey \
    --signing-key-file      addresses/${ADDR}.skey

## Payment addresses
./cardano-cli address build \
    --payment-verification-key-file addresses/${ADDR}.vkey \
    --testnet-magic ${NETWORK_MAGIC} \
    --out-file addresses/${ADDR}.addr

## Send funds to Mithril era address
N=1
SCRIPT_TX_VALUE=2000000
AMOUNT_TRANSFERRED=$(( SCRIPT_TX_VALUE * 10 ))
cat >> era-mithril.sh <<EOF
#!/usr/bin/env bash
set -e
set -x

# Wait for a number of blocks has elapsed
function wait_for_elapsed_blocks {
    CARDANO_BLOCK_OFFSET=\$1
    CARDANO_NEXT_BLOCK_WAIT_ROUNDS_MAX=30
    CARDANO_NEXT_BLOCK_WAIT_ROUNDS=1
    CARDANO_NEXT_BLOCK_WAIT_ROUND_DELAY=2
    CURRENT_CARDANO_BLOCK=\$(CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli query tip \\
        --testnet-magic ${NETWORK_MAGIC} \\
        | jq  -r '.block')
    while true
    do
        CARDANO_BLOCK_TARGET=\$(( \${CURRENT_CARDANO_BLOCK} + \${CARDANO_BLOCK_OFFSET} ))
        CARDANO_BLOCK=\$(CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli query tip \\
        --testnet-magic ${NETWORK_MAGIC} \\
        | jq  -r '.block')
        if [ \$CARDANO_BLOCK -lt \$CARDANO_BLOCK_TARGET ] ; then
            echo ">>>> Cardano target block not reached yet... [current: \$CARDANO_BLOCK, target: \$CARDANO_BLOCK_TARGET] [attempt \$CARDANO_NEXT_BLOCK_WAIT_ROUNDS]"
            sleep \$CARDANO_NEXT_BLOCK_WAIT_ROUND_DELAY
        else
            echo ">>>> Cardano target block is reached [current: \$CARDANO_BLOCK, target: \$CARDANO_BLOCK_TARGET] [attempt \$CARDANO_NEXT_BLOCK_WAIT_ROUNDS]"
            sleep \$CARDANO_NEXT_BLOCK_WAIT_ROUND_DELAY
            break
        fi
        CARDANO_NEXT_BLOCK_WAIT_ROUNDS=\$(( \$CARDANO_NEXT_BLOCK_WAIT_ROUNDS + 1 ))
        if [ "\$CARDANO_NEXT_BLOCK_WAIT_ROUNDS" -gt "\$CARDANO_NEXT_BLOCK_WAIT_ROUNDS_MAX" ] ; then
            echo ">>>> Timeout: Cardano target block was not reached within \$CARDANO_NEXT_BLOCK_WAIT_ROUNDS_MAX attempts"
            exit 1
        fi
    done
}

EOF

cat >> era-mithril.sh <<EOF
    # Get current Cardano era
    CURRENT_CARDANO_ERA=\$(CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli query tip \\
        --testnet-magic ${NETWORK_MAGIC} \\
        | jq  -r '.era |= ascii_downcase | .era')
    echo ">>>> Current Cardano Era: \${CURRENT_CARDANO_ERA}"

    # Fix: era related command is not (well) supported in Cardano node version '8.1.2'
    if [ "${CARDANO_NODE_VERSION}" = "8.1.2" ]; then
        CURRENT_CARDANO_ERA=""
    fi

    # Get current Cardano block
    CURRENT_CARDANO_BLOCK=\$(CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli query tip \\
        --testnet-magic ${NETWORK_MAGIC} \\
        | jq  -r '.block')
    echo ">>>> Current Cardano Block: \${CURRENT_CARDANO_BLOCK}"

    # Send funds to Mithril era address
    ## Get the UTxO of utxo${N}
    TX_IN=\$(CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli query utxo \\
        --testnet-magic ${NETWORK_MAGIC} --address \$(cat addresses/utxo${N}.addr) --out-file /dev/stdout \\
        | jq  -r 'to_entries | [last] | .[0].key')

    ## Build the transaction
    CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli \${CURRENT_CARDANO_ERA} transaction build \\
        --tx-in \${TX_IN} \\
        --tx-out \$(cat addresses/${ADDR}.addr)+${AMOUNT_TRANSFERRED} \\
        --change-address \$(cat addresses/utxo${N}.addr) \\
        --testnet-magic ${NETWORK_MAGIC} \\
        --invalid-hereafter 100000 \\
        --out-file node-pool${N}/tx/tx${N}-era-funds.txbody

    ## Sign the transaction
    CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli \${CURRENT_CARDANO_ERA} transaction sign \\
        --signing-key-file addresses/utxo${N}.skey \\
        --testnet-magic ${NETWORK_MAGIC} \\
        --tx-body-file  node-pool${N}/tx/tx${N}-era-funds.txbody \\
        --out-file      node-pool${N}/tx/tx${N}-era-funds.tx

    ## Submit the transaction
    CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli \${CURRENT_CARDANO_ERA} transaction submit \\
        --tx-file node-pool${N}/tx/tx${N}-era-funds.tx \\
        --testnet-magic ${NETWORK_MAGIC}

    ## Wait at least for 10 blocks so that the transaction is confirmed
    wait_for_elapsed_blocks 10

    # Write the era datum on chain
    TX_IN=\$(CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli query utxo \\
        --testnet-magic ${NETWORK_MAGIC} --address \$(cat addresses/${ADDR}.addr) --out-file /dev/stdout \\
        | jq  -r 'to_entries | [last] | .[0].key')

    ## Build the transaction
    CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli \${CURRENT_CARDANO_ERA} transaction build \\
        --tx-in \${TX_IN} \\
        --tx-out \$(cat addresses/${ADDR}.addr)+${SCRIPT_TX_VALUE} \\
        --tx-out-inline-datum-file \${DATUM_FILE} \\
        --change-address \$(cat addresses/${ADDR}.addr) \\
        --testnet-magic ${NETWORK_MAGIC} \\
        --invalid-hereafter 100000 \\
        --out-file node-pool${N}/tx/tx${N}-era-datum.txbody

    ## Sign the transaction
    CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli \${CURRENT_CARDANO_ERA} transaction sign \\
        --signing-key-file addresses/${ADDR}.skey \\
        --testnet-magic ${NETWORK_MAGIC} \\
        --tx-body-file  node-pool${N}/tx/tx${N}-era-datum.txbody \\
        --out-file      node-pool${N}/tx/tx${N}-era-datum.tx

    ## Submit the transaction
    CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli \${CURRENT_CARDANO_ERA} transaction submit \\
        --tx-file node-pool${N}/tx/tx${N}-era-datum.tx \\
        --testnet-magic ${NETWORK_MAGIC}

    ## Wait at least for 10 blocks so that the transaction is confirmed
    wait_for_elapsed_blocks 10

    
EOF

chmod u+x era-mithril.sh

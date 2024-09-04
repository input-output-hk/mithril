# Create Mithril era keypair and address
ADDR=mithril-era

## Payment address keys
$CARDANO_CLI address key-gen \
    --verification-key-file addresses/${ADDR}.vkey \
    --signing-key-file      addresses/${ADDR}.skey

## Payment addresses
$CARDANO_CLI address build \
    --payment-verification-key-file addresses/${ADDR}.vkey \
    --testnet-magic ${NETWORK_MAGIC} \
    --out-file addresses/${ADDR}.addr

## Write datums for Mithril era address
N=1
SCRIPT_TX_VALUE=2000000
AMOUNT_TRANSFERRED=$(( SCRIPT_TX_VALUE * 10 ))
MITHRIL_ERA_ERROR_FILE=./era-mithril-error
cat >> era-mithril.sh <<EOF
#!/usr/bin/env bash
set -e

# Wait for a number of blocks has elapsed
function wait_for_elapsed_blocks {
    CARDANO_BLOCK_OFFSET=\$1
    CARDANO_NEXT_BLOCK_WAIT_ROUNDS_MAX=30
    CARDANO_NEXT_BLOCK_WAIT_ROUNDS=1
    CARDANO_NEXT_BLOCK_WAIT_ROUND_DELAY=2
    CURRENT_CARDANO_BLOCK=\$(CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock $CARDANO_CLI query tip \\
        --testnet-magic ${NETWORK_MAGIC} \\
        | jq  -r '.block')
    while true
    do
        CARDANO_BLOCK_TARGET=\$(( \${CURRENT_CARDANO_BLOCK} + \${CARDANO_BLOCK_OFFSET} ))
        CARDANO_BLOCK=\$(CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock $CARDANO_CLI query tip \\
        --testnet-magic ${NETWORK_MAGIC} \\
        | jq  -r '.block')
        if [ \$CARDANO_BLOCK -lt \$CARDANO_BLOCK_TARGET ] ; then
            echo ">>>> Cardano target block not reached yet... [current: \$CARDANO_BLOCK, target: \$CARDANO_BLOCK_TARGET] [attempt \$CARDANO_NEXT_BLOCK_WAIT_ROUNDS]"
            sleep \$CARDANO_NEXT_BLOCK_WAIT_ROUND_DELAY
        else
            echo ">>>> Cardano target block is reached [current: \$CARDANO_BLOCK, target: \$CARDANO_BLOCK_TARGET] [attempt \$CARDANO_NEXT_BLOCK_WAIT_ROUNDS]"
            break
        fi
        CARDANO_NEXT_BLOCK_WAIT_ROUNDS=\$(( \$CARDANO_NEXT_BLOCK_WAIT_ROUNDS + 1 ))
        if [ "\$CARDANO_NEXT_BLOCK_WAIT_ROUNDS" -gt "\$CARDANO_NEXT_BLOCK_WAIT_ROUNDS_MAX" ] ; then
            echo ">>>> Timeout: Cardano target block was not reached within \$CARDANO_NEXT_BLOCK_WAIT_ROUNDS_MAX attempts"
            exit 1
        fi
    done
}

# Send funds to Mithril era address
function send_funds_to_era_address {
    # Remove if exists previous error file
    rm -f ${MITHRIL_ERA_ERROR_FILE}

    # Get current Cardano era
    CURRENT_CARDANO_ERA=\$(CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock $CARDANO_CLI query tip \\
        --testnet-magic ${NETWORK_MAGIC} \\
        | jq  -r '.era |= ascii_downcase | .era')
    echo ">>>> Current Cardano Era: \${CURRENT_CARDANO_ERA}"

    # Get current Cardano block
    CURRENT_CARDANO_BLOCK=\$(CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock $CARDANO_CLI query tip \\
        --testnet-magic ${NETWORK_MAGIC} \\
        | jq  -r '.block')
    echo ">>>> Current Cardano Block: \${CURRENT_CARDANO_BLOCK}"

    # Send funds to Mithril era address
    ## Get the UTxO of utxo${N}
    TX_IN=\$(CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock $CARDANO_CLI query utxo \\
        --testnet-magic ${NETWORK_MAGIC} --address \$(cat addresses/utxo${N}.addr) --out-file /dev/stdout \\
        | jq  -r 'to_entries | [last] | .[0].key')

    ## Build the transaction
    CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock $CARDANO_CLI \${CURRENT_CARDANO_ERA} transaction build \\
        --tx-in \${TX_IN} \\
        --tx-out \$(cat addresses/${ADDR}.addr)+${AMOUNT_TRANSFERRED} \\
        --change-address \$(cat addresses/utxo${N}.addr) \\
        --testnet-magic ${NETWORK_MAGIC} \\
        --invalid-hereafter 100000 \\
        --out-file node-pool${N}/tx/tx${N}-era-funds.txbody

    ## Sign the transaction
    CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock $CARDANO_CLI \${CURRENT_CARDANO_ERA} transaction sign \\
        --signing-key-file addresses/utxo${N}.skey \\
        --testnet-magic ${NETWORK_MAGIC} \\
        --tx-body-file  node-pool${N}/tx/tx${N}-era-funds.txbody \\
        --out-file      node-pool${N}/tx/tx${N}-era-funds.tx

    ## Submit the transaction
    CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock $CARDANO_CLI \${CURRENT_CARDANO_ERA} transaction submit \\
        --tx-file node-pool${N}/tx/tx${N}-era-funds.tx \\
        --testnet-magic ${NETWORK_MAGIC}

    ## Wait at least for 10 blocks so that the transaction is confirmed
    wait_for_elapsed_blocks 10

    ## Wait for all pool nodes to see the new funds
    for (( i=1; i<=${NUM_POOL_NODES}; i++ )); do
        AMOUNT_RETRIEVED=\$(CARDANO_NODE_SOCKET_PATH=node-pool\${i}/ipc/node.sock $CARDANO_CLI query utxo \\
        --testnet-magic ${NETWORK_MAGIC} --address \$(cat addresses/${ADDR}.addr) --out-file /dev/stdout \\
        | jq '. [] | select(.value.lovelace | . != null and . != "") | .value.lovelace')
        echo ">>>>>> Era address funds retrieved on node-pool\${i}: \${AMOUNT_RETRIEVED}"
        if [ "\${AMOUNT_RETRIEVED}" != "${AMOUNT_TRANSFERRED}" ]; then
            touch ${MITHRIL_ERA_ERROR_FILE}
            break
        fi
    done
}

# Try to send funds to Mithril era address
function try_send_funds_to_era_address {
    SEND_FUNDS_ERA_ADDRESS_WAIT_ROUNDS_MAX=3
    SEND_FUNDS_ERA_ADDRESS_WAIT_ROUNDS=1
    SEND_FUNDS_ERA_ADDRESS_WAIT_ROUND_DELAY=2
    while true
    do
        send_funds_to_era_address
        if [ -f ${MITHRIL_ERA_ERROR_FILE} ]; then
            echo ">>>> Funds not transferred successfully to Mithril era address, a rollback has happened [attempt \$SEND_FUNDS_ERA_ADDRESS_WAIT_ROUNDS]!"
            sleep \$SEND_FUNDS_ERA_ADDRESS_WAIT_ROUND_DELAY
        else
            echo ">>>> Funds transferred successfully to Mithril era address [attempt \$SEND_FUNDS_ERA_ADDRESS_WAIT_ROUNDS]!"
            break
        fi
        SEND_FUNDS_ERA_ADDRESS_WAIT_ROUNDS=\$(( \$SEND_FUNDS_ERA_ADDRESS_WAIT_ROUNDS + 1 ))
        if [ "\$SEND_FUNDS_ERA_ADDRESS_WAIT_ROUNDS" -gt "\$SEND_FUNDS_ERA_ADDRESS_WAIT_ROUNDS_MAX" ] ; then
            echo ">>>> Timeout: Funds were not transferred successfully to Mithril era address within \$SEND_FUNDS_ERA_ADDRESS_WAIT_ROUNDS_MAX attempts"
            exit 1
        fi
    done
}

# Write datums for Mithril era address
function write_datums_for_era_address {
    # Remove if exists previous error file
    rm -f ${MITHRIL_ERA_ERROR_FILE}

    # Write the era datum on chain
    TX_IN=\$(CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock $CARDANO_CLI query utxo \\
        --testnet-magic ${NETWORK_MAGIC} --address \$(cat addresses/${ADDR}.addr) --out-file /dev/stdout \\
        | jq  -r 'to_entries | [last] | .[0].key')

    ## Build the transaction
    CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock $CARDANO_CLI \${CURRENT_CARDANO_ERA} transaction build \\
        --tx-in \${TX_IN} \\
        --tx-out \$(cat addresses/${ADDR}.addr)+${SCRIPT_TX_VALUE} \\
        --tx-out-inline-datum-file \${DATUM_FILE} \\
        --change-address \$(cat addresses/${ADDR}.addr) \\
        --testnet-magic ${NETWORK_MAGIC} \\
        --invalid-hereafter 100000 \\
        --out-file node-pool${N}/tx/tx${N}-era-datum.txbody

    ## Sign the transaction
    CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock $CARDANO_CLI \${CURRENT_CARDANO_ERA} transaction sign \\
        --signing-key-file addresses/${ADDR}.skey \\
        --testnet-magic ${NETWORK_MAGIC} \\
        --tx-body-file  node-pool${N}/tx/tx${N}-era-datum.txbody \\
        --out-file      node-pool${N}/tx/tx${N}-era-datum.tx

    ## Submit the transaction
    CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock $CARDANO_CLI \${CURRENT_CARDANO_ERA} transaction submit \\
        --tx-file node-pool${N}/tx/tx${N}-era-datum.tx \\
        --testnet-magic ${NETWORK_MAGIC}

    ## Wait at least for 10 blocks so that the transaction is confirmed
    wait_for_elapsed_blocks 10

    ## Wait for all pool nodes to see the new era datum
    for (( i=1; i<=${NUM_POOL_NODES}; i++ )); do
        INLINE_DATUM=\$(CARDANO_NODE_SOCKET_PATH=node-pool\${i}/ipc/node.sock $CARDANO_CLI query utxo \\
        --testnet-magic ${NETWORK_MAGIC} --address \$(cat addresses/${ADDR}.addr) --out-file /dev/stdout \\
        | jq  -r '. [] | select(.inlineDatum | . != null and . != "") | .inlineDatum.fields[].bytes' | xxd -r -p)
        echo ">>>>>> Era address inline datum retrieved on node-pool\${i}: \${INLINE_DATUM}"
        if [ "\${INLINE_DATUM}" == "" ]; then
            touch ${MITHRIL_ERA_ERROR_FILE}
            break
        fi
    done
}

# Try to write datums for Mithril era address
function try_write_datums_for_era_address {
    WRITE_DATUMS_ERA_ADDRESS_WAIT_ROUNDS_MAX=3
    WRITE_DATUMS_ERA_ADDRESS_WAIT_ROUNDS=1
    WRITE_DATUMS_ERA_ADDRESS_WAIT_ROUND_DELAY=2
    while true
    do
        write_datums_for_era_address
        if [ -f ${MITHRIL_ERA_ERROR_FILE} ]; then
            echo ">>>> Datums not written successfully for Mithril era address, a rollback has happened [attempt \$WRITE_DATUMS_ERA_ADDRESS_WAIT_ROUNDS]!"
            sleep \$WRITE_DATUMS_ERA_ADDRESS_WAIT_ROUND_DELAY
        else
            echo ">>>> Datums successfully written for Mithril era address [attempt \$WRITE_DATUMS_ERA_ADDRESS_WAIT_ROUNDS]!"
            break
        fi
        WRITE_DATUMS_ERA_ADDRESS_WAIT_ROUNDS=\$(( \$WRITE_DATUMS_ERA_ADDRESS_WAIT_ROUNDS + 1 ))
        if [ "\$WRITE_DATUMS_ERA_ADDRESS_WAIT_ROUNDS" -gt "\$WRITE_DATUMS_ERA_ADDRESS_WAIT_ROUNDS_MAX" ] ; then
            echo ">>>> Timeout: Datums were not written successfully for Mithril era address within \$WRITE_DATUMS_ERA_ADDRESS_WAIT_ROUNDS_MAX attempts"
            exit 1
        fi
    done
}

# Try to send funds to Mithril era address
try_send_funds_to_era_address

# Try to write datums for Mithril era address
try_write_datums_for_era_address
    
EOF

chmod u+x era-mithril.sh

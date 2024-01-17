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

EOF

cat >> era-mithril.sh <<EOF
    # Send funds to Mithril era address
    ## Get the UTxO of utxo${N}
    TX_IN=\$(CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli query utxo \\
        --testnet-magic ${NETWORK_MAGIC}  --address \$(cat addresses/utxo${N}.addr) --out-file /dev/stdout \\
        | jq  -r 'to_entries | [last] | .[0].key')

    ## Build the transaction
    CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli transaction build \\
        --tx-in \${TX_IN} \\
        --tx-out \$(cat addresses/${ADDR}.addr)+${AMOUNT_TRANSFERRED} \\
        --change-address \$(cat addresses/utxo${N}.addr) \\
        --testnet-magic ${NETWORK_MAGIC} \\
        --invalid-hereafter 100000 \\
        --out-file node-pool${N}/tx/tx${N}-era-funds.txbody

    ## Sign the transaction
    CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli transaction sign \\
        --signing-key-file addresses/utxo${N}.skey \\
        --testnet-magic ${NETWORK_MAGIC} \\
        --tx-body-file  node-pool${N}/tx/tx${N}-era-funds.txbody \\
        --out-file      node-pool${N}/tx/tx${N}-era-funds.tx

    ## Submit the transaction
    CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli transaction submit \\
        --tx-file node-pool${N}/tx/tx${N}-era-funds.tx \\
        --testnet-magic ${NETWORK_MAGIC}

    ## Wait for the transaction to be confirmed
    sleep 1

    # Write the era datum on chain
    TX_IN=\$(CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli query utxo \\
        --testnet-magic ${NETWORK_MAGIC}  --address \$(cat addresses/${ADDR}.addr) --out-file /dev/stdout \\
        | jq  -r 'to_entries | [last] | .[0].key')

    ## Build the transaction
    CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli transaction build \\
        --tx-in \${TX_IN} \\
        --tx-out \$(cat addresses/${ADDR}.addr)+${SCRIPT_TX_VALUE} \\
        --tx-out-inline-datum-file \${DATUM_FILE} \\
        --change-address \$(cat addresses/${ADDR}.addr) \\
        --testnet-magic ${NETWORK_MAGIC} \\
        --invalid-hereafter 100000 \\
        --out-file node-pool${N}/tx/tx${N}-era-datum.txbody

    ## Sign the transaction
    CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli transaction sign \\
        --signing-key-file addresses/${ADDR}.skey \\
        --testnet-magic ${NETWORK_MAGIC} \\
        --tx-body-file  node-pool${N}/tx/tx${N}-era-datum.txbody \\
        --out-file      node-pool${N}/tx/tx${N}-era-datum.tx

    ## Submit the transaction
    CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli transaction submit \\
        --tx-file node-pool${N}/tx/tx${N}-era-datum.tx \\
        --testnet-magic ${NETWORK_MAGIC}

    ## Wait for the transaction to be confirmed
    sleep 2
    
EOF

chmod u+x era-mithril.sh


cat >> pools.sh <<EOF
#!/usr/bin/env bash

CARDANO_NODE_SOCKET_PATH=node-pool1/ipc/node.sock ./cardano-cli query stake-pools \\
    --cardano-mode \\
    --testnet-magic ${NETWORK_MAGIC}
EOF

chmod u+x pools.sh

# Next is to prepare the pool metadata & env files
NODE_ID=1
PARTY_IDS=()
for NODE in ${POOL_NODES}; do
    PARTY_ID=$(./cardano-cli stake-pool id \
                --cold-verification-key-file ${NODE}/shelley/cold.vkey)
    PARTY_IDS[$NODE_ID]=$PARTY_ID
    echo PARTY_ID=${PARTY_ID} > ${NODE}/pool.env
    cat >> ${NODE}/metadata.json <<EOF
{
"name": "Mithril Pool ${NODE_ID}",
"description": "Mithril Pool ${NODE_ID}",
"ticker": "MITHRIL-DEVNET-SPO-${NODE_ID}",
"homepage": "https://mithril.network"
}
EOF
    NODE_ID=$(( $NODE_ID + 1))
done

# Delegation script
cat >> delegate.sh <<EOF
#!/usr/bin/env bash
set -e

CURRENT_EPOCH=\$(CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli query tip \\
                    --cardano-mode \\
                    --testnet-magic ${NETWORK_MAGIC} | jq .epoch)
echo ">>>> Current Epoch: \${CURRENT_EPOCH}"
EOF

# Prepare transactions for delegating to stake pools
for N in ${POOL_NODES_N}; do
  cat >> delegate.sh <<EOF
    AMOUNT_STAKED=\$(( $N*1000000 +  DELEGATION_ROUND*1 ))

    # Get the UTxO
    TX_IN=\$(CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli query utxo \\
      --testnet-magic ${NETWORK_MAGIC}  --address \$(cat addresses/utxo${N}.addr) --out-file /dev/stdout \\
      | jq  -r 'to_entries | [last] | .[0].key')

    # Build the transaction
    if [ "\$DELEGATION_ROUND" -eq 1 ]; then
      # First delegation round, we need to include registration certificate and delegation certificate
      CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli transaction build \\
          --tx-in \${TX_IN} \\
          --tx-out \$(cat addresses/user${N}.addr)+\${AMOUNT_STAKED} \\
          --change-address \$(cat addresses/utxo${N}.addr) \\
          --testnet-magic ${NETWORK_MAGIC} \\
          --certificate-file addresses/user${N}-stake.reg.cert \\
          --certificate-file addresses/user${N}-stake.deleg.cert \\
          --invalid-hereafter 100000 \\
          --out-file node-pool${N}/tx/tx${N}-\${DELEGATION_ROUND}.txbody \\
          --witness-override 2
    else
      # All other delegation rounds, we need to include only delegation certificate
      CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli transaction build \\
          --tx-in \${TX_IN} \\
          --tx-out \$(cat addresses/user${N}.addr)+\${AMOUNT_STAKED} \\
          --change-address \$(cat addresses/utxo${N}.addr) \\
          --testnet-magic ${NETWORK_MAGIC} \\
          --certificate-file addresses/user${N}-stake.deleg.cert \\
          --invalid-hereafter 100000 \\
          --out-file node-pool${N}/tx/tx${N}-\${DELEGATION_ROUND}.txbody \\
          --witness-override 2
    fi

    # Sign the transaction
    CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli transaction sign \\
        --signing-key-file addresses/utxo${N}.skey \\
        --signing-key-file addresses/user${N}-stake.skey \\
        --testnet-magic ${NETWORK_MAGIC} \\
        --tx-body-file  node-pool${N}/tx/tx${N}-\${DELEGATION_ROUND}.txbody \\
        --out-file      node-pool${N}/tx/tx${N}-\${DELEGATION_ROUND}.tx

    # Submit the transaction
    CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli transaction submit \\
        --tx-file node-pool${N}/tx/tx${N}-\${DELEGATION_ROUND}.tx \\
        --testnet-magic ${NETWORK_MAGIC}

EOF

done

chmod u+x delegate.sh
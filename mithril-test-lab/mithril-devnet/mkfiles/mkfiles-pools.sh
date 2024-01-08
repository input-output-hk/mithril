
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
                --cold-verification-key-file ${NODE}/shelley/operator.vkey)
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

# Prepare transactions for activating stake pools
for N in ${POOL_NODES_N}; do

  # We'll transfer funds to the user1, which delegates to pool1
  # We'll register certs to:
  #  1. delegate from the user1 stake address to the stake pool
  cat >> delegate.sh <<EOF
    
    AMOUNT_STAKED=\$(( $N*1000000 +  DELEGATION_ROUND*1 ))

    TX_IN=\$(CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli query utxo \\
      --testnet-magic ${NETWORK_MAGIC}  --address \$(cat addresses/utxo${N}.addr) --out-file tmp.txt \\
      && cat tmp.txt | jq  -r 'to_entries | [last] | .[0].key' \\
      && rm -f tmp.txt)

    CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli transaction build \\
        --alonzo-era \\
        --tx-in \${TX_IN} \\
        --tx-out \$(cat addresses/user${N}.addr)+\${AMOUNT_STAKED} \\
        --change-address \$(cat addresses/utxo${N}.addr) \\
        --testnet-magic ${NETWORK_MAGIC} \\
        --certificate-file addresses/user${N}-stake.deleg.cert \\
        --invalid-hereafter 100000 \\
        --out-file node-pool${N}/tx/tx${N}-\${DELEGATION_ROUND}.txbody \\
        --witness-override 2

EOF

  # So we'll need to sign this with a the following keys:
  # 1. the user1 stake address key, due to the delegation cert
  cat >> delegate.sh <<EOF
    CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli transaction sign \\
        --signing-key-file addresses/utxo${N}.skey \\
        --signing-key-file addresses/user${N}-stake.skey \\
        --testnet-magic ${NETWORK_MAGIC} \\
        --tx-body-file  node-pool${N}/tx/tx${N}-\${DELEGATION_ROUND}.txbody \\
        --out-file      node-pool${N}/tx/tx${N}-\${DELEGATION_ROUND}.tx

EOF

  # Copy submit transaction to delegate.sh script
  cat >> delegate.sh <<EOF
    CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli transaction submit \\
        --tx-file node-pool${N}/tx/tx${N}-\${DELEGATION_ROUND}.tx \\
        --testnet-magic ${NETWORK_MAGIC}

EOF

done

chmod u+x delegate.sh
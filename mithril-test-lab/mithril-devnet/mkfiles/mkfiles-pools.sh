
cat >> pools.sh <<EOF
#!/usr/bin/env bash

CARDANO_NODE_SOCKET_PATH=node-pool1/ipc/node.sock $CARDANO_CLI $CARDANO_CLI_ERA query stake-pools \\
    --cardano-mode \\
    --testnet-magic ${NETWORK_MAGIC}
EOF

chmod u+x pools.sh

# Next is to prepare the pool metadata & env files
NODE_ID=1
PARTY_IDS=()
for NODE in ${POOL_NODES}; do
    PARTY_ID=$($CARDANO_CLI $CARDANO_CLI_ERA stake-pool id \
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

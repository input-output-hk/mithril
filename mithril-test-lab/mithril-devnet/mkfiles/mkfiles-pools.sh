
cat >> pools.sh <<EOF
#!/usr/bin/env bash

CARDANO_NODE_SOCKET_PATH=node-bft1/ipc/node.sock ./cardano-cli query stake-pools \\
    --cardano-mode \\
    --testnet-magic ${NETWORK_MAGIC}
EOF

chmod u+x pools.sh

echo "Generated pools.sh script"
echo "====================================================================="
echo

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

echo "Generated pool env files:"
ls -1 node-*/pool.env
echo "====================================================================="

cat >> activate.sh <<EOF
#!/usr/bin/env bash
set -e

EOF

# Prepare transactions for activating stake pools
for N in ${POOL_NODES_N}; do

  AMOUNT_STAKED=$(( N*1000000 ))
  
  # We'll transfer funds to the user1, which delegates to pool1
  # We'll register certs to:
  #  1. register the pool-owner1 stake address
  #  2. register the stake pool 1
  #  3. register the user1 stake address
  #  4. delegate from the user1 stake address to the stake pool
  cat >> activate.sh <<EOF
CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli transaction build \\
    --alonzo-era \\
    --tx-in \$(./cardano-cli genesis initial-txin \\
                --testnet-magic ${NETWORK_MAGIC} \\
                --verification-key-file addresses/utxo${N}.vkey) \\
    --tx-out \$(cat addresses/user${N}.addr)+${AMOUNT_STAKED} \\
    --change-address \$(cat addresses/utxo${N}.addr) \\
    --testnet-magic ${NETWORK_MAGIC} \\
    --certificate-file addresses/pool-owner${N}-stake.reg.cert \\
    --certificate-file node-pool${N}/registration.cert \\
    --certificate-file addresses/user${N}-stake.reg.cert \\
    --certificate-file addresses/user${N}-stake.deleg.cert \\
    --invalid-hereafter 100000 \\
    --out-file node-pool${N}/tx/tx${N}.txbody \\
    --witness-override 4

EOF

  # So we'll need to sign this with a bunch of keys:
  # 1. the initial utxo spending key, for the funds
  # 2. the user1 stake address key, due to the delegation cert
  # 3. the pool1 owner key, due to the pool registration cert
  # 4. the pool1 operator key, due to the pool registration cert
  cat >> activate.sh <<EOF
CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli transaction sign \\
    --signing-key-file addresses/utxo${N}.skey \\
    --signing-key-file addresses/user${N}-stake.skey \\
    --signing-key-file node-pool${N}/owner.skey \\
    --signing-key-file node-pool${N}/shelley/operator.skey \\
    --testnet-magic ${NETWORK_MAGIC} \\
    --tx-body-file  node-pool${N}/tx/tx${N}.txbody \\
    --out-file      node-pool${N}/tx/tx${N}.tx

EOF

  # Copy submit transaction to activate.sh script
  cat >> activate.sh <<EOF
CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli transaction submit \\
    --tx-file node-pool${N}/tx/tx${N}.tx \\
    --testnet-magic ${NETWORK_MAGIC}

EOF

  # Copy retrieve transaction id to activate.sh script
  cat >> activate.sh <<EOF
CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli transaction txid \\
    --tx-file node-pool${N}/tx/tx${N}.tx

EOF

done

    # Wait until pools are activated on the Cardano network
    cat >> activate.sh <<EOF
echo ">> Wait for Cardano pools to be activated"
POOLS_ACTIVATION_WAIT_ROUND_DELAY=2
POOLS_ACTIVATION_WAIT_ROUNDS_MAX=\$(echo "scale=0; 20 * $EPOCH_LENGTH * $SLOT_LENGTH / \$POOLS_ACTIVATION_WAIT_ROUND_DELAY" | bc)
POOLS_ACTIVATION_WAIT_ROUNDS=1
POOL_STAKE_RETRIEVAL_WAIT_ROUND_DELAY=2
POOL_STAKE_RETRIEVAL_WAIT_ROUNDS_MAX=\$POOLS_ACTIVATION_WAIT_ROUNDS_MAX

while true
do
    POOLS=\$(./pools.sh 2> /dev/null)
    if [ "\$POOLS" != "" ] ; then
        echo ">>>> Cardano pools are activated!"
        ./pools.sh | while read POOL_ID ; do
            POOL_STAKE_RETRIEVAL_WAIT_ROUNDS=1
            echo ">>>> Retrieve stakes for pool: \$POOL_ID"
            while true
            do
                POOL_STAKE_PREVIOUS_EPOCH=\$(CARDANO_NODE_SOCKET_PATH=node-pool${N}/ipc/node.sock ./cardano-cli query stake-snapshot \\
                    --stake-pool-id \$POOL_ID \\
                    --testnet-magic ${NETWORK_MAGIC} | jq '.pools | values | flatten | .[0].stakeMark')
                if [ "\$POOL_STAKE_PREVIOUS_EPOCH" != "0" ] ; then
                    break
                else
                    echo ">>>> Stakes are not retrievable for this pool yet... [attempt \$POOL_STAKE_RETRIEVAL_WAIT_ROUNDS]"
                    sleep \$POOL_STAKE_RETRIEVAL_WAIT_ROUND_DELAY
                fi
                POOL_STAKE_RETRIEVAL_WAIT_ROUNDS=\$(( \$POOL_STAKE_RETRIEVAL_WAIT_ROUNDS + 1 ))
                if [ "\$POOL_STAKE_RETRIEVAL_WAIT_ROUNDS" -gt "\$POOL_STAKE_RETRIEVAL_WAIT_ROUNDS_MAX" ] ; then
                    echo ">>>> Timeout: could not retrieve stakes of pool \$POOL_ID within \$POOL_STAKE_RETRIEVAL_WAIT_ROUNDS_MAX attempts"
                    exit 1
                fi
            done
            echo ">>>> Stakes retrieved for pool: \$POOL_ID / \$POOL_STAKE_PREVIOUS_EPOCH Lovelace"
        done
        break
    else
        echo ">>>> Cardano pools are not activated yet... [attempt \$POOLS_ACTIVATION_WAIT_ROUNDS]"
        sleep \$POOLS_ACTIVATION_WAIT_ROUND_DELAY
    fi
    POOLS_ACTIVATION_WAIT_ROUNDS=\$(( \$POOLS_ACTIVATION_WAIT_ROUNDS + 1 ))
    if [ "\$POOLS_ACTIVATION_WAIT_ROUNDS" -gt "\$POOLS_ACTIVATION_WAIT_ROUNDS_MAX" ] ; then
        echo ">>>> Timeout: pools could not be activated within \$POOLS_ACTIVATION_WAIT_ROUNDS_MAX attempts"
        exit 1
    fi
done

echo ">> Cardano pools activation was successful!"
EOF

chmod u+x activate.sh

echo "Generated activate.sh script"
echo "====================================================================="
echo

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

echo "Generated delegate.sh script"
echo "====================================================================="
echo
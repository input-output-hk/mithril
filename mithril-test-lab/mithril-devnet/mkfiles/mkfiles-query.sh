cat >> query-mithril.sh <<EOF
#!/usr/bin/env bash

AGGREGATOR_API_ENDPOINT="http://0.0.0.0:8080/aggregator"

echo ">> Query pending certificate"
curl -sL \${AGGREGATOR_API_ENDPOINT}/certificate-pending | jq .
echo

echo ">> Query latest certificates"
curl -sL \${AGGREGATOR_API_ENDPOINT}/certificates | jq '.[:2]'
echo

echo ">> Query latest mithril stake distributions"
curl -sL \${AGGREGATOR_API_ENDPOINT}/artifact/mithril-stake-distributions | jq '.[:2]'
echo

echo ">> Query latest snapshots"
curl -sL \${AGGREGATOR_API_ENDPOINT}/artifact/snapshots | jq '.[:2]'
echo

EOF

cat >> query-cardano.sh <<EOF
#!/usr/bin/env bash

echo ">> Query chain tip"
CARDANO_NODE_SOCKET_PATH=node-bft1/ipc/node.sock ./cardano-cli query tip \\
    --cardano-mode \\
    --testnet-magic ${NETWORK_MAGIC} | jq .

echo
echo ">> Query whole utxo"
CARDANO_NODE_SOCKET_PATH=node-bft1/ipc/node.sock ./cardano-cli query utxo \\
    --cardano-mode \\
    --testnet-magic ${NETWORK_MAGIC} \\
    --whole-utxo
echo

echo ">> Query stake pools"
CARDANO_NODE_SOCKET_PATH=node-bft1/ipc/node.sock ./cardano-cli query stake-pools \\
    --cardano-mode \\
    --testnet-magic ${NETWORK_MAGIC}
echo

echo ">> Query stake distribution"
CARDANO_NODE_SOCKET_PATH=node-bft1/ipc/node.sock ./cardano-cli query stake-snapshot --all-stake-pools \\
    --cardano-mode \\
    --testnet-magic ${NETWORK_MAGIC} | jq .
echo

EOF

cat >> query-unused.sh <<EOF
#!/usr/bin/env bash

echo
echo ">> Query utxo1 utxo 'utxo1.addr'"
CARDANO_NODE_SOCKET_PATH=node-bft1/ipc/node.sock ./cardano-cli query utxo \\
    --cardano-mode \\
    --testnet-magic ${NETWORK_MAGIC} \\
    --address \$(cat addresses/utxo1.addr)

echo
echo ">> Query user1 utxo 'user1.addr'"
CARDANO_NODE_SOCKET_PATH=node-bft1/ipc/node.sock ./cardano-cli query utxo \\
    --cardano-mode \\
    --testnet-magic ${NETWORK_MAGIC} \\
    --address \$(cat addresses/user1.addr)

echo ">> Query stake pool params"
echo CARDANO_NODE_SOCKET_PATH=node-bft1/ipc/node.sock ./cardano-cli query pool-params \\
    --cardano-mode \\
    --testnet-magic ${NETWORK_MAGIC} \\
    --stake-pool-id \${STAKE_POOL_ID}
echo

echo ">> Query stake pool snapshot"
echo CARDANO_NODE_SOCKET_PATH=node-bft1/ipc/node.sock ./cardano-cli query stake-snapshot \\
    --cardano-mode \\
    --testnet-magic ${NETWORK_MAGIC} \\
    --stake-pool-id \${STAKE_POOL_ID}
echo

EOF

chmod u+x query-mithril.sh
chmod u+x query-cardano.sh

echo "Generated query-mithril.sh script"
echo "Generated query-cardano.sh script"
echo "====================================================================="
echo

cat >> log-mithril.sh <<EOF
#!/usr/bin/env bash

SEPARATOR="---------------------------------------------------------------------"

# Mithril nodes logs
docker ps --format='{{.Names}}' | grep "mithril" | sort -n | xargs -i  sh -c 'echo '\${SEPARATOR}' && echo docker logs -n '\${LINES}' {} && echo '\${SEPARATOR}' && docker logs -n '\${LINES}' {} && echo '\${SEPARATOR}' && echo'

EOF
chmod u+x log-mithril.sh

cat >> log-cardano.sh <<EOF
#!/usr/bin/env bash

SEPARATOR="---------------------------------------------------------------------"

# Cardano nodes logs
find . -type f -print | grep "node.log" | sort -n | xargs -i  sh -c 'echo '\${SEPARATOR}' && echo tail -n '\${LINES}' {} && echo '\${SEPARATOR}' && tail -n '\${LINES}' {} && echo '\${SEPARATOR}' && echo'

EOF
chmod u+x log-cardano.sh
echo "====================================================================="
echo
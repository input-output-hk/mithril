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
CARDANO_NODE_SOCKET_PATH=node-pool1/ipc/node.sock $CARDANO_CLI $CARDANO_CLI_ERA query tip \\
    --cardano-mode \\
    --testnet-magic ${NETWORK_MAGIC} | jq .

echo
echo ">> Query whole utxo"
CARDANO_NODE_SOCKET_PATH=node-pool1/ipc/node.sock $CARDANO_CLI $CARDANO_CLI_ERA query utxo \\
    --cardano-mode \\
    --testnet-magic ${NETWORK_MAGIC} \\
    --whole-utxo
echo

echo ">> Query stake pools"
CARDANO_NODE_SOCKET_PATH=node-pool1/ipc/node.sock $CARDANO_CLI $CARDANO_CLI_ERA query stake-pools \\
    --cardano-mode \\
    --testnet-magic ${NETWORK_MAGIC}
echo

echo ">> Query stake distribution"
CARDANO_NODE_SOCKET_PATH=node-pool1/ipc/node.sock $CARDANO_CLI $CARDANO_CLI_ERA query stake-snapshot --all-stake-pools \\
    --cardano-mode \\
    --testnet-magic ${NETWORK_MAGIC} | jq .
echo

EOF

chmod u+x query-mithril.sh
chmod u+x query-cardano.sh

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

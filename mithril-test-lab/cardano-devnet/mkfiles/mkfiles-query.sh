cat >> query-cardano.sh <<EOF
#!/usr/bin/env bash

echo ">> Query chain tip"
CARDANO_NODE_SOCKET_PATH=node-pool1/ipc/node.sock $CARDANO_CLI $CARDANO_CLI_ERA query tip \\
    --testnet-magic ${NETWORK_MAGIC} | jq .

echo
echo ">> Query whole utxo"
CARDANO_NODE_SOCKET_PATH=node-pool1/ipc/node.sock $CARDANO_CLI $CARDANO_CLI_ERA query utxo \\
    --testnet-magic ${NETWORK_MAGIC} \\
    --whole-utxo
echo

echo ">> Query stake pools"
CARDANO_NODE_SOCKET_PATH=node-pool1/ipc/node.sock $CARDANO_CLI $CARDANO_CLI_ERA query stake-pools \\
    --testnet-magic ${NETWORK_MAGIC}
echo

echo ">> Query stake distribution"
CARDANO_NODE_SOCKET_PATH=node-pool1/ipc/node.sock $CARDANO_CLI $CARDANO_CLI_ERA query stake-snapshot --all-stake-pools \\
    --testnet-magic ${NETWORK_MAGIC} | jq .
echo

EOF

chmod u+x query-cardano.sh

cat >> log-cardano.sh <<EOF
#!/usr/bin/env bash

SEPARATOR="---------------------------------------------------------------------"

# Cardano nodes logs
find . -type f -print | grep "node.log" | sort -n | xargs -i  sh -c 'echo '\${SEPARATOR}' && echo tail -n '\${LINES}' {} && echo '\${SEPARATOR}' && tail -n '\${LINES}' {} && echo '\${SEPARATOR}' && echo'

EOF
chmod u+x log-cardano.sh

cat >> log-dmq.sh <<EOF
#!/usr/bin/env bash

SEPARATOR="---------------------------------------------------------------------"

# DMQ nodes logs
find . -type f -print | grep "dmq.node.log" | sort -n | xargs -i  sh -c 'echo '\${SEPARATOR}' && echo tail -n '\${LINES}' {} && echo '\${SEPARATOR}' && tail -n '\${LINES}' {} && echo '\${SEPARATOR}' && echo'

EOF
chmod u+x log-dmq.sh

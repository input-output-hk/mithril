

echo "====================================================================="
echo
echo "First change directory:"
echo
echo cd ${ROOT}
echo
echo "To start the nodes, in separate terminals use:"
echo
cat >> start-cardano.sh <<EOF
#!/usr/bin/env bash

echo ">> Start Cardano network"
killall cardano-node > /dev/null 2>&1

# Stop when there's an error, activate it after the killall since it will report an error if it doesn't kill anything
set -e

./cardano-cli --version
./cardano-node --version

EOF
for NODE in ${BFT_NODES}; do

  echo ./${ROOT}/${NODE}/start-node.sh

  cat >> ${NODE}/start-node.sh <<EOF
#!/usr/bin/env bash

./cardano-node run \\
  --config                          ${NODE}/configuration.yaml \\
  --topology                        ${NODE}/topology.json \\
  --database-path                   ${NODE}/db \\
  --socket-path                     ${NODE}/ipc/node.sock \\
  --shelley-kes-key                 ${NODE}/shelley/kes.skey \\
  --shelley-vrf-key                 ${NODE}/shelley/vrf.skey \\
  --shelley-operational-certificate ${NODE}/shelley/node.cert \\
  --port                            $(cat ${NODE}/port) \\
  --delegation-certificate          ${NODE}/byron/delegate.cert \\
  --signing-key                     ${NODE}/byron/delegate.key \\
  > ${NODE}/node.log
EOF
  chmod u+x ${NODE}/start-node.sh

  cat >> start-cardano.sh <<EOF
echo ">> Starting Cardano node '${NODE}'"
./${NODE}/start-node.sh &

EOF

done
for NODE in ${POOL_NODES}; do

  echo ./${ROOT}/${NODE}/start-node.sh

  cat >> ${NODE}/start-node.sh <<EOF
#!/usr/bin/env bash

./cardano-node run \\
  --config                          ${NODE}/configuration.yaml \\
  --topology                        ${NODE}/topology.json \\
  --database-path                   ${NODE}/db \\
  --socket-path                     ${NODE}/ipc/node.sock \\
  --shelley-kes-key                 ${NODE}/shelley/kes.skey \\
  --shelley-vrf-key                 ${NODE}/shelley/vrf.skey \\
  --shelley-operational-certificate ${NODE}/shelley/node.cert \\
  --port                            $(cat ${NODE}/port) \\
  > ${NODE}/node.log
EOF
  chmod u+x ${NODE}/start-node.sh

  cat >> start-cardano.sh <<EOF
echo ">> Starting Cardano node '${NODE}'"
./${NODE}/start-node.sh &

EOF

done

cat >> start-cardano.sh <<EOF
echo ">> Wait for Cardano network to be ready"
CARDANO_ACTIVATION_WAIT_ROUNDS_MAX=30
CARDANO_ACTIVATION_WAIT_ROUNDS=1
CARDANO_ACTIVATION_WAIT_ROUND_DELAY=2
while true
do
    EPOCH=\$(CARDANO_NODE_SOCKET_PATH=node-bft1/ipc/node.sock ./cardano-cli query tip --cardano-mode --testnet-magic ${NETWORK_MAGIC} 2> /dev/null | jq -r .epoch)
    if [ "\$EPOCH" != "" ] ; then
        echo ">>>> Cardano network is ready!"
        break
    else
        echo ">>>> Cardano network is not ready yet... [attempt \$CARDANO_ACTIVATION_WAIT_ROUNDS]"
        sleep \$CARDANO_ACTIVATION_WAIT_ROUND_DELAY
    fi
    CARDANO_ACTIVATION_WAIT_ROUNDS=\$(( \$CARDANO_ACTIVATION_WAIT_ROUNDS + 1 ))
    if [ "\$CARDANO_ACTIVATION_WAIT_ROUNDS" -gt "\$CARDANO_ACTIVATION_WAIT_ROUNDS_MAX" ] ; then
        echo ">>>> Timeout: Cardano network could no start within \$CARDANO_ACTIVATION_WAIT_ROUNDS_MAX attempts"
        exit 1
    fi
done

echo ">> Activate Cardano pools"
./activate.sh ${ROOT}

EOF

cat >> start-cardano.sh <<EOF
echo ">> Wait for Cardano nodes to have enough immutable files"
for NODE in ${ALL_NODES}; do
  echo ">> Wait for \${NODE} to have enough immutable files"
  NODE_ACTIVATION_WAIT_ROUNDS_MAX=100
  NODE_ACTIVATION_WAIT_ROUNDS=1
  NODE_ACTIVATION_WAIT_ROUND_DELAY=2
  while true
  do
      TOTAL_IMMUTABLE_FILES=\$(ls -1q \${NODE}/db/immutable | grep ".chunk" | wc -l)
      if [ "\$TOTAL_IMMUTABLE_FILES" -gt "1" ] ; then
          echo ">>>> \${NODE} has enough immutable files!"
          break
      else
          echo ">>>> \${NODE} has not enough immutable files yet... [attempt \$NODE_ACTIVATION_WAIT_ROUNDS]"
          sleep \$NODE_ACTIVATION_WAIT_ROUND_DELAY
      fi
      NODE_ACTIVATION_WAIT_ROUNDS=\$(( \$NODE_ACTIVATION_WAIT_ROUNDS + 1 ))
      if [ "\$NODE_ACTIVATION_WAIT_ROUNDS" -gt "\$NODE_ACTIVATION_WAIT_ROUNDS_MAX" ] ; then
          echo ">>>> Timeout: \${NODE} has not enough immutable files within \$NODE_ACTIVATION_WAIT_ROUNDS_MAX attempts"
          exit 1
      fi
  done
done

EOF

chmod u+x start-cardano.sh

cat >> start-mithril.sh <<EOF
#!/usr/bin/env bash

echo ">> Start Mithril network"
if [ -z "\${MITHRIL_IMAGE_ID}" ]; then 
  export MITHRIL_AGGREGATOR_IMAGE="mithril/mithril-aggregator"
  export MITHRIL_CLIENT_IMAGE="mithril/mithril-client"
  export MITHRIL_SIGNER_IMAGE="mithril/mithril-signer"
  echo ">> Build Mithril node Docker images"
  PWD=$(pwd)
  cd ../../../
  if [ -z "\${MITHRIL_NODE_DOCKER_BUILD_TYPE}" ]; then 
    MITHRIL_NODE_DOCKER_BUILD_TYPE=ci
  fi
  if [ -z "\${MITHRIL_NODE_DOCKER_CI_IMAGE_FROM}" ]; then 
    MITHRIL_NODE_DOCKER_CI_IMAGE_FROM=debian:12-slim
  fi
  export DOCKER_IMAGE_FROM=\$MITHRIL_NODE_DOCKER_CI_IMAGE_FROM
  if [ "\${MITHRIL_NODE_DOCKER_BUILD_TYPE}" = "ci" ]; then
    DOCKER_BUILD_CMD="make docker-build-ci" 
  else
    DOCKER_BUILD_CMD="make docker-build"
  fi
  echo ">>>> Docker builder will build images with command: '\$DOCKER_BUILD_CMD'"
  echo ">>>> Building Mithril Aggregator node Docker image"
  cd mithril-aggregator && \$DOCKER_BUILD_CMD && cd ..
  echo ">>>> Building Mithril Client node Docker image"
  cd mithril-client-cli && \$DOCKER_BUILD_CMD && cd ..
  echo ">>>> Building Mithril Signer node Docker image"
  cd mithril-signer && \$DOCKER_BUILD_CMD && cd ..
  cd $PWD
else
  export MITHRIL_AGGREGATOR_IMAGE="ghcr.io/input-output-hk/mithril-aggregator:\${MITHRIL_IMAGE_ID}"
  export MITHRIL_CLIENT_IMAGE="ghcr.io/input-output-hk/mithril-client:\${MITHRIL_IMAGE_ID}"
  export MITHRIL_SIGNER_IMAGE="ghcr.io/input-output-hk/mithril-signer:\${MITHRIL_IMAGE_ID}"
fi

docker compose rm -f
docker compose -f docker-compose.yaml --profile mithril up --remove-orphans --force-recreate -d --no-build

echo ">> List of Mithril signers"
    echo --------,--------------------------------------------------------,----------------------------------- | column -t -s,                                                 
EOF

for NODE in ${POOL_NODES}; do
    cat >> start-mithril.sh <<EOF
    cat ${NODE}/info.json | jq -r '"\(.name),\(.pool_id),\(.description)"' | column -t -s,
EOF
done

cat >> start-mithril.sh <<EOF

echo ">> Wait for Mithril signers to be registered"
EPOCH_NOW=\$(CARDANO_NODE_SOCKET_PATH=node-bft1/ipc/node.sock ./cardano-cli query tip --cardano-mode --testnet-magic ${NETWORK_MAGIC} 2> /dev/null | jq -r .epoch)
while true
do
    EPOCH=\$(CARDANO_NODE_SOCKET_PATH=node-bft1/ipc/node.sock ./cardano-cli query tip --cardano-mode --testnet-magic ${NETWORK_MAGIC} 2> /dev/null | jq -r .epoch)
    EPOCH_DELTA=\$(( \$EPOCH - \$EPOCH_NOW ))
    if [ \$EPOCH_DELTA -ge 2 ] ; then
        echo ">>>> Ready!"
        break
    else
        echo ">>>> Not ready yet"
        sleep 2
    fi
done

echo ">> Bootstrap the Genesis certificate"
docker compose -f docker-compose.yaml --profile mithril-genesis run mithril-aggregator-genesis

EOF
chmod u+x start-mithril.sh

cat >> stop.sh <<EOF
#!/usr/bin/env bash

echo ">> Stop Cardano network"
killall cardano-node

echo ">> Stop Mithril network"
if [ -z "\${MITHRIL_IMAGE_ID}" ]; then 
  export MITHRIL_AGGREGATOR_IMAGE="mithril/mithril-aggregator"
  export MITHRIL_CLIENT_IMAGE="mithril/mithril-client"
  export MITHRIL_SIGNER_IMAGE="mithril/mithril-signer"
else
  export MITHRIL_AGGREGATOR_IMAGE="ghcr.io/input-output-hk/mithril-aggregator:\${MITHRIL_IMAGE_ID}"
  export MITHRIL_CLIENT_IMAGE="ghcr.io/input-output-hk/mithril-client:\${MITHRIL_IMAGE_ID}"
  export MITHRIL_SIGNER_IMAGE="ghcr.io/input-output-hk/mithril-signer:\${MITHRIL_IMAGE_ID}"
fi
docker compose -f docker-compose.yaml --profile mithril down
EOF
chmod u+x stop.sh
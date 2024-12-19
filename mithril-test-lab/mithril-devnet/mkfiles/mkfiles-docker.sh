
# Mithril genesis keys
GENESIS_VERIFICATION_KEY=5b33322c3235332c3138362c3230312c3137372c31312c3131372c3133352c3138372c3136372c3138312c3138382c32322c35392c3230362c3130352c3233312c3135302c3231352c33302c37382c3231322c37362c31362c3235322c3138302c37322c3133342c3133372c3234372c3136312c36385d
GENESIS_SECRET_KEY=5b3131382c3138342c3232342c3137332c3136302c3234312c36312c3134342c36342c39332c3130362c3232392c38332c3133342c3138392c34302c3138392c3231302c32352c3138342c3136302c3134312c3233372c32362c3136382c35342c3233392c3230342c3133392c3131392c31332c3139395d
CHAIN_OBSERVER_TYPE=pallas

cat >> docker-compose.yaml <<EOF
services:
EOF

for NODE in ${FULL_NODES}; do

    PORT=$(cat ${NODE}/port)
    HOST=$(cat ${NODE}/host)
cat >> docker-compose.yaml <<EOF
  cardano-${NODE}:
    image: ghcr.io/intersectmbo/cardano-node:${CARDANO_NODE_VERSION}
    profiles:
      - cardano
    volumes:
    - ./${NODE}:/data:z
    environment:
    - CARDANO_NODE_SOCKET_PATH=/data/ipc/node.sock
    networks:
      cardano_network:
        ipv4_address: ${HOST}
    ports:
    - "${PORT}:3001"
    command:
      [
        "run",
        "--config",
        "/data/configuration.yaml",
        "--topology",
        "/data/topology.json",
        "--database-path",
        "/data/db",
        "--socket-path",
        "/data/ipc/node.sock",
        "--shelley-operational-certificate",
        "/data/shelley/opcert.cert",
        "--shelley-kes-key",
        "/data/shelley/kes.skey",
        "--shelley-vrf-key",
        "/data/shelley/vrf.skey",
        "--delegation-certificate",
        "/data/byron/delegate.cert",
        "--signing-key",
        "/data/byron/delegate.key",
        "--host-addr",
        "${HOST}",
        "--port",
        "3001"
      ]

EOF

done

for NODE in ${POOL_NODES}; do

    PORT=$(cat ${NODE}/port)
    HOST=$(cat ${NODE}/host)
cat >> docker-compose.yaml <<EOF
  cardano-${NODE}:
    image: ghcr.io/intersectmbo/cardano-node:latest
    profiles:
      - cardano
    volumes:
    - ./${NODE}:/data:z
    environment:
    - CARDANO_NODE_SOCKET_PATH=/data/ipc/node.sock
    networks:
      cardano_network:
        ipv4_address: ${HOST}
    ports:
    - "${PORT}:3001"
    command:
      [
        "run",
        "--config",
        "/data/configuration.yaml",
        "--topology",
        "/data/topology.json",
        "--database-path",
        "/data/db",
        "--socket-path",
        "/data/ipc/node.sock",
        "--shelley-operational-certificate",
        "/data/shelley/opcert.cert",
        "--shelley-kes-key",
        "/data/shelley/kes.skey",
        "--shelley-vrf-key",
        "/data/shelley/vrf.skey",
        "--host-addr",
        "${HOST}",
        "--port",
        "3001"
      ]

EOF

done

for NODE in ${FULL_NODES}; do

cat >> docker-compose.yaml <<EOF
  mithril-aggregator:
    image: \${MITHRIL_AGGREGATOR_IMAGE}
    restart: always
    profiles:
      - mithril
    volumes:
      - ./${NODE}:/data:z
    networks:
    - mithril_network
    ports:
      - "8080:8080"
    environment:
      - RUST_BACKTRACE=1
      - GOOGLE_APPLICATION_CREDENTIALS_JSON=
      - NETWORK=devnet
      - NETWORK_MAGIC=${NETWORK_MAGIC}
      - PROTOCOL_PARAMETERS__K=5
      - PROTOCOL_PARAMETERS__M=100
      - PROTOCOL_PARAMETERS__PHI_F=0.65
      - RUN_INTERVAL=1000
      - SNAPSHOT_STORE_TYPE=local
      - SNAPSHOT_UPLOADER_TYPE=local
      - SNAPSHOT_COMPRESSION_ALGORITHM=zstandard
      - DATA_STORES_DIRECTORY=/data/mithril/aggregator/stores
      - CARDANO_NODE_SOCKET_PATH=/data/ipc/node.sock
      - CARDANO_NODE_VERSION=${CARDANO_NODE_VERSION}
      - CARDANO_CLI_PATH=/app/bin/cardano-cli
      - CHAIN_OBSERVER_TYPE=${CHAIN_OBSERVER_TYPE}
      - GENESIS_VERIFICATION_KEY=${GENESIS_VERIFICATION_KEY}
      - DB_DIRECTORY=/data/db
      - SNAPSHOT_DIRECTORY=/data/mithril/aggregator
      - SERVER_PORT=8080
    command:
      [
        "-vvv",
        "serve"
      ]

  mithril-aggregator-genesis:
    image: \${MITHRIL_AGGREGATOR_IMAGE}
    profiles:
      - mithril-genesis
    volumes:
      - ./${NODE}:/data
    networks:
    - mithril_network
    ports:
      - "8080:8080"
    environment:
      - RUST_BACKTRACE=1
      - GOOGLE_APPLICATION_CREDENTIALS_JSON=
      - NETWORK=devnet
      - NETWORK_MAGIC=${NETWORK_MAGIC}
      - PROTOCOL_PARAMETERS__K=5
      - PROTOCOL_PARAMETERS__M=100
      - PROTOCOL_PARAMETERS__PHI_F=0.65
      - RUN_INTERVAL=1000
      - SNAPSHOT_STORE_TYPE=local
      - SNAPSHOT_UPLOADER_TYPE=local
      - DATA_STORES_DIRECTORY=/data/mithril/aggregator/stores
      - CARDANO_NODE_SOCKET_PATH=/data/ipc/node.sock
      - CARDANO_NODE_VERSION=${CARDANO_NODE_VERSION}
      - CARDANO_CLI_PATH=/app/bin/cardano-cli
      - CHAIN_OBSERVER_TYPE=${CHAIN_OBSERVER_TYPE}
      - GENESIS_VERIFICATION_KEY=${GENESIS_VERIFICATION_KEY}
      - GENESIS_SECRET_KEY=${GENESIS_SECRET_KEY}
      - DB_DIRECTORY=/data/db
    command:
      [
        "-vvv",
        "genesis",
        "bootstrap"
      ]
    
EOF
break

done

NODE_IX=0
for NODE in ${POOL_NODES}; do
    NODE_ID=$(( $NODE_IX + 1))
if [ `expr $NODE_IX % 2` == 0 ] || [ -z "${WITH_UNCERTIFIED_SIGNERS}" ]; then 
    # 50% of signers with key certification
    cat >> ${NODE}/info.json <<EOF
{
"name": "Signer ${NODE_ID}",
"description": "Certified PoolId",
"pool_id": "${PARTY_IDS[$NODE_ID]}"
}
EOF

    cat >> docker-compose.yaml <<EOF
  mithril-signer-${NODE}:
    image: \${MITHRIL_SIGNER_IMAGE}
    restart: always
    profiles:
      - mithril
    volumes:
      - ./${NODE}:/data:z
    networks:
    - mithril_network
    env_file:
    - ./${NODE}/pool.env
    environment:
      - RUST_BACKTRACE=1
      - AGGREGATOR_ENDPOINT=http://mithril-aggregator:8080/aggregator
      - NETWORK=devnet
      - NETWORK_MAGIC=${NETWORK_MAGIC}
      - RUN_INTERVAL=700
      - DB_DIRECTORY=/data/db
      - DATA_STORES_DIRECTORY=/data/mithril/signer-${NODE}/stores
      - CARDANO_NODE_SOCKET_PATH=/data/ipc/node.sock
      - CARDANO_CLI_PATH=/app/bin/cardano-cli
      - KES_SECRET_KEY_PATH=/data/shelley/kes.skey
      - OPERATIONAL_CERTIFICATE_PATH=/data/shelley/opcert.cert
    command:
      [
        "-vvv"
      ]

EOF
else
    # 50% of signers without key certification (legacy)
    # TODO: Should be removed once the signer certification is fully deployed
    cat >> ${NODE}/info.json <<EOF
{
"name": "Signer ${NODE_ID}",
"description": "Uncertified PoolId (Legacy)",
"pool_id": "${PARTY_IDS[$NODE_ID]}"
}
EOF

cat >> docker-compose.yaml <<EOF
  mithril-signer-${NODE}:
    image: \${MITHRIL_SIGNER_IMAGE}
    restart: always
    profiles:
      - mithril
    volumes:
      - ./${NODE}:/data:z
    networks:
    - mithril_network
    env_file:
    - ./${NODE}/pool.env
    environment:
      - RUST_BACKTRACE=1
      - AGGREGATOR_ENDPOINT=http://mithril-aggregator:8080/aggregator
      - NETWORK=devnet
      - NETWORK_MAGIC=${NETWORK_MAGIC}
      - RUN_INTERVAL=700
      - DB_DIRECTORY=/data/db
      - DATA_STORES_DIRECTORY=/data/mithril/signer-${NODE}/stores
      - CARDANO_NODE_SOCKET_PATH=/data/ipc/node.sock
      - CARDANO_CLI_PATH=/app/bin/cardano-cli
    command:
      [
        "-vvv"
      ]

EOF
fi

    NODE_IX=$(( $NODE_IX + 1))

done

cat >> docker-compose.yaml <<EOF
  mithril-client:
    image: \${MITHRIL_CLIENT_IMAGE}
    profiles:
      - mithril-client
    networks:
    - mithril_network
    environment:
      - RUST_BACKTRACE=1
      - AGGREGATOR_ENDPOINT=http://mithril-aggregator:8080/aggregator
      - NETWORK=devnet
      - GENESIS_VERIFICATION_KEY=${GENESIS_VERIFICATION_KEY}
    
EOF

cat >> docker-compose.yaml <<EOF
networks:
  mithril_network:
    driver: bridge
  cardano_network:
    driver: bridge
    
EOF
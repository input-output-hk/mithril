
# Debug mode
if [ -v DEBUG ]; then
    set -x
fi

# Check if all env vars are set 
if [ -z "${NETWORK}" ]; then
    echo Missing environment variable: NETWORK
    exit 1
fi

if [ -z "${NETWORK_MAGIC}" ]; then
    echo Missing environment variable: NETWORK_MAGIC
    exit 1
fi

if [ -z "${CARDANO_NODE}" ]; then
    echo Missing environment variable: CARDANO_NODE
    exit 1
fi

if [ -z "${SIGNER_NODE}" ]; then
    echo Missing environment variable: SIGNER_NODE
    exit 1
fi

# Cardano cli function
CARDANO_CLI_CMD() {
    docker exec ${CARDANO_NODE} cardano-cli ${@}
}

# Compute auxiliary env vars
POOL_ARTIFACTS_DIR=/pool
POOL_ARTIFACTS_DIR_PREFIX=./data/${NETWORK}/${SIGNER_NODE}/cardano
POOL_WWW_DIR=/www
GENESIS_FILE=/home/curry/docker/cardano-configurations/network/preview/genesis/shelley.json
if [ -z "${AMOUNT_STAKED}" ]; then
    AMOUNT_STAKED=1000000000
fi

#if [ -z "${TX_IN}" ]; then 
#  TX_IN=$(CARDANO_CLI_CMD query utxo  \
#            --testnet-magic $NETWORK_MAGIC \
#            --address $(cat ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/payment.addr) | tail -n 1  | awk '{print $1;}')#1
#fi
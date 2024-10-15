
# Exit on error
set -e

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

# Compute current Cardano era if needed
if [ -z "${CARDANO_ERA}" ]; then
    CARDANO_ERA=$(CARDANO_CLI_CMD query tip --testnet-magic $NETWORK_MAGIC | jq  -r '.era |= ascii_downcase | .era')
fi

# Compute auxiliary env vars
POOL_ARTIFACTS_DIR=/pool
POOL_ARTIFACTS_DIR_PREFIX=./data/${NETWORK}/${SIGNER_NODE}/cardano
POOL_WWW_DIR=/www
if [ -z "${AMOUNT_STAKED}" ]; then
    AMOUNT_STAKED=1000000000
fi

# Get the operating system
UNAME=$(uname -s) OPERATING_SYSTEM=
case $UNAME in
  Darwin )      OPERATING_SYSTEM="macos";;
  Linux )       OPERATING_SYSTEM="linux";;
  * )           echo "Error: Unsupported operating system. This script can only be run on Linux or MacOS."
                exit 1;;
esac

# Cardano node version
if [ -z "${CARDANO_NODE_VERSION}" ]; then 
  CARDANO_NODE_VERSION="9.1.0"
fi
if [ -z "${CARDANO_NODE_VERSION_RELEASE}" ]; then 
  CARDANO_NODE_VERSION_RELEASE=$(echo "${CARDANO_NODE_VERSION}" | cut -d'-' -f1)
fi
if [ -z "${CARDANO_BINARY_URL}" ]; then 
  CARDANO_BINARY_URL="https://github.com/input-output-hk/cardano-node/releases/download/${CARDANO_NODE_VERSION}/cardano-node-${CARDANO_NODE_VERSION_RELEASE}-${OPERATING_SYSTEM}.tar.gz"
fi
if [ -z "${NETWORK_MAGIC}" ]; then 
  NETWORK_MAGIC=42
fi
if [ -z "${HARD_FORK_BABBAGE_AT_EPOCH}" ]; then 
  HARD_FORK_BABBAGE_AT_EPOCH=0
fi
if [ -n "${CARDANO_HARD_FORK_LATEST_ERA_AT_EPOCH}" ]; then 
  HARD_FORK_CONWAY_AT_EPOCH=$(( HARD_FORK_BABBAGE_AT_EPOCH + CARDANO_HARD_FORK_LATEST_ERA_AT_EPOCH ))
fi
if [ -z "${HARD_FORK_CONWAY_AT_EPOCH}" ]; then 
  HARD_FORK_CONWAY_AT_EPOCH=0
fi
if [ -z "${NUM_FULL_NODES}" ]; then 
  NUM_FULL_NODES="1"
fi
if [ -z "${NUM_POOL_NODES}" ]; then 
  NUM_POOL_NODES="2"
fi
if [ -z "${SLOT_LENGTH}" ]; then 
  SLOT_LENGTH="0.75"
fi
if [ -z "${EPOCH_LENGTH}" ]; then 
  EPOCH_LENGTH="100"
fi
if [ -z "${LISTENING_ADDR}" ]; then 
  LISTENING_ADDR="127.0.0.1"
fi
DEVNET_VERSION=$(cat VERSION)

# Display configuration summary
echo ">> Devnet Version: ${DEVNET_VERSION}"
echo ">> Artifacts Directory[env::ARTIFACTS_DIR]: ${ARTIFACTS_DIR}"
echo ">> Cardano Full nodes [env::NUM_FULL_NODES]: ${NUM_FULL_NODES}"
echo ">> Cardano SPO nodes [env::NUM_POOL_NODES]: ${NUM_POOL_NODES}"
echo ">> Cardano Node Version [env::CARDANO_NODE_VERSION]: ${CARDANO_NODE_VERSION}"
echo ">> Cardano Network Magic [env::NETWORK_MAGIC]: ${NETWORK_MAGIC}"
echo ">> Cardano Hard Fork Babbage At Epoch [env::HARD_FORK_BABBAGE_AT_EPOCH]: ${HARD_FORK_BABBAGE_AT_EPOCH}"
echo ">> Cardano Hard Fork Conway At Epoch [env::HARD_FORK_CONWAY_AT_EPOCH]: ${HARD_FORK_CONWAY_AT_EPOCH}"
echo ">> Cardano Slot Length [env::SLOT_LENGTH]: ${SLOT_LENGTH}s"
echo ">> Cardano Epoch Length [env::EPOCH_LENGTH]: ${EPOCH_LENGTH}s"
echo ">> Cardano Listening Address [env::LISTENING_ADDR]: ${LISTENING_ADDR}"

# Check if root directory already exists
if ! mkdir -p "${ARTIFACTS_DIR}"; then
  echo "The ${ARTIFACTS_DIR} directory already exists, please move or remove it"
  exit
fi

echo ">> Creating artifacts directory..."
# Download cardano-cli & cardano-node if enabled (default: yes)
if [[ "$SKIP_CARDANO_BIN_DOWNLOAD" != "true" ]]; then
  echo ">> Downloading cardano-cli & cardano-node..."
  curl -sL "${CARDANO_BINARY_URL}" --output cardano-bin.tar.gz
  echo ">> Extracting cardano-cli & cardano-node..."
  tar xzf cardano-bin.tar.gz ./bin || (mkdir -p ./bin && tar --strip-components=1 -C ./bin -xzf cardano-bin.tar.gz)
  rm -f cardano-bin.tar.gz
fi

cp -r ./bin "${ARTIFACTS_DIR}/" 2>/dev/null || true
rm -rf ./bin

# Switch to artifacts directory
pushd "${ARTIFACTS_DIR}" > /dev/null || exit

# Create addresses sub-directory
mkdir addresses

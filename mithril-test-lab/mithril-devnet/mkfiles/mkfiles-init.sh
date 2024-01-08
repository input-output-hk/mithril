# Cardano node version
if [ -z "${CARDANO_NODE_VERSION}" ]; then 
  CARDANO_NODE_VERSION="8.1.2"
fi
if [ -z "${CARDANO_BINARY_URL}" ]; then 
  CARDANO_BINARY_URL="https://github.com/input-output-hk/cardano-node/releases/download/${CARDANO_NODE_VERSION}/cardano-node-${CARDANO_NODE_VERSION}-linux.tar.gz"
fi
if [ -z "${NETWORK_MAGIC}" ]; then 
  NETWORK_MAGIC=42
fi
if [ -z "${NUM_BFT_NODES}" ]; then 
  NUM_BFT_NODES="1"
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

# Display configuration summary
echo ">> Directory: ${ROOT}"
echo ">> Nodes: ${NODES}"
echo ">> Cardano Node Version: ${CARDANO_NODE_VERSION}"
echo ">> Cardano Network Magic: ${NETWORK_MAGIC}"
echo ">> Cardano BFT nodes: ${NUM_BFT_NODES}"
echo ">> Cardano SPO nodes: ${NUM_POOL_NODES}"
echo ">> Cardano Slot Length: ${SLOT_LENGTH}s"
echo ">> Cardano Epoch Length: ${EPOCH_LENGTH}s"
echo ">> Cardano Delegation Period: ${DELEGATE_PERIOD}s"
echo ">> Cardano Listening Address: ${LISTENING_ADDR}"

# Check if root directory already exists
if ! mkdir -p "${ROOT}"; then
  echo ">> The ${ROOT} directory already exists, please move or remove it"
  exit
fi

# Download cardano-cli & cardano-node if enabled (default: yes)
if [[ "$SKIP_CARDANO_BIN_DOWNLOAD" != "true" ]]; then
  echo ">> Downloading cardano-cli & cardano-node..."
  curl -sL ${CARDANO_BINARY_URL} --output cardano-bin.tar.gz
  tar xzf cardano-bin.tar.gz ./cardano-cli ./cardano-node 
  rm -f cardano-bin.tar.gz
fi

# And copy cardano-cli & cardano-node
cp cardano-cli ${ROOT}/cardano-cli
cp cardano-node ${ROOT}/cardano-node

pushd ${ROOT} > /dev/null
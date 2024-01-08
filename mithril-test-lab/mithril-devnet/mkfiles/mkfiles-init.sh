
# download cardano-cli & cardano-node if enabled (default: yes)
if [[ "$SKIP_CARDANO_BIN_DOWNLOAD" != "true" ]]; then
  curl -sL ${CARDANO_BINARY_URL} --output cardano-bin.tar.gz
  tar xzf cardano-bin.tar.gz ./cardano-cli ./cardano-node 
  rm -f cardano-bin.tar.gz
fi

# and copy cardano-cli & cardano-node
cp cardano-cli ${ROOT}/cardano-cli
cp cardano-node ${ROOT}/cardano-node

pushd ${ROOT}
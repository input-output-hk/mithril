#!/usr/bin/env bash

# Default values
if [ -z "${ARTIFACTS_DIR}" ]; then 
  ARTIFACTS_DIR="artifacts"
fi

# Change directory
cd ${ARTIFACTS_DIR}

# Query devnet
echo "====================================================================="
echo " Query Cardano devnet"
echo "====================================================================="
echo
./query-cardano.sh
echo
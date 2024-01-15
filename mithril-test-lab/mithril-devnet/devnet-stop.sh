#!/usr/bin/env bash

# Default values
if [ -z "${ARTIFACTS_DIR}" ]; then 
  ARTIFACTS_DIR="artifacts"
fi

# Change directory
cd ${ARTIFACTS_DIR}

# Stop devnet
echo "====================================================================="
echo " Stop Mithril/Cardano devnet"
echo "====================================================================="
echo
./stop.sh
echo
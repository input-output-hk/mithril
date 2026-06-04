#!/usr/bin/env bash

# Default values
if [ -z "${ARTIFACTS_DIR}" ]; then 
  ARTIFACTS_DIR="artifacts"
fi
if [ -z "${LINES}" ]; then 
  LINES="10"
fi
if [ -z "${NODES}" ]; then 
  NODES="*"
fi

# Change directory
cd ${ARTIFACTS_DIR}

# Logs devnet
echo "====================================================================="
echo " Logs Cardano devnet"
echo "====================================================================="
echo "====================================================================="
echo "=== Cardano Network"
echo "====================================================================="
echo
LINES=${LINES} ./log-cardano.sh
echo
if [ "${NODES}" = "dmq" ] || [ "${NODES}" = "*" ]; then 
    echo "====================================================================="
    echo "=== DMQ Network"
    echo "====================================================================="
    echo
    LINES=${LINES} ./log-dmq.sh
    echo
fi
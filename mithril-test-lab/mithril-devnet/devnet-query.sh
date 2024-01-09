#!/usr/bin/env bash

# Default values
if [ -z "${ARTIFACTS_DIR}" ]; then 
  ARTIFACTS_DIR="artifacts"
fi
if [ -z "${NODES}" ]; then 
  NODES="*"
fi

# Change directory
cd ${ARTIFACTS_DIR}

# Query devnet
echo "====================================================================="
echo " Query Mithril/Cardano devnet"
echo "====================================================================="
echo
if [ "${NODES}" = "*" ]; then 
    echo "====================================================================="
    echo "=== Mithril Network"
    echo "====================================================================="
    echo
fi
if [ "${NODES}" = "mithril" ] || [ "${NODES}" = "*" ]; then 
    ./query-mithril.sh
    echo
fi
if [ "${NODES}" = "*" ]; then 
    echo "====================================================================="
    echo "=== Cardano Network"
    echo "====================================================================="
    echo
fi
if [ "${NODES}" = "cardano" ] || [ "${NODES}" = "*" ]; then 
    ./query-cardano.sh
    echo
fi
#!/usr/bin/env bash

# Default values
if [ -z "${ROOT}" ]; then 
  ROOT="artifacts"
fi
if [ -z "${NODES}" ]; then 
  NODES="*"
fi

# Change directory
cd ${ROOT}

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
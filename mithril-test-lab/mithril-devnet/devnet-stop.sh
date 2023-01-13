#!/usr/bin/env bash

# Default values
if [ -z "${ROOT}" ]; then 
  ROOT="artifacts"
fi

# Change directory
cd ${ROOT}

# Stop devnet
echo "====================================================================="
echo " Stop Mithril/Cardano devnet"
echo "====================================================================="
echo
./stop.sh
echo
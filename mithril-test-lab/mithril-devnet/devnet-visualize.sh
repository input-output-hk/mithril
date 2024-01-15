#!/usr/bin/env bash

# Default values
if [ -z "${ARTIFACTS_DIR}" ]; then 
  ARTIFACTS_DIR="artifacts"
fi

# Change directory
cd ${ARTIFACTS_DIR}

# Stop devnet
echo "====================================================================="
echo " Visualize Mithril/Cardano devnet"
echo "====================================================================="
echo
docker run --rm -it --name dcv -v ${PWD}:/input pmsipilot/docker-compose-viz render -m image docker-compose.yaml -r --output-file=network.png --force
mimeopen -d network.png
echo


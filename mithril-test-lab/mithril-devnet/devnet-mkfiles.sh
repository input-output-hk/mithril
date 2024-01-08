#!/usr/bin/env bash

set -e

# Debug mode
if [ -v DEBUG ]; then
    set -x
fi

# Script directory variable
SCRIPT_DIRECTORY=$(dirname $0)

# Init script
. $SCRIPT_DIRECTORY/mkfiles/mkfiles-init.sh

# Generate the topology
. ${SCRIPT_DIRECTORY}/mkfiles/mkfiles-topology.sh

# Generate Cardano devnet artifacts
. $SCRIPT_DIRECTORY/mkfiles/mkfiles-cardano.sh

# Generate the start scripts
. $SCRIPT_DIRECTORY/mkfiles/mkfiles-start.sh

# Generate the pools scripts
#. ${SCRIPT_DIRECTORY}/mkfiles/mkfiles-pools.sh

# Generate the query scripts
. $SCRIPT_DIRECTORY/mkfiles/mkfiles-query.sh

# Generate the docker files
. $SCRIPT_DIRECTORY/mkfiles/mkfiles-docker.sh

# Cleanup
. $SCRIPT_DIRECTORY/mkfiles/mkfiles-cleanup.sh


#!/usr/bin/env bash

set -e

# Debug mode
if [[ -n $DEBUG ]]; then
    set -x
fi

# Script directory variable
SCRIPT_DIRECTORY=$(dirname $0)

# Init script
. $SCRIPT_DIRECTORY/mkfiles/mkfiles-init.sh

# Generate the Cardano nodes topology
. ${SCRIPT_DIRECTORY}/mkfiles/mkfiles-topology-cardano.sh

# Generate Cardano devnet artifacts
. $SCRIPT_DIRECTORY/mkfiles/mkfiles-cardano.sh

# Generate the DMQ nodes topology
. ${SCRIPT_DIRECTORY}/mkfiles/mkfiles-topology-dmq.sh

# Generate Cardano DMQ artifacts
. $SCRIPT_DIRECTORY/mkfiles/mkfiles-dmq.sh

# Generate the start scripts
. $SCRIPT_DIRECTORY/mkfiles/mkfiles-start.sh

# Generate the pools scripts
. ${SCRIPT_DIRECTORY}/mkfiles/mkfiles-pools.sh

# Generate the Mithril delegation scripts
. ${SCRIPT_DIRECTORY}/mkfiles/mkfiles-mithril-delegation.sh

# Generate the Mithril era scripts
. ${SCRIPT_DIRECTORY}/mkfiles/mkfiles-mithril-era.sh

# Generate the Mithril payment scripts
. ${SCRIPT_DIRECTORY}/mkfiles/mkfiles-mithril-payment.sh

# Generate the query scripts
. $SCRIPT_DIRECTORY/mkfiles/mkfiles-query.sh

# Generate the docker files
. $SCRIPT_DIRECTORY/mkfiles/mkfiles-docker.sh

# Cleanup
. $SCRIPT_DIRECTORY/mkfiles/mkfiles-cleanup.sh


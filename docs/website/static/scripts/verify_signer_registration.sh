#!/bin/bash

set -e

if [ -z "$SIGNER_LOGS_PATH" ]; then
    echo ">> ERROR: Required environment variable SIGNER_LOGS_PATH is not set."
    exit 1
fi

if grep -q "STATE MACHINE: new cycle: Registered" "$SIGNER_LOGS_PATH"; then
    echo ">> Congrats, your signer node is registered!"
else
    echo ">> Oops, your signer node is not registered. Check your configuration."
fi

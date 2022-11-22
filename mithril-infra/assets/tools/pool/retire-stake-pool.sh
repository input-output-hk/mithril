#!/usr/bin/env bash

# Script for retiring a cardano pool (SPO) / Tests only
## Inspired from https://docs.cardano.org/development-guidelines/operating-a-stake-pool/creating-a-stake-pool

# Import prelude
. $(dirname -- "$0")/_prelude.sh

# Check if all env vars are set
if [ -z "${TX_IN}" ]; then
    echo Missing environment variable: TX_IN
    exit 1
fi

if [ -z "${VALUE_OUT}" ]; then
    echo Missing environment variable: VALUE_OUT
    exit 1
fi

# Compute retirement epoch
SLOT=$(CARDANO_CLI_CMD query tip --testnet-magic $NETWORK_MAGIC | jq .slot)
EPOCH_CURRENT=$(CARDANO_CLI_CMD query tip --testnet-magic $NETWORK_MAGIC | jq .epoch)
EPOCH_RETIREMENT=$(( EPOCH_CURRENT + 1 ))

# Build deregistration certificate
CARDANO_CLI_CMD stake-pool deregistration-certificate \
--cold-verification-key-file ${POOL_ARTIFACTS_DIR}/cold.vkey \
--epoch $EPOCH_RETIREMENT \
--out-file ${POOL_ARTIFACTS_DIR}/pool-deregistration.cert

# Submit the pool certificate and delegation certificate to the blockchain
## Build transaction
CARDANO_CLI_CMD transaction build \
--babbage-era \
--tx-in $TX_IN \
--tx-out $(cat ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/payment.addr)+${VALUE_OUT} \
--change-address $(cat ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/payment.addr) \
--invalid-hereafter $(( $SLOT + 100000 )) \
--out-file ${POOL_ARTIFACTS_DIR}/pool-deregistration.txbody \
--certificate-file ${POOL_ARTIFACTS_DIR}/pool-deregistration.cert \
--witness-override 2 \
--testnet-magic $NETWORK_MAGIC

# Sign transaction
CARDANO_CLI_CMD transaction sign \
--signing-key-file ${POOL_ARTIFACTS_DIR}/payment.skey \
--signing-key-file ${POOL_ARTIFACTS_DIR}/cold.skey \
--testnet-magic $NETWORK_MAGIC \
--tx-body-file  ${POOL_ARTIFACTS_DIR}/pool-deregistration.txbody \
--out-file      ${POOL_ARTIFACTS_DIR}/pool-deregistration.tx

# Submit transaction
CARDANO_NODE_SOCKET_PATH=$CARDANO_NODE_SOCKET_PATH CARDANO_CLI_CMD transaction submit \
--tx-file ${POOL_ARTIFACTS_DIR}/pool-deregistration.tx \
--testnet-magic $NETWORK_MAGIC

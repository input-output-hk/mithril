#!/usr/bin/env bash

# Script for registering stake address of a cardano pool (SPO) / Tests only
## Inspired from https://docs.cardano.org/development-guidelines/operating-a-stake-pool/creating-a-stake-pool

# Import prelude
. $(dirname -- "$0")/_prelude.sh

# Check if all env vars are set
if [ -z "${TX_IN}" ]; then
    echo Missing environment variable: TX_IN
    exit 1
fi

# Register stake address on chain
SLOT=$(CARDANO_CLI_CMD query tip --testnet-magic $NETWORK_MAGIC | jq .slot)

## Build transaction
CARDANO_CLI_CMD transaction build \
--babbage-era \
--tx-in $TX_IN \
--tx-out $(cat ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/payment.addr)+${AMOUNT_STAKED} \
--change-address $(cat ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/payment.addr) \
--testnet-magic $NETWORK_MAGIC \
--certificate-file ${POOL_ARTIFACTS_DIR}/stake.cert \
--invalid-hereafter $(( $SLOT + 100000 )) \
--out-file ${POOL_ARTIFACTS_DIR}/stake-registration.txbody \
--witness-override 2

## Sign transaction
CARDANO_CLI_CMD transaction sign \
--signing-key-file ${POOL_ARTIFACTS_DIR}/payment.skey \
--signing-key-file ${POOL_ARTIFACTS_DIR}/stake.skey \
--testnet-magic $NETWORK_MAGIC \
--tx-body-file  ${POOL_ARTIFACTS_DIR}/stake-registration.txbody \
--out-file      ${POOL_ARTIFACTS_DIR}/stake-registration.tx

## Submit transaction
CARDANO_CLI_CMD transaction submit \
--tx-file ${POOL_ARTIFACTS_DIR}/stake-registration.tx \
--testnet-magic $NETWORK_MAGIC
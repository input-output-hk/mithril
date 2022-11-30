#!/usr/bin/env bash

# Script for renewing Operational Certificate for a cardano pool (SPO) / Tests only
## Inspired from https://docs.cardano.org/development-guidelines/operating-a-stake-pool/creating-a-stake-pool

# Import prelude
. $(dirname -- "$0")/_prelude.sh

# Compute Operational Certificate new counter on running node
CARDANO_CLI_CMD query kes-period-info \
--testnet-magic $NETWORK_MAGIC \
--op-cert-file ${POOL_ARTIFACTS_DIR}/opcert.cert \
--out-file ${POOL_ARTIFACTS_DIR}/kes_period_info.json
KES_PERIOD_CURRENT=$(cat ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/kes_period_info.json | jq .qKesCurrentKesPeriod)
OPCERT_COUNTER=$(cat ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/kes_period_info.json | jq .qKesNodeStateOperationalCertificateNumber)
OPCERT_COUNTER_NEW=$(( $OPCERT_COUNTER + 1 ))

# Issue new Operational Certificate on cold environment
## Create new counter
CARDANO_CLI_CMD node new-counter \
--cold-verification-key-file ${POOL_ARTIFACTS_DIR}/cold.vkey \
--counter-value $OPCERT_COUNTER_NEW \
--operational-certificate-issue-counter-file ${POOL_ARTIFACTS_DIR}/opcert.counter.new

## Generate new KES keys (required only if MaxKESEvolutions is reached)
CARDANO_CLI_CMD node key-gen-KES \
--verification-key-file ${POOL_ARTIFACTS_DIR}/kes.vkey.new \
--signing-key-file ${POOL_ARTIFACTS_DIR}/kes.skey.new

## Issue new Operational Certificate
CARDANO_CLI_CMD node issue-op-cert \
--kes-verification-key-file ${POOL_ARTIFACTS_DIR}/kes.vkey.new \
--cold-signing-key-file ${POOL_ARTIFACTS_DIR}/cold.skey \
--operational-certificate-issue-counter-file ${POOL_ARTIFACTS_DIR}/opcert.counter.new \
--kes-period $KES_PERIOD_CURRENT \
--out-file ${POOL_ARTIFACTS_DIR}/opcert.cert.new

## Deploy new artifacts on running node
mv ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/opcert.counter ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/opcert.counter.old
mv ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/opcert.counter.new ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/opcert.counter
mv ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/kes.vkey ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/kes.vkey.old
mv ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/kes.vkey.new ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/kes.vkey
mv ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/kes.skey ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/kes.skey.old
mv ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/kes.skey.new ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/kes.skey
mv ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/opcert.cert ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/opcert.certr.old
mv ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/opcert.cert.new ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/opcert.cert

## Restart node
docker restart $CARDANO_NODE
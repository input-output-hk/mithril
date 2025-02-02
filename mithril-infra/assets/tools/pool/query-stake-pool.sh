#!/usr/bin/env bash

# Script for querying info about a cardano pool (SPO) / Tests only
## Inspired from https://docs.cardano.org/development-guidelines/operating-a-stake-pool/creating-a-stake-pool

# Import prelude
. $(dirname -- "$0")/_prelude.sh

# Show version
CARDANO_CLI_CMD --version

# Compute Pool Id
POOL_ID=$(CARDANO_CLI_CMD ${CARDANO_ERA} stake-pool id --cold-verification-key-file ${POOL_ARTIFACTS_DIR}/cold.vkey)
echo POOL_ID=$POOL_ID

# Query tip
CARDANO_CLI_CMD ${CARDANO_ERA} query tip \
--testnet-magic $NETWORK_MAGIC | jq .

# Payment address utxo
CARDANO_CLI_CMD ${CARDANO_ERA} query utxo  \
--testnet-magic $NETWORK_MAGIC \
--address $(cat ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/payment.addr)

# Query stake address info
CARDANO_CLI_CMD ${CARDANO_ERA} query stake-address-info \
--testnet-magic $NETWORK_MAGIC \
--address $(cat ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/stake.addr) | jq .

# Query stake snapshot
CARDANO_CLI_CMD ${CARDANO_ERA} query stake-snapshot \
--testnet-magic $NETWORK_MAGIC \
--stake-pool-id $POOL_ID | jq .

# Query pool params
CARDANO_CLI_CMD ${CARDANO_ERA} query pool-params \
--testnet-magic $NETWORK_MAGIC \
--stake-pool-id $POOL_ID | jq .

# Query KES period info
CARDANO_CLI_CMD ${CARDANO_ERA} query kes-period-info \
--testnet-magic $NETWORK_MAGIC \
--op-cert-file ${POOL_ARTIFACTS_DIR}/opcert.cert

# Query current leadership schedule
CARDANO_CLI_CMD ${CARDANO_ERA} query leadership-schedule \
--testnet-magic $NETWORK_MAGIC \
--genesis /config/genesis/shelley.json \
--stake-pool-id $POOL_ID \
--vrf-signing-key-file ${POOL_ARTIFACTS_DIR}/vrf.skey \
--current

# Query next leadership schedule
CARDANO_CLI_CMD ${CARDANO_ERA} query leadership-schedule \
--testnet-magic $NETWORK_MAGIC \
--genesis /config/genesis/shelley.json \
--stake-pool-id $POOL_ID \
--vrf-signing-key-file ${POOL_ARTIFACTS_DIR}/vrf.skey \
--next
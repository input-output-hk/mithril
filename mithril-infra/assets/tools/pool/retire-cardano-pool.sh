#!/usr/bin/env bash

# Script for retiring a cardano pool (SPO)

NETWORK=$1
NETWORK_MAGIC=$2
SIGNER_NODE=$3
POOL_ARTIFACTS_DIR=data/$NETWORK/$SIGNER_NODE/cardano/pool
mkdir -p $POOL_ARTIFACTS_DIR
cd $POOL_ARTIFACTS_DIR
## TODO: return funds to faucet
## TODO: Create deregisration certificate as in https://github.com/input-output-hk/cardano-node/blob/master/doc/stake-pool-operations/12_retire_stakepool.md
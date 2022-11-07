#!/usr/bin/env bash

# Script for operating a SPO

ARTIFACTS_DIR=pool
NETWORK_MAGIC=2
cd $ARTIFACTS_DIR

# 1. Create keys and addresses
## Create payment keypair
./cardano-cli address key-gen \
--verification-key-file payment.vkey \
--signing-key-file payment.skey

## Create stake keypair
./cardano-cli stake-address key-gen \
--verification-key-file stake.vkey \
--signing-key-file stake.skey

## Create payment address
./cardano-cli address build \
--payment-verification-key-file payment.vkey \
--stake-verification-key-file stake.vkey \
--out-file payment.addr \
--testnet-magic $NETWORK_MAGIC

## Create stake address
./cardano-cli stake-address build \
--stake-verification-key-file stake.vkey \
--out-file stake.addr \
--testnet-magic $NETWORK_MAGIC

# 2. Generate your stake pool keys
## Generate Cold Keys and a Cold_counter
./cardano-cli node key-gen \
--cold-verification-key-file cold.vkey \
--cold-signing-key-file cold.skey \
--operational-certificate-issue-counter-file opcert.counter

## Generate VRF Key pair
./cardano-cli node key-gen-VRF \
--verification-key-file vrf.vkey \
--signing-key-file vrf.skey

## Generate the KES Key pair
./cardano-cli node key-gen-KES \
--verification-key-file kes.vkey \
--signing-key-file kes.skey

## Generate the Operational Certificate
### Compute KES period
### Replace /home/curry/docker/cardano-configurations/network/preview w/ /config
PROTOCOL_FILE=/home/curry/docker/cardano-configurations/network/preview/genesis/shelley.json
SLOTS_KES_PERIOD=$(cat $PROTOCOL_FILE | jq .slotsPerKESPeriod)
SLOT=$(CARDANO_NODE_SOCKET_PATH=../ipc/node.socket ./cardano-cli query tip --testnet-magic $NETWORK_MAGIC | jq .slot)
KES_PERIOD=`expr $SLOT / $SLOTS_KES_PERIOD`

### Generate Operational Certificate
./cardano-cli node issue-op-cert \
--kes-verification-key-file kes.vkey \
--cold-signing-key-file cold.skey \
--operational-certificate-issue-counter opcert.counter \
--kes-period $KES_PERIOD \
--out-file opcert.cert

### Create a registration certificate
./cardano-cli stake-address registration-certificate \
--stake-verification-key-file stake.vkey \
--out-file stake.cert

### Send tokens to / Check utxo payment address
CARDANO_NODE_SOCKET_PATH=../ipc/node.socket
echo Send tokens to '$(cat payment.addr)' at https://docs.cardano.org/cardano-testnet/tools/faucet
watch CARDANO_NODE_SOCKET_PATH=$CARDANO_NODE_SOCKET_PATH ./cardano-cli query utxo --address $(cat payment.addr) --testnet-magic $NETWORK_MAGIC

### Register stake address on chain
AMOUNT_STAKED=1000000000
#### Build transaction
CARDANO_NODE_SOCKET_PATH=$CARDANO_NODE_SOCKET_PATH ./cardano-cli transaction build \
--babbage-era \
--tx-in $(CARDANO_NODE_SOCKET_PATH=$CARDANO_NODE_SOCKET_PATH ./cardano-cli query utxo  \
            --testnet-magic $NETWORK_MAGIC  \
            --address $(cat payment.addr) | tail -n 1  | awk '{print $1;}')#0 \
--tx-out $(cat payment.addr)+${AMOUNT_STAKED} \
--change-address $(cat payment.addr) \
--testnet-magic $NETWORK_MAGIC \
--certificate-file stake.cert \
--invalid-hereafter $(( $SLOT + 100000 )) \
--out-file stake-registration.txbody \
--witness-override 2

#### Sign transaction
CARDANO_NODE_SOCKET_PATH=$CARDANO_NODE_SOCKET_PATH ./cardano-cli transaction sign \
--signing-key-file payment.skey \
--signing-key-file stake.skey \
--testnet-magic $NETWORK_MAGIC \
--tx-body-file  stake-registration.txbody \
--out-file      stake-registration.tx

#### Submit transaction
CARDANO_NODE_SOCKET_PATH=$CARDANO_NODE_SOCKET_PATH ./cardano-cli transaction submit \
--tx-file stake-registration.tx \
--testnet-magic $NETWORK_MAGIC

### Register stake pool on chain

#### Create pool metadata
#### TODO: custom metadata
cat > ./pool-metadata.json << EOF
{
"name": "Mithril Pool 1",
"description": "Mithril Pool 1",
"ticker": "MIT01",
"homepage": "https://mithril.network"
}
EOF
POOL_METADATA_HASH=$(./cardano-cli stake-pool metadata-hash --pool-metadata-file pool-metadata.json)

#### Generate stake pool registration certificate
POOL_PLEDGE=$AMOUNT_STAKED
POOL_COST=$(cat $PROTOCOL_FILE | jq .protocolParams.minPoolCost)
POOL_MARGIN=0.1
POOL_METADATA_URL="https://mithril.network/tbd.json"
POOL_RELAY_URL="https://aggregator.dev-preview.api.mithril.network/"

./cardano-cli stake-pool registration-certificate \
--cold-verification-key-file cold.vkey \
--vrf-verification-key-file vrf.vkey \
--pool-pledge $POOL_PLEDGE \
--pool-cost $POOL_COST \
--pool-margin $POOL_MARGIN \
--pool-reward-account-verification-key-file stake.vkey \
--pool-owner-stake-verification-key-file stake.vkey \
--testnet-magic $NETWORK_MAGIC \
--single-host-pool-relay $POOL_RELAY_URL \
--metadata-url $POOL_METADATA_URL \
--metadata-hash $POOL_METADATA_HASH \
--out-file pool-registration.cert

#### Generate delegation certificate pledge
./cardano-cli stake-address delegation-certificate \
--stake-verification-key-file stake.vkey \
--cold-verification-key-file cold.vkey \
--out-file delegation.cert

#### Submit the pool certificate and delegation certificate to the blockchain
##### Build transaction
STAKE_ADDRESS_DEPOSIT=$(cat $PROTOCOL_FILE | jq .protocolParams.poolDeposit)
CARDANO_NODE_SOCKET_PATH=$CARDANO_NODE_SOCKET_PATH ./cardano-cli transaction build \
--babbage-era \
--tx-in $(CARDANO_NODE_SOCKET_PATH=$CARDANO_NODE_SOCKET_PATH ./cardano-cli query utxo  \
            --testnet-magic $NETWORK_MAGIC  \
            --address $(cat payment.addr) | tail -n 1  | awk '{print $1;}')#1 \
--tx-out $(cat payment.addr)+$STAKE_ADDRESS_DEPOSIT \
--change-address $(cat payment.addr) \
--invalid-hereafter $(( $SLOT + 100000 )) \
--out-file pool-registration.txbody \
--certificate-file pool-registration.cert \
--certificate-file delegation.cert \
--witness-override 4 \
--testnet-magic $NETWORK_MAGIC

#### Sign transaction
CARDANO_NODE_SOCKET_PATH=$CARDANO_NODE_SOCKET_PATH ./cardano-cli transaction sign \
--signing-key-file payment.skey \
--signing-key-file stake.skey \
--signing-key-file cold.skey \
--testnet-magic $NETWORK_MAGIC \
--tx-body-file  pool-registration.txbody \
--out-file      pool-registration.tx

#### Submit transaction
CARDANO_NODE_SOCKET_PATH=$CARDANO_NODE_SOCKET_PATH ./cardano-cli transaction submit \
--tx-file pool-registration.tx \
--testnet-magic $NETWORK_MAGIC

### Compute Pool Id
POOL_ID=$(./cardano-cli stake-pool id --cold-verification-key-file cold.vkey)
echo $POOL_ID
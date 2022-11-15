#!/usr/bin/env bash

# Script for creating a cardano pool (SPO) / Tests only
## Inspired from https://docs.cardano.org/development-guidelines/operating-a-stake-pool/creating-a-stake-pool

NETWORK=$1
NETWORK_MAGIC=$2
CARDANO_NODE=$3
SIGNER_NODE=$4
SIGNER_TICKER=$5
DOMAIN=$6
POOL_ARTIFACTS_DIR=/pool
POOL_ARTIFACTS_DIR_PREFIX=./data/${NETWORK}/${SIGNER_NODE}/cardano
POOL_WWW_DIR=/www

# Cardano cli function
CARDANO_CLI_CMD() {
    docker exec ${CARDANO_NODE} cardano-cli ${@}
}

# 1. Create keys and addresses
## Create payment keypair
CARDANO_CLI_CMD address key-gen \
--verification-key-file ${POOL_ARTIFACTS_DIR}/payment.vkey \
--signing-key-file ${POOL_ARTIFACTS_DIR}/payment.skey

## Create stake keypair
CARDANO_CLI_CMD stake-address key-gen \
--verification-key-file ${POOL_ARTIFACTS_DIR}/stake.vkey \
--signing-key-file ${POOL_ARTIFACTS_DIR}/stake.skey

## Create payment address
CARDANO_CLI_CMD address build \
--payment-verification-key-file ${POOL_ARTIFACTS_DIR}/payment.vkey \
--stake-verification-key-file ${POOL_ARTIFACTS_DIR}/stake.vkey \
--out-file ${POOL_ARTIFACTS_DIR}/payment.addr \
--testnet-magic $NETWORK_MAGIC

## Create stake address
CARDANO_CLI_CMD stake-address build \
--stake-verification-key-file ${POOL_ARTIFACTS_DIR}/stake.vkey \
--out-file ${POOL_ARTIFACTS_DIR}/stake.addr \
--testnet-magic $NETWORK_MAGIC

# 2. Generate your stake pool keys
## Generate Cold Keys and a Cold_counter
CARDANO_CLI_CMD node key-gen \
--cold-verification-key-file ${POOL_ARTIFACTS_DIR}/cold.vkey \
--cold-signing-key-file ${POOL_ARTIFACTS_DIR}/cold.skey \
--operational-certificate-issue-counter-file ${POOL_ARTIFACTS_DIR}/opcert.counter

## Generate VRF Key pair
CARDANO_CLI_CMD node key-gen-VRF \
--verification-key-file ${POOL_ARTIFACTS_DIR}/vrf.vkey \
--signing-key-file ${POOL_ARTIFACTS_DIR}/vrf.skey

## Generate the KES Key pair
CARDANO_CLI_CMD node key-gen-KES \
--verification-key-file ${POOL_ARTIFACTS_DIR}/kes.vkey \
--signing-key-file ${POOL_ARTIFACTS_DIR}/kes.skey

## Generate the Operational Certificate
### Compute KES period
### Replace /home/curry/docker/cardano-configurations/network/preview w/ /config
PROTOCOL_FILE=/home/curry/docker/cardano-configurations/network/preview/genesis/shelley.json
SLOTS_KES_PERIOD=$(cat $PROTOCOL_FILE | jq .slotsPerKESPeriod)
SLOT=$(CARDANO_CLI_CMD query tip --testnet-magic $NETWORK_MAGIC | jq .slot)
KES_PERIOD=`expr $SLOT / $SLOTS_KES_PERIOD`

### Generate Operational Certificate
CARDANO_CLI_CMD node issue-op-cert \
--kes-verification-key-file ${POOL_ARTIFACTS_DIR}/kes.vkey \
--cold-signing-key-file ${POOL_ARTIFACTS_DIR}/cold.skey \
--operational-certificate-issue-counter ${POOL_ARTIFACTS_DIR}/opcert.counter \
--kes-period $KES_PERIOD \
--out-file ${POOL_ARTIFACTS_DIR}/opcert.cert

### Create a registration certificate
CARDANO_CLI_CMD stake-address registration-certificate \
--stake-verification-key-file ${POOL_ARTIFACTS_DIR}/stake.vkey \
--out-file ${POOL_ARTIFACTS_DIR}/stake.cert

### Send funds to / Check utxo payment address
echo Send funds to "$(cat ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/payment.addr)" at https://docs.cardano.org/cardano-testnet/tools/faucet
while true
do  
    UTXO_ROWS_NUMBER=`expr $(CARDANO_CLI_CMD query utxo --address $(cat ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/payment.addr) --testnet-magic $NETWORK_MAGIC 2> /dev/null | wc -l) - 2`
    if [ $UTXO_ROWS_NUMBER -gt 0 ] ; then
        echo ">>>> Funds Received!"
        break
    else
        echo ">>>> Waiting for Funds..."
        sleep 10
    fi
done


### Register stake address on chain
AMOUNT_STAKED=1000000000
#### Build transaction
CARDANO_CLI_CMD transaction build \
--babbage-era \
--tx-in $(CARDANO_CLI_CMD query utxo  \
            --testnet-magic $NETWORK_MAGIC  \
            --address $(cat ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/payment.addr) | tail -n 1  | awk '{print $1;}')#0 \
--tx-out $(cat ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/payment.addr)+${AMOUNT_STAKED} \
--change-address $(cat ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/payment.addr) \
--testnet-magic $NETWORK_MAGIC \
--certificate-file ${POOL_ARTIFACTS_DIR}/stake.cert \
--invalid-hereafter $(( $SLOT + 100000 )) \
--out-file ${POOL_ARTIFACTS_DIR}/stake-registration.txbody \
--witness-override 2

#### Sign transaction
CARDANO_CLI_CMD transaction sign \
--signing-key-file ${POOL_ARTIFACTS_DIR}/payment.skey \
--signing-key-file ${POOL_ARTIFACTS_DIR}/stake.skey \
--testnet-magic $NETWORK_MAGIC \
--tx-body-file  ${POOL_ARTIFACTS_DIR}/stake-registration.txbody \
--out-file      ${POOL_ARTIFACTS_DIR}/stake-registration.tx

#### Submit transaction
CARDANO_CLI_CMD transaction submit \
--tx-file ${POOL_ARTIFACTS_DIR}/stake-registration.tx \
--testnet-magic $NETWORK_MAGIC

### Register stake pool on chain

#### Create pool metadata
cat > ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_WWW_DIR}/pool-metadata.json << EOF
{
"name": "Mithril Signer ${SIGNER_TICKER}",
"description": "Mithril signer node ${SIGNER_TICKER}",
"ticker": "${SIGNER_TICKER}",
"homepage": "https://mithril.network"
}
EOF
POOL_METADATA_HASH=$(CARDANO_CLI_CMD stake-pool metadata-hash --pool-metadata-file ${POOL_WWW_DIR}/pool-metadata.json)

#### Generate stake pool registration certificate
POOL_PLEDGE=$AMOUNT_STAKED
POOL_COST=$(cat $PROTOCOL_FILE | jq .protocolParams.minPoolCost)
POOL_MARGIN=0.1
POOL_METADATA_URL=$(curl -s https://tinyurl.com/api-create.php?url=https://${SIGNER_NODE}.${DOMAIN}/pool-metadata.json)
POOL_RELAY_URL="https://${SIGNER_NODE}.${DOMAIN}/"

CARDANO_CLI_CMD stake-pool registration-certificate \
--cold-verification-key-file ${POOL_ARTIFACTS_DIR}/cold.vkey \
--vrf-verification-key-file ${POOL_ARTIFACTS_DIR}/vrf.vkey \
--pool-pledge $POOL_PLEDGE \
--pool-cost $POOL_COST \
--pool-margin $POOL_MARGIN \
--pool-reward-account-verification-key-file ${POOL_ARTIFACTS_DIR}/stake.vkey \
--pool-owner-stake-verification-key-file ${POOL_ARTIFACTS_DIR}/stake.vkey \
--testnet-magic $NETWORK_MAGIC \
--single-host-pool-relay $POOL_RELAY_URL \
--metadata-url $POOL_METADATA_URL \
--metadata-hash $POOL_METADATA_HASH \
--out-file ${POOL_ARTIFACTS_DIR}/pool-registration.cert

#### Generate delegation certificate pledge
CARDANO_CLI_CMD stake-address delegation-certificate \
--stake-verification-key-file ${POOL_ARTIFACTS_DIR}/stake.vkey \
--cold-verification-key-file ${POOL_ARTIFACTS_DIR}/cold.vkey \
--out-file ${POOL_ARTIFACTS_DIR}/delegation.cert

#### Submit the pool certificate and delegation certificate to the blockchain
##### Build transaction
STAKE_ADDRESS_DEPOSIT=$(cat $PROTOCOL_FILE | jq .protocolParams.poolDeposit)
CARDANO_CLI_CMD transaction build \
--babbage-era \
--tx-in $(CARDANO_CLI_CMD query utxo  \
            --testnet-magic $NETWORK_MAGIC  \
            --address $(cat ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/payment.addr) | tail -n 1  | awk '{print $1;}')#1 \
--tx-out $(cat ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/payment.addr)+$STAKE_ADDRESS_DEPOSIT \
--change-address $(cat ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/payment.addr) \
--invalid-hereafter $(( $SLOT + 100000 )) \
--out-file ${POOL_ARTIFACTS_DIR}/pool-registration.txbody \
--certificate-file ${POOL_ARTIFACTS_DIR}/pool-registration.cert \
--certificate-file ${POOL_ARTIFACTS_DIR}/delegation.cert \
--witness-override 4 \
--testnet-magic $NETWORK_MAGIC

#### Sign transaction
CARDANO_CLI_CMD transaction sign \
--signing-key-file ${POOL_ARTIFACTS_DIR}/payment.skey \
--signing-key-file ${POOL_ARTIFACTS_DIR}/stake.skey \
--signing-key-file ${POOL_ARTIFACTS_DIR}/cold.skey \
--testnet-magic $NETWORK_MAGIC \
--tx-body-file  ${POOL_ARTIFACTS_DIR}/pool-registration.txbody \
--out-file      ${POOL_ARTIFACTS_DIR}/pool-registration.tx

#### Submit transaction
CARDANO_NODE_SOCKET_PATH=$CARDANO_NODE_SOCKET_PATH CARDANO_CLI_CMD transaction submit \
--tx-file ${POOL_ARTIFACTS_DIR}/pool-registration.tx \
--testnet-magic $NETWORK_MAGIC

### Compute Pool Id
POOL_ID=$(CARDANO_CLI_CMD stake-pool id --cold-verification-key-file ${POOL_ARTIFACTS_DIR}/cold.vkey)
echo $POOL_ID > ${POOL_ARTIFACTS_DIR_PREFIX}${POOL_ARTIFACTS_DIR}/pool-id.txt
echo POOL_ID=$POOL_ID

### Delegate stakes to the pool
echo Delegate stakes to pool id "$POOL_ID" at https://docs.cardano.org/cardano-testnet/tools/faucet


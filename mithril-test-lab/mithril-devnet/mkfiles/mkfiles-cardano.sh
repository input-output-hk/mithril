ALONZO_GENESIS_URL="https://book.world.dev.cardano.org/environments/private/alonzo-genesis.json"
CONWAY_GENESIS_URL="https://book.world.dev.cardano.org/environments/private/conway-genesis.json"

INIT_SUPPLY=$(( SUPPLY+2000000 ))
FUNDS_PER_GENESIS_ADDRESS=$((${INIT_SUPPLY} / ${NUM_BFT_NODES}))
FUNDS_PER_BYRON_ADDRESS=$((${FUNDS_PER_GENESIS_ADDRESS} - 1000000))
# We need to allow for a fee to transfer the funds out of the genesis.
# We don't care too much, 1 ada is more than enough.

OS=$(uname -s) DATE=
case $OS in
  Darwin )       DATE="gdate";;
  * )            DATE="date";;
esac

START_TIME="$(${DATE} -d "now + 30 seconds" +%s)"

# copy and tweak the configuration
popd
cp configuration.yaml ${ROOT}/
sed -i ${ROOT}/configuration.yaml \
    -e 's/Protocol: RealPBFT/Protocol: Cardano\nPBftSignatureThreshold: 0.6/' \
    -e 's/minSeverity: Info/minSeverity: Info/' \
    -e 's/TracingVerbosity: NormalVerbosity/TracingVerbosity: MinimalVerbosity/' \
    -e 's/TurnOnLogMetrics: True/TurnOnLogMetrics: False/' \
    -e 's|GenesisFile: genesis.json|ByronGenesisFile: byron/genesis.json|' \
    -e '/ByronGenesisFile/ aConwayGenesisFile: shelley/genesis.conway.json' \
    -e '/ByronGenesisFile/ aAlonzoGenesisFile: shelley/genesis.alonzo.json' \
    -e '/ByronGenesisFile/ aShelleyGenesisFile: shelley/genesis.json' \
    -e 's/RequiresNoMagic/RequiresMagic/' \
    -e 's/LastKnownBlockVersion-Major: 0/LastKnownBlockVersion-Major: 8/' \
    -e 's/LastKnownBlockVersion-Minor: 2/LastKnownBlockVersion-Minor: 0/' \
    -e 's/LastKnownBlockVersion-Alt: 0/LastKnownBlockVersion-Alt: 0/'
# Options for making it easier to trigger the transition to Shelley
# If neither of those are used, we have to
# - post an update proposal + votes to go to protocol version 1
# - after that's activated, change the configuration to have
#   'LastKnownBlockVersion-Major: 2', and restart the nodes
# - post another proposal + vote to go to protocol version 2

#uncomment this for an automatic transition after the first epoch
echo "TestShelleyHardForkAtEpoch: 0" >> ${ROOT}/configuration.yaml
echo "TestAllegraHardForkAtEpoch: 0" >> ${ROOT}/configuration.yaml
echo "TestMaryHardForkAtEpoch: 0" >> ${ROOT}/configuration.yaml
echo "TestAlonzoHardForkAtEpoch: 0" >> ${ROOT}/configuration.yaml
echo "ExperimentalHardForksEnabled: True" >> ${ROOT}/configuration.yaml
echo "ExperimentalProtocolsEnabled: True" >> ${ROOT}/configuration.yaml

#uncomment this to trigger the hardfork with protocol version 1
#echo "TestShelleyHardForkAtVersion: 1"  >> ${ROOT}/configuration.yaml

pushd ${ROOT}

# create the configuration files
for NODE in ${ALL_NODES}; do

  cp configuration.yaml ${NODE}/configuration.yaml

done

# Byron setup
cat > byron.genesis.spec.json <<EOF
{
  "heavyDelThd":     "300000000000",
  "maxBlockSize":    "2000000",
  "maxTxSize":       "4096",
  "maxHeaderSize":   "2000000",
  "maxProposalSize": "700",
  "mpcThd": "20000000000000",
  "scriptVersion": 0,
  "slotDuration": "1000",
  "softforkRule": {
    "initThd": "900000000000000",
    "minThd": "600000000000000",
    "thdDecrement": "50000000000000"
  },
  "txFeePolicy": {
    "multiplier": "43946000000",
    "summand": "155381000000000"
  },
  "unlockStakeEpoch": "18446744073709551615",
  "updateImplicit": "10000",
  "updateProposalThd": "100000000000000",
  "updateVoteThd": "1000000000000"
}
EOF

./cardano-cli byron genesis genesis \
  --protocol-magic ${NETWORK_MAGIC} \
  --start-time ${START_TIME} \
  --k ${SECURITY_PARAM} \
  --n-poor-addresses 0 \
  --n-delegate-addresses ${NUM_BFT_NODES} \
  --total-balance ${INIT_SUPPLY} \
  --delegate-share 1 \
  --avvm-entry-count 0 \
  --avvm-entry-balance 0 \
  --protocol-parameters-file byron.genesis.spec.json \
  --genesis-output-dir byron

mv byron.genesis.spec.json byron/genesis.spec.json

# Copy the genesis files
for NODE in ${ALL_NODES}; do

  cp byron/genesis*.json     ${NODE}/byron/

done

# Copy the BFT operator keys from the genesis delegates, for uniformity
for N in ${BFT_NODES_N}; do

  cp byron/delegate-keys.00$((${N} - 1)).key     node-bft${N}/byron/delegate.key
  cp byron/delegation-cert.00$((${N} - 1)).json  node-bft${N}/byron/delegate.cert

done

# Create keys, addresses and transactions to withdraw the initial UTxO into
# regular addresses.
for N in ${BFT_NODES_N}; do

  ./cardano-cli byron key keygen \
    --secret byron/payment-keys.00$((${N} - 1)).key \

  ./cardano-cli byron key signing-key-address \
    --testnet-magic ${NETWORK_MAGIC} \
    --secret byron/payment-keys.00$((${N} - 1)).key > byron/address-00$((${N} - 1))

  ./cardano-cli byron key signing-key-address \
    --testnet-magic ${NETWORK_MAGIC} \
    --secret byron/genesis-keys.00$((${N} - 1)).key > byron/genesis-address-00$((${N} - 1))
done

# Update Proposal and votes
./cardano-cli byron governance create-update-proposal \
            --filepath byron/update-proposal \
            --testnet-magic ${NETWORK_MAGIC} \
            --signing-key byron/delegate-keys.000.key \
            --protocol-version-major 1 \
            --protocol-version-minor 0 \
            --protocol-version-alt 0 \
            --application-name "cardano-sl" \
            --software-version-num 1 \
            --system-tag "linux" \
            --installer-hash 0

for N in ${BFT_NODES_N}; do
    ./cardano-cli byron governance create-proposal-vote \
                --proposal-filepath byron/update-proposal \
                --testnet-magic ${NETWORK_MAGIC} \
                --signing-key byron/delegate-keys.00$((${N} - 1)).key \
                --vote-yes \
                --output-filepath byron/update-vote.00$((${N} - 1))
done

./cardano-cli byron governance create-update-proposal \
            --filepath byron/update-proposal-1 \
            --testnet-magic ${NETWORK_MAGIC} \
            --signing-key byron/delegate-keys.000.key \
            --protocol-version-major 2 \
            --protocol-version-minor 0 \
            --protocol-version-alt 0 \
            --application-name "cardano-sl" \
            --software-version-num 1 \
            --system-tag "linux" \
            --installer-hash 0

for N in ${BFT_NODES_N}; do
    ./cardano-cli byron governance create-proposal-vote \
                --proposal-filepath byron/update-proposal-1 \
                --testnet-magic ${NETWORK_MAGIC} \
                --signing-key byron/delegate-keys.00$((${N} - 1)).key \
                --vote-yes \
                --output-filepath byron/update-vote-1.00$((${N} - 1))
done

echo "====================================================================="
echo "Generated genesis keys and genesis files:"
echo
ls -1 byron/*
echo "====================================================================="


# Shelley era. Set up our template
mkdir shelley
curl -s ${ALONZO_GENESIS_URL} -o shelley/genesis.alonzo.spec.json
curl -s ${CONWAY_GENESIS_URL} -o shelley/genesis.conway.spec.json
# Fix the error that crashes the startup of the devnet: 
# `Command failed: genesis create  Error: Error while decoding Shelley genesis at: shelley/genesis.conway.spec.json Error: Error in $: key "genDelegs" not found`
# The error is due to a missing field 'genDelegs' in the genesis configuration file downloaded. This fix adds it manually after the download.
mv shelley/genesis.conway.spec.json shelley/genesis.conway.spec.json.tmp && cat shelley/genesis.conway.spec.json.tmp | jq '. += {"genDelegs":{}}' > shelley/genesis.conway.spec.json && rm shelley/genesis.conway.spec.json.tmp
./cardano-cli genesis create --testnet-magic ${NETWORK_MAGIC} --genesis-dir shelley --start-time $(date -u +%Y-%m-%dT%H:%M:%SZ)
mv shelley/genesis.spec.json shelley/genesis.spec.json.tmp && cat shelley/genesis.spec.json.tmp | jq . > shelley/genesis.spec.json && rm shelley/genesis.spec.json.tmp

# Then edit the genesis.spec.json ...

# We're going to use really quick epochs (300 seconds), by using short slots 0.2s
# and K=10, but we'll keep long KES periods so we don't have to bother
# cycling KES keys
sed -i shelley/genesis.spec.json \
    -e 's/"slotLength": 1/"slotLength": '${SLOT_LENGTH}'/' \
    -e 's/"activeSlotsCoeff": 5.0e-2/"activeSlotsCoeff": 0.50/' \
    -e 's/"securityParam": 2160/"securityParam": '${SECURITY_PARAM}'/' \
    -e 's/"epochLength": 432000/"epochLength": '${EPOCH_LENGTH}'/' \
    -e 's/"maxLovelaceSupply": 0/"maxLovelaceSupply": 1000000000/' \
    -e 's/"decentralisationParam": 1.0/"decentralisationParam": 0.7/' \
    -e 's/"major": 0/"major": 4/' \
    -e 's/"updateQuorum": 5/"updateQuorum": 2/'

# Now generate for real:

./cardano-cli genesis create \
    --testnet-magic ${NETWORK_MAGIC} \
    --genesis-dir shelley/ \
    --gen-genesis-keys ${NUM_BFT_NODES} \
    --gen-utxo-keys ${NUM_POOL_NODES}

echo "====================================================================="
echo "Generated genesis keys and genesis files:"
echo
ls -1 shelley/*
echo "====================================================================="

echo "Generated genesis.json:"
echo
cat shelley/genesis.json
echo
echo "====================================================================="

# Copy the genesis files
for NODE in ${ALL_NODES}; do

  cp shelley/genesis*.json     ${NODE}/shelley/

done

# Make the pool operator cold keys
# This was done already for the BFT nodes as part of the genesis creation

for NODE in ${POOL_NODES}; do

  ./cardano-cli node key-gen \
      --cold-verification-key-file                 ${NODE}/shelley/operator.vkey \
      --cold-signing-key-file                      ${NODE}/shelley/operator.skey \
      --operational-certificate-issue-counter-file ${NODE}/shelley/operator.counter

  ./cardano-cli node key-gen-VRF \
      --verification-key-file ${NODE}/shelley/vrf.vkey \
      --signing-key-file      ${NODE}/shelley/vrf.skey

done

# Copy the BFT operator keys from the genesis delegates, for uniformity

for N in ${BFT_NODES_N}; do

  cp shelley/delegate-keys/delegate${N}.skey node-bft${N}/shelley/operator.skey
  cp shelley/delegate-keys/delegate${N}.vkey node-bft${N}/shelley/operator.vkey
  cp shelley/delegate-keys/delegate${N}.counter node-bft${N}/shelley/operator.counter
  cp shelley/delegate-keys/delegate${N}.vrf.vkey node-bft${N}/shelley/vrf.vkey
  cp shelley/delegate-keys/delegate${N}.vrf.skey node-bft${N}/shelley/vrf.skey

done


# Make hot keys and for all nodes

for NODE in ${ALL_NODES}; do

  ./cardano-cli node key-gen-KES \
      --verification-key-file ${NODE}/shelley/kes.vkey \
      --signing-key-file      ${NODE}/shelley/kes.skey

  ./cardano-cli node issue-op-cert \
      --kes-period 0 \
      --kes-verification-key-file                  ${NODE}/shelley/kes.vkey \
      --cold-signing-key-file                      ${NODE}/shelley/operator.skey \
      --operational-certificate-issue-counter-file ${NODE}/shelley/operator.counter \
      --out-file                                   ${NODE}/shelley/node.cert

done

echo "Generated node operator keys (cold, hot) and operational certs:"
echo
ls -1 ${ALL_NODES}
echo "====================================================================="


# Make some payment and stake addresses
# user1..n:       will own all the funds in the system, we'll set this up from
#                 initial utxo the
# pool-owner1..n: will be the owner of the pools and we'll use their reward
#                 account for pool rewards

ADDRS="${USER_ADDRS} ${POOL_ADDRS}"

mkdir addresses

cp -r shelley/utxo-keys/* addresses

for ADDR in ${UTXO_ADDRS}; do
     # Payment addresses
    ./cardano-cli address build \
        --payment-verification-key-file addresses/${ADDR}.vkey \
        --testnet-magic ${NETWORK_MAGIC} \
        --out-file addresses/${ADDR}.addr
done

for ADDR in ${ADDRS}; do

  # Payment address keys
  ./cardano-cli address key-gen \
      --verification-key-file addresses/${ADDR}.vkey \
      --signing-key-file      addresses/${ADDR}.skey

  # Stake address keys
  ./cardano-cli stake-address key-gen \
      --verification-key-file addresses/${ADDR}-stake.vkey \
      --signing-key-file      addresses/${ADDR}-stake.skey

  # Payment addresses
  ./cardano-cli address build \
      --payment-verification-key-file addresses/${ADDR}.vkey \
      --stake-verification-key-file addresses/${ADDR}-stake.vkey \
      --testnet-magic ${NETWORK_MAGIC} \
      --out-file addresses/${ADDR}.addr

  # Stake addresses
  ./cardano-cli stake-address build \
      --stake-verification-key-file addresses/${ADDR}-stake.vkey \
      --testnet-magic ${NETWORK_MAGIC} \
      --out-file addresses/${ADDR}-stake.addr

  # Stake addresses registration certs
  ./cardano-cli stake-address registration-certificate \
      --stake-verification-key-file addresses/${ADDR}-stake.vkey \
      --out-file addresses/${ADDR}-stake.reg.cert

done

# user N will delegate to pool N
for N in ${POOL_NODES_N}; do

  # Stake address delegation certs
  ./cardano-cli stake-address delegation-certificate \
      --stake-verification-key-file addresses/user${N}-stake.vkey \
      --cold-verification-key-file  node-pool${N}/shelley/operator.vkey \
      --out-file addresses/user${N}-stake.deleg.cert

  cp addresses/pool-owner${N}-stake.vkey node-pool${N}/owner.vkey
  cp addresses/pool-owner${N}-stake.skey node-pool${N}/owner.skey

done

echo "Generated payment address keys, stake address keys,"
echo "stake address regitration certs, and stake address delegatation certs"
echo
ls -1 addresses/
echo "====================================================================="
echo

# Next is to make the stake pool registration cert
for NODE in ${POOL_NODES}; do

  ./cardano-cli stake-pool registration-certificate \
    --testnet-magic ${NETWORK_MAGIC} \
    --pool-pledge 0 --pool-cost 0 --pool-margin 0 \
    --cold-verification-key-file             ${NODE}/shelley/operator.vkey \
    --vrf-verification-key-file              ${NODE}/shelley/vrf.vkey \
    --reward-account-verification-key-file   ${NODE}/owner.vkey \
    --pool-owner-stake-verification-key-file ${NODE}/owner.vkey \
    --out-file                               ${NODE}/registration.cert

done

echo "Generated stake pool registration certs:"
ls -1 node-*/registration.cert
echo "====================================================================="
echo
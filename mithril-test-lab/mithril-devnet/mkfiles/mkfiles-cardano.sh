ARTIFACTS_DIR_TEMP=./temp
mkdir -p ${ARTIFACTS_DIR_TEMP}

# Step 1: Bootstrap the devnet artifacts
# Adapted from https://github.com/IntersectMBO/cardano-node/blob/master/scripts/babbage/mkfiles.sh

# Is semver on the first argument strictly lower than equal to the second argument?
version_lt() {
  VERSION_LHS=$1
  VERSION_RHS=$2
  if [ "${VERSION_LHS}" != "${VERSION_RHS}" ] && [ "${VERSION_LHS}" = "`echo -e "${VERSION_LHS}\n${VERSION_RHS}" | sort -V | head -n1`" ]; then
    echo "true"
  else
    echo "false"
  fi
}

UNAME=$(uname -s) SED=
case $UNAME in
  Darwin )      SED="gsed";;
  Linux )       SED="sed";;
esac


UNAME=$(uname -s) DATE=
case $UNAME in
  Darwin )      DATE="gdate";;
  Linux )       DATE="date";;
  MINGW64_NT* ) UNAME="Windows_NT"
                DATE="date";;
esac

CARDANO_CLI=./cardano-cli
NUM_SPO_NODES=$NUM_POOL_NODES
INIT_SUPPLY=12000000
TOTAL_SUPPLY=2000000000000
DELEGATED_SUPPLY=240000000002
ACTIVE_SLOTS_COEFF=1.0
SECURITY_PARAM=10
START_GENESIS_DELAY=5

START_TIME="$(${DATE} -d "now + ${START_GENESIS_DELAY} seconds" +%s)"

cat > "${ARTIFACTS_DIR_TEMP}/byron.genesis.spec.json" <<EOF
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

$CARDANO_CLI byron genesis genesis \
  --protocol-magic ${NETWORK_MAGIC} \
  --start-time "${START_TIME}" \
  --k ${SECURITY_PARAM} \
  --n-poor-addresses 0 \
  --n-delegate-addresses ${NUM_SPO_NODES} \
  --total-balance ${INIT_SUPPLY} \
  --delegate-share 1 \
  --avvm-entry-count 0 \
  --avvm-entry-balance 0 \
  --protocol-parameters-file "${ARTIFACTS_DIR_TEMP}/byron.genesis.spec.json" \
  --genesis-output-dir "${ARTIFACTS_DIR_TEMP}/byron-gen-command"

cp $SCRIPT_DIRECTORY/configuration/babbage/alonzo-babbage-test-genesis.json "${ARTIFACTS_DIR_TEMP}/genesis.alonzo.spec.json"
cp $SCRIPT_DIRECTORY/configuration/babbage/conway-babbage-test-genesis.json "${ARTIFACTS_DIR_TEMP}/genesis.conway.spec.json"

if [ "${CARDANO_NODE_VERSION_RELEASE}" = "8.1.2" ]; then
  # Fix 8.1.2, to avoid the following error: 'Command failed: genesis create-staked  Error: Error while decoding Shelley genesis at: example/genesis.conway.spec.json Error: Error in $: key "genDelegs" not found'
  mv ${ARTIFACTS_DIR_TEMP}/genesis.conway.spec.json ${ARTIFACTS_DIR_TEMP}/genesis.conway.spec.json.tmp && cat ${ARTIFACTS_DIR_TEMP}/genesis.conway.spec.json.tmp | jq '. += {"genDelegs":{}}' > ${ARTIFACTS_DIR_TEMP}/genesis.conway.spec.json && rm ${ARTIFACTS_DIR_TEMP}/genesis.conway.spec.json.tmp
fi

if [ $(version_lt "${CARDANO_NODE_VERSION_RELEASE}" "8.8.0") = "false" ]; then
  # Fix >=8.8.0, to avoid the following errors: 'Command failed: genesis create-staked  Error: Error: Error while decoding Shelley genesis at: ./temp/genesis.conway.spec.json Error: Error in $.poolVotingThresholds: key "motionNoConfidence" not found
  mv ${ARTIFACTS_DIR_TEMP}/genesis.conway.spec.json ${ARTIFACTS_DIR_TEMP}/genesis.conway.spec.json.tmp && cat ${ARTIFACTS_DIR_TEMP}/genesis.conway.spec.json.tmp | jq '. += {"poolVotingThresholds": {"motionNoConfidence": 0.51, "committeeNormal": 0.51, "committeeNoConfidence": 0.51, "hardForkInitiation": 0.51, "ppSecurityGroup": 0.51}, "dRepVotingThresholds": {"motionNoConfidence": 0.51, "committeeNormal": 0.51, "committeeNoConfidence": 0.51, "updateToConstitution": 0.51, "hardForkInitiation": 0.51, "ppNetworkGroup": 0.51, "ppEconomicGroup": 0.51, "ppTechnicalGroup": 0.51, "ppGovGroup": 0.51, "treasuryWithdrawal": 0.51}}' > ${ARTIFACTS_DIR_TEMP}/genesis.conway.spec.json && rm ${ARTIFACTS_DIR_TEMP}/genesis.conway.spec.json.tmp
  cat ${ARTIFACTS_DIR_TEMP}/genesis.conway.spec.json
fi

if [ $(version_lt "${CARDANO_NODE_VERSION_RELEASE}" "8.10.0") = "false" ]; then
  # Fix >=8.10.0, to avoid the following errors: 'Command failed: genesis create-staked  Error: Error while decoding Shelley genesis at: ./temp/genesis.conway.spec.json Error: Error in $: key "minFeeRefScriptCostPerByte" not found
  mv ${ARTIFACTS_DIR_TEMP}/genesis.conway.spec.json ${ARTIFACTS_DIR_TEMP}/genesis.conway.spec.json.tmp && cat ${ARTIFACTS_DIR_TEMP}/genesis.conway.spec.json.tmp | jq '. += {"minFeeRefScriptCostPerByte": 0, "committee": {"members": {}, "threshold": 0.67}}' > ${ARTIFACTS_DIR_TEMP}/genesis.conway.spec.json && rm ${ARTIFACTS_DIR_TEMP}/genesis.conway.spec.json.tmp
  cat ${ARTIFACTS_DIR_TEMP}/genesis.conway.spec.json
fi

if [ $(version_lt "${CARDANO_NODE_VERSION_RELEASE}" "8.11.0") = "false" ]; then
  # Fix >=8.11.0, to avoid the following errors: 'Command failed: genesis create-staked  Error: Error while decoding Shelley genesis at: ./temp/genesis.conway.spec.json Error: Error in $: key "plutusV3CostModel" not found
  mv ${ARTIFACTS_DIR_TEMP}/genesis.conway.spec.json ${ARTIFACTS_DIR_TEMP}/genesis.conway.spec.json.tmp && cat ${ARTIFACTS_DIR_TEMP}/genesis.conway.spec.json.tmp | jq '. += {"plutusV3CostModel": []}' > ${ARTIFACTS_DIR_TEMP}/genesis.conway.spec.json && rm ${ARTIFACTS_DIR_TEMP}/genesis.conway.spec.json.tmp
  cat ${ARTIFACTS_DIR_TEMP}/genesis.conway.spec.json
fi

cp $SCRIPT_DIRECTORY/configuration/byron/configuration.yaml "${ARTIFACTS_DIR_TEMP}/"
$SED -i "${ARTIFACTS_DIR_TEMP}/configuration.yaml" \
     -e 's/Protocol: RealPBFT/Protocol: Cardano/' \
     -e '/Protocol/ aPBftSignatureThreshold: 0.6' \
     -e 's/minSeverity: Info/minSeverity: Info/' \
     -e 's/: True/: False/' \
     -e 's/TracingVerbosity: NormalVerbosity/TracingVerbosity: NormalVerbosity/' \
     -e 's/TurnOnLogging: False/TurnOnLogging: True/' \
     -e 's/TraceChainDb: False/TraceChainDb: True/' \
     -e 's/TraceErrorPolicy: False/TraceErrorPolicy: True/' \
     -e 's/TraceLocalErrorPolicy: False/TraceLocalErrorPolicy: True/' \
     -e 's/TraceMempool: False/TraceMempool: True/' \
     -e 's|GenesisFile: genesis.json|ByronGenesisFile: byron/genesis.json|' \
     -e '/ByronGenesisFile/ aShelleyGenesisFile: shelley/genesis.json' \
     -e '/ByronGenesisFile/ aAlonzoGenesisFile: shelley/genesis.alonzo.json' \
     -e '/ByronGenesisFile/ aConwayGenesisFile: shelley/genesis.conway.json' \
     -e 's/RequiresNoMagic/RequiresMagic/' \
     -e 's/LastKnownBlockVersion-Major: 0/LastKnownBlockVersion-Major: 6/' \
     -e 's/LastKnownBlockVersion-Minor: 2/LastKnownBlockVersion-Minor: 0/'

echo "TestShelleyHardForkAtEpoch: 0" >> "${ARTIFACTS_DIR_TEMP}/configuration.yaml"
echo "TestAllegraHardForkAtEpoch: 0" >> "${ARTIFACTS_DIR_TEMP}/configuration.yaml"
echo "TestMaryHardForkAtEpoch: 0" >> "${ARTIFACTS_DIR_TEMP}/configuration.yaml"
echo "TestAlonzoHardForkAtEpoch: 0" >> "${ARTIFACTS_DIR_TEMP}/configuration.yaml"
echo "TestBabbageHardForkAtEpoch: ${HARD_FORK_BABBAGE_AT_EPOCH}" >> "${ARTIFACTS_DIR_TEMP}/configuration.yaml"
echo "TestConwayHardForkAtEpoch: ${HARD_FORK_CONWAY_AT_EPOCH}" >> "${ARTIFACTS_DIR_TEMP}/configuration.yaml"
echo "ExperimentalProtocolsEnabled: True" >> "${ARTIFACTS_DIR_TEMP}/configuration.yaml"

$CARDANO_CLI genesis create-staked --genesis-dir "${ARTIFACTS_DIR_TEMP}" \
  --testnet-magic "${NETWORK_MAGIC}" \
  --gen-pools ${NUM_SPO_NODES} \
  --supply ${TOTAL_SUPPLY} \
  --supply-delegated ${DELEGATED_SUPPLY} \
  --gen-stake-delegs ${NUM_SPO_NODES} \
  --gen-utxo-keys ${NUM_SPO_NODES}

## Customize the Shelley genesis file
cat ${ARTIFACTS_DIR_TEMP}/genesis.json | jq --argjson slot_length ${SLOT_LENGTH} --argjson epoch_length ${EPOCH_LENGTH} --argjson active_slots_coeff ${ACTIVE_SLOTS_COEFF} --argjson security_param ${SECURITY_PARAM} '. + {slotLength: $slot_length, activeSlotsCoeff: $active_slots_coeff, securityParam: $security_param, epochLength: $epoch_length, maxLovelaceSupply: 10000000000000, updateQuorum: 2}' > ${ARTIFACTS_DIR_TEMP}/genesis.json.tmp
cat ${ARTIFACTS_DIR_TEMP}/genesis.json.tmp | jq --raw-output '.protocolParams.protocolVersion.major = 7 | .protocolParams.minFeeA = 44 | .protocolParams.minFeeB = 155381 | .protocolParams.minUTxOValue = 1000000 | .protocolParams.decentralisationParam = 0.7 | .protocolParams.rho = 0.1 | .protocolParams.tau = 0.1'  > ${ARTIFACTS_DIR_TEMP}/genesis.json
rm ${ARTIFACTS_DIR_TEMP}/genesis.json.tmp

# Step 2: Dispatch artifacts in the correct directories

## Copy the configuration files
for NODE in ${ALL_NODES}; do
  cp ${ARTIFACTS_DIR_TEMP}/configuration.yaml ${NODE}/
done

## Copy the Byron genesis files
for NODE in ${ALL_NODES}; do
  cp ${ARTIFACTS_DIR_TEMP}/byron-gen-command/genesis.json ${NODE}/byron/
  cp ${ARTIFACTS_DIR_TEMP}/genesis.spec.json ${NODE}/byron/
done

## Copy the Byron delegation artifacts
for N in ${POOL_NODES_N}; do
  cp ${ARTIFACTS_DIR_TEMP}/byron-gen-command/delegate-keys.00$((${N} - 1)).key node-pool${N}/byron/delegate.key
  cp ${ARTIFACTS_DIR_TEMP}/byron-gen-command/delegation-cert.00$((${N} - 1)).json node-pool${N}/byron/delegate.cert
done

## Copy the Shelley genesis files
for NODE in ${ALL_NODES}; do
  cp ${ARTIFACTS_DIR_TEMP}/genesis*.json ${NODE}/shelley/
done

## Copy the SPO artifacts
for N in ${POOL_NODES_N}; do
  cp ${ARTIFACTS_DIR_TEMP}/pools/vrf${N}.skey node-pool${N}/shelley/vrf.skey
  cp ${ARTIFACTS_DIR_TEMP}/pools/vrf${N}.vkey node-pool${N}/shelley/vrf.vkey
  cp ${ARTIFACTS_DIR_TEMP}/pools/cold${N}.skey node-pool${N}/shelley/cold.skey
  cp ${ARTIFACTS_DIR_TEMP}/pools/cold${N}.vkey node-pool${N}/shelley/cold.vkey
  cp ${ARTIFACTS_DIR_TEMP}/pools/kes${N}.skey node-pool${N}/shelley/kes.skey
  cp ${ARTIFACTS_DIR_TEMP}/pools/kes${N}.vkey node-pool${N}/shelley/kes.vkey
  cp ${ARTIFACTS_DIR_TEMP}/pools/opcert${N}.counter node-pool${N}/shelley/opcert.counter  
  cp ${ARTIFACTS_DIR_TEMP}/pools/opcert${N}.cert node-pool${N}/shelley/opcert.cert  
done

# Prepare staking
cp -r ${ARTIFACTS_DIR_TEMP}/utxo-keys/* addresses

for ADDR in ${UTXO_ADDRS}; do
    # Payment addresses
    ./cardano-cli address build \
        --payment-verification-key-file addresses/${ADDR}.vkey \
        --testnet-magic ${NETWORK_MAGIC} \
        --out-file addresses/${ADDR}.addr
done

for ADDR in ${USER_ADDRS}; do
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
  
done


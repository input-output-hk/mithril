
NODE_PORT_START=3000

# Create network topology
FULL_NODES=()
FULL_NODES_N=()
for (( i=1; i<=${NUM_FULL_NODES}; i++ ))
  do
    FULL_NODES=("${FULL_NODES[@]}" "node-full${i}")
    FULL_NODES_N=("${FULL_NODES_N[@]}" "${i}")
done
FULL_NODES=${FULL_NODES[@]}
FULL_NODES_N=${FULL_NODES_N[@]}

POOL_NODES=()
POOL_NODES_N=()
UTXO_ADDRS=()
USER_ADDRS=()
POOL_ADDRS=()
for (( i=1; i<=${NUM_POOL_NODES}; i++ ))
  do
    POOL_NODES=("${POOL_NODES[@]}" "node-pool${i}")
    POOL_NODES_N=("${POOL_NODES_N[@]}" "${i}")
    UTXO_ADDRS=("${UTXO_ADDRS[@]}" "utxo${i}")
    USER_ADDRS=("${USER_ADDRS[@]}" "user${i}")
    POOL_ADDRS=("${POOL_ADDRS[@]}" "pool-owner${i}")
done
POOL_NODES=${POOL_NODES[@]}
POOL_NODES_N=${POOL_NODES_N[@]}
UTXO_ADDRS=${UTXO_ADDRS[@]}
USER_ADDRS=${USER_ADDRS[@]}
POOL_ADDRS=${POOL_ADDRS[@]}

ALL_NODES="${FULL_NODES} ${POOL_NODES}"

# create the node directories
for NODE in ${ALL_NODES}; do
  mkdir -p ${NODE} ${NODE}/byron ${NODE}/shelley ${NODE}/ipc ${NODE}/tx
done

# create the topology files
NODE_ADDR=$LISTENING_ADDR
NODE_PORT=NODE_PORT_START
TOPOLOGY='{"Producers": []}'
TOPOLOGY_DOCKER=$TOPOLOGY
for NODE in ${FULL_NODES}; do
  NODE_PORT=$(( ${NODE_PORT} + 1))
  echo ${NODE_PORT} > ${NODE}/port
  echo ${LISTENING_ADDR} > ${NODE}/host
done
for NODE in ${POOL_NODES}; do
  NODE_PORT=$(( ${NODE_PORT} + 1))
  echo ${NODE_PORT} > ${NODE}/port
  TOPOLOGY=$(echo ${TOPOLOGY} | jq '.Producers[.Producers| length] |= . + {"addr": "'${NODE_ADDR}'","port": '${NODE_PORT}', "valency": 1}')
  echo ${LISTENING_ADDR} > ${NODE}/host
done
echo $TOPOLOGY | jq . > topology.json

for NODE in ${FULL_NODES}; do
  cat topology.json > ${NODE}/topology.json
done
NODE_IX=0
for NODE in ${POOL_NODES}; do
  cat topology.json |  jq '.Producers |= del(.['${NODE_IX}'])' > ${NODE}/topology.json
  NODE_IX=$(( ${NODE_IX} + 1))
done
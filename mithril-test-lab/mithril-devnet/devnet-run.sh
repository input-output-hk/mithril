# Default values
if [ -z "${ROOT}" ]; then 
  ROOT="artifacts"
fi
if [ -z "${NUM_BFT_NODES}" ]; then 
  NUM_BFT_NODES="1"
fi
if [ -z "${NUM_POOL_NODES}" ]; then 
  NUM_POOL_NODES="2"
fi


# Bootstrap devnet
echo "> Bootstrap Mithril/Cardano devnet"
echo ">> Directory: ${ROOT}"
echo ">> Cardano BFT nodes: ${NUM_BFT_NODES}"
echo ">> Cardano SPO nodes: ${NUM_POOL_NODES}"
echo ">> Info: Mithril Aggregator will be attached to the first Cardano BFT node"
echo ">> Info: Mithril Signers will be attached to each Cardano SPO node"
rm -rf ${ROOT} && ./devnet-mkfiles.sh ${ROOT} ${NUM_BFT_NODES} ${NUM_POOL_NODES} > /dev/null
echo

# Change directory
cd ${ROOT}

# Start devnet
echo "> Start Mithril/Cardano devnet"
./start.sh
echo

# Query devnet
echo "> Query Mithril/Cardano devnet"
sleep 10
./query.sh
echo
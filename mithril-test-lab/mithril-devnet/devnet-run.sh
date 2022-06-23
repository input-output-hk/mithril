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
if [ -z "${NODES}" ]; then 
  NODES="*"
fi

# Bootstrap devnet
echo "====================================================================="
echo " Bootstrap Mithril/Cardano devnet"
echo "====================================================================="
echo
echo ">> Directory: ${ROOT}"
echo ">> Cardano BFT nodes: ${NUM_BFT_NODES}"
echo ">> Cardano SPO nodes: ${NUM_POOL_NODES}"
echo ">> Info: Mithril Aggregator will be attached to the first Cardano BFT node"
echo ">> Info: Mithril Signers will be attached to each Cardano SPO node"
rm -rf ${ROOT} && ./devnet-mkfiles.sh ${ROOT} ${NUM_BFT_NODES} ${NUM_POOL_NODES} > /dev/null
echo

# Change directory
cd ${ROOT}

# Start devnet Cardano nodes
if [ "${NODES}" = "cardano" ] || [ "${NODES}" = "*" ]; then 
    echo "====================================================================="
    echo " Start Cardano nodes"
    echo "====================================================================="
    echo
    ./start-cardano.sh
    echo
fi

# Start devnet Mithril nodes
if [ "${NODES}" = "mithril" ] || [ "${NODES}" = "*" ]; then 
    echo "====================================================================="
    echo " Start Mithril nodes"
    echo "====================================================================="
    echo
    ./start-mithril.sh
    echo
fi
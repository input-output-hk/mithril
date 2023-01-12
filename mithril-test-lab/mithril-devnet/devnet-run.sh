#!/bin/bash
set -e

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
if [ -z "${SLOT_LENGTH}" ]; then 
  SLOT_LENGTH="0.75"
fi
if [ -z "${EPOCH_LENGTH}" ]; then 
  EPOCH_LENGTH="100"
fi
if [ -z "${DELEGATE_PERIOD}" ]; then 
  DELEGATE_PERIOD="180"
fi

# Bootstrap devnet
echo "====================================================================="
echo " Bootstrap Mithril/Cardano devnet"
echo "====================================================================="
echo
echo ">> Directory: ${ROOT}"
echo ">> Cardano BFT nodes: ${NUM_BFT_NODES}"
echo ">> Cardano SPO nodes: ${NUM_POOL_NODES}"
echo ">> Cardano Slot Length: ${SLOT_LENGTH}s"
echo ">> Cardano Epoch Length: ${EPOCH_LENGTH}s"
echo ">> Info: Mithril Aggregator will be attached to the first Cardano BFT node"
echo ">> Info: Mithril Signers will be attached to each Cardano SPO node"
rm -rf ${ROOT} && ./devnet-mkfiles.sh ${ROOT} ${NUM_BFT_NODES} ${NUM_POOL_NODES} ${SLOT_LENGTH} ${EPOCH_LENGTH}> /dev/null
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

# Schedule stake delegation
echo "====================================================================="
echo " Schedule Cardano Stake Delegation"
echo "====================================================================="
echo
DELEGATION_ROUND=0
echo ">> Begin scheduled stakes delegation"
while true
do
    echo ">> $(date +"%T"): Wait ${DELEGATE_PERIOD}s until next stakes delegation round..."
    sleep ${DELEGATE_PERIOD}
    DELEGATION_ROUND=$(( $DELEGATION_ROUND + 1 ))
    echo ">> Run stakes delegation round #${DELEGATION_ROUND}!"
    DELEGATION_ROUND=${DELEGATION_ROUND} ./delegate.sh
done
echo
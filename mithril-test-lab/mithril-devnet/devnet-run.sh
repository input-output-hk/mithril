#!/usr/bin/env bash
set -e

# Default values
if [ -z "${NODES}" ]; then 
  NODES="*"
fi
if [ -z "${ARTIFACTS_DIR}" ]; then 
  ARTIFACTS_DIR="artifacts"
fi
if [ -z "${FORCE_DELETE_ARTIFACTS_DIR}" ]; then 
  FORCE_DELETE_ARTIFACTS_DIR="true"
fi
if [ -z "${DELEGATE_PERIOD}" ]; then 
  DELEGATE_PERIOD="180"
fi


# Bootstrap devnet
echo "====================================================================="
echo " Bootstrap Mithril/Cardano devnet"
echo "====================================================================="
echo
if [[ "$FORCE_DELETE_ARTIFACTS_DIR" == "true" ]]; then
  echo ">> The ${ARTIFACTS_DIR} directory was force deleted"
  rm -rf ${ARTIFACTS_DIR} > /dev/null
fi
ARTIFACTS_DIR=${ARTIFACTS_DIR} $(pwd)/devnet-mkfiles.sh
echo

# Change directory
pushd ${ARTIFACTS_DIR} > /dev/null

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
    echo ">> Info: Mithril Aggregator will be attached to the first Cardano Full node"
    echo ">> Info: Mithril Signers will be attached to each Cardano SPO node"
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

popd
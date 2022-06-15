# Default values
if [ -z "${ROOT}" ]; then 
  ROOT="artifacts"
fi
if [ -z "${LINES}" ]; then 
  LINES="10"
fi

# Change directory
cd ${ROOT}

# Logs devnet
echo "====================================================================="
echo " Logs Mithril/Cardano devnet"
echo "====================================================================="
echo
./log.sh ${LINES}
echo
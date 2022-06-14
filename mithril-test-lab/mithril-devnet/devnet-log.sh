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
echo "> Start Mithril/Cardano devnet"
./log.sh ${LINES}
echo
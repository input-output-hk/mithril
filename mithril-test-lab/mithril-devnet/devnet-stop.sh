# Default values
if [ -z "${ROOT}" ]; then 
  ROOT="artifacts"
fi

# Change directory
cd ${ROOT}

# Stop devnet
echo "> Start Mithril/Cardano devnet"
./stop.sh
echo
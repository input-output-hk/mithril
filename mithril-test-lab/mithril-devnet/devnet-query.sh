# Default values
if [ -z "${ROOT}" ]; then 
  ROOT="artifacts"
fi

# Change directory
cd ${ROOT}

# Query devnet
echo "> Query Mithril/Cardano devnet"
./query.sh
echo
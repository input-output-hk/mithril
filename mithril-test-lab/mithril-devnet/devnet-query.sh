# Default values
if [ -z "${ROOT}" ]; then 
  ROOT="artifacts"
fi

# Change directory
cd ${ROOT}

# Query devnet
echo "====================================================================="
echo " Query Mithril/Cardano devnet"
echo "====================================================================="
echo
./query.sh
echo
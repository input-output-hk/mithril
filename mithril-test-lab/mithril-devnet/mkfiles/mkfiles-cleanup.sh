
echo "Cleanup artifacts directory"
rm -rf byron shelley 
rm configuration.yaml topology.json
for NODE in ${ALL_NODES}; do

  rm ${NODE}/host
  rm ${NODE}/port

done
echo "====================================================================="
echo

echo
echo "Then, activate the pools:"
echo
echo ./activate.sh .
echo
echo "Or do all at once with:"
echo
echo "./start-cardano.sh && ./start-mithril.sh"
echo
echo "Then query the devnet:"
echo
echo ./query.sh
echo
echo "And stop with:"
echo
echo ./stop.sh
echo
echo

popd

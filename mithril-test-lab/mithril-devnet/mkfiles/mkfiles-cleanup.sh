
rm -rf temp

for NODE in ${ALL_NODES}; do
  rm ${NODE}/host
  rm ${NODE}/port
done


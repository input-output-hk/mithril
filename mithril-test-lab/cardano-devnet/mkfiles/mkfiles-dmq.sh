for NODE in ${ALL_NODES}; do
  cat >> ${NODE}/config.dmq.json <<EOF
{
  "NetworkMagic": ${DMQ_NETWORK_MAGIC},
  "CardanoNodeSocket": "${NODE}/ipc/node.sock",
  "CardanoNetworkMagic": ${NETWORK_MAGIC},
  "PeerSharing": true,
  "PeerSelectionCounters": true,
  "TraceOptions": {
    "": {
      "backends": ["Stdout MachineFormat"],
      "severity": "Debug"
    }
  }
}
EOF
done

for NODE in ${ALL_NODES}; do
  cat >> ${NODE}/config.dmq.json <<EOF
{
  "NetworkMagic": ${DMQ_NETWORK_MAGIC},
  "CardanoNodeSocket": "${NODE}/ipc/node.sock",
  "CardanoNetworkMagic": ${NETWORK_MAGIC},
  "LocalMsgSubmissionTracer": true,
  "LocalMsgNotificationTracer": true,
  "ConnectionManagerTracer": true,
  "DiffusionTracer": false,
  "InboundGovernorTracer": false,
  "LocalInboundGovernorTracer": false,
  "PeerSelectionTracer": true,
  "PeerSelectionCounters": false,
  "PeerSharing": false,
  "SigSubmissionLogicTracer": true,
  "SigSubmissionClientTracer": true,
  "SigSubmissionServerTracer": true,
  "MuxTracer": true,
  "ChannelTracer": true,
  "DebugPeerSelectionTracer": true
}

EOF
done

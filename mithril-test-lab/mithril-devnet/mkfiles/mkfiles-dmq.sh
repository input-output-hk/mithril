for NODE in ${ALL_NODES}; do
  cat >> ${NODE}/config.dmq.json <<EOF
{
  "NetworkMagic": ${DMQ_NETWORK_MAGIC},
  "CardanoNodeSocket": "${NODE}/ipc/node.sock",
  "CardanoNetworkMagic": ${NETWORK_MAGIC},
  "PeerSharing": true,
  "LocalMsgSubmissionTracer": true,
  "LocalMsgNotificationTracer": true,
  "ConnectionManagerTracer": true,
  "DiffusionTracer": true,
  "InboundGovernorTracer": true,
  "LocalInboundGovernorTracer": true,
  "PeerSelectionTracer": true,
  "PeerSelectionCounters": true,
  "SigSubmissionLogicTracer": true,
  "SigSubmissionClientTracer": true,
  "SigSubmissionServerTracer": true,
  "SigSubmissionClientProtocolTracer": true,
  "SigSubmissionServerProtocolTracer": true,
  "MuxTracer": true,
  "ChannelTracer": false,
  "DebugPeerSelectionTracer": true,
  "ValidationTracer": true
}

EOF
done

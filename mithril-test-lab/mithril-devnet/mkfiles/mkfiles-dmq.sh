# Temporary script to copy DMQ node binary
cp $DMQ_NODE_BINARY/dmq-node ./bin/

for NODE in ${ALL_NODES}; do
  cat >> ${NODE}/config.dmq.json <<EOF
{
  "LocalMsgSubmissionTracer": true,
  "LocalMsgNotificationTracer": true,
  "ConnectionManagerTracer": false,
  "DiffusionTracer": true,
  "InboundGovernorTracer": false,
  "LocalInboundGovernorTracer": false,
  "PeerSelectionTracer": false,
  "PeerSelectionCounters": false,
  "PeerSharing": false
}
EOF
done
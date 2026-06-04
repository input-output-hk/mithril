#!/usr/bin/env bash

echo ">> Query chain tip"
CARDANO_NODE_SOCKET_PATH=node-pool1/ipc/node.sock ./cardano-cli latest query tip \
    --testnet-magic 42 | jq .

echo
echo ">> Query whole utxo"
CARDANO_NODE_SOCKET_PATH=node-pool1/ipc/node.sock ./cardano-cli latest query utxo \
    --testnet-magic 42 \
    --whole-utxo
echo

echo ">> Query stake pools"
CARDANO_NODE_SOCKET_PATH=node-pool1/ipc/node.sock ./cardano-cli latest query stake-pools \
    --testnet-magic 42
echo

echo ">> Query stake distribution"
CARDANO_NODE_SOCKET_PATH=node-pool1/ipc/node.sock ./cardano-cli latest query stake-snapshot --all-stake-pools \
    --testnet-magic 42 | jq .
echo


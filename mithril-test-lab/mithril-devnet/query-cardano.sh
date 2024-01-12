#!/usr/bin/env bash

echo ">> Query chain tip"
CARDANO_NODE_SOCKET_PATH=node-pool1/ipc/node.sock ./cardano-cli query tip \
    --cardano-mode \
    --testnet-magic 42 | jq .

echo
echo ">> Query whole utxo"
CARDANO_NODE_SOCKET_PATH=node-pool1/ipc/node.sock ./cardano-cli query utxo \
    --cardano-mode \
    --testnet-magic 42 \
    --whole-utxo
echo

echo ">> Query stake pools"
CARDANO_NODE_SOCKET_PATH=node-pool1/ipc/node.sock ./cardano-cli query stake-pools \
    --cardano-mode \
    --testnet-magic 42
echo

echo ">> Query stake distribution"
CARDANO_NODE_SOCKET_PATH=node-pool1/ipc/node.sock ./cardano-cli query stake-snapshot --all-stake-pools \
    --cardano-mode \
    --testnet-magic 42 | jq .
echo


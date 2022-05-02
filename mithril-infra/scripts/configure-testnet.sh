#!/bin/bash
# Configure environment for user curry

# fail if something goes wrong
set -e

# accept github.com key
ssh-keyscan github.com >> ~/.ssh/known_hosts

# download gpg key signing testnet dump
curl https://api.github.com/users/abailly-iohk/gpg_keys | jq -r '.[] | .raw_key' | gpg --import

# download & verify testnet archive
# TODO: use mithril client!
curl -o testnet.tar.gz https://storage.googleapis.com/cardano-testnet/testnet.tar.gz
curl -o testnet.tar.gz.asc https://storage.googleapis.com/cardano-testnet/testnet.tar.gz.asc
gpg --verify testnet.tar.gz.asc

tar xzf testnet.tar.gz

# get cardano network configuration
git clone https://github.com/input-output-hk/cardano-configurations

# get mithril repository for scripts
git clone git@github.com:input-output-hk/mithril

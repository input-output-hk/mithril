#!/bin/bash
# Configure environment for user curry

# fail if something goes wrong
set -e

# accept github.com key
ssh-keyscan github.com >> ~/.ssh/known_hosts

# get cardano network configuration
git clone https://github.com/input-output-hk/cardano-configurations

# get mithril repository for scripts
git clone git@github.com:input-output-hk/mithril

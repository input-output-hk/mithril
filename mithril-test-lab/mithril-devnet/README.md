# Cardano/Mithril Private Devnet

**This is a work in progress** :hammer_and_wrench:

It scaffolds a private compound devnet with Cardano and Mithril nodes:

- `N` Cardano Full nodes
- `P` Cardano SPO nodes
- `1` Mithril Aggregator node (attached to the first Cardano Full node)
- `P` Mithril Signer nodes (attached to each Cardano SPO nodes)

## Credits

This cli is inspired by this [script](https://github.com/IntersectMBO/cardano-node/blob/master/scripts/byron-to-alonzo/mkfiles.sh) from the Cardano node team.

## Pre-requisites

- You need to run a Linux computer
- You need to have a recent version of `jq` running (1.6+)

## Download source code

```bash
# Download sources from github
git clone https://github.com/input-output-hk/mithril

# Go to sources directory
cd mithril/mithril-test-lab/mithril-devnet

# Chmod scripts
chmod u+x *.sh
```

## One step run with default configuration

```bash
# Run devnet with 1 Full node and 2 SPO nodes (with local docker images)
./devnet-run.sh

# Run devnet with 1 Full node and 2 SPO nodes (with remote docker images)
MITHRIL_IMAGE_ID=main-c9213ca ./devnet-run.sh

# Run devnet with Cardano nodes only
NODES=cardano ./devnet-run.sh

# Run devnet with Mithril nodes only
NODES=mithril ./devnet-run.sh

# Build Mithril Docker images available options
## from locally built binaries (fast build times, enabled by default)
./devnet-run.sh
### or
MITHRIL_NODE_DOCKER_BUILD_TYPE=ci ./devnet-run.sh

## from locally built binaries with a custom slim image used as Docker image source
### 'debian:13-slim': default value, works on Ubuntu 24.04
MITHRIL_NODE_DOCKER_CI_IMAGE_FROM=debian:13-slim MITHRIL_NODE_DOCKER_BUILD_TYPE=ci ./devnet-run.sh

## from rust builder in Docker (slower build times, always works)
MITHRIL_NODE_DOCKER_BUILD_TYPE=legacy ./devnet-run.sh

# Logs devnet
./devnet-log.sh

# Logs Cardano nodes only on devnet
NODES=cardano ./devnet-log.sh

# Logs Mithril nodes only on devnet
NODES=mithril ./devnet-log.sh

# Query devnet
./devnet-query.sh

# Query Cardano nodes only on devnet
NODES=cardano ./devnet-query.sh

# Query Mithril nodes only on devnet
NODES=mithril ./devnet-query.sh

# Stop devnet
./devnet-stop.sh

# Visualize devnet
./devnet-visualize.sh
```

## One step run with custom configuration

```bash
# Run devnet with 2 Full nodes and 5 SPO nodes
ARTIFACTS_DIR=artifacts NUM_FULL_NODES=2 NUM_POOL_NODES=5 ./devnet-run.sh

# Run devnet custom slot length (0.5s) and custom epoch length (120s)
# Slot length: the duration of a Cardano Eslot (can help modulate the immutables creation rate)
# Epoch Length: the duration of a Cardano Epoch
ARTIFACTS_DIR=artifacts SLOT_LENGTH=0.5 EPOCH_LENGTH=120 ./devnet-run.sh

# Logs devnet
ARTIFACTS_DIR=artifacts LINES=10 ./devnet-log.sh

# Query devnet
ARTIFACTS_DIR=artifacts ./devnet-query.sh

# Stop devnet
ARTIFACTS_DIR=artifacts ./devnet-stop.sh

# Visualize devnet
ARTIFACTS_DIR=artifacts ./devnet-visualize.sh
```

## Step by step run with custom configuration

```bash
# Parameters
ARTIFACTS_DIR=artifacts # Directory where artifacts are produced
NUM_FULL_NODES=1 # Number of Cardano Full nodes
NUM_POOL_NODES=3 # Number of Cardano SPO nodes

# Bootstrap devnet with 1 Full node and 3 SPO nodes
rm -rf ${ARTIFACTS_DIR} && ./devnet-mkfiles.sh ${ARTIFACTS_DIR} ${NUM_FULL_NODES} ${NUM_POOL_NODES}

# Change directory
cd ${ARTIFACTS_DIR}

# Start devnet Cardano nodes
./start-cardano.sh

# Start devnet Mithril nodes
./start-mithril.sh

# Query devnet
./query.sh

# Logs devnet
./log.sh 10

# Stop devnet
./stop.sh
```

## Artifacts generated for the network

```bash
# Example of artifacts
artifacts
├── activate.sh
├── addresses
│   ├── pool-owner1.addr
│   ├── pool-owner1.skey
│   ├── pool-owner1-stake.addr
│   ├── pool-owner1-stake.reg.cert
│   ├── pool-owner1-stake.skey
│   ├── pool-owner1-stake.vkey
│   ├── pool-owner1.vkey
│   ├── user1.addr
│   ├── user1.skey
│   ├── user1-stake.addr
│   ├── user1-stake.deleg.cert
│   ├── user1-stake.reg.cert
│   ├── user1-stake.skey
│   ├── user1-stake.vkey
│   ├── user1.vkey
│   ├── utxo1.addr
│   ├── utxo1.skey
│   └── utxo1.vkey
├── cardano-cli
├── cardano-node
├── docker-compose.yaml
├── log.sh
├── node-full1
│   ├── byron
│   │   ├── delegate.cert
│   │   ├── delegate.key
│   │   ├── genesis.json
│   │   └── genesis.spec.json
│   ├── configuration.yaml
│   ├── ipc
│   ├── shelley
│   │   ├── genesis.alonzo.json
│   │   ├── genesis.alonzo.spec.json
│   │   ├── genesis.json
│   │   ├── genesis.spec.json
│   │   ├── kes.skey
│   │   ├── kes.vkey
│   │   ├── node.cert
│   │   ├── operator.counter
│   │   ├── operator.skey
│   │   ├── operator.vkey
│   │   ├── vrf.skey
│   │   └── vrf.vkey
│   ├── start-node.sh
│   ├── topology.docker.json
│   ├── topology.json
│   └── tx
├── node-pool1
│   ├── byron
│   │   ├── genesis.json
│   │   └── genesis.spec.json
│   ├── configuration.yaml
│   ├── ipc
│   ├── owner.skey
│   ├── owner.vkey
│   ├── registration.cert
│   ├── shelley
│   │   ├── genesis.alonzo.json
│   │   ├── genesis.alonzo.spec.json
│   │   ├── genesis.json
│   │   ├── genesis.spec.json
│   │   ├── kes.skey
│   │   ├── kes.vkey
│   │   ├── node.cert
│   │   ├── operator.counter
│   │   ├── operator.skey
│   │   ├── operator.vkey
│   │   ├── vrf.skey
│   │   └── vrf.vkey
│   ├── start-node.sh
│   ├── topology.docker.json
│   ├── topology.json
│   └── tx
├── query.sh
├── start-cardano.sh
├── start-mithril.sh
└── stop.sh
```

## Example utxo & stakes informations retrieved from the network

```bash
=====================================================================
 Query Mithril/Cardano devnet
=====================================================================

=====================================================================
=== Mithril Network
=====================================================================

>> Query snapshots
[
  {
    "digest": "4bb710c0788711bae384edad1f0a5aaa6f004e8911db7acfc78471a6bea41154",
    "certificate_hash": "128bd468b48395bf62d9b88fcccff60b147433374c134efaf579c6faa7b04d9f",
    "size": 7979,
    "created_at": "2022-06-16T09:32:37.084429383Z",
    "locations": [
      "http://0.0.0.0:8080/aggregator/snapshot/4bb710c0788711bae384edad1f0a5aaa6f004e8911db7acfc78471a6bea41154/download"
    ]
  }
]

=====================================================================
=== Cardano Network
=====================================================================

>> Query chain tip
{
    "era": "Alonzo",
    "syncProgress": "100.00",
    "hash": "3be9b4493fea53f5da1f765ec49fe0268f6dd4097bc2437a5ed6e8600bfbb61e",
    "epoch": 2,
    "slot": 200,
    "block": 8
}

>> Query whole utxo
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
6c12b0d33e83fdaa830f00d16bcc91a6ed897e770600473b579fa746ba633194     0        448999157 lovelace + TxOutDatumNone
6c12b0d33e83fdaa830f00d16bcc91a6ed897e770600473b579fa746ba633194     1        1000000 lovelace + TxOutDatumNone
9134193f0cd9d838752c8215313a22213c08dbb6d58218ae4ef2fbabe5bf6c30     0        447999157 lovelace + TxOutDatumNone
9134193f0cd9d838752c8215313a22213c08dbb6d58218ae4ef2fbabe5bf6c30     1        2000000 lovelace + TxOutDatumNone
f90d90f58d0decba651c7fe476cf9b866254363347b8bd38e14c03e0a57e8aa1     0        1002000000 lovelace + TxOutDatumNone

>> Query stake pools
pool1vu2kd36hm9wp5hrzczp5ahdh0j2ls59srq5s8ju542zyqr5qd6j
pool1n6sxl7cfe9j9mf6jv228nluvy3k3xdu62chqk2wfaazrsenz4jz

>> Query stake distribution
                           PoolId                                 Stake frac
------------------------------------------------------------------------------
pool1vu2kd36hm9wp5hrzczp5ahdh0j2ls59srq5s8ju542zyqr5qd6j   1.052e-3
pool1n6sxl7cfe9j9mf6jv228nluvy3k3xdu62chqk2wfaazrsenz4jz   5.258e-4
```

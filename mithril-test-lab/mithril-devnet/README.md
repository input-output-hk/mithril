# Cardano/Mithril Private Devnet

**This is a work in progress** :hammer_and_wrench:

It scaffolds a private compound devnet with Cardano and Mithril nodes:

* `N` Cardano BFT nodes
* `P` Cardano SPO nodes
* `1` Mithril Aggregator node (attached to the first Cardano BFT node)
* `P` Mithril Signer nodes (attached to each Cardano SPO nodes)

## Credits

This cli is inspired by this [script](https://github.com/input-output-hk/cardano-node/blob/master/scripts/byron-to-alonzo/mkfiles.sh) from the Cardano node team.

## Pre-requisites

* You need to run a Linux computer
* You need to have a recent version of `jq` running (1.6+)

## Download source code

```bash
# Download sources from github
git clone https://github.com/input-output-hk/mithril

# Go to sources directory
cd mithril-test-lab/mitril-devnet

# Chmod scripts
chmod u+x *.sh
```

## One step run with default configuration

```bash
# Run devnet with 1 BFT node and 2 SPO nodes (with local docker images)
./devnet-run.sh

# Run devnet with 1 BFT node and 2 SPO nodes (with remote docker images)
MITHRIL_IMAGE_ID=main-c9213ca ./devnet-run.sh

# Logs devnet
./devnet-log.sh

# Query devnet
./devnet-query.sh

# Stop devnet
./devnet-stop.sh
```

## One step run with custom configuration

```bash
# Run devnet with 2 BFT nodes and 5 SPO nodes
ROOT=artifacts NUM_BFT_NODES=2 NUM_POOL_NODES=5 ./devnet-run.sh

# Logs devnet
ROOT=artifacts LINES=10 ./devnet-log.sh

# Query devnet
ROOT=artifacts ./devnet-query.sh

# Stop devnet
ROOT=artifacts ./devnet-stop.sh
```

## Step by step run with custom configuration

```bash
# Parameters
ROOT=artifacts # Directory where artifacts are produced
NUM_BFT_NODES=1 # Number of Cardano BFT nodes
NUM_POOL_NODES=3 # Number of Cardano SPO nodes

# Bootstrap devnet with 1 BFT nodes and 3 SPO nodes
rm -rf ${ROOT} && ./devnet-mkfiles.sh ${ROOT} ${NUM_BFT_NODES} ${NUM_POOL_NODES}

# Change directory
cd ${ROOT}

# Start devnet
./start.sh

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
tree artifacts
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
├── node-bft1
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
├── start.sh
└── stop.sh
```

## Example utxo & stakes informations retrieved from the network

```bash
> Query Mithril/Cardano devnet
=====================================================================
=== Cardano Network
=====================================================================

>> Query chain tip
{
    "era": "Alonzo",
    "syncProgress": "100.00",
    "hash": "b7fde5dea1a85879410c03c049f2ce199f9b1a17f22043e801ed7328a3a8f0ac",
    "epoch": 4,
    "slot": 400,
    "block": 18
}

>> Query whole utxo
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
23f3844ba659a38da8d857ebd6ec93528adc603cc46f1605cdabdaaea57111d0     0        447999157 lovelace + TxOutDatumNone
23f3844ba659a38da8d857ebd6ec93528adc603cc46f1605cdabdaaea57111d0     1        2000000 lovelace + TxOutDatumNone
2c754076364cfc2a5279e97165f7d46b4035f7ff288ae41c407e1c3001ab895c     0        448999157 lovelace + TxOutDatumNone
2c754076364cfc2a5279e97165f7d46b4035f7ff288ae41c407e1c3001ab895c     1        1000000 lovelace + TxOutDatumNone
32ca53e1d9c33b221aff44f57f94b25f4523170912a5fa5ff32cd1c26b72a70d     0        1002000000 lovelace + TxOutDatumNone

>> Query stake pools
pool1vqpavnczv7v2nrlx4l8f0prn4g4hmdyakrpfa8svkgls5clz5ck
pool1s7rnz8hzkeu8fu62enc0x6smn2yzhnufrwxpxz0f4l2dk3vs4kc

>> Query stake distribution
                           PoolId                                 Stake frac
------------------------------------------------------------------------------
pool1vqpavnczv7v2nrlx4l8f0prn4g4hmdyakrpfa8svkgls5clz5ck   1.052e-3
pool1s7rnz8hzkeu8fu62enc0x6smn2yzhnufrwxpxz0f4l2dk3vs4kc   5.258e-4

=====================================================================
=== Mithril Network
=====================================================================

>> Query pending certificate
>> Query snapshots
[
  {
    "digest": "d508794f186c2a6c75ccf71f13fccf37256fd01246b3ec74bcf456ee6ed8dddd",
    "certificate_hash": "a0386153a88a52683fc2bb6f97914221445dee335858b15f023cbadc1166b103",
    "size": 6212,
    "created_at": "2022-06-15T11:36:29.362686957Z",
    "locations": [
      "http://0.0.0.0:8080/aggregator/snapshot/d508794f186c2a6c75ccf71f13fccf37256fd01246b3ec74bcf456ee6ed8dddd/download"
    ]
  }
]
```

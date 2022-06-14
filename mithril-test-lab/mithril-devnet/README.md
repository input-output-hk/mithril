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

## One step run with default configuration

```bash
# Run devnet with 1 BFT node and 2 SPO nodes
./devnet-run.sh

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
│   ├── utxo1.vkey
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
│   ├── start.sh
│   ├── topology.docker.json
│   ├── topology.json
│   └── tx
├── node-pool1
│   ├── byron
│   │   ├── genesis.json
│   │   └── genesis.spec.json
│   ├── configuration.yaml
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
│   ├── start.sh
│   ├── topology.docker.json
│   ├── topology.json
│   └── tx
│       ├── tx1.tx
│       └── tx1.txbody
├── query.sh
├── start.sh
└── stop.sh
```

## Example utxo & stakes informations retrieved from the network

```bash
#Query whole utxo
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
18985163fce6aadfe5f1d0948cca54486bd56056b4c2e6f2efc603f5207090ed     0        447999157 lovelace + TxOutDatumNone
18985163fce6aadfe5f1d0948cca54486bd56056b4c2e6f2efc603f5207090ed     1        2000000 lovelace + TxOutDatumNone
4a03ad5ad34bd2af65cc98d312fcf5c7e488f2a8129d8858fa7a0f4663df6fa7     0        1002000000 lovelace + TxOutDatumNone
a3309948319c3e512857e8ff3d1921564344daf0b1bb2fa46e2c29e0fdc12beb     0        448999157 lovelace + TxOutDatumNone
a3309948319c3e512857e8ff3d1921564344daf0b1bb2fa46e2c29e0fdc12beb     1        1000000 lovelace + TxOutDatumNone

#Query stake pools
pool16p2stm4les4jffzr87dljhelfdqh9cl4ccnkrvp0098xcs20acl
pool1axjrjzexkcec782k0vpqfr0lsrc45fpwdjjm7vjvt94y2plyann

#Query stake distribution
                           PoolId                                 Stake frac
------------------------------------------------------------------------------
pool16p2stm4les4jffzr87dljhelfdqh9cl4ccnkrvp0098xcs20acl   1.052e-3
pool1axjrjzexkcec782k0vpqfr0lsrc45fpwdjjm7vjvt94y2plyann   5.258e-4
```

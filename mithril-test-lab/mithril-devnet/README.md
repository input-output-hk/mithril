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
cd mithril-test-lab/mithril-devnet

# Chmod scripts
chmod u+x *.sh
```

## One step run with default configuration

```bash
# Run devnet with 1 BFT node and 2 SPO nodes (with local docker images)
./devnet-run.sh

# Run devnet with 1 BFT node and 2 SPO nodes (with remote docker images)
MITHRIL_IMAGE_ID=main-c9213ca ./devnet-run.sh

# Run devnet with Cardano nodes only
NODES=cardano ./devnet-run.sh

# Run devnet with Mithril nodes only
NODES=mithril ./devnet-run.sh

# Logs devnet
./devnet-log.sh

# Logs Cardano nodes only on devnet
NODES=cardano ./devnet-log.sh

# Logs Mithril nodes only on devnet
NODES=mithril ./devnet-log.sh

# Query devnet
./devnet-query.sh

# Query Cardano nodes only on devnet
.NODES=cardano /devnet-query.sh

# Query Mithril nodes only on devnet
.NODES=mithril /devnet-query.sh


# Stop devnet
./devnet-stop.sh

# Visualize devnet
./devnet-visualize.sh
```

## One step run with custom configuration

```bash
# Run devnet with 2 BFT nodes and 5 SPO nodes
ROOT=artifacts NUM_BFT_NODES=2 NUM_POOL_NODES=5 ./devnet-run.sh

# Run devnet custom slot length (0.5s) and custom epoch length (120s)
# Slot length: the duration of a Cardano Eslot (can help modulate the immutables creation rate)
# Epoch Length: the duration of a Cardano Epoch
ROOT=artifacts SLOT_LENGTH=0.5 EPOCH_LENGTH=120 ./devnet-run.sh

# Logs devnet
ROOT=artifacts LINES=10 ./devnet-log.sh

# Query devnet
ROOT=artifacts ./devnet-query.sh

# Stop devnet
ROOT=artifacts ./devnet-stop.sh

# Visualize devnet
ROOT=artifacts ./devnet-visualize.sh
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

>> Query pending certificate
{
  "beacon": {
    "network": "devnet",
    "epoch": 0,
    "immutable_file_number": 6
  },
  "protocol": {
    "k": 5,
    "m": 100,
    "phi_f": 0.65
  },
  "signers": [
    {
      "party_id": 0,
      "verification_key": "7b22766b223a5b3138312c35392c3230312c3133362c3138332c3234372c3137392c3134372c3135322c3139302c38352c3133302c3132322c3139342c3234392c32352c32382c35322c3235342c37302c392c3232372c31302c34362c3133302c32392c3231342c3136312c3135362c38362c3232392c392c3133302c3136362c3230372c3137362c3138342c3134352c3132322c3231352c3137322c32392c3235312c3138382c3134312c31382c3139352c3231392c342c39302c372c3137352c3234362c33322c3137332c3131382c302c31332c33322c3135342c39392c3136382c3139302c37362c35312c3131332c31332c3130372c38362c3132312c34332c38392c3134312c3133322c3135392c3131322c3136362c3232352c3233322c3130362c3136362c3136362c3231392c3139312c3230302c36392c37352c3234312c3130332c3234332c312c3132382c3234302c37382c37362c3232385d2c22706f70223a5b3134312c38352c3231372c312c3132322c37312c36302c3137332c3136312c3233372c3231332c3130322c39322c3136352c3134382c3232302c32392c38382c32392c3138302c3230322c37362c3231342c3132312c3138392c3131332c3136332c3234392c3139342c35302c3138332c34302c36322c3137332c31362c31372c39302c3135372c392c3230372c3230382c3232372c3137392c3136372c3230382c3233322c3231342c3135362c3136352c3132372c362c3137362c3138362c3130362c33312c3235312c3135332c37352c36342c35382c35382c36382c3134332c3136312c3231302c31312c36392c3139382c31302c342c31362c3233332c3136302c3134322c3135382c3131322c31302c3133392c3130372c3134382c3230392c33372c3130312c3231322c37392c3131362c37322c3134322c3130332c36372c312c3136332c3234382c3233392c37382c3136315d7d"
    },
    {
      "party_id": 1,
      "verification_key": "7b22766b223a5b3133382c3234322c3130372c3137332c312c3136362c3138392c38312c36302c38332c33312c392c3138322c39322c3131382c35382c3137382c3136322c3132352c36342c3231362c3232322c39352c3130352c3233332c3234322c34362c38342c36362c33362c3137322c3139342c3233342c32362c35302c3133392c37352c32362c37302c32342c3139362c3135372c38362c3137302c31342c39332c3138372c32332c332c36332c3232332c3137342c352c34392c32322c3137392c3134312c36352c3133392c3135332c3136342c39362c3231362c3139392c33392c3138362c39392c3232332c3232392c3133312c39382c3131392c35392c3232322c3137312c3132312c3235312c3231362c33332c3131372c34352c3134322c3130322c3131362c3233332c382c3134352c3137332c3135392c3138352c3131332c3232372c3139362c3131302c35392c35325d2c22706f70223a5b3133392c3131382c35382c3231352c3136352c3139312c3234322c32382c3131312c3134392c3234332c3232312c3137332c3130312c3137312c3232322c3136302c3138312c3134392c38392c3138312c38312c372c3235322c31322c3136332c3135372c3134362c3138392c33322c3234362c3234382c3230322c33332c3231392c3134342c34392c3231302c3130302c3138392c3135302c3230382c32322c3231372c3136302c322c3138342c36392c3134302c3233312c3132332c3138392c3130382c392c3232342c36372c3139302c39322c36372c35372c3130352c34352c32332c33342c3230372c3136392c3231322c3134372c312c3235332c3133302c3130382c3138322c3130342c3139322c3132322c3232322c3235322c3233302c3132352c3231382c34302c3137362c3131372c3131352c38392c3132372c35362c3135392c31372c3139342c3230302c3130362c33342c3232312c3137365d7d"
    }
  ]
}

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

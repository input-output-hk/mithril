---
sidebar_position: 2
---

# Run a private network

:::info

In this guide, you will learn how to run a demonstration of a **Mithril network** working on top of a private Cardano `devnet` network. This network is completely autonomous and set up to produce Mithril snapshots every minute.

:::

You can launch a private Mithril network using the following topology:

- `2` **Cardano nodes** configured as **stake pool operators (SPOs)** with a **Mithril signer** on top
- `1` **Cardano node** configured as a **Full node** with a **Mithril aggregator** on top.

![Devnet Topology](images/devnet-topology.png)

:::danger

This demonstration works exclusively on Linux machines.

:::

:::tip

More information about the private Cardano/Mithril `devnet` is available [here](https://github.com/input-output-hk/mithril/blob/main/mithril-test-lab/mithril-devnet/README.md).

:::

## Video demonstration

<iframe style={{width: '100%', height: '480px'}} src="https://www.youtube.com/embed/qu3GoO1UwYI" title="Run a Private Mithril network" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen="true"></iframe>

## Prerequisites

- Install the latest stable version of a [correctly configured](https://www.rust-lang.org/learn/get-started) Rust toolchain

- Install build tools `build-essential` and `m4`; for example, on Ubuntu/Debian/Mint, run `sudo apt install build-essential m4`

- Install the OpenSSL development libraries; for example, on Ubuntu/Debian/Mint, run `apt install libssl-dev`.

## Download the source file

Download the source file from GitHub (HTTPS):

```bash
git clone https://github.com/input-output-hk/mithril.git
```

Or (SSH):

```bash
git clone git@github.com:input-output-hk/mithril.git
```

## Change the directory

To change the directory, go to the devnet folder:

```bash
cd mithril-test-lab/mithril-devnet
```

## Run a private Mithril/Cardano network (`devnet`) locally

### Step 1: Launch the devnet

Open a terminal window. Run a devnet with one BTF and two SPO Cardano nodes.

**Option 1**: Use remote Docker images

The network setup will be quicker when using remote Docker images:

```bash
MITHRIL_IMAGE_ID=latest FULL_NODES=1 NUM_POOL_NODES=2 ./devnet-run.sh
```

**Option 2**: Use local Docker images

Note that using local Docker images to build Mithril nodes may take more time:

```bash
FULL_NODES=1 NUM_POOL_NODES=2 ./devnet-run.sh
```

:::info

Throughout the devnet launch process, you will encounter the following steps:

- **Bootstrapping the devnet**: generate the artifacts of the devnet, depending on the configuration parameters (cryptographic keys, network topology, transactions to set up pool nodes, etc)
- **Starting the Cardano network**: run the nodes of the **Cardano network**, wait for it to be ready, and activate the pool nodes
- **Starting the Mithril network**: run the nodes of the **Mithril network**, which will work on top of the **Cardano network**.

:::

You should see the following information displayed:

```bash
=====================================================================
 Bootstrap Mithril/Cardano devnet
=====================================================================

>> The artifacts directory was force deleted
>> Devnet Version: 0.4.6
>> Artifacts Directory[env::ARTIFACTS_DIR]: artifacts
>> Cardano Full nodes [env::NUM_FULL_NODES]: 1
>> Cardano SPO nodes [env::NUM_POOL_NODES]: 2
>> Cardano Node Version [env::CARDANO_NODE_VERSION]: 10.1.3
>> Cardano Network Magic [env::NETWORK_MAGIC]: 42
>> Cardano Hard Fork Babbage At Epoch [env::HARD_FORK_BABBAGE_AT_EPOCH]: 0
>> Cardano Hard Fork Conway At Epoch [env::HARD_FORK_CONWAY_AT_EPOCH]: 0
>> Cardano Slot Length [env::SLOT_LENGTH]: 0.75s
>> Cardano Epoch Length [env::EPOCH_LENGTH]: 100s
>> Cardano Listening Address [env::LISTENING_ADDR]: 127.0.0.1
>> Creating artifacts directory...
>> Downloading cardano-cli & cardano-node...
>> Extracting cardano-cli & cardano-node...
generated genesis with: 3 genesis keys, 2 non-delegating UTxO keys, 2 stake pools, 2 delegating UTxO keys, 2 delegation map entries,

=====================================================================
 Start Cardano nodes
=====================================================================

>> Start Cardano network
cardano-cli 10.1.1.0 - linux-x86_64 - ghc-8.10
git rev 01bda2e2cb0a70cd95067d696dbb44665f1d680a
cardano-node 10.1.3 - linux-x86_64 - ghc-8.10
git rev 01bda2e2cb0a70cd95067d696dbb44665f1d680a
>> Starting Cardano node 'node-full1'
>> Starting Cardano node 'node-pool1'
>> Starting Cardano node 'node-pool2'
>> Wait for Cardano network to be ready
>>>> Cardano network is not ready yet... [attempt 1]
>>>> Cardano network is not ready yet... [attempt 2]
>>>> Cardano network is not ready yet... [attempt 3]
>>>> Cardano network is ready!
>> Wait for Cardano nodes to have enough immutable files
>> Wait for node-full1 to have enough immutable files
>>>> node-full1 has not enough immutable files yet... [attempt 1]
>>>> node-full1 has not enough immutable files yet... [attempt 2]
>>>> node-full1 has not enough immutable files yet... [attempt 3]
>>>> node-full1 has not enough immutable files yet... [attempt 4]
>>>> node-full1 has not enough immutable files yet... [attempt 5]
>>>> node-full1 has not enough immutable files yet... [attempt 6]
>>>> node-full1 has not enough immutable files yet... [attempt 7]
>>>> node-full1 has not enough immutable files yet... [attempt 8]
>>>> node-full1 has not enough immutable files yet... [attempt 9]
>>>> node-full1 has not enough immutable files yet... [attempt 10]
>>>> node-full1 has not enough immutable files yet... [attempt 11]
>>>> node-full1 has enough immutable files!
>> Wait for node-pool1 to have enough immutable files
>>>> node-pool1 has enough immutable files!
>> Wait for node-pool2 to have enough immutable files
>>>> node-pool2 has enough immutable files!

>> Info: Mithril Aggregator will be attached to the first Cardano Full node
>> Info: Mithril Signers will be attached to each Cardano SPO node
=====================================================================
 Start Mithril nodes
=====================================================================

>> Start Mithril network
Going to remove artifacts_mithril-aggregator-genesis_run_48d3af826a41
[+] Running 1/0
 ⠿ Container artifacts_mithril-aggregator-genesis_run_48d3af826a41  Removed                                                       0.0s
[+] Running 4/4
 ⠿ Network artifacts_mithril_network                Created                                                                       0.1s
 ⠿ Container artifacts-mithril-signer-node-pool1-1  Started                                                                       0.4s
 ⠿ Container artifacts-mithril-aggregator-1         Started                                                                       0.5s
 ⠿ Container artifacts-mithril-signer-node-pool2-1  Started                                                                       0.5s
>> List of Mithril signers
--------  --------------------------------------------------------  -----------------------------------
Signer 1  pool1vtck0eeqq2x3d5avpxhd904y73syn4l00yurvl0teg7u6nh3afv  Certified PoolId
Signer 2  pool1y3pxhtqytcwy3mmnawqf2ej0x9sz5frkkwkz6scfqmzyyw8u38v  Certified PoolId
>> Wait for Mithril signers to be registered
>>>> Not ready yet
>>>> Not ready yet
>>>> Not ready yet
>>>> Not ready yet
>>>> Not ready yet
>>>> Not ready yet
>>>> Not ready yet
>>>> Not ready yet
>>>> Not ready yet
>>>> Not ready yet
>>>> Not ready yet
>>>> Not ready yet
>>>> Ready!
>> Bootstrap the Genesis certificate
{"msg":"Started","v":0,"name":"mithril-aggregator","level":20,"time":"2024-11-14T10:29:07.953666896Z","hostname":"c993b6b764f2","pid":1,"node_version":"0.5.110+e2fa1e0","run_mode":"dev"}
{Genesis bootstrap for test only!
"msg":"BOOTSTRAP GENESIS command","v":0,"name":"mithril-aggregator","level":20,"time":"2024-11-14T10:29:07.95394937Z","hostname":"c993b6b764f2","pid":1,"config":"Configuration { environment: Production, cardano_cli_path: \"/app/bin/cardano-cli\", cardano_node_socket_path: \"/data/ipc/node.sock\", cardano_node_version: \"10.1.3\", network_magic: Some(42), network: \"devnet\", chain_observer_type: Pallas, protocol_parameters: ProtocolParameters { k: 5, m: 100, phi_f: 0.65 }, snapshot_uploader_type: Local, snapshot_bucket_name: None, snapshot_use_cdn_domain: false, server_ip: \"0.0.0.0\", server_port: 8080, run_interval: 1000, db_directory: \"/data/db\", snapshot_directory: \".\", data_stores_directory: \"/data/mithril/aggregator/stores\", genesis_verification_key: \"5b33322c3235332c3138362c3230312c3137372c31312c3131372c3133352c3138372c3136372c3138312c3138382c32322c35392c3230362c3130352c3233312c3135302c3231352c33302c37382c3231322c37362c31362c3235322c3138302c37322c3133342c3133372c3234372c3136312c36385d\", reset_digests_cache: false, disable_digests_cache: false, store_retention_limit: None, era_reader_adapter_type: Bootstrap, era_reader_adapter_params: None, signed_entity_types: None, snapshot_compression_algorithm: Zstandard, zstandard_parameters: None, cexplorer_pools_url: None, signer_importer_run_interval: 720, allow_unparsable_block: false, cardano_transactions_prover_cache_pool_size: 10, cardano_transactions_database_connection_pool_size: 10, cardano_transactions_signing_config: CardanoTransactionsSigningConfig { security_parameter: BlockNumber(3000), step: BlockNumber(120) }, cardano_transactions_prover_max_hashes_allowed_by_request: 100, cardano_transactions_block_streamer_max_roll_forwards_per_poll: 10000, enable_metrics_server: false, metrics_server_ip: \"0.0.0.0\", metrics_server_port: 9090, persist_usage_report_interval_in_seconds: 10 }"}
{"msg":"Opening SQLite connection","v":0,"name":"mithril-aggregator","level":20,"time":"2024-11-14T10:29:07.954098066Z","hostname":"c993b6b764f2","pid":1,"src":"ConnectionBuilder","path":"/data/mithril/aggregator/stores/aggregator.sqlite3"}
{"msg":"Enabling SQLite Write Ahead Log journal mode","v":0,"name":"mithril-aggregator","level":20,"time":"2024-11-14T10:29:07.954185725Z","hostname":"c993b6b764f2","pid":1,"src":"ConnectionBuilder"}
{"msg":"Enabling SQLite foreign key support","v":0,"name":"mithril-aggregator","level":20,"time":"2024-11-14T10:29:07.954483371Z","hostname":"c993b6b764f2","pid":1,"src":"ConnectionBuilder"}
{"msg":"Applying database migrations","v":0,"name":"mithril-aggregator","level":20,"time":"2024-11-14T10:29:07.954561009Z","hostname":"c993b6b764f2","pid":1,"src":"ConnectionBuilder"}
{"msg":"Check database version","v":0,"name":"mithril-aggregator","level":20,"time":"2024-11-14T10:29:07.954635652Z","hostname":"c993b6b764f2","pid":1,"src":"DatabaseVersionChecker"}
{"msg":"database up to date","v":0,"name":"mithril-aggregator","level":20,"time":"2024-11-14T10:29:07.954704978Z","hostname":"c993b6b764f2","pid":1,"src":"DatabaseVersionChecker"}
{"msg":"New MithrilCertificateVerifier created","v":0,"name":"mithril-aggregator","level":20,"time":"2024-11-14T10:29:07.954756269Z","hostname":"c993b6b764f2","pid":1}
{"msg":"Handle discrepancies at startup of epoch settings store, will record epoch settings from the configuration for epoch 3","v":0,"name":"mithril-aggregator","level":20,"time":"2024-11-14T10:29:07.956326275Z","hostname":"c993b6b764f2","pid":1,"epoch_settings_configuration":"AggregatorEpochSettings { protocol_parameters: ProtocolParameters { k: 5, m: 100, phi_f: 0.65 }, cardano_transactions_signing_config: CardanoTransactionsSigningConfig { security_parameter: BlockNumber(3000), step: BlockNumber(120) } }"}

=====================================================================
 Schedule Cardano Stake Delegation
=====================================================================

>> Begin scheduled stakes delegation
>> 11:29:08: Wait 180s until next stakes delegation round...
```

### Step 2: Query the devnet

Open a second terminal window. Watch the state queried from the devnet:

```bash
watch -n 1 ./devnet-query.sh
```

The networks will be queried every second and will display:

- Certificate production information gathered from the **Mithril network**
- UTXO, stake pools, and stake distribution from the **Cardano network**.

```bash
=====================================================================
 Query Mithril/Cardano devnet
=====================================================================

=====================================================================
=== Mithril Network
=====================================================================

>> Query pending certificate

>> Query latest certificates
[
  {
    "hash": "cbbf3fc1165ff41f2f0691643febe7f8c2d4f99b5c551d01d5f6cca538db0cd4",
    "previous_hash": "3d1ceee23e79b1f510c7a6ce4503552495c6f7134afd04a88042d99264da8515",
    "epoch": 15,
    "signed_entity_type": {
      "CardanoTransactions": [
        15,
        1589
      ]
    },
    "metadata": {
      "network": "devnet",
      "version": "0.1.0",
      "parameters": {
        "k": 5,
        "m": 100,
        "phi_f": 0.65
      },
      "initiated_at": "2024-11-14T10:45:15.434075727Z",
      "sealed_at": "2024-11-14T10:45:16.441653366Z",
      "total_signers": 2
    },
    "protocol_message": {
      "message_parts": {
        "cardano_transactions_merkle_root": "ecfeb3584f505906de1fadcc6f4acaa9fda55da82c771073a83bbb288bb62b6e",
        "next_aggregate_verification_key": "7b226d745f636f6d6d69746d656e74223a7b22726f6f74223a5b3137382c39342c33322c3132392c3132302c3132382c31372c3232352c37332c3135372c3136362c3132342c3234362c3130362c35352c33362c3135372c332c3137372c3231392c35332c3139342c3139322c39342c3133382c36332c3134332c3233312c3230332c3138362c36342c3134335d2c226e725f6c6561766573223a322c22686173686572223a6e756c6c7d2c22746f74616c5f7374616b65223a3230323830313337303631347d",
        "latest_block_number": "1589"
      }
    },
    "signed_message": "3b2242e276f1d1e8f28e5ffeb0f0c666ef3d5a5b2c8bf827576d4a513ab2de29",
    "aggregate_verification_key": "7b226d745f636f6d6d69746d656e74223a7b22726f6f74223a5b3233322c33332c3132362c38392c3134332c3138312c32362c3130362c3131362c37392c3230392c34382c3232302c3136332c3133322c3232302c33312c3136392c3131312c3133382c3130332c39382c37332c37312c3135322c3235352c34352c3138322c39362c32382c3137362c3136355d2c226e725f6c6561766573223a322c22686173686572223a6e756c6c7d2c22746f74616c5f7374616b65223a3139303036353437303735387d"
  },
  {
    "hash": "47068954d8600c214a194be00790a6108c7b397fc91f950f063c7877d8121b92",
    "previous_hash": "3d1ceee23e79b1f510c7a6ce4503552495c6f7134afd04a88042d99264da8515",
    "epoch": 15,
    "signed_entity_type": {
      "CardanoTransactions": [
        15,
        1574
      ]
    },
    "metadata": {
      "network": "devnet",
      "version": "0.1.0",
      "parameters": {
        "k": 5,
        "m": 100,
        "phi_f": 0.65
      },
      "initiated_at": "2024-11-14T10:45:04.382631109Z",
      "sealed_at": "2024-11-14T10:45:05.394615861Z",
      "total_signers": 2
    },
    "protocol_message": {
      "message_parts": {
        "cardano_transactions_merkle_root": "ecfeb3584f505906de1fadcc6f4acaa9fda55da82c771073a83bbb288bb62b6e",
        "next_aggregate_verification_key": "7b226d745f636f6d6d69746d656e74223a7b22726f6f74223a5b3137382c39342c33322c3132392c3132302c3132382c31372c3232352c37332c3135372c3136362c3132342c3234362c3130362c35352c33362c3135372c332c3137372c3231392c35332c3139342c3139322c39342c3133382c36332c3134332c3233312c3230332c3138362c36342c3134335d2c226e725f6c6561766573223a322c22686173686572223a6e756c6c7d2c22746f74616c5f7374616b65223a3230323830313337303631347d",
        "latest_block_number": "1574"
      }
    },
    "signed_message": "ed76b5a8b41e4800fd4babc5bcffe483fe9daf43342aaddefe1f197907f0af06",
    "aggregate_verification_key": "7b226d745f636f6d6d69746d656e74223a7b22726f6f74223a5b3233322c33332c3132362c38392c3134332c3138312c32362c3130362c3131362c37392c3230392c34382c3232302c3136332c3133322c3232302c33312c3136392c3131312c3133382c3130332c39382c37332c37312c3135322c3235352c34352c3138322c39362c32382c3137362c3136355d2c226e725f6c6561766573223a322c22686173686572223a6e756c6c7d2c22746f74616c5f7374616b65223a3139303036353437303735387d"
  }
]

>> Query latest mithril stake distributions
[
  {
    "epoch": 15,
    "hash": "61239e7ea3388238aae9336ccbce50c477ad42478f6fc1642210708118c7ca00",
    "certificate_hash": "3d1ceee23e79b1f510c7a6ce4503552495c6f7134afd04a88042d99264da8515",
    "created_at": "2024-11-14T10:44:11.099191475Z"
  },
  {
    "epoch": 14,
    "hash": "0c1962696593e52c21683287df30f47027ae5d5f669384f5534784e6f51cdd33",
    "certificate_hash": "fa6fc82a1a2bf93a81f5920f1d0ac143aa73183b4b8226a54f3e16741f3705a1",
    "created_at": "2024-11-14T10:42:55.567493973Z"
  }
]

>> Query latest snapshots
[
  {
    "digest": "4c7b06dd2bef1416391b92a46dae7d2f606ced2954b628f844b021ba5b52b15f",
    "network": "devnet",
    "beacon": {
      "network": "devnet",
      "epoch": 15,
      "immutable_file_number": 14
    },
    "certificate_hash": "f601ab107b546d8791075916b9fd6ba374b30a7ce6030549e140bac04078b3b6",
    "size": 519351,
    "created_at": "2024-11-14T10:44:17.153390171Z",
    "locations": [
      "http://0.0.0.0:8080/aggregator/artifact/snapshot/4c7b06dd2bef1416391b92a46dae7d2f606ced2954b628f844b021ba5b52b15f/download"
    ],
    "compression_algorithm": "zstandard",
    "cardano_node_version": "10.1.3"
  },
  {
    "digest": "b98b25f505401e967df1012a4c13385290db15d157d0292e9f8290bd9933a66e",
    "network": "devnet",
    "beacon": {
      "network": "devnet",
      "epoch": 14,
      "immutable_file_number": 13
    },
    "certificate_hash": "1ec38ed6f158664bf792fb2fddd08e49b52232023b4130b3ebd92e90d659f200",
    "size": 469807,
    "created_at": "2024-11-14T10:43:01.620428990Z",
    "locations": [
      "http://0.0.0.0:8080/aggregator/artifact/snapshot/b98b25f505401e967df1012a4c13385290db15d157d0292e9f8290bd9933a66e/download"
    ],
    "compression_algorithm": "zstandard",
    "cardano_node_version": "10.1.3"
  }
]


=====================================================================
=== Cardano Network
=====================================================================

>> Query chain tip
{
  "block": 1599,
  "epoch": 15,
  "era": "Conway",
  "hash": "1d46bbd5179968366568822961f8df9d0e0ea0d74bcf1b371402ddeee315ea41",
  "slot": 1599,
  "slotInEpoch": 99,
  "slotsToEpochEnd": 1,
  "syncProgress": "100.00"
}

>> Query whole utxo
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
0227b08e4971e69af83e4f7f37f9b6db11a4e5b0293259bcbc18b960cca2aa34     0        2000003 lovelace + TxOutDatumNone
07ccbb09c88f1789512879adc5c46b70f746d27fc77a938d879b0235ffd06e10     0        20000000001 lovelace + TxOutDatumNone
08823e13632b63dcd2eaf6f064c960b94f1d51652dc218e306305ee13fec5d9f     0        2000002 lovelace + TxOutDatumNone
099b200f28beedd811298150f55f96e3f9c100975dd1b978040e8cc667d44093     0        1000001 lovelace + TxOutDatumNone
13c36c25cbd8afb893813f996907ab13f9f89fd46b5c9a79710996869311ae83     0        6000000 lovelace + TxOutDatumNone
22d4daa27b05d1045f788ae981297ec6038c0856c974f8042be7209b8e6d94db     0        1000002 lovelace + TxOutDatumNone
46e00ecedaa75c64bf23b9c4d95b74e1432570feb800ecb0ab5eb1de05a6e4f1     0        2000004 lovelace + TxOutDatumNone
4a1c33bb474d27398bcd2d79ffca99e675850cba01655a7a7fc1110b3a0b85e3     0        1000003 lovelace + TxOutDatumNone
993b375acdd9f28ce1e7eab5a870d382d090b6ec1ce3372581c7c8b03e356d3b     0        2000001 lovelace + TxOutDatumNone
b9b28ace208bd3fb5feda08e6978ea274be8c2f8d8852ac8ea08dd27cd99133e     0        6000000 lovelace + TxOutDatumNone
bf208279833e96f02cf5d91805898b40784a9516f7f122edcf1fb2c773019861     0        1000005 lovelace + TxOutDatumNone
bf208279833e96f02cf5d91805898b40784a9516f7f122edcf1fb2c773019861     1        899993679904 lovelace + TxOutDatumNone
e0e68233e335f20b319d220a4e3e0fbca8f4b0c493a2ba216fa81a17520de4a4     0        20000000001 lovelace + TxOutDatumNone
e9ca751d4fc6bd00203d36184de814f6687f017ec6a4c6e481e1e188208c8b1f     0        2000005 lovelace + TxOutDatumNone
e9ca751d4fc6bd00203d36184de814f6687f017ec6a4c6e481e1e188208c8b1f     1        899988679904 lovelace + TxOutDatumNone
f5f61cdd76949003172e85941f4aae85ce99764b4a66e378dfb1e1f7fd1abde7     0        1000004 lovelace + TxOutDatumNone

>> Query stake pools
pool1y3pxhtqytcwy3mmnawqf2ej0x9sz5frkkwkz6scfqmzyyw8u38v
pool1vtck0eeqq2x3d5avpxhd904y73syn4l00yurvl0teg7u6nh3afv

>> Query stake distribution
{
  "pools": {
    "24426bac045e1c48ef73eb8095664f31602a2476b3ac2d430906c442": {
      "stakeGo": 96954320166,
      "stakeMark": 110102978737,
      "stakeSet": 103772355610
    },
    "62f167e720028d16d3ac09aed2bea4f46049d7ef7938367debca3dcd": {
      "stakeGo": 93111150592,
      "stakeMark": 105309514088,
      "stakeSet": 99029015004
    }
  },
  "total": {
    "stakeGo": 190065470758,
    "stakeMark": 215412492825,
    "stakeSet": 202801370614
  }
}
```

### Step 3: Observe the devnet

Open a third terminal window. Watch the logs of each devnet node:

```bash
watch -n 1 `LINES=5 ./devnet-log.sh`
```

The nodes will be queried every second and will display thus:

```bash
=====================================================================
 Logs Mithril/Cardano devnet
=====================================================================

=====================================================================
=== Mithril Network
=====================================================================

---------------------------------------------------------------------
docker logs -n 5 artifacts-mithril-aggregator-1
---------------------------------------------------------------------
{"msg":"Marked expired open messages","v":0,"name":"mithril-aggregator","level":20,"time":"2024-11-14T10:45:00.368456608Z","hostname":"bd8372c8c9d5","pid":1,"src":"AggregatorRunner","expired_open_message":"None"}
{"msg":">> get_open_message(signed_entity_type: CardanoTransactions(Epoch(15), BlockNumber(1559)))","v":0,"name":"mithril-aggregator","level":20,"time":"2024-11-14T10:45:00.368471645Z","hostname":"bd8372c8c9d5","pid":1,"src":"MithrilCertifierService"}
{"msg":" ⋅ No open message to certify, waiting…","v":0,"name":"mithril-aggregator","level":30,"time":"2024-11-14T10:45:00.36863267Z","hostname":"bd8372c8c9d5","pid":1,"src":"AggregatorRuntime","time_point":"TimePoint { epoch: Epoch(15), immutable_file_number: 14, chain_point: ChainPoint { slot_number: SlotNumber(1569), block_number: BlockNumber(1569), block_hash: \"6aadd9a1c21c53f79b703cabfc2a49032dcde2464b94ab578719b6aadb222f23\" } }"}
{"msg":"Incrementing 'mithril_aggregator_runtime_cycle_success_since_startup' counter","v":0,"name":"mithril-aggregator","level":20,"time":"2024-11-14T10:45:00.368647673Z","hostname":"bd8372c8c9d5","pid":1,"src":"MetricsService"}
{"msg":"… Cycle finished, Sleeping for 1000 ms","v":0,"name":"mithril-aggregator","level":30,"time":"2024-11-14T10:45:00.368664195Z","hostname":"bd8372c8c9d5","pid":1,"src":"AggregatorRuntime"}
---------------------------------------------------------------------

---------------------------------------------------------------------
docker logs -n 5 artifacts-mithril-signer-node-pool1-1
---------------------------------------------------------------------
{"msg":">> get_current_time_point","v":0,"name":"mithril-signer","level":20,"time":"2024-11-14T10:45:00.5408476Z","hostname":"143e20c318ed","pid":1,"src":"SignerRunner"}
{"msg":">> get_beacon_to_sign(time_point: TimePoint (epoch: 15, immutable_file_number: 14, chain_point: ChainPoint (slot_number: 1571, block_number: 1571, block_hash: 50640d2cbcc7a36cda9bb42b127769448b3799b95613ebcc021b0b416b793942)))","v":0,"name":"mithril-signer","level":20,"time":"2024-11-14T10:45:00.541841221Z","hostname":"143e20c318ed","pid":1,"src":"SignerRunner"}
{"msg":" ⋅ No beacon to sign, waiting…","v":0,"name":"mithril-signer","level":30,"time":"2024-11-14T10:45:00.541994982Z","hostname":"143e20c318ed","pid":1,"src":"StateMachine"}
{"msg":"Incrementing 'mithril_signer_runtime_cycle_success_since_startup' counter","v":0,"name":"mithril-signer","level":20,"time":"2024-11-14T10:45:00.542006483Z","hostname":"143e20c318ed","pid":1,"src":"MetricsService"}
{"msg":"… Cycle finished, Sleeping for 700 ms","v":0,"name":"mithril-signer","level":30,"time":"2024-11-14T10:45:00.542022062Z","hostname":"143e20c318ed","pid":1,"src":"StateMachine"}
---------------------------------------------------------------------

---------------------------------------------------------------------
docker logs -n 5 artifacts-mithril-signer-node-pool2-1
---------------------------------------------------------------------
{"msg":">> get_current_time_point","v":0,"name":"mithril-signer","level":20,"time":"2024-11-14T10:45:00.526442506Z","hostname":"b675f9c9de52","pid":1,"src":"SignerRunner"}
{"msg":">> get_beacon_to_sign(time_point: TimePoint (epoch: 15, immutable_file_number: 14, chain_point: ChainPoint (slot_number: 1571, block_number: 1571, block_hash: 50640d2cbcc7a36cda9bb42b127769448b3799b95613ebcc021b0b416b793942)))","v":0,"name":"mithril-signer","level":20,"time":"2024-11-14T10:45:00.527386742Z","hostname":"b675f9c9de52","pid":1,"src":"SignerRunner"}
{"msg":" ⋅ No beacon to sign, waiting…","v":0,"name":"mithril-signer","level":30,"time":"2024-11-14T10:45:00.527529799Z","hostname":"b675f9c9de52","pid":1,"src":"StateMachine"}
{"msg":"Incrementing 'mithril_signer_runtime_cycle_success_since_startup' counter","v":0,"name":"mithril-signer","level":20,"time":"2024-11-14T10:45:00.527539103Z","hostname":"b675f9c9de52","pid":1,"src":"MetricsService"}
{"msg":"… Cycle finished, Sleeping for 700 ms","v":0,"name":"mithril-signer","level":30,"time":"2024-11-14T10:45:00.527547906Z","hostname":"b675f9c9de52","pid":1,"src":"StateMachine"}
---------------------------------------------------------------------


=====================================================================
=== Cardano Network
=====================================================================

---------------------------------------------------------------------
tail -n 5 ./node-full1/node.log
---------------------------------------------------------------------
[denis-XP:cardano.node.LocalErrorPolicy:Error:63] [2024-11-14 10:45:00.36 UTC] IP LocalAddress "node-full1/ipc/node.sock@2660" ErrorPolicyUnhandledApplicationException (MuxError MuxBearerClosed "<socket: 28> closed when reading data, waiting on next header True")
[denis-XP:cardano.node.LocalErrorPolicy:Error:63] [2024-11-14 10:45:00.36 UTC] IP LocalAddress "node-full1/ipc/node.sock@2659" ErrorPolicyUnhandledApplicationException (MuxError MuxBearerClosed "<socket: 27> closed when reading data, waiting on next header True")
[denis-XP:cardano.node.ChainDB:Notice:23] [2024-11-14 10:45:01.00 UTC] Chain extended, new tip: d710c33fa606ae604b9de61eb10e83cd937ef4ab33cfb5afdce42e782b752a0a at slot 1572
[denis-XP:cardano.node.Mempool:Info:31] [2024-11-14 10:45:01.00 UTC] fromList [("enclosingTime",Object (fromList [("tag",String "RisingEdge")])),("kind",String "TraceMempoolSynced")]
[denis-XP:cardano.node.Mempool:Info:31] [2024-11-14 10:45:01.00 UTC] fromList [("enclosingTime",Object (fromList [("contents",Number 6.8471e-5),("tag",String "FallingEdgeWith")])),("kind",String "TraceMempoolSynced")]
---------------------------------------------------------------------

---------------------------------------------------------------------
tail -n 5 ./node-pool1/node.log
---------------------------------------------------------------------
[denis-XP:cardano.node.LocalErrorPolicy:Error:64] [2024-11-14 10:45:00.83 UTC] IP LocalAddress "node-pool1/ipc/node.sock@3353" ErrorPolicyUnhandledApplicationException (MuxError MuxBearerClosed "<socket: 31> closed when reading data, waiting on next header True")
[denis-XP:cardano.node.LocalErrorPolicy:Error:64] [2024-11-14 10:45:00.83 UTC] IP LocalAddress "node-pool1/ipc/node.sock@3352" ErrorPolicyUnhandledApplicationException (MuxError MuxBearerClosed "<socket: 30> closed when reading data, waiting on next header True")
[denis-XP:cardano.node.ChainDB:Notice:22] [2024-11-14 10:45:01.00 UTC] Chain extended, new tip: d710c33fa606ae604b9de61eb10e83cd937ef4ab33cfb5afdce42e782b752a0a at slot 1572
[denis-XP:cardano.node.Mempool:Info:30] [2024-11-14 10:45:01.00 UTC] fromList [("enclosingTime",Object (fromList [("tag",String "RisingEdge")])),("kind",String "TraceMempoolSynced")]
[denis-XP:cardano.node.Mempool:Info:30] [2024-11-14 10:45:01.00 UTC] fromList [("enclosingTime",Object (fromList [("contents",Number 6.2125e-5),("tag",String "FallingEdgeWith")])),("kind",String "TraceMempoolSynced")]
---------------------------------------------------------------------

---------------------------------------------------------------------
tail -n 5 ./node-pool2/node.log
---------------------------------------------------------------------
[denis-XP:cardano.node.Mempool:Info:30] [2024-11-14 10:45:01.00 UTC] fromList [("enclosingTime",Object (fromList [("contents",Number 5.1172e-5),("tag",String "FallingEdgeWith")])),("kind",String "TraceMempoolSynced")]
[denis-XP:cardano.node.ChainDB:Info:22] [2024-11-14 10:45:01.00 UTC] Block fits onto some fork: d710c33fa606ae604b9de61eb10e83cd937ef4ab33cfb5afdce42e782b752a0a at slot 1572
[denis-XP:cardano.node.ChainDB:Notice:22] [2024-11-14 10:45:01.00 UTC] Switched to a fork, new tip: d710c33fa606ae604b9de61eb10e83cd937ef4ab33cfb5afdce42e782b752a0a at slot 1572
[denis-XP:cardano.node.Mempool:Info:30] [2024-11-14 10:45:01.00 UTC] fromList [("enclosingTime",Object (fromList [("tag",String "RisingEdge")])),("kind",String "TraceMempoolSynced")]
[denis-XP:cardano.node.Mempool:Info:30] [2024-11-14 10:45:01.00 UTC] fromList [("enclosingTime",Object (fromList [("contents",Number 4.089e-5),("tag",String "FallingEdgeWith")])),("kind",String "TraceMempoolSynced")]
---------------------------------------------------------------------

```

## Interact with the Mithril aggregator by using the Mithril client

### Step 1: Prepare some useful variables

```bash
# Aggregator API endpoint URL
export AGGREGATOR_ENDPOINT=http://localhost:8080/aggregator

# Digest of the latest produced cardano db snapshot for convenience of the demo
# You can also modify this variable and set it to the value of the digest of a cardano db snapshot that you can retrieve at step 2
export SNAPSHOT_DIGEST=latest
```

You can pick an online test aggregator directly from the [Mithril Explorer](https://mithril.network/explorer).

### Step 2: Select a Cardano DB snapshot

List the available snapshots with which you can bootstrap a Cardano node:

```bash
AGGREGATOR_ENDPOINT=$AGGREGATOR_ENDPOINT ./mithril-client cardano-db snapshot list
```

You will see a list of snapshots:

```bash
+-------+-----------+---------+------------------------------------------------------------------+------------+-----------+-----------------------------------+
| Epoch | Immutable | Network | Digest                                                           |       Size | Locations |                           Created |
+-------+-----------+---------+------------------------------------------------------------------+------------+-----------+-----------------------------------+
| 539   | 10787     | preview | db5f50a060d4b813125c4263b700ecc96e5d8c8710f0430e5c80d2f0fa79b667 | 2323485648 | 1         | 2024-04-16 12:56:22.170174972 UTC |
+-------+-----------+---------+------------------------------------------------------------------+------------+-----------+-----------------------------------+
| 539   | 10786     | preview | 6af5dac31e7697c4481426712742f4d6391aea0a5b1df145e08e9eaa105af4a5 | 2323875790 | 1         | 2024-04-16 11:44:25.583804349 UTC |
+-------+-----------+---------+------------------------------------------------------------------+------------+-----------+-----------------------------------+
| 539   | 10785     | preview | 39770647f027a214ac955668dffe4d6d51b9cf67798041de1b003b21ef2208da | 2323295044 | 1         | 2024-04-16 10:31:26.056746652 UTC |
+-------+-----------+---------+------------------------------------------------------------------+------------+-----------+-----------------------------------+
| 539   | 10784     | preview | 9ce64187cb6af25266563e039e8d15962d281482979df94e3ac5c5ca6a914eea | 2323079205 | 1         | 2024-04-16 09:08:14.605224999 UTC |
+-------+-----------+---------+------------------------------------------------------------------+------------+-----------+-----------------------------------+

```

### Step 3: Show Cardano DB snapshot details

To get more details from a specific snapshot (optional), run:

```bash
AGGREGATOR_ENDPOINT=$AGGREGATOR_ENDPOINT ./mithril-client cardano-db snapshot show $SNAPSHOT_DIGEST
```

You will see more information about the snapshot:

```bash
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Epoch                 | 539                                                                                                                                                                            |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Immutable File Number | 10787                                                                                                                                                                          |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Network               | preview                                                                                                                                                                        |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Digest                | db5f50a060d4b813125c4263b700ecc96e5d8c8710f0430e5c80d2f0fa79b667                                                                                                               |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Size                  | 2323485648                                                                                                                                                                     |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Cardano node version  | 10.1.3                                                                                                                                                                          |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Location              | https://storage.googleapis.com/cdn.aggregator.testing-preview.api.mithril.network/preview-e539-i10787.db5f50a060d4b813125c4263b700ecc96e5d8c8710f0430e5c80d2f0fa79b667.tar.zst |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Created               | 2024-04-16 12:56:22.170174972 UTC                                                                                                                                              |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
| Compression Algorithm | Zstandard                                                                                                                                                                      |
+-----------------------+--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------+
```

### Step 4: Download and verify the selected Cardano DB snapshot

To download the selected snapshot from the remote location to your remote location, run:

```bash
AGGREGATOR_ENDPOINT=$AGGREGATOR_ENDPOINT ./mithril-client cardano-db download $SNAPSHOT_DIGEST
```

You will see that the certificate chain is validated to ensure the issued certificate is genuine and then the selected snapshot archive is downloaded, unpacked and verified against the corresponding certificate.

```bash
1/5 - Checking local disk info…
2/5 - Fetching the certificate and verifying the certificate chain…
3/5 - Downloading and unpacking the Cardano db
4/5 - Computing the Cardano db message
5/5 - Verifying the cardano db signature…
Cardano db 'db5f50a060d4b813125c4263b700ecc96e5d8c8710f0430e5c80d2f0fa79b667' has been unpacked and successfully checked against Mithril multi-signature contained in the certificate.

    Files in the directory '/home/mithril/data/testnet/db5f50a060d4b813125c4263b700ecc96e5d8c8710f0430e5c80d2f0fa79b667/db' can be used to run a Cardano node with version >= 10.1.3.

    If you are using Cardano Docker image, you can restore a Cardano Node with:

    docker run -v cardano-node-ipc:/ipc -v cardano-node-data:/data --mount type=bind,source="/home/mithril/data/testnet/db5f50a060d4b813125c4263b700ecc96e5d8c8710f0430e5c80d2f0fa79b667/db",target=/data/db/ -e NETWORK=preview ghcr.io/intersectmbo/cardano-node:10.1.3
```

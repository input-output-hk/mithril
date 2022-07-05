---
sidebar_position: 1
---

# Bootstrap a Cardano Node

:::info

Thanks to a **Mithril Client** connected to a **Mithril Aggregator**, you will restore a full Cardano node in less than 2 hours!

:::

## Download source

```bash
# Download from Github (HTTPS)
$ git clone https://github.com/input-output-hk/mithril.git

# or (SSH)
$ git clone git@github.com:input-output-hk/mithril.git
```

## Build Mithril Client binary

```bash
# Change directory
$ cd mithril/mithril-client

# Run tests (Optional)
$ make test

# Build executable
$ make build
```

## Verify build

```bash
# Check that the Mithril Client binary is working fine by running its help
$ ./mithril-client help

mithril-client 
An implementation of a Mithril Client

USAGE:
    mithril-client [OPTIONS] <SUBCOMMAND>

OPTIONS:
    -h, --help                   Print help information
    -q, --quiet                  Less output per occurrence
    -r, --run-mode <RUN_MODE>    Run Mode [default: dev]
    -v, --verbose                More output per occurrence

SUBCOMMANDS:
    download    Download a snapshot
    help        Print this message or the help of the given subcommand(s)
    list        List available snapshots
    restore     Restore a snapshot
    show        Infos about a snapshot
```

:::tip

If you want to dig deeper, you can get access to several level of logs from the Mithril Client:

* Add `-v` for some logs (WARN)
* Add `-vv` for more logs (INFO)
* Add `-vvv` for even more logs (DEBUG)
* Add `-vvvv` for all logs (TRACE)

:::

## Bootstrap a Cardano node from a testnet Mithril snapshot

### Step 1: Prepare some useful variables

```bash
# Cardano network
$ NETWORK=testnet

# Aggregator API endpoint URL
$ AGGREGATOR_ENDPOINT=http://aggregator.api.mithril.network/aggregator

# Digest of the latest produced snapshot for convenience of the demo
# You can also modify this variable and set it to the value of the digest of a snapshot that you can retrieve at step 2
$ SNAPSHOT_DIGEST=$(curl -s $AGGREGATOR_ENDPOINT | jq -r '.[0].digest')
```

### Step 2: Select A Snapshot

```bash
# List the available snapshots with which you can bootstrap a Cardano node
$ NETWORK=$NETWORK AGGREGATOR_ENDPOINT=$AGGREGATOR_ENDPOINT ./mithril-client list

+---------+------------------------------------------------------------------+-------+-----------+--------------------------------+
| Network | Digest                                                           | Size  | Locations | Created                        |
+---------+------------------------------------------------------------------+-------+-----------+--------------------------------+
| testnet | 85f09b39b0b5a13cec9d8fe7ffb82b5e5f236f02ae896f4e47b77e5cd1f2a917 | 11808 |         1 | 2022-07-04T16:47:00.258482685Z |
+---------+------------------------------------------------------------------+-------+-----------+--------------------------------+
| testnet | 60d9c6e014d22335b34f55f83da728667f04fc1c63152ccff0bce7d217d08447 | 10793 |         1 | 2022-07-04T16:46:45.069646321Z |
+---------+------------------------------------------------------------------+-------+-----------+--------------------------------+
| testnet | a3c4bb5f413f1b9648f0a086b3752d25ec62b540b8390917a5a7e78809896d92 |  7991 |         1 | 2022-07-04T16:46:09.817821220Z |
+---------+------------------------------------------------------------------+-------+-----------+--------------------------------+
| testnet | b952adaa04dbb42206c69589b9951660f40c7262b088b13434b7a446ec90bc36 |  6746 |         1 | 2022-07-04T16:45:49.616260734Z |
+---------+------------------------------------------------------------------+-------+-----------+--------------------------------+
| testnet | 46425fdcfe89ad5ba41a7822a4395e21b539e80c20e2b10546017b14cdcd4e4b |  6196 |         1 | 2022-07-04T16:45:29.425195132Z |
+---------+------------------------------------------------------------------+-------+-----------+--------------------------------+
```

### Step 3: Show Snapshot Details

```bash
# GEt some more details from a specific snapshot (Optional)
$ NETWORK=$NETWORK AGGREGATOR_ENDPOINT=$AGGREGATOR_ENDPOINT ./mithril-client show $SNAPSHOT_DIGEST

+------------+-------------------------------------------------------------------------------------------------------------------+
| Info       | Value                                                                                                             |
+------------+-------------------------------------------------------------------------------------------------------------------+
| Network    | testnet                                                                                                           |
+------------+-------------------------------------------------------------------------------------------------------------------+
| Digest     | 85f09b39b0b5a13cec9d8fe7ffb82b5e5f236f02ae896f4e47b77e5cd1f2a917                                                  |
+------------+-------------------------------------------------------------------------------------------------------------------+
| Size       | 11808                                                                                                             |
+------------+-------------------------------------------------------------------------------------------------------------------+
| Location 1 | http://0.0.0.0:8080/aggregator/snapshot/85f09b39b0b5a13cec9d8fe7ffb82b5e5f236f02ae896f4e47b77e5cd1f2a917/download |
+------------+-------------------------------------------------------------------------------------------------------------------+
| Created    | 2022-07-04T16:47:00.258482685Z                                                                                    |
+------------+-------------------------------------------------------------------------------------------------------------------+
```

### Step 4: Download Selected Snapshot

```bash
# Download the selected snapshot from the remote location to your remote location
$ NETWORK=$NETWORK AGGREGATOR_ENDPOINT=$AGGREGATOR_ENDPOINT ./mithril-client download $SNAPSHOT_DIGEST

Download success 85f09b39b0b5a13cec9d8fe7ffb82b5e5f236f02ae896f4e47b77e5cd1f2a917 #1
from http://0.0.0.0:8080/aggregator/snapshot/85f09b39b0b5a13cec9d8fe7ffb82b5e5f236f02ae896f4e47b77e5cd1f2a917/download
to /home/jp/Works/Cardano/Mithril/mithril/mithril-client/data/testnet/85f09b39b0b5a13cec9d8fe7ffb82b5e5f236f02ae896f4e47b77e5cd1f2a917/snapshot.archive.tar.gz
```

### Step 5: Restore Selected Snapshot

```bash
# Verify the Certificate of the snapshot and unpack its content in order to feed the Cardano node database
$ NETWORK=$NETWORK AGGREGATOR_ENDPOINT=$AGGREGATOR_ENDPOINT ./mithril-client restore $SNAPSHOT_DIGEST

Unpacking snapshot...
Unpack success 85f09b39b0b5a13cec9d8fe7ffb82b5e5f236f02ae896f4e47b77e5cd1f2a917
to /home/jp/Works/Cardano/Mithril/mithril/mithril-client/data/testnet/85f09b39b0b5a13cec9d8fe7ffb82b5e5f236f02ae896f4e47b77e5cd1f2a917/db

Restore a Cardano Node with:

docker run -v cardano-node-ipc:/ipc -v cardano-node-data:/data --mount type=bind,source="./data/testnet/85f09b39b0b5a13cec9d8fe7ffb82b5e5f236f02ae896f4e47b77e5cd1f2a917/db",target=/data/db/ -e NETWORK=testnet inputoutput/cardano-node
```

### Step 6: Launch a Cardano Node From Restored Snapshot

```bash
# Launch an empty Cardano node and make it live in minutes!
$ docker run -v cardano-node-ipc:/ipc -v cardano-node-data:/data --mount type=bind,source="$(pwd)/data/testnet/$SNAPSHOT_DIGEST/db",target=/data/db/ -e NETWORK=testnet inputoutput/cardano-node
```

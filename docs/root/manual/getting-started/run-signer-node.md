---
sidebar_position: 2
---

# Run a Mithril Signer node (SPO)

:::info

In this guide, you will learn how to setup a **Mithril Signer** on top of a **Cardano SPO Node** for the `testnet`.

:::

:::tip

The Mithril test networks are:

* `preview`: Test network with magic id `2`, implemented on the IOG hosted Mithril Aggregator
* `preprod`: Test network with magic id `1`, not implemented yet on the IOG hosted Mithril Aggregator
* `testnet`: Legacy test network with magic id `1097911063`, used to be on the IOG hosted Mithril Aggregator, now deprecated

In this documentation, we use the generic `testnet` identifier, but you need to replace it with the identifier of the network that runs on your Cardano node

:::

:::danger

This guide is working only on a Linux machine.

:::

:::tip

For more information about the **Mithril Protocol**, please refer to the [About Mithril](../../mithril/intro.md) section.

:::

## Pre-requisites

## What you'll need

* Operating a **Cardano Node** as a **Stake Pool**:
  * The `Pool Id` in a `BECH32` format such as `pool1frevxe70aqw2ce58c0muyesnahl88nfjjsp25h85jwakzgd2g2l`

* Access to the file system of a `relay` **Cardano Node** running on the `testnet`:
  * Read rights on the `Database` folder (`--database-path` setting of the **Cardano Node**)
  * Read/Write rights on the `Inter Process Communication` file (usually `CARDANO_NODE_SOCKET_PATH` env var used to launch the **Cardano Node**)

* Install a recent version of the [`cardano-cli`](https://hydra.iohk.io/job/Cardano/cardano-node/linux.native.cardano-cli) (version 1.35.3+)

* Install a [correctly configured](https://www.rust-lang.org/learn/get-started) Rust toolchain (version 1.62.0+).

* Install OpenSSL development libraries, for example on Ubuntu/Debian/Mint run `apt install libssl-dev`

:::danger

This document is subject to change as some cryptographic security will be added soon.
At this point, secret keys from the Cardano Node will be needed as well.

:::

## Building your own executable

### Download source

Download from Github (HTTPS)

```bash
git clone https://github.com/input-output-hk/mithril.git
```

Or (SSH)

```bash
git clone git@github.com:input-output-hk/mithril.git
```

### Build Mithril Signer binary

Change directory

```bash
cd mithril/mithril-signer
```

Run tests (Optional)

```bash
make test
```

Build executable

```bash
make build
```

### Verify build

Check that the Mithril Signer binary is working fine by running its help

```bash
./mithril-signer -h
```

You should see

```bash
mithril-signer 
An implementation of a Mithril Signer

USAGE:
    mithril-signer [OPTIONS]

OPTIONS:
    -h, --help                   Print help information
    -r, --run-mode <RUN_MODE>    Run Mode [default: dev]
    -v, --verbose                Verbosity level
```

:::tip

If you want to dig deeper, you can get access to several level of logs from the Mithril Signer:

* Add `-v` for some logs (WARN)
* Add `-vv` for more logs (INFO)
* Add `-vvv` for even more logs (DEBUG)
* Add `-vvvv` for all logs (TRACE)

:::

### Move executable

Move executable to /opt/mithril

```bash
sudo mkdir -p /opt/mithril
sudo mv mithril-signer /opt/mithril
```

### Setup the service

:::caution

* `User=cardano`:
Replace this value with the correct user. We assume that the user used to run the **Cardano Node** is `cardano`. The **Mithril Signer** must imperatively run with the same user.

* in the `/opt/mithril/mithril-signer/service.env` env file:
  * `PARTY_ID=YOUR_POOL_ID_BECH32`: replace `YOUR_POOL_ID_BECH32` with your BECH32 `Pool Id`
  * `DB_DIRECTORY=/cardano/db`: replace `/cardano/db` with the path to the database folder of the **Cardano Node** (the one in `--database-path`)
  * `CARDANO_NODE_SOCKET_PATH=/cardano/ipc/node.socket`: replace with the path to the IPC file (`CARDANO_NODE_SOCKET_PATH` env var)
  * `CARDANO_CLI_PATH=/app/bin/cardano-cli`: replace with the path to the `cardano-cli` executable
  * `DATA_STORES_DIRECTORY=/opt/mithril/mithril-signer/stores`: replace with the path to a folder where the **Mithril Signer** will store its data (`/opt/mithril/mithril-signer/stores` e.g.)

:::

First create an env file that will be used by the service

```bash
sudo cat > /opt/mithril/mithril-signer.env << EOF
PARTY_ID=YOUR_POOL_ID_BECH32
NETWORK=testnet
AGGREGATOR_ENDPOINT=https://aggregator.api.mithril.network/aggregator
RUN_INTERVAL=60000 DB_DIRECTORY=/cardano/db
CARDANO_NODE_SOCKET_PATH=/cardano/ipc/node.socket
CARDANO_CLI_PATH=/app/bin/cardano-cli
DATA_STORES_DIRECTORY=/opt/mithril/mithril-signer/stores
EOF
```

Then we will create a `/etc/systemd/system/mithril-signer.service` description file for our service

```bash
sudo cat > /etc/systemd/system/mithril-signer.service << EOF
[Unit]
Description=Mithril Signer service
StartLimitIntervalSec=0

[Service]
Type=simple
Restart=always
RestartSec=1
User=cardano
EnvironmentFile=/opt/mithril/mithril-signer.env
ExecStart=/opt/mithril/mithril-signer -vvv

[Install]
WantedBy=multi-user.target
EOF
```

Reload the service configuration (Optional)

```bash
sudo systemctl daemon-reload
```

Then start the service

```bash
sudo systemctl start mithril-signer
```

Then register the service to start on boot

```bash
sudo systemctl enable mithril-signer
```

Then monitor status of the service

```bash
systemctl status mithril-signer.service
```

And monitor the logs of the service

```bash
tail /var/log/syslog
```

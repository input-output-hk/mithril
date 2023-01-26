---
sidebar_position: 2
---

import NetworksMatrix from '../../networks-matrix.md';
import CompiledBinaries from '../../compiled-binaries.md'

# Run a Mithril Signer node (SPO)

:::info

In this guide, you will learn how to setup a **Mithril Signer** on top of a **Cardano SPO Node** for the `testnet`.

:::

:::note Mithril Networks

<NetworksMatrix />

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
  * The Cardano `Operational Certificate` file of the pool
  * The Cardano `KES Secret Key` file of the pool

* Access to the file system of a `relay` **Cardano Node** running on the `testnet`:
  * Read rights on the `Database` folder (`--database-path` setting of the **Cardano Node**)
  * Read/Write rights on the `Inter Process Communication` file (usually `CARDANO_NODE_SOCKET_PATH` env var used to launch the **Cardano Node**)

* Install a recent version of the [`cardano-cli`](https://github.com/input-output-hk/cardano-node/releases/tag/1.35.4) (version 1.35.4+)

* Install a [correctly configured](https://www.rust-lang.org/learn/get-started) Rust toolchain (latest stable version).

* Install OpenSSL development libraries, for example on Ubuntu/Debian/Mint run `apt install libssl-dev`

* Ensure the SQLite3 version is at lease `3.35` (released Apr. 2021)

## Mithril Keys Certification


### Certify your Pool Id

You must declare your Cardano `Operational Certificate` file and `KES Secret Key` file which allows to:

* Compute automatically the `PoolId`
* Verify that you are the owner of the `PoolId`, and thus of the associated stakes used by Mithril protocol
* Verify that you are the owner of the Mithril `Signer Secret Key`, and thus allowed to contribute to the multi-signatures and certificate production of the Mithril network

## Building your own executable

### Download source

Download from GitHub (HTTPS)

```bash
git clone https://github.com/input-output-hk/mithril.git
```

Or (SSH)

```bash
git clone git@github.com:input-output-hk/mithril.git
```

### Build Mithril Signer binary

Switch to build branch / tag

```bash
# **YOUR_BUILD_BRANCH_OR_TAG** depends on the Mithril network you target, 
# please refer to the **Build From** column of the above **Mithril Networks** table
git switch **YOUR_BUILD_BRANCH_OR_TAG**
```

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

<CompiledBinaries />

## Verify binary

### Verify version

Check that the Mithril Signer binary is running the correct version by running

```bash
./mithril-signer -V
```

You should see something like

```bash
mithril-signer 0.2.0
```

:warning: Verify that the version displayed is the version described in the content of the Release / Pre-Release note (see the **Build From** column of the above **Mithril Networks** table)

### Verify build

Check that the Mithril Signer binary is working fine by running its help

```bash
./mithril-signer -h
```

You should see

```bash
An implementation of a Mithril Signer

Usage: mithril-signer [OPTIONS]

Options:
  -r, --run-mode <RUN_MODE>
          Run Mode [env: RUN_MODE=] [default: dev]
  -v, --verbose...
          Verbosity level, add more v to increase
  -c, --configuration-dir <CONFIGURATION_DIR>
          Directory where the configuration file is located [default: ./config]
      --disable-digests-cache
          Disable immutables digests cache
      --reset-digests-cache
          If set the existing immutables digests cache will be reset
  -h, --help
          Print help information (use `--help` for more detail)
  -V, --version
          Print version information
```

:::tip

If you want to dig deeper, you can get access to several level of logs from the Mithril Signer:

* Add `-v` for some logs (WARN)
* Add `-vv` for more logs (INFO)
* Add `-vvv` for even more logs (DEBUG)
* Add `-vvvv` for all logs (TRACE)

:::

## Install the service

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

* In the `/opt/mithril/mithril-signer/service.env` env file:
  * `KES_SECRET_KEY_PATH=/cardano/keys/kes.skey`: replace `/cardano/keys/kes.skey` with the path to your Cardano `KES Secret Key` file
  * `OPERATIONAL_CERTIFICATE_PATH=/cardano/cert/opcert.cert`: replace `/cardano/cert/opcert.cert` with the path to your Cardano `Operational Certificate` file
  * `DB_DIRECTORY=/cardano/db`: replace `/cardano/db` with the path to the database folder of the **Cardano Node** (the one in `--database-path`)
  * `CARDANO_NODE_SOCKET_PATH=/cardano/ipc/node.socket`: replace with the path to the IPC file (`CARDANO_NODE_SOCKET_PATH` env var)
  * `CARDANO_CLI_PATH=/app/bin/cardano-cli`: replace with the path to the `cardano-cli` executable
  * `DATA_STORES_DIRECTORY=/opt/mithril/stores`: replace with the path to a folder where the **Mithril Signer** will store its data (`/opt/mithril/stores` e.g.)
  * `STORE_RETENTION_LIMIT`: if set, this will limit the number of records in some internal stores (5 is a good fit).

:::

First create an env file that will be used by the service:

```bash
sudo bash -c 'cat > /opt/mithril/mithril-signer.env << EOF
KES_SECRET_KEY_PATH=**YOUR_KES_SECRET_KEY_PATH**
OPERATIONAL_CERTIFICATE_PATH=**YOUR_OPERATIONAL_CERTIFICATE_PATH**
NETWORK=**YOUR_CARDANO_NETWORK**
AGGREGATOR_ENDPOINT=**YOUR_AGGREGATOR_ENDPOINT**
RUN_INTERVAL=60000
DB_DIRECTORY=/cardano/db
CARDANO_NODE_SOCKET_PATH=/cardano/ipc/node.socket
CARDANO_CLI_PATH=/app/bin/cardano-cli
DATA_STORES_DIRECTORY=/opt/mithril/stores
STORE_RETENTION_LIMIT=5
EOF'
```

Then we will create a `/etc/systemd/system/mithril-signer.service` description file for our service

```bash
sudo bash -c 'cat > /etc/systemd/system/mithril-signer.service << EOF
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
EOF'
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

:::tip
There is a `2` epochs delay between the signer node registration and its ability to create individual signatures, as explained in the [Mithril Certificate Chain in depth](https://mithril.network/doc/mithril/mithril-protocol/certificates).
After this delay, you should be able to see your `PoolId` listed in some of the certificates available on the [`Mithril Explorer`](https://mithril.network/explorer)
:::

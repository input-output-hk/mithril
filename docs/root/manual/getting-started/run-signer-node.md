---
sidebar_position: 2
---

import NetworksMatrix from '../../networks-matrix.md';
import CompiledBinaries from '../../compiled-binaries.md'

# Run a Mithril Signer node (SPO)

:::note Mithril Networks

<NetworksMatrix />

:::

:::tip

For more information about the **Mithril Protocol**, please refer to the [About Mithril](../../mithril/intro.md) section.

:::

## Mithril Signer Deployment model

:::info

In this guide, you will learn how to setup a **Mithril Signer** on a **Cardano SPO Infrastructure** for the `mainnet` and the `testnet`:
- On the `mainnet`, you **must** run the **production** deployment where the **Mithril Signer** runs one the **Cardano Block Producer** machine and the **Mithril Relay** runs on the **Cardano Relay** machine. You can also run the **production** deployment on the `testnet`.
- On the `testnet` __only__, it is also acceptable (and easier) to run the **naive** deployment where the **Mithril Signer** runs on the **Cardano Relay** machine (in that case you don't need to setup a **Mithril Relay**).

:::

Here is the schema of the **production** deployment for the `mainnet`:
[![Production Mithril Signer Deployment Model](images/signer-deployment-production.jpg)](images/signer-deployment-production.jpg)

and the schema of the **naive** deployment only for the `testnet`:
[![Naive Mithril Signer Deployment Model](images/signer-deployment-naive.jpg)](images/signer-deployment-naive.jpg)

**Note**: You can also deploy the production deployment model on the `testnet`.

:::danger

On the `mainnet`, you must **never** copy the `KES Secret Key` out of the **Cardano Block Producer** machine!

:::

## Mithril Keys Certification

The **Mithril Signer** is using your Cardano `Operational Certificate` and `KES Secret Key` files which allow to:

* Compute automatically the `PoolId`
* Verify that you are the owner of the `PoolId`, and thus of the associated stakes used by Mithril protocol
* Verify that you are the owner of the Mithril `Signer Secret Key`, and thus allowed to contribute to the multi-signatures and certificate production of the Mithril network

## Pre-requisites

### What you'll need

:::info

This guide is working only on a Linux machine.

:::

* Operating a **Cardano Node** as a **Stake Pool**:
  * The Cardano `Operational Certificate` file of the pool
  * The Cardano `KES Secret Key` file of the pool

* Access to the file system of the **Cardano Block Producer** node for the **production** deployment (or of the **Cardano Relay** node for the **naive** deployment):
  * Read rights on the `Database` folder (`--database-path` setting of the **Cardano Node**)
  * Read/Write rights on the `Inter Process Communication` file (usually `CARDANO_NODE_SOCKET_PATH` env var used to launch the **Cardano Node**)

* Install a recent version of the [`cardano-cli`](https://github.com/input-output-hk/cardano-node/releases/tag/8.1.1) (version 8.1.1+)

* Install a [correctly configured](https://www.rust-lang.org/learn/get-started) Rust toolchain (latest stable version)

* Install OpenSSL development libraries, for example on Ubuntu/Debian/Mint run `apt install libssl-dev`

* Ensure the SQLite3 version is at least `3.35` (released Apr. 2021)

* Install a recent version of `jq` (version `1.6+`) `apt install jq`

* Only for the **production** deployment, install a recent version of [`squid-cache`](http://www.squid-cache.org/) (version `5.2+`) `apt install squid`

## Setup the Mithril Signer node

:::caution

- For the **production** deployment: the setup of the **Mithril Signer** is done on the **Cardano Block Producer** machine.

- For the **naive** deployment: the setup of the **Mithril Signer** is done on the **Cardano Relay** machine.

:::

### Building your own executable

#### Download source

Download from GitHub (HTTPS)

```bash
git clone https://github.com/input-output-hk/mithril.git
```

Or (SSH)

```bash
git clone git@github.com:input-output-hk/mithril.git
```

#### Build Mithril Signer binary

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

### Verify binary

#### Verify version

Check that the Mithril Signer binary is running the correct version by running

```bash
./mithril-signer -V
```

You should see something like

```bash
mithril-signer 0.2.0
```

:warning: Verify that the version displayed is the version described in the content of the Release / Pre-Release note (see the **Build From** column of the above **Mithril Networks** table)

#### Verify build

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

### Install the service

#### Move executable

Move executable to /opt/mithril

```bash
sudo mkdir -p /opt/mithril
sudo mv mithril-signer /opt/mithril
```

#### Setup the service

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
  * `ERA_READER_ADAPTER_TYPE=cardano-chain`: replace `cardano-chain` with the era reader adapter type used in your Mithril network
  * `ERA_READER_ADAPTER_PARAMS={"address": "...", "verification_key": "..."}`: replace `{"address": "...", "verification_key": "..."}` with the era reader params that you need to compute by running the command `jq -nc --arg address $(wget -q -O - **YOUR_ERA_READER_ADDRESS**) --arg verification_key $(wget -q -O - **YOUR_ERA_READER_VERIFICATION_KEY**) '{"address": $address, "verification_key": $verification_key}'`
  * `RELAY_ENDPOINT=http://192.168.1.50:3128` **(optional)**: this is the endpoint of the **Mithril Relay**, which is required for the **production** deployment only. For the **naive** deployment, do not set this variable in your env file.
:::

First create an env file that will be used by the service:

- for the **production** deployment:
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
ERA_READER_ADAPTER_TYPE=**YOUR_ERA_READER_ADAPTER_TYPE**
ERA_READER_ADAPTER_PARAMS=**YOUR_ERA_READER_ADAPTER_PARAMS**
RELAY_ENDPOINT=**YOUR_RELAY_ENDPOINT**
EOF'
```

- for the **naive** deployment:
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
ERA_READER_ADAPTER_TYPE=**YOUR_ERA_READER_ADAPTER_TYPE**
ERA_READER_ADAPTER_PARAMS=**YOUR_ERA_READER_ADAPTER_PARAMS**
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

## Setup the Mithril Relay node

:::caution

- For the **production** deployment: the setup of the **Mithril Relay** is done on the **Cardano Relay** machine.

- For the **naive** deployment: this step is not necessary.

:::

### Configure Squid service

:::info

The **Mithril Relay** node is a forward proxy that relays the traffic between the **Mithril Signer** and the **Mithril Aggregator**. When properly configured, it guarantees the security of the **Block Producing** node. We use `squid` to operate this forward proxy, and this section provides a recommended configuration.

:::

Verify that the service was correctly configured at install:

```bash
sudo systemctl status squid
```

Make a copy of the original configuration:

```bash
sudo cp /etc/squid/squid.conf /etc/squid/squid.conf.bak
```

Prepare the forward proxy configuration file:

```bash
sudo bash -c 'cat > /etc/squid/squid.conf << EOF
# Listening port (port 3128 is recommended)
http_port **YOUR_RELAY_LISTENING_PORT**

# ACL for internal IP of your relay node
acl relay_internal_ip src **YOUR_RELAY_INTERNAL_IP**

# ACL for aggregator endpoint
acl aggregator_domain dstdomain .mithril.network

# ACL for SSL port only
acl SSL_port port 443

# Allowed traffic
http_access allow relay_internal_ip aggregator_domain SSL_port

# Deny everything else
http_access deny all
EOF'
```

With this configuration, the proxy will:
- accept incoming traffic made to the internal IP of the relay machine
- accept incoming traffic made to the listening port of the proxy
- accept incoming HTTPS traffic proxied to `mithril.network` domain hosts
- deny all other traffic

Restart the service:

```bash
sudo systemctl restart squid
```

And make sure that it is running properly:

```bash
sudo systemctl status squid
```

And monitor the logs of the service

```bash
tail /var/log/syslog
```

### Firewall configuration

:::info

We assume that the **Cardano Relay** machine is protected by a firewall, and that the proxied traffic originating from the **Cardano Block Producer** must be allowed on this firewall.

:::

#### On the Cardano Relay machine

We need to allow the incoming traffic on the listening port of the **Mithril Relay** on **Cardano Relay** machine that is originating from the **Cardano Block Producer** machine.

Assuming you are using [`Uncomplicated Firewall`](https://en.wikipedia.org/wiki/Uncomplicated_Firewall), the command to run in order to open that traffic is:

```bash
sudo ufw allow tcp from **YOUR_BLOCK_PRODUCER_INTERNAL_IP** to any port **YOUR_RELAY_LISTENING_PORT**
```

## Verify the Mithril Signer Deployment

:::tip
There is a `2` epochs delay between the signer node registration and its ability to create individual signatures, as explained in the [Mithril Certificate Chain in depth](https://mithril.network/doc/mithril/mithril-protocol/certificates).
After this delay, you should be able to see your `PoolId` listed in some of the certificates available on the [`Mithril Explorer`](https://mithril.network/explorer)
:::

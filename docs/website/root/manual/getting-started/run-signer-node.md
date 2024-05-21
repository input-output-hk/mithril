---
sidebar_position: 3
---

import NetworksMatrix from '../../networks-matrix.md';
import CompiledBinaries from '../../compiled-binaries.md'

# Run a Mithril signer as an SPO

:::note Mithril networks

<NetworksMatrix />

:::

:::tip

For more information about the **Mithril protocol**, see the section [About Mithril](../../mithril/intro.md).

:::

## Mithril signer deployment model

:::info

In this guide, you will learn how to set up a **Mithril signer** within the stake pool operator (SPO) infrastructure both on Cardano `mainnet` and `testnet` environments:
- On `mainnet`, you **must** run the **production** deployment where the **Mithril signer** runs on the **Cardano block producer** machine and the **Mithril relay** runs on the **Cardano relay** machine. **Note** that you can run the **production** deployment on `testnet`.
- You can also run **naive** deployment, where the **Mithril signer** runs on the **Cardano relay** machine. This is possible in the testnet environment only, and does not require setting up a **Mithril relay**.

:::

:::info

In the current setup, you don't need to install a Mithril aggregator.

:::

:::caution

The **production** deployment model is currently in the beta version.

:::

Here is the schema of the **production** deployment on mainnet:
[![Production Mithril Signer Deployment Model](images/signer-deployment-production.jpg)](images/signer-deployment-production.jpg)

and the schema of the **naive** deployment specifically for `testnets`:
[![Naive Mithril Signer Deployment Model](images/signer-deployment-naive.jpg)](images/signer-deployment-naive.jpg)

:::danger

On `mainnet`, you must **never** copy the `KES secret key` from the **Cardano block producer** machine!

:::

## Mithril keys certification

The **Mithril signer** uses your Cardano `operational certificate` and `KES secret key` files which enable:

* Automatic computation of the `PoolId`
* Verification of your `PoolId` ownership and the associated stake used by the Mithril protocol
* Verification of your Mithril `signer secret key` ownership, which allows you to participate in the multi-signature process for certificate production on the Mithril network

## Mithril signer footprint

The **Mithril signer** has been designed to have the lowest footprint possible in terms of CPU, memory, disk i/o and storage.
Thus, there are no extra requirements on the recommended hardware than for running a Cardano stake pool, as detailed in this [guide](https://developers.cardano.org/docs/operate-a-stake-pool/hardware-requirements).

:::info

Here are some figures about the Mithril signer node running on the `mainnet` Cardano network:
- It is **idle** most of the time at `<50MB` memory usage and `<1%` CPU.
- It sends to the aggregator a **new signature** roughly every `4 hours` and a **new registration** every `5 days` (`<1MB` per day).
- When signing for the **first time**, the digest cache needs to be built with a spike of `50-70%` CPU on one core for `~1 hour`.
- Also, the full Cardano database will be **read from disk once** during this cache building process.
- Only **stake distributions**, **Mithril keys** and some **digest cache** are stored on the disk (`<100MB`).

:::

## Pre-requisites

:::info

Note that this guide works on a Linux machine only.

:::

* To operate a **Cardano node** as a **stake pool**, you need:
  * The pool's `operational certificate` 
  * The pool's `KES secret key` 

* To access the file system of the **Cardano block producer** node for **production** deployment (or of the **Cardano relay** node for **naive** deployment), you will need the following permissions:
  * Read rights on the `Database` folder (specified by the `--database-path` setting of the **Cardano node**)
  * Read and write rights on the `Inter Process Communication` file (typically defined by the `CARDANO_NODE_SOCKET_PATH` environment variable used to launch the **Cardano node**)

* Install a recent version of [`cardano-cli`](https://github.com/IntersectMBO/cardano-node/releases/tag/8.9.1) (version 8.9.1+).

* Install a correctly configured Rust toolchain (latest stable version). You can follow the instructions provided [here](https://www.rust-lang.org/learn/get-started).

* Install OpenSSL development libraries. For example, on Ubuntu/Debian/Mint, run `apt install libssl-dev`.

* Install a recent version of `jq` (version 1.6+). You can install it by running `apt install jq`.

* Only for the **production** deployment, install a recent version of [`squid-cache`](http://www.squid-cache.org/) (version 6.9+).

## Set up the Mithril signer node

:::caution

- For **production** deployment, the **Mithril signer** setup is performed on the **Cardano block producer** machine.

- For **naive** deployment, the **Mithril signer** setup is performed on the **Cardano relay** machine.

:::

:::info

Compare the version of your Cardano node with the minimum supported versions listed in the [`networks.json`](https://github.com/input-output-hk/mithril/blob/main/networks.json) to verify its compatibility with the Mithril signer.

First, check the version of your Cardano node by running the following command:

```bash
cardano-node --version
```

Then, refer to the minimum supported versions listed in the the [`networks.json`](https://github.com/input-output-hk/mithril/blob/main/networks.json) file.

You can also fetch the minimum supported version for your network using the command below:
```bash
wget -q -O - https://raw.githubusercontent.com/input-output-hk/mithril/main/networks.json | jq -r '."**YOUR_CARDANO_NETWORK**"."cardano-minimum-version"."mithril-signer"'
```

Here is an example for `preprod`:
```bash
wget -q -O - https://raw.githubusercontent.com/input-output-hk/mithril/main/networks.json | jq -r '."preprod"."cardano-minimum-version"."mithril-signer"'
```

:::

### Building your own executable

#### Download the source file

To download the source from GitHub (HTTPS), run:

```bash
git clone https://github.com/input-output-hk/mithril.git
```

Or (SSH):

```bash
git clone git@github.com:input-output-hk/mithril.git
```

#### Build the Mithril signer binary

First, switch to build a branch/tag:

```bash
# **YOUR_BUILD_BRANCH_OR_TAG** depends on the Mithril network you target, 
# please refer to the **Build from** column of the above **Mithril networks** table
git checkout **YOUR_BUILD_BRANCH_OR_TAG**
```

Then, change the directory:

```bash
cd mithril/mithril-signer
```

Run tests (optional):

```bash
make test
```

Finally, build the executable:

```bash
make build
```

### Download the pre-built binary

<CompiledBinaries />

### Verifying the binary

#### Verify the version of the binary

You can check that the Mithril signer binary is running the correct version by running:

```bash
./mithril-signer -V
```

You should see something like:

```bash
mithril-signer 0.2.0
```

:warning: Please verify that the displayed version matches the version described in the release/pre-release notes (refer to the **Build from** column in the **Mithril networks** table above).

#### Verify the build

Check that the Mithril signer binary is working correctly by running the help function:

```bash
./mithril-signer -h
```

You should see:

```bash
An implementation of a Mithril signer

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

If you wish to delve deeper, you can access logs at various levels from the Mithril signer:

* Add `-v` for some logs (WARN)
* Add `-vv` for more logs (INFO)
* Add `-vvv` for even more logs (DEBUG)
* Add `-vvvv` for all logs (TRACE)

:::

### Installing the service

#### Move the executable

To move the executable to /opt/mithril, run:

```bash
sudo mkdir -p /opt/mithril
sudo mv mithril-signer /opt/mithril
```

#### Set up the service

:::caution

* `User=cardano`:
Replace this value with the correct user. We assume that the user used to run the **Cardano node** is `cardano`. The **Mithril signer** must imperatively run with the same user.

* In the `/opt/mithril/mithril-signer.env` env file:
  * `KES_SECRET_KEY_PATH=/cardano/keys/kes.skey`: replace `/cardano/keys/kes.skey` with the path to your Cardano `KES secret key` file
  * `OPERATIONAL_CERTIFICATE_PATH=/cardano/cert/opcert.cert`: replace `/cardano/cert/opcert.cert` with the path to your Cardano `operational certificate` file
  * `DB_DIRECTORY=/cardano/db`: replace `/cardano/db` with the path to the database folder of the **Cardano node** (the one in `--database-path`)
  * `CARDANO_NODE_SOCKET_PATH=/cardano/ipc/node.socket`: replace with the path to the IPC file (`CARDANO_NODE_SOCKET_PATH` env var)
  * `CARDANO_CLI_PATH=/app/bin/cardano-cli`: replace with the path to the `cardano-cli` executable
  * `DATA_STORES_DIRECTORY=/opt/mithril/stores`: replace with the path to a folder where the **Mithril signer** will store its data (`/opt/mithril/stores` e.g.)
  * `STORE_RETENTION_LIMIT`: if set, this will limit the number of records in some internal stores (5 is a good fit).
  * `ERA_READER_ADAPTER_TYPE=cardano-chain`: replace `cardano-chain` with the era reader adapter type used in your Mithril network
  * `ERA_READER_ADAPTER_PARAMS={"address": "...", "verification_key": "..."}`: replace `{"address": "...", "verification_key": "..."}` with the era reader parameters that you need to compute by running the command `jq -nc --arg address $(wget -q -O - **YOUR_ERA_READER_ADDRESS**) --arg verification_key $(wget -q -O - **YOUR_ERA_READER_VERIFICATION_KEY**) '{"address": $address, "verification_key": $verification_key}'`
  * `RELAY_ENDPOINT=http://192.168.1.50:3132` **(optional)**: this is the endpoint of the **Mithril relay**, which is required for **production** deployment only. For **naive** deployment, do not set this variable in your environment file.
:::

:::tip

Here is an **example** set of values for **release-preprod** that will be used in this guide in the **tip** boxes to illustrate some commands:  

* ****YOUR_KES_SECRET_KEY_PATH****: `/cardano/keys/kes.skey`
* ****YOUR_OPERATIONAL_CERTIFICATE_PATH****: `/cardano/keys/node.cert`
* ****YOUR_CARDANO_NETWORK****: `preprod`
* ****YOUR_AGGREGATOR_ENDPOINT****: `https://aggregator.release-preprod.api.mithril.network/aggregator`
* ****YOUR_ERA_READER_ADAPTER_TYPE****: `cardano-chain`
* ****YOUR_ERA_READER_ADAPTER_PARAMS****: `{"address": "addr_test1qpkyv2ws0deszm67t840sdnruqgr492n80g3y96xw3p2ksk6suj5musy6w8lsg3yjd09cnpgctc2qh386rtxphxt248qr0npnx", "verification_key": "5b35352c3232382c3134342c38372c3133382c3133362c34382c382c31342c3138372c38352c3134382c39372c3233322c3235352c3232392c33382c3234342c3234372c3230342c3139382c31332c33312c3232322c32352c3136342c35322c3130322c39312c3132302c3230382c3134375d"}`
* ****YOUR_RELAY_ENDPOINT****: `192.168.1.50`
* ****YOUR_RELAY_LISTENING_PORT****: `3132`
* ****YOUR_BLOCK_PRODUCER_INTERNAL_IP****: `192.168.1.75`
* ****YOUR_SIGNER_LOGS_PATH****: `/var/log/syslog`
* ****YOUR_PARTY_ID****: `pool1hp72sauk0g0yqm4dzllz0pz6j93gewhllkzphn4hykkfmne43y`

:::

First, create an environment file that will be used by the service:

- for **production** deployment:
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

:::tip

Here is an example of the aforementioned command created with the example set for `release-preprod`:

```bash
sudo bash -c 'cat > /opt/mithril/mithril-signer.env << EOF
KES_SECRET_KEY_PATH=/cardano/keys/kes.skey
OPERATIONAL_CERTIFICATE_PATH=/cardano/keys/node.cert
NETWORK=preprod
AGGREGATOR_ENDPOINT=https://aggregator.release-preprod.api.mithril.network/aggregator
RUN_INTERVAL=60000
DB_DIRECTORY=/cardano/db
CARDANO_NODE_SOCKET_PATH=/cardano/ipc/node.socket
CARDANO_CLI_PATH=/app/bin/cardano-cli
DATA_STORES_DIRECTORY=/opt/mithril/stores
STORE_RETENTION_LIMIT=5
ERA_READER_ADAPTER_TYPE=cardano-chain
ERA_READER_ADAPTER_PARAMS={"address": "addr_test1qpkyv2ws0deszm67t840sdnruqgr492n80g3y96xw3p2ksk6suj5musy6w8lsg3yjd09cnpgctc2qh386rtxphxt248qr0npnx", "verification_key": "5b35352c3232382c3134342c38372c3133382c3133362c34382c382c31342c3138372c38352c3134382c39372c3233322c3235352c3232392c33382c3234342c3234372c3230342c3139382c31332c33312c3232322c32352c3136342c35322c3130322c39312c3132302c3230382c3134375d"}
RELAY_ENDPOINT=http://192.168.1.50:3132
EOF'
```

:::

- for **naive** deployment:
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

:::tip

Here is an example of the aforementioned command created with the example set for `release-preprod`:

```bash
sudo bash -c 'cat > /opt/mithril/mithril-signer.env << EOF
KES_SECRET_KEY_PATH=/cardano/keys/kes.skey
OPERATIONAL_CERTIFICATE_PATH=/cardano/keys/node.cert
NETWORK=preprod
AGGREGATOR_ENDPOINT=https://aggregator.release-preprod.api.mithril.network/aggregator
RUN_INTERVAL=60000
DB_DIRECTORY=/cardano/db
CARDANO_NODE_SOCKET_PATH=/cardano/ipc/node.socket
CARDANO_CLI_PATH=/app/bin/cardano-cli
DATA_STORES_DIRECTORY=/opt/mithril/stores
STORE_RETENTION_LIMIT=5
ERA_READER_ADAPTER_TYPE=cardano-chain
ERA_READER_ADAPTER_PARAMS={"address": "addr_test1qpkyv2ws0deszm67t840sdnruqgr492n80g3y96xw3p2ksk6suj5musy6w8lsg3yjd09cnpgctc2qh386rtxphxt248qr0npnx", "verification_key": "5b35352c3232382c3134342c38372c3133382c3133362c34382c382c31342c3138372c38352c3134382c39372c3233322c3235352c3232392c33382c3234342c3234372c3230342c3139382c31332c33312c3232322c32352c3136342c35322c3130322c39312c3132302c3230382c3134375d"}
EOF'
```

:::

Then, create a `/etc/systemd/system/mithril-signer.service` description file for the service:

```bash
sudo bash -c 'cat > /etc/systemd/system/mithril-signer.service << EOF
[Unit]
Description=Mithril signer service
StartLimitIntervalSec=0

[Service]
Type=simple
Restart=always
RestartSec=60
User=cardano
EnvironmentFile=/opt/mithril/mithril-signer.env
ExecStart=/opt/mithril/mithril-signer -vvv

[Install]
WantedBy=multi-user.target
EOF'
```

Reload the service configuration (optional):

```bash
sudo systemctl daemon-reload
```

Then, start the service:

```bash
sudo systemctl start mithril-signer
```

Register the service to start on boot:

```bash
sudo systemctl enable mithril-signer
```

Monitor the status of the service:

```bash
systemctl status mithril-signer.service
```

Finally, monitor the logs of the service:

```bash
tail -f /var/log/syslog | grep mithril-signer
```

### Activate Prometheus endpoint

The Mithril signer node can expose basic metrics on a Prometheus endpoint, which is not activated by default.

In order to expose metrics on the endpoint, you need to append the following environment variable to your environment file. In that case, the metrics server will listen on the `9090` port:

```bash
sudo bash -c 'cat >> /opt/mithril/mithril-signer.env << EOF
ENABLE_METRICS_SERVER=true
EOF`
```

You can also specify custom listen IP address and port for the metrics server:
```bash
sudo bash -c 'cat >> /opt/mithril/mithril-signer.env << EOF
ENABLE_METRICS_SERVER=true
METRICS_SERVER_IP=**YOUR_METRICS_SERVER_IP**
METRICS_SERVER_PORT=**YOUR_METRICS_SERVER_PORT**
EOF`
```

:::tip

Here is an example that reproduces the default configuration for the prometheus server:

```bash
sudo bash -c 'cat >> /opt/mithril/mithril-signer.env << EOF
ENABLE_METRICS_SERVER=true
METRICS_SERVER_IP=0.0.0.0
METRICS_SERVER_PORT=9090
EOF`
```

:::


:::info

When activated, the metrics endpoint will be accessible to the location `http://**YOUR_METRICS_SERVER_IP**:**YOUR_METRICS_SERVER_PORT**/metrics`, which translates to [`http://0.0.0.0:9090/metrics`](http://0.0.0.0:9090/metrics) with the default configuration.

:::

:::info

Additionally, a **Grafana template** has been created to easily setup a dashboard for this Prometheus endpoint (ID 20776): https://grafana.com/grafana/dashboards/20776-mithril-signer/

:::

### Rotating the KES keys

:::danger

When the KES keys expire, the Mithril signer is unable to register with the Mithril protocol.

:::

After rotating the KES keys on your Cardano block producer, we recommend following this upgrade procedure for your Mithril signer node:
1. Update the `KES_SECRET_KEY_PATH` entry of your environment file to reflect the location of the **new KES secret key file**.
2. Update the `OPERATIONAL_CERTIFICATE_PATH` entry of your environment file to reflect the location of the **new operational certificate file**.
3. Restart your Mithril signer service with the following command:
```bash
sudo systemctl restart mithril-signer
```
4. Check the logs of your signer node and make sure that it has successfully registered after restarting (the following log should be displayed: `STATE MACHINE: new cycle: Registered`).

## Set up the Mithril relay node

:::caution

- For **production** deployment, the setup of the **Mithril relay** is performed on the **Cardano relay** machine.

- For **naive** deployment: this step is not necessary.

:::

### Building Squid from source

:::info

- If you have already installed `Squid` via `apt` package manager, we recommend that you delete it before manually building it from source by running the following commands: 
  - `sudo systemctl stop squid`
  - `sudo apt remove squid`
  - `sudo apt autoremove`

- The FAQ for compiling `Squid` is available [here](https://wiki.squid-cache.org/SquidFaq/CompilingSquid).

- You will need a C++ compiler that can be installed with `sudo apt install build-essential` command.

:::

On the [Squid page listing released versions](https://www.squid-cache.org/Versions/) identify the latest stable released version (currently `6.9`) and download it:

```bash
wget https://www.squid-cache.org/Versions/v6/squid-6.9.tar.gz
```

Uncompress the downloaded archive, and change directory:
```bash
tar xzf squid-6.9.tar.gz
cd squid-6.9
```

Then, configure the compilation:
```bash
./configure \
    --prefix=/opt/squid \
    --localstatedir=/opt/squid/var \
    --libexecdir=/opt/squid/lib/squid \
    --datadir=/opt/squid/share/squid \
    --sysconfdir=/etc/squid \
    --with-default-user=squid \
    --with-logdir=/opt/squid/var/log/squid \
    --with-pidfile=/opt/squid/var/run/squid.pid
```

Compile the sources:
```bash
make
```

And install `squid` binary:
```bash
sudo make install
```

Optionally, verify that the version is correct:
```bash
/opt/squid/sbin/squid -v
```

You should see a result like this:
```bash
Squid Cache: Version 6.9
Service Name: squid
configure options:  '--prefix=/opt/squid' '--localstatedir=/opt/squid/var' '--libexecdir=/opt/squid/lib/squid' '--datadir=/opt/squid/share/squid' '--sysconfdir=/etc/squid' '--with-default-user=squid' '--with-logdir=/opt/squid/var/log/squid' '--with-pidfile=/opt/squid/var/run/squid.pid'
```

### Configuring the Squid proxy

:::info

The **Mithril relay** node serves as a forward proxy, relaying traffic between the **Mithril signer** and the **Mithril aggregator**. When appropriately configured, it facilitates the security of the **block-producing** node. You can use `squid` to operate this forward proxy, and this section presents a recommended configuration.

:::

Make a copy of the original configuration:

```bash
sudo cp /etc/squid/squid.conf /etc/squid/squid.conf.bak
```

Prepare the forward proxy configuration file:

```bash
sudo bash -c 'cat > /etc/squid/squid.conf << EOF
# Listening port (port 3132 is recommended)
http_port **YOUR_RELAY_LISTENING_PORT**

# ACL for internal IP of your block producer node
acl block_producer_internal_ip src **YOUR_BLOCK_PRODUCER_INTERNAL_IP**

# ACL for aggregator endpoint
acl aggregator_domain dstdomain .mithril.network

# ACL for SSL port only
acl SSL_port port 443

# Allowed traffic
http_access allow block_producer_internal_ip aggregator_domain SSL_port

# Do not disclose block producer internal IP
forwarded_for delete

# Turn off via header
via off
 
# Deny request for original source of a request
follow_x_forwarded_for deny all
 
# Anonymize request headers
request_header_access Authorization allow all
request_header_access Proxy-Authorization allow all
request_header_access Cache-Control allow all
request_header_access Content-Length allow all
request_header_access Content-Type allow all
request_header_access Date allow all
request_header_access Host allow all
request_header_access If-Modified-Since allow all
request_header_access Pragma allow all
request_header_access Accept allow all
request_header_access Accept-Charset allow all
request_header_access Accept-Encoding allow all
request_header_access Accept-Language allow all
request_header_access Connection allow all
request_header_access All deny all

# Disable cache
cache deny all

# Deny everything else
http_access deny all

EOF'
```

:::tip

Here is an example of the aforementioned command created with the example set for `release-preprod`:

```bash
sudo bash -c 'cat > /etc/squid/squid.conf << EOF
# Listening port (port 3132 is recommended)
http_port 3132

# ACL for internal IP of your block producer node
acl block_producer_internal_ip src 192.168.1.75

# ACL for aggregator endpoint
acl aggregator_domain dstdomain .mithril.network

# ACL for SSL port only
acl SSL_port port 443

# Allowed traffic
http_access allow block_producer_internal_ip aggregator_domain SSL_port

# Do not disclose block producer internal IP
forwarded_for delete

# Turn off via header
via off
 
# Deny request for original source of a request
follow_x_forwarded_for deny all
 
# Anonymize request headers
request_header_access Authorization allow all
request_header_access Proxy-Authorization allow all
request_header_access Cache-Control allow all
request_header_access Content-Length allow all
request_header_access Content-Type allow all
request_header_access Date allow all
request_header_access Host allow all
request_header_access If-Modified-Since allow all
request_header_access Pragma allow all
request_header_access Accept allow all
request_header_access Accept-Charset allow all
request_header_access Accept-Encoding allow all
request_header_access Accept-Language allow all
request_header_access Connection allow all
request_header_access All deny all

# Disable cache
cache deny all

# Deny everything else
http_access deny all

EOF'
```

:::

:::tip

In case you are using the same Cardano relay for multiple Cardano block producers, you will need to add a new line per block producer for authorizing its internal IP:

```bash
# ACL for internal IP of your block producer node
acl block_producer_internal_ip src **YOUR_BLOCK_PRODUCER_INTERNAL_IP_1**
acl block_producer_internal_ip src **YOUR_BLOCK_PRODUCER_INTERNAL_IP_2**
acl block_producer_internal_ip src **YOUR_BLOCK_PRODUCER_INTERNAL_IP_3**
```

:::

With this configuration, the proxy will:
- accept incoming traffic originating from the internal IP of the block-producing machine
- accept incoming traffic directed to the listening port of the proxy
- accept incoming HTTPS traffic proxied to `mithril.network` domain hosts
- anonymize completely the traffic and avoid disclosing any information about the block-producing machine
- deny all other traffic

:::info

:::

### Installing the service

Create (or re-use) an unpriviledged system user on the machine:
```bash
sudo adduser --system --no-create-home --group squid
```

Change ownership of `/opt/squid/var` directory:
```bash
sudo chown squid -R /opt/squid/var/
sudo chgrp squid -R /opt/squid/var/
```

Create a `/etc/systemd/system/squid.service` description file for the service:
```bash
sudo bash -c 'cat > /etc/systemd/system/squid.service << EOF
[Unit]
Description=Squid service
StartLimitIntervalSec=0

[Service]
Type=simple
Restart=always
RestartSec=60
User=squid
Group=squid
ExecStart=/opt/squid/sbin/squid -f /etc/squid/squid.conf -d5
PIDFile=/opt/squid/var/run/squid.pid

[Install]
WantedBy=multi-user.target
EOF'
```

Reload the service configuration (optional):

```bash
sudo systemctl daemon-reload
```

Then, start the service:

```bash
sudo systemctl start squid
```

Register the service to start on boot:

```bash
sudo systemctl enable squid
```

Monitor the status of the service:

```bash
systemctl status squid
```

Finally, monitor the logs of the service:

```bash
tail -f /var/log/syslog | grep squid
```

And monitor squid access logs:

```bash
sudo tail -f /opt/squid/var/log/squid/access.log
```

:::tip

If your **Squid service** does not start properly and you have the error message `FATAL: /dev/null (13) Permission denied` in the logs of the service, it means that some permissions need to be fixed following the creation of the new `squid` user by running the following commands:
- `sudo chmod 666 /dev/null`
- `sudo systemctl restart squid`

:::

### Firewall configuration

:::info

We assume that the **Cardano relay** machine is protected by a firewall. It is necessary to allow the proxied traffic, originating from the **Cardano block producer**, through this firewall.
:::

#### About the Cardano relay machine

You need to allow incoming traffic on the listening port of the **Mithril relay** on the **Cardano relay** machine, originating from the **Cardano block producer** machine.

Assuming you are using [`Uncomplicated Firewall`](https://en.wikipedia.org/wiki/Uncomplicated_Firewall) (`0.36+`), the command to open that traffic is:

```bash
sudo ufw allow from **YOUR_BLOCK_PRODUCER_INTERNAL_IP** to any port **YOUR_RELAY_LISTENING_PORT** proto tcp
```

:::tip

Here is an example of the aforementioned command created with the example set for `release-preprod`:

```bash
sudo ufw allow from 192.168.1.75 to any port 3132 proto tcp
```

:::

Assuming you are using [`Iptables`](https://en.wikipedia.org/wiki/Iptables) (`1.8.7+`), the command to open that traffic is:

```bash
sudo iptables -A INPUT -s **YOUR_BLOCK_PRODUCER_INTERNAL_IP** -p tcp --dport **YOUR_RELAY_LISTENING_PORT** -j ACCEPT
sudo iptables -L -v
sudo service netfilter-persistent save
```

:::tip

Here is an example of the aforementioned command created with the example set for `release-preprod`:

```bash
sudo iptables -A INPUT -s 192.168.1.75 -p tcp --dport 3132 -j ACCEPT
sudo iptables -L -v
sudo service netfilter-persistent save
```

:::

## Verify the Mithril signer deployment

:::tip
There is a delay of `2` epochs between the registration of the signer node and its ability to generate individual signatures. This delay is further explained in the [Mithril certificate chain in depth](https://mithril.network/doc/mithril/mithril-protocol/certificates) documentation.

Once this delay has passed, you should be able to observe your `PoolId` listed in some of the certificates accessible on the [`Mithril Explorer`](https://mithril.network/explorer).
:::

### Verify your signer is registered

After installing the Mithril signer, you can verify that your node is registered by checking your Mithril signer node logs.  

First, download the script into the desired directory:

```bash
wget https://mithril.network/doc/scripts/verify_signer_registration.sh
```

Make the script executable:

```bash
chmod +x verify_signer_registration.sh
```

Finally, execute the script:
```bash
PARTY_ID=**YOUR_PARTY_ID** AGGREGATOR_ENDPOINT=**YOUR_AGGREGATOR_ENDPOINT** ./verify_signer_registration.sh
```

:::tip

Here is an example command:

```bash
PARTY_ID=pool1hp72sauk0g0yqm4dzllz0pz6j93gewhllkzphn4hykkfmne43y AGGREGATOR_ENDPOINT=https://aggregator.release-preprod.api.mithril.network/aggregator ./verify_signer_registration.sh
```

:::

If your signer is registered, you should see this message:
```bash
>> Congrats, your signer node is registered!
```

Otherwise, you should see this error message:
```bash
>> Oops, your signer node is not registered. Party ID not found among the signers registered at epoch 430.
```

### Verify your signer contributes with individual signatures

After waiting for two epochs, you will be able to verify that your signer is contributing with individual signatures.

First, download the script into the desired directory:

```bash
wget https://mithril.network/doc/scripts/verify_signer_signature.sh
```

Make the script executable:

```bash
chmod +x verify_signer_signature.sh
```

Finally, execute the script:
```bash
PARTY_ID=**YOUR_PARTY_ID** AGGREGATOR_ENDPOINT=**YOUR_AGGREGATOR_ENDPOINT** ./verify_signer_signature.sh
```

:::tip

Here is an example of the aforementioned command created with the example set for `release-preprod`:

```bash
PARTY_ID=pool1hp72sauk0g0yqm4dzllz0pz6j93gewhllkzphn4hykkfmne43y AGGREGATOR_ENDPOINT=https://aggregator.release-preprod.api.mithril.network/aggregator ./verify_signer_signature.sh
```

:::

If your signer is contributing, you should see this message:
```bash
>> Congrats, you have signed this certificate: https://aggregator.release-preprod.api.mithril.network/aggregator/certificate/el3p289b03a223244285b2ls10839846ae7a69f1e8362824a383f376f93f723f !
```

Otherwise, you should see this error message:
```bash
>> Oops, your party id was not found in the last 20 certificates. Please try again later.
```

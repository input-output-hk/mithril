---
sidebar_position: 4
---

import CompiledBinaries from '../../compiled-binaries.mdx'

# Run a Mithril aggregator node

:::danger

Running multiple Mithril aggregators on a Mithril network is enabled by the DMQ protocol which is currently **unstable** and not suitable for production use.

:::

:::info

You don't need to be a Cardano SPO to run a Mithril aggregator node.

:::

:::tip

For more information about the **Mithril protocol**, see the [About Mithril](../../mithril/intro.md) section.

:::

On a Mithril network, there exists two types of aggregator nodes:

- **Leader aggregator node**: this node is managed by the entity which generates the Genesis certificates of the Mithril network (Input Output for all networks specified in the [Network configurations](../getting-started/network-configurations.md). It is responsible for collecting signers registrations (which is still a centralized process) and broadcast them to other nodes of the network. It provides also a bootstrap point for other aggregator nodes to synchronize the certificate chain.
- **Follower aggregator node**: this node is managed by any entity willing to contribute to the Mithril network reliability or to provide additional availability guarantees to an infrastructure (e.g. for a bridge powered by Mithril). This document explains how to set up and run such a follower Mithril aggregator node.

:::info

The follower aggregator listens to signatures diffused by the SPOs' Mithril signer nodes and produces Mithril certificates accordingly. Until the DMQ protocol is stabilized and widely deployed (at least **60%** of stake registered in the Mithril protocol operating it), the follower aggregator will not be able to produce certificates.

:::

Every follower aggregator node is free to select the types of data it wants to support certification for:

- **Cardano database v2**<sup>(\*)</sup>
- **Cardano transactions**
- **Cardano stake distribution**

_<sup>(\*)</sup>: requires an access to GCP to store Cardano database and may incur costs._

:::tip

For more information about the **supported data**, see the [Mithril certification](../../mithril/advanced/mithril-certification/README.mdx) section.

:::

## Prerequisites

:::info

Note that this guide works on a Linux machine only.

:::

- Operate a **Cardano full node**

- To access the file system of the **Cardano full node**, you will need the following permissions:

  - Read rights on the `Database` folder (specified by the `--database-path` setting of the **Cardano node**)
  - Read and write rights on the `Inter Process Communication` file (typically defined by the `CARDANO_NODE_SOCKET_PATH` environment variable used to launch the **Cardano node**)

- Install a correctly configured Rust toolchain (latest stable version); you can follow the instructions provided [here](https://www.rust-lang.org/learn/get-started)

- Install build tools `build-essential` and `m4`; for example, on Ubuntu/Debian/Mint, run `sudo apt install build-essential m4`

- Install OpenSSL development libraries; for example, on Ubuntu/Debian/Mint, run `apt install libssl-dev`

- Install a recent version of `jq` (version 1.6+) by running `apt install jq`

## Set up the Mithril aggregator node

:::info

Compare the version of your Cardano node with the minimum supported versions listed in the [`networks.json`](https://github.com/input-output-hk/mithril/blob/main/networks.json) file to verify its compatibility with the Mithril aggregator.

First, check the version of your Cardano node by running the following command:

```bash
cardano-node --version
```

Then, refer to the minimum supported versions listed in the the [`networks.json`](https://github.com/input-output-hk/mithril/blob/main/networks.json) file.

You can also fetch the minimum supported version for your network using the command below:

```bash
wget -q -O - https://raw.githubusercontent.com/input-output-hk/mithril/main/networks.json | jq -r '."**YOUR_CARDANO_NETWORK**"."cardano-minimum-version"."mithril-aggregator"'
```

Here is an example for `preprod`:

```bash
wget -q -O - https://raw.githubusercontent.com/input-output-hk/mithril/main/networks.json | jq -r '."preprod"."cardano-minimum-version"."mithril-aggregator"'
```

:::

### Building your own Mithril aggregator executable

#### Download the source file

To download the source from GitHub (HTTPS), run:

```bash
git clone https://github.com/input-output-hk/mithril.git
```

Or (SSH):

```bash
git clone git@github.com:input-output-hk/mithril.git
```

#### Build the Mithril aggregator binary

First, switch to build a branch/tag:

```bash
# **YOUR_BUILD_BRANCH_OR_TAG** depends on the Mithril network you target,
# please refer to the **Build from** column of the **Mithril networks** section
git checkout **YOUR_BUILD_BRANCH_OR_TAG**
```

Then, change the directory:

```bash
cd mithril/mithril-aggregator
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

<CompiledBinaries node="mithril-aggregator"/>

### Verifying the binary

#### Verify the version of the binary

You can check that the Mithril aggregator binary is running the correct version by running:

```bash
./mithril-aggregator -V
```

You should see something like:

```bash
mithril-aggregator 0.2.0
```

:warning: Please verify that the displayed version matches the version described in the release/pre-release notes (refer to the **Build from** column in the **Mithril networks** section).

#### Verify the build

Check that the Mithril aggregator binary is working correctly by running the help function:

```bash
./mithril-aggregator -h
```

You should see:

```bash
Mithril aggregator node

Usage: mithril-aggregator [OPTIONS] <COMMAND>

Commands:
  genesis   Genesis tools
  era       Era tools
  serve     Server runtime mode
  tools     List of tools to upkeep the aggregator
  database  Database tools
  help      Print this message or the help of the given subcommand(s)

Options:
  -r, --run-mode <RUN_MODE>
          Run Mode [default: dev]
  -v, --verbose...
          Verbosity level
      --db-directory <DB_DIRECTORY>
          Directory of the Cardano node files
      --config-directory <CONFIG_DIRECTORY>
          Directory where configuration file is located [default: ./config]
  -h, --help
          Print help
  -V, --version
          Print version
```

:::tip

If you wish to delve deeper, you can access logs at various levels from the Mithril aggregator:

- Add `-v` for some logs (WARN)
- Add `-vv` for more logs (INFO)
- Add `-vvv` for even more logs (DEBUG)
- Add `-vvvv` for all logs (TRACE).

:::

### Installing the service

#### Move the executable

To move the executable to /opt/mithril, run:

```bash
sudo mkdir -p /opt/mithril
sudo mv mithril-aggregator /opt/mithril
```

#### Set up the service

:::caution

TODO

- `User=cardano`:
  Replace this value with the correct user. We assume that the user used to run the **Cardano node** is `cardano`. The **Mithril aggregator** must imperatively run with the same user.

- In the `/opt/mithril/mithril-aggregator.env` env file:

  - `DB_DIRECTORY=/cardano/db`: replace `/cardano/db` with the path to the database folder of the **Cardano node** (the one in `--database-path`)
  - `CARDANO_NODE_SOCKET_PATH=/cardano/ipc/node.socket`: replace with the path to the IPC file (`CARDANO_NODE_SOCKET_PATH` env var)
  - `DATA_STORES_DIRECTORY=/opt/mithril/stores`: replace with the path to a folder where the **Mithril aggregator** will store its data (eg, `/opt/mithril/stores`)
  - `STORE_RETENTION_LIMIT`: if set, this will limit the number of records in some internal stores (5 is a good fit)
  - `ERA_READER_ADAPTER_TYPE=cardano-chain`: replace `cardano-chain` with the era reader adapter type used in your Mithril network
  - `ERA_READER_ADAPTER_PARAMS={"address": "...", "verification_key": "..."}`: replace `{"address": "...", "verification_key": "..."}` with the era reader parameters that you need to compute by running the command `jq -nc --arg address $(wget -q -O - **YOUR_ERA_READER_ADDRESS**) --arg verification_key $(wget -q -O - **YOUR_ERA_READER_VERIFICATION_KEY**) '{"address": $address, "verification_key": $verification_key}'`

- In the `/opt/mithril/mithril-aggregator.env` env file, the **base configuration** values are:

  - `SIGNED_ENTITY_TYPES`: TBD
  - `SERVER_PORT`: TBD
  - `PUBLIC_SERVER_URL`: TBD
  - `LEADER_AGGREGATOR_ENDPOINT`: TBD
  - `RUN_INTERVAL`: TBD
  - `AGGREGATE_SIGNATURE_TYPE`: TBD
  - `NETWORK`: TBD
  - `NETWORK_MAGIC`: TBD
  - `CARDANO_NODE_SOCKET_PATH`: TBD
  - `CARDANO_NODE_VERSION`: TBD
  - `CHAIN_OBSERVER_TYPE`: TBD
  - `ERA_READER_ADAPTER_TYPE`: TBD
  - `ERA_READER_ADAPTER_PARAMS`: TBD
  - `GENESIS_VERIFICATION_KEY`: TBD
  - `DMQ_NODE_SOCKET_PATH`: TBD
  - `CUSTOM_ORIGIN_TAG_WHITE_LIST`: TBD

- The **Cardano database** configuration values are:

  - `DB_DIRECTORY`: TBD
  - `DATA_STORES_DIRECTORY`: TBD
  - `GOOGLE_APPLICATION_CREDENTIALS_JSON`: TBD
  - `GOOGLE_APPLICATION_CREDENTIALS_GCP_KMS_JSON`: TBD
  - `SNAPSHOT_UPLOADER_TYPE`: TBD
  - `SNAPSHOT_BUCKET_NAME`: TBD
  - `SNAPSHOT_USE_CDN_DOMAIN`: TBD
  - `SNAPSHOT_COMPRESSION_ALGORITHM`: TBD
  - `ZSTANDARD_PARAMETERS__LEVEL`: TBD
  - `ZSTANDARD_PARAMETERS__NUMBER_OF_WORKERS`: TBD
  - `ANCILLARY_FILES_SIGNER_CONFIG`: TBD

- The **Cardano transactions** configuration values are:
  - `ALLOW_UNPARSABLE_BLOCK`: TBD
  - `CARDANO_TRANSACTIONS_PROVER_CACHE_POOL_SIZE`: TBD
  - `CARDANO_TRANSACTIONS_DATABASE_CONNECTION_POOL_SIZE`: TBD

:::

:::tip

TODO

Here is an **example** set of values for **pre-release-preview** that will be used in this guide in the **tip** boxes to illustrate some commands:

- **Base configuration**:

  - **SIGNED_ENTITY_TYPES**: `YOUR_SIGNED_ENTITY_TYPES`
  - **SERVER_PORT**: `YOUR_SERVER_PORT`
  - **PUBLIC_SERVER_URL**: `YOUR_PUBLIC_SERVER_URL`
  - **LEADER_AGGREGATOR_ENDPOINT**: `YOUR_LEADER_AGGREGATOR_ENDPOINT`
  - **RUN_INTERVAL**: `YOUR_RUN_INTERVAL`
  - **AGGREGATE_SIGNATURE_TYPE**: `YOUR_AGGREGATE_SIGNATURE_TYPE`
  - **STORE_RETENTION_LIMIT**: `YOUR_STORE_RETENTION_LIMIT`
  - **NETWORK**: `YOUR_NETWORK`
  - **NETWORK_MAGIC**: `YOUR_NETWORK_MAGIC`
  - **CARDANO_NODE_SOCKET_PATH**: `YOUR_CARDANO_NODE_SOCKET_PATH`
  - **CARDANO_NODE_VERSION**: `YOUR_CARDANO_IMAGE_ID`
  - **CHAIN_OBSERVER_TYPE**: `YOUR_CHAIN_OBSERVER_TYPE`
  - **ERA_READER_ADAPTER_TYPE**: `YOUR_ERA_READER_ADAPTER_TYPE`
  - **ERA_READER_ADAPTER_PARAMS**: `YOUR_ERA_READER_ADAPTER_PARAMS`
  - **GENESIS_VERIFICATION_KEY**: `YOUR_GENESIS_VERIFICATION_KEY`
  - **DMQ_NODE_SOCKET_PATH**: `YOUR_DMQ_NODE_SOCKET_PATH`
  - **CUSTOM_ORIGIN_TAG_WHITE_LIST**: `YOUR_CUSTOM_ORIGIN_TAG_WHITE_LIST`

- **Cardano database configuration**:

  - **DB_DIRECTORY**: `YOUR_DB_DIRECTORY`
  - **DATA_STORES_DIRECTORY**: `YOUR_DATA_STORES_DIRECTORY`
  - **GOOGLE_APPLICATION_CREDENTIALS_JSON**: `YOUR_GOOGLE_APPLICATION_CREDENTIALS_JSON`
  - **GOOGLE_APPLICATION_CREDENTIALS_GCP_KMS_JSON**: `YOUR_GOOGLE_APPLICATION_CREDENTIALS_GCP_KMS_JSON`
  - **SNAPSHOT_UPLOADER_TYPE**: `YOUR_SNAPSHOT_UPLOADER_TYPE`
  - **SNAPSHOT_BUCKET_NAME**: `YOUR_SNAPSHOT_BUCKET_NAME`
  - **SNAPSHOT_USE_CDN_DOMAIN**: `YOUR_SNAPSHOT_USE_CDN_DOMAIN`
  - **SNAPSHOT_COMPRESSION_ALGORITHM**: `YOUR_SNAPSHOT_COMPRESSION_ALGORITHM`
  - **ZSTANDARD_PARAMETERS\_\_LEVEL**: `YOUR_ZSTANDARD_PARAMETERS__LEVEL`
  - **ZSTANDARD_PARAMETERS\_\_NUMBER_OF_WORKERS**: `YOUR_ZSTANDARD_PARAMETERS__NUMBER_OF_WORKERS`
  - **ANCILLARY_FILES_SIGNER_CONFIG**: `YOUR_ANCILLARY_FILES_SIGNER_CONFIG`

- **Cardano transactions configuration**:

  - **ALLOW_UNPARSABLE_BLOCK**: `YOUR_ALLOW_UNPARSABLE_BLOCK`
  - **CARDANO_TRANSACTIONS_PROVER_CACHE_POOL_SIZE**: `YOUR_CARDANO_TRANSACTIONS_PROVER_CACHE_POOL_SIZE`
  - **CARDANO_TRANSACTIONS_DATABASE_CONNECTION_POOL_SIZE**: `YOUR_CARDANO_TRANSACTIONS_DATABASE_CONNECTION_POOL_SIZE`

:::

First, create an environment file that the service will use:

```bash
sudo bash -c 'cat > /opt/mithril/mithril-aggregator.env << EOF
# Base configuration
SIGNED_ENTITY_TYPES=**YOUR_SIGNED_ENTITY_TYPES**
SERVER_PORT=**YOUR_SERVER_PORT**
PUBLIC_SERVER_URL=**YOUR_PUBLIC_SERVER_URL**
LEADER_AGGREGATOR_ENDPOINT=**YOUR_LEADER_AGGREGATOR_ENDPOINT**
RUN_INTERVAL=**YOUR_RUN_INTERVAL**
AGGREGATE_SIGNATURE_TYPE=**YOUR_AGGREGATE_SIGNATURE_TYPE**
STORE_RETENTION_LIMIT=**YOUR_STORE_RETENTION_LIMIT**
NETWORK=**YOUR_NETWORK**
NETWORK_MAGIC=**YOUR_NETWORK_MAGIC**
CARDANO_NODE_SOCKET_PATH=**YOUR_CARDANO_NODE_SOCKET_PATH**
CARDANO_NODE_VERSION=**YOUR_CARDANO_IMAGE_ID**
CHAIN_OBSERVER_TYPE=**YOUR_CHAIN_OBSERVER_TYPE**
ERA_READER_ADAPTER_TYPE=**YOUR_ERA_READER_ADAPTER_TYPE**
ERA_READER_ADAPTER_PARAMS=**YOUR_ERA_READER_ADAPTER_PARAMS**
GENESIS_VERIFICATION_KEY=**YOUR_GENESIS_VERIFICATION_KEY**
DMQ_NODE_SOCKET_PATH=**YOUR_DMQ_NODE_SOCKET_PATH**
CUSTOM_ORIGIN_TAG_WHITE_LIST=**YOUR_CUSTOM_ORIGIN_TAG_WHITE_LIST**
EOF'
```

If you want to support **Cardano database v2** certification, append the following variables to the `/opt/mithril/mithril-aggregator.env` file:

```bash
sudo bash -c 'cat >> /opt/mithril/mithril-aggregator.env << EOF
# Cardano database configuration
DB_DIRECTORY=**YOUR_DB_DIRECTORY**
DATA_STORES_DIRECTORY=**YOUR_DATA_STORES_DIRECTORY**
GOOGLE_APPLICATION_CREDENTIALS_JSON=**YOUR_GOOGLE_APPLICATION_CREDENTIALS_JSON**
GOOGLE_APPLICATION_CREDENTIALS_GCP_KMS_JSON=**YOUR_GOOGLE_APPLICATION_CREDENTIALS_GCP_KMS_JSON**
SNAPSHOT_UPLOADER_TYPE=**YOUR_SNAPSHOT_UPLOADER_TYPE**
SNAPSHOT_BUCKET_NAME=**YOUR_SNAPSHOT_BUCKET_NAME**
SNAPSHOT_USE_CDN_DOMAIN=**YOUR_SNAPSHOT_USE_CDN_DOMAIN** # Needed?
SNAPSHOT_COMPRESSION_ALGORITHM=**YOUR_SNAPSHOT_COMPRESSION_ALGORITHM**
ZSTANDARD_PARAMETERS__LEVEL=**YOUR_ZSTANDARD_PARAMETERS__LEVEL**
ZSTANDARD_PARAMETERS__NUMBER_OF_WORKERS=**YOUR_ZSTANDARD_PARAMETERS__NUMBER_OF_WORKERS**
ANCILLARY_FILES_SIGNER_CONFIG=**YOUR_ANCILLARY_FILES_SIGNER_CONFIG**
EOF`
```

If you want to support **Cardano transactions** certification, append the following variables to the `/opt/mithril/mithril-aggregator.env` file:

```bash
sudo bash -c 'cat >> /opt/mithril/mithril-aggregator.env << EOF
# Cardano transactions configuration
ALLOW_UNPARSABLE_BLOCK=**YOUR_ALLOW_UNPARSABLE_BLOCK**
CARDANO_TRANSACTIONS_PROVER_CACHE_POOL_SIZE=**YOUR_CARDANO_TRANSACTIONS_PROVER_CACHE_POOL_SIZE**
CARDANO_TRANSACTIONS_DATABASE_CONNECTION_POOL_SIZE=**YOUR_CARDANO_TRANSACTIONS_DATABASE_CONNECTION_POOL_SIZE**
EOF`
```

:::tip

TODO

Here is an example of the aforementioned command created with the example set for `pre-release-preview`:

```bash
sudo bash -c 'cat > /opt/mithril/mithril-aggregator.env << EOF
SERVER_PORT=8080
PUBLIC_SERVER_URL=**PUBLIC_SERVER_URL**
LEADER_AGGREGATOR_ENDPOINT=**LEADER_AGGREGATOR_ENDPOINT**
RUN_INTERVAL=60000
SIGNED_ENTITY_TYPES=**SIGNED_ENTITY_TYPES**
AGGREGATE_SIGNATURE_TYPE=**AGGREGATE_SIGNATURE_TYPE**
STORE_RETENTION_LIMIT=5
NETWORK=**NETWORK**
NETWORK_MAGIC=**NETWORK_MAGIC**
CARDANO_NODE_SOCKET_PATH=**CARDANO_NODE_SOCKET_PATH**
CARDANO_NODE_VERSION=**CARDANO_IMAGE_ID**
CHAIN_OBSERVER_TYPE=**CHAIN_OBSERVER_TYPE**
ERA_READER_ADAPTER_TYPE=**ERA_READER_ADAPTER_TYPE**
ERA_READER_ADAPTER_PARAMS=**ERA_READER_ADAPTER_PARAMS**
GENESIS_VERIFICATION_KEY=**GENESIS_VERIFICATION_KEY**
DMQ_NODE_SOCKET_PATH=**DMQ_NODE_SOCKET_PATH**
CUSTOM_ORIGIN_TAG_WHITE_LIST=**CUSTOM_ORIGIN_TAG_WHITE_LIST**
EOF'
```

If you want to support **Cardano database v2** certification, append the following variables to the `/opt/mithril/mithril-aggregator.env` file:

```bash
sudo bash -c 'cat >> /opt/mithril/mithril-aggregator.env << EOF
# Cardano database
DB_DIRECTORY=**DB_DIRECTORY**
DATA_STORES_DIRECTORY=**DATA_STORES_DIRECTORY**
GOOGLE_APPLICATION_CREDENTIALS_JSON=**GOOGLE_APPLICATION_CREDENTIALS_JSON**
GOOGLE_APPLICATION_CREDENTIALS_GCP_KMS_JSON=**GOOGLE_APPLICATION_CREDENTIALS_GCP_KMS_JSON**
SNAPSHOT_UPLOADER_TYPE=**SNAPSHOT_UPLOADER_TYPE**
SNAPSHOT_BUCKET_NAME=**SNAPSHOT_BUCKET_NAME**
SNAPSHOT_USE_CDN_DOMAIN=**SNAPSHOT_USE_CDN_DOMAIN** # Needed?
SNAPSHOT_COMPRESSION_ALGORITHM=**SNAPSHOT_COMPRESSION_ALGORITHM**
ZSTANDARD_PARAMETERS__LEVEL=**ZSTANDARD_PARAMETERS__LEVEL**
ZSTANDARD_PARAMETERS__NUMBER_OF_WORKERS=**ZSTANDARD_PARAMETERS__NUMBER_OF_WORKERS**
ANCILLARY_FILES_SIGNER_CONFIG=**ANCILLARY_FILES_SIGNER_CONFIG**
EOF`
```

If you want to support **Cardano transactions** certification, append the following variables to the `/opt/mithril/mithril-aggregator.env` file:

```bash
sudo bash -c 'cat >> /opt/mithril/mithril-aggregator.env << EOF
# Cardano transactions
ALLOW_UNPARSABLE_BLOCK=**ALLOW_UNPARSABLE_BLOCK**
CARDANO_TRANSACTIONS_PROVER_CACHE_POOL_SIZE=**CARDANO_TRANSACTIONS_PROVER_CACHE_POOL_SIZE**
CARDANO_TRANSACTIONS_DATABASE_CONNECTION_POOL_SIZE=**CARDANO_TRANSACTIONS_DATABASE_CONNECTION_POOL_SIZE**
EOF`
```

:::

Then, create a `/etc/systemd/system/mithril-aggregator.service` description file for the service:

```bash
sudo bash -c 'cat > /etc/systemd/system/mithril-aggregator.service << EOF
[Unit]
Description=Mithril aggregator service
StartLimitIntervalSec=0

[Service]
Type=simple
Restart=always
RestartSec=60
User=cardano
EnvironmentFile=/opt/mithril/mithril-aggregator.env
ExecStart=/opt/mithril/mithril-aggregator -vvv

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
sudo systemctl start mithril-aggregator
```

Register the service to start on boot:

```bash
sudo systemctl enable mithril-aggregator
```

Monitor the status of the service:

```bash
systemctl status mithril-aggregator.service
```

Finally, monitor the logs of the service:

```bash
tail -f /var/log/syslog | grep mithril-aggregator
```

### Activate Prometheus endpoint

The Mithril aggregator node can expose basic metrics on a Prometheus endpoint, which is not activated by default.

| Metrics                                                                                      | Description                                                                                               |
| -------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------- |
| **mithril_aggregator_certificate_detail_total_served_since_startup**                         | Number of certificate details served since startup on a Mithril aggregator node                           |
| **mithril_aggregator_artifact_detail_cardano_db_total_served_since_startup**                 | Number of Cardano immutable files full artifact details served since startup on a Mithril aggregator node |
| **mithril_aggregator_cardano_db_total_restoration_since_startup**                            | Number of Cardano immutable files full restorations since startup on a Mithril aggregator node            |
| **mithril_aggregator_cardano_db_immutable_files_restored_since_startup**                     | Number of Cardano immutable files restored since startup on a Mithril aggregator node                     |
| **mithril_aggregator_cardano_db_ancillary_files_restored_since_startup**                     | Number of Cardano ancillary files restored since startup on a Mithril aggregator node                     |
| **mithril_aggregator_cardano_db_complete_restoration_since_startup**                         | Number of complete Cardano database restoration since startup on a Mithril aggregator node                |
| **mithril_aggregator_cardano_db_partial_restoration_since_startup**                          | Number of partial Cardano database restoration since startup on a Mithril aggregator node                 |
| **mithril_aggregator_artifact_detail_cardano_database_total_served_since_startup**           | Number of Cardano database artifact details served since startup on a Mithril aggregator node             |
| **mithril_aggregator_artifact_detail_mithril_stake_distribution_total_served_since_startup** | Number of Mithril stake distribution artifact details served since startup on a Mithril aggregator node   |
| **mithril_aggregator_artifact_detail_cardano_stake_distribution_total_served_since_startup** | Number of Cardano stake distribution artifact details served since startup on a Mithril aggregator node   |
| **mithril_aggregator_artifact_detail_cardano_transaction_total_served_since_startup**        | Number of Cardano transaction artifact details served since startup on a Mithril aggregator node          |
| **mithril_aggregator_proof_cardano_transaction_total_proofs_served_since_startup**           | Number of Cardano transaction proofs served since startup on a Mithril aggregator node                    |
| **mithril_aggregator_proof_cardano_transaction_total_transactions_served_since_startup**     | Number of Cardano transaction hashes requested for proof since startup on a Mithril aggregator node       |
| **mithril_aggregator_signer_registration_total_received_since_startup**                      | Number of signer registrations received since startup on a Mithril aggregator node                        |
| **mithril_aggregator_signer_registration_total_successful_since_startup**                    | Number of successful signer registrations received since startup on a Mithril aggregator node             |
| **mithril_aggregator_signature_registration_total_received_since_startup**                   | Number of signature registrations received since startup on a Mithril aggregator node                     |
| **mithril_aggregator_signature_registration_total_successful_since_startup**                 | Number of successful signature registrations received since startup on a Mithril aggregator node          |
| **mithril_aggregator_certificate_total_produced_since_startup**                              | Number of certificates produced since startup on a Mithril aggregator node                                |
| **mithril_aggregator_artifact_cardano_db_total_produced_since_startup**                      | Number of Cardano immutable files full artifacts produced since startup on a Mithril aggregator node      |
| **mithril_aggregator_artifact_cardano_database_total_produced_since_startup**                | Number of Cardano database artifacts produced since startup on a Mithril aggregator node                  |
| **mithril_aggregator_artifact_mithril_stake_distribution_total_produced_since_startup**      | Number of Mithril stake distribution artifacts produced since startup on a Mithril aggregator node        |
| **mithril_aggregator_artifact_cardano_stake_distribution_total_produced_since_startup**      | Number of Cardano stake distribution artifacts produced since startup on a Mithril aggregator node        |
| **mithril_aggregator_artifact_cardano_transaction_total_produced_since_startup**             | Number of Cardano transaction artifacts produced since startup on a Mithril aggregator node               |
| **mithril_aggregator_runtime_cycle_success_since_startup**                                   | Number of successful runtime cycles since startup on a Mithril aggregator                                 |
| **mithril_aggregator_runtime_cycle_total_since_startup**                                     | Number of runtime cycles since startup on a Mithril aggregator                                            |

To expose metrics on the endpoint, append the following environment variable to your environment file. In that case, the metrics server will listen on the `9090` port:

```bash
sudo bash -c 'cat >> /opt/mithril/mithril-aggregator.env << EOF
ENABLE_METRICS_SERVER=true
EOF`
```

You can also specify a custom listen IP address and port for the metrics server:

```bash
sudo bash -c 'cat >> /opt/mithril/mithril-aggregator.env << EOF
ENABLE_METRICS_SERVER=true
METRICS_SERVER_IP=**YOUR_METRICS_SERVER_IP**
METRICS_SERVER_PORT=**YOUR_METRICS_SERVER_PORT**
EOF`
```

:::tip

Here is an example that reproduces the default configuration for the Prometheus server:

```bash
sudo bash -c 'cat >> /opt/mithril/mithril-aggregator.env << EOF
ENABLE_METRICS_SERVER=true
METRICS_SERVER_IP=0.0.0.0
METRICS_SERVER_PORT=9090
EOF`
```

:::

:::info

When activated, the metrics endpoint will be accessible to the `http://**YOUR_METRICS_SERVER_IP**:**YOUR_METRICS_SERVER_PORT**/metrics` location, which translates to [`http://0.0.0.0:9090/metrics`](http://0.0.0.0:9090/metrics) with the default configuration.

:::

:::info

Additionally, a **Grafana template** has been created to easily set up a dashboard for this Prometheus endpoint (ID 22165): https://grafana.com/grafana/dashboards/22165-mithril-aggregator/

:::

## Setup the DMQ node (unstable)

:::danger

The DMQ node setup is currently **unstable** and not suitable for production use.

During the stabilization and rampup phase of the DMQ network:

- Signatures are still sent to the central aggregator (using the DMQ node is harmless)
- This section is subject to frequent changes.

:::

The DMQ node supports the implementation of a **Decentralized Message Queue** (DMQ) for Mithril. The DMQ protocol allows Mithril signers to exchange signatures in a decentralized manner, enhancing the robustness and scalability of the Mithril networks. Once stabilized and deployed on a majority of SPOs on a Mithril network, the DMQ protocol will allow multiple aggregators to operate simultaneously, improving the overall availability of the Mithril protocol. The DMQ protocol is fully described in the [CIP-0137](https://cips.cardano.org/cip/CIP-0137).

:::caution

- Here are the needed information to setup a DMQ node:
  - `**YOUR_DMQ_NODE_SOCKET_PATH**`: replace with the path to the IPC file of the DMQ node
  - `**YOUR_CARDANO_NODE_SOCKET_PATH**`: replace with the path to the IPC file of the Cardano node
  - `**YOUR_CARDANO_NETWORK_MAGIC**`: replace with the network magic number of your Cardano network
  - `**YOUR_DMQ_NODE_PUBLIC_ADDRESS**`: replace with the **public** IP address of your DMQ node
  - `**YOUR_DMQ_NODE_PORT**`: replace with the listening port of your DMQ node
  - `**YOUR_DMQ_BOOTSTRAP_PEER_ADDRESS**`: replace with the IP address of your DMQ bootstrap peer (the address can be found in the [Mithril networks](../getting-started/network-configurations.md) table)
  - `**YOUR_DMQ_BOOTSTRAP_PEER_PORT**`: replace with the listening port of your DMQ bootstrap peer (the value can be found in the [Mithril networks](../getting-started/network-configurations.md) table).

:::

:::tip

Here is an **example** set of values for **pre-release-preview** that will be used in this guide in the **tip** boxes to illustrate some commands:

- **YOUR_DMQ_NODE_SOCKET_PATH**: `/dmq/ipc/node.socket`
- **YOUR_CARDANO_NODE_SOCKET_PATH**: `/cardano/ipc/node.socket`
- **YOUR_CARDANO_NETWORK_MAGIC**: `2`
- **YOUR_DMQ_NODE_PUBLIC_ADDRESS**: `34.14.65.160`
- **YOUR_DMQ_NODE_PORT**: `6161`
- **YOUR_DMQ_BOOTSTRAP_PEER_ADDRESS**: `34.76.22.193`
- **YOUR_DMQ_BOOTSTRAP_PEER_PORT**: `11001`

:::

### Download the pre-built binary

:::tip

As we are still in a testing stage, we only support the `pre-release-preview` network.

You can use these parameters for the **pre-release-preview** network:

- **DMQ_RELEASE_URL**: `https://github.com/input-output-hk/mithril/raw/refs/heads/jpraynaud/dmq-node-binary/mithril-test-lab/mithril-devnet/bin/dmq-node-0.2.0.0-53bf9652787dc768abd86cf3844f1206f0fd7d8c`

_These URLs may change in the future; please refer to this page for the latest released version of the DMQ node binary._

:::

To download the latest released version of the DMQ node binary, run the following command:

```bash
curl --fail -sL -o dmq-node **DMQ_RELEASE_URL**
```

### Installing the service

#### Make the binary executable

To make the binary executable, run:

```bash
chmod +x dmq-node
```

#### Move the executable

To move the executable to /opt/dmq, run:

```bash
sudo mkdir -p /opt/dmq
sudo mv dmq-node /opt/dmq
```

#### Prepare the configuration file of the DMQ node

Create a `/opt/dmq/config.json` configuration file:

```bash
bash -c 'cat > /opt/dmq/config.json << EOF
{
  "CardanoNetworkMagic": **YOUR_CARDANO_NETWORK_MAGIC**,
  "CardanoNodeSocket": "**YOUR_CARDANO_NODE_SOCKET_PATH**"
  "PeerSharing": true,
  "LocalMsgSubmissionTracer": true,
  "LocalMsgNotificationTracer": true,
  "ConnectionManagerTracer": true,
  "DiffusionTracer": true,
  "InboundGovernorTracer": true,
  "LocalInboundGovernorTracer": true,
  "PeerSelectionTracer": true,
  "PeerSelectionCounters": true,
  "SigSubmissionLogicTracer": true,
  "SigSubmissionClientTracer": true,
  "SigSubmissionServerTracer": true,
  "MuxTracer": true,
  "ChannelTracer": true,
  "DebugPeerSelectionTracer": true,
}
EOF'
```

:::tip

Here is an example of the aforementioned command created with the example set for `pre-release-preview`:

```bash
bash -c 'cat > /opt/dmq/config.json << EOF
{
  "CardanoNetworkMagic": 2,
  "CardanoNodeSocket": "/cardano/ipc/node.socket"
  "PeerSharing": true,
  "LocalMsgSubmissionTracer": true,
  "LocalMsgNotificationTracer": true,
  "ConnectionManagerTracer": true,
  "DiffusionTracer": true,
  "InboundGovernorTracer": true,
  "LocalInboundGovernorTracer": true,
  "PeerSelectionTracer": true,
  "PeerSelectionCounters": true,
  "SigSubmissionLogicTracer": true,
  "SigSubmissionClientTracer": true,
  "SigSubmissionServerTracer": true,
  "MuxTracer": true,
  "ChannelTracer": true,
  "DebugPeerSelectionTracer": true,
}
EOF'
```

:::

#### Prepare the topology file for the DMQ node

:::tip

The topology file of the DMQ node follows a similar format than the Cardano node topology file. More information is available at [Cardano node topology](https://developers.cardano.org/docs/get-started/infrastructure/node/topology/).

:::

- Create a `/opt/dmq/topology.json` configuration file:

```bash
bash -c 'cat > /opt/dmq/topology.json << EOF
{
  "bootstrapPeers": [],
  "localRoots": [
    {
      "accessPoints": [
        {
          "address": "**YOUR_DMQ_BOOTSTRAP_PEER_ADDRESS**",
          "port": **YOUR_DMQ_BOOTSTRAP_PEER_PORT**,
          "valency": 1
        }
      ],
      "advertise": true,
      "trustable": false,
      "valency": 2
    }
  ],
  "peerSnapshotFile": null,
  "publicRoots": [
    {
      "accessPoints": [],
      "advertise": false
    }
  ]
}
EOF'
```

:::tip

Here is an example of the aforementioned command created with the example set for `pre-release-preview`:

```bash
bash -c 'cat > /opt/dmq/topology.json << EOF
{
  "bootstrapPeers": [],
  "localRoots": [
    {
      "accessPoints": [
        {
          "address": "34.76.22.193",
          "port": 11001,
          "valency": 1
        }
      ],
      "advertise": true,
      "trustable": false,
      "valency": 2
    }
  ],
  "peerSnapshotFile": null,
  "publicRoots": [
    {
      "accessPoints": [],
      "advertise": false
    }
  ]
}
EOF'
```

:::

#### Set up the service

:::caution

- `User=cardano`:
  Replace this value with the correct user. We assume that the user used to run the **Cardano node** is `cardano`. The **DMQ node** must imperatively run with the same user.

:::

- Create a `/etc/systemd/system/dmq-aggregator.service` description file for the service:

```bash
sudo bash -c 'cat > /etc/systemd/system/dmq-aggregator.service << EOF
[Unit]
Description=DMQ aggregator service
StartLimitIntervalSec=0

[Service]
Type=simple
Restart=always
RestartSec=60
User=cardano
ExecStart=/opt/dmq/dmq-node --configuration-file /opt/dmq/config.json --topology-file /opt/dmq/topology.json --local-socket **YOUR_DMQ_NODE_SOCKET_PATH** --host-addr **YOUR_DMQ_NODE_PUBLIC_ADDRESS** --port **YOUR_DMQ_NODE_PORT**

[Install]
WantedBy=multi-user.target
EOF'
```

:::tip

Here is an example of the aforementioned command created with the example set for `pre-release-preview`:

```bash
sudo bash -c 'cat > /etc/systemd/system/dmq-aggregator.service << EOF
[Unit]
Description=DMQ aggregator service
StartLimitIntervalSec=0

[Service]
Type=simple
Restart=always
RestartSec=60
User=cardano
ExecStart=/opt/dmq/dmq-node --configuration-file /opt/dmq/config.json --topology-file /opt/dmq/topology.json --local-socket /dmq/ipc/node.socket --host-addr 34.14.65.160 --port 11001
[Install]
WantedBy=multi-user.target
EOF'
```

:::

Reload the service configuration (optional):

```bash
sudo systemctl daemon-reload
```

Then, start the service:

```bash
sudo systemctl start dmq-aggregator
```

Register the service to start on boot:

```bash
sudo systemctl enable dmq-aggregator
```

Monitor the status of the service:

```bash
systemctl status dmq-aggregator.service
```

Finally, monitor the logs of the service:

```bash
tail -f /var/log/syslog | grep dmq-aggregator
```

### Update the configuration of the Mithril aggegator

TODO: to do directly upper in the aggregator configuration

The Mithril aggregator node must be configured to use the DMQ node (use the **block-producer** DMQ node socket path for the **production** deployment):

```bash
sudo bash -c 'cat >> /opt/mithril/mithril-aggregator.env << EOF
DMQ_NODE_SOCKET_PATH=**YOUR_DMQ_NODE_SOCKET_PATH**
EOF'
```

:::tip

Here is an example of the aforementioned command created with the example set for `pre-release-preview`:

```bash
sudo bash -c 'cat >> /opt/mithril/mithril-aggregator.env << EOF
DMQ_NODE_SOCKET_PATH=/dmq/ipc/node.socket
EOF'
```

:::

## Verify the Mithril aggregator deployment

TODO

:::tip
There is a delay of `2` epochs between the aggregator node's registration and its ability to generate individual signatures. This delay is further explained in the [Mithril certificate chain in depth](https://mithril.network/doc/mithril/mithril-protocol/certificates) documentation.

Once this delay has passed, you should be able to observe your `PoolId` listed in some of the certificates accessible on the [`Mithril Explorer`](https://mithril.network/explorer).
:::

### Verify your aggregator is synchronizing signer registrations with the leader aggregator

TODO

After installing the Mithril aggregator, you can verify that your node is registered.

First, download the script into the desired directory:

```bash
wget https://mithril.network/doc/scripts/verify_aggregator_registration.sh
```

Make the script executable:

```bash
chmod +x verify_aggregator_registration.sh
```

Finally, execute the script:

```bash
PARTY_ID=**YOUR_PARTY_ID** AGGREGATOR_ENDPOINT=**YOUR_AGGREGATOR_ENDPOINT** ./verify_aggregator_registration.sh
```

:::tip

Here is an example command:

```bash
PARTY_ID=pool1hp72sauk0g0yqm4dzllz0pz6j93gewhllkzphn4hykkfmne43y AGGREGATOR_ENDPOINT=https://aggregator.pre-release-preview.api.mithril.network/aggregator ./verify_aggregator_registration.sh
```

:::

If your aggregator is registered, you should see this message:

```bash
>> Congrats, your aggregator node is registered!
```

Otherwise, you should see this error message:

```bash
>> Oops, your aggregator node is not registered. Party ID not found among the aggregators registered at epoch 430.
```

### Verify your aggregator is synchronizing the certificate chain with the leader aggregator

TODO

After waiting for two epochs, you will be able to verify that your aggregator is contributing with individual signatures.

First, download the script into the desired directory:

```bash
wget https://mithril.network/doc/scripts/verify_aggregator_signature.sh
```

Make the script executable:

```bash
chmod +x verify_aggregator_signature.sh
```

Finally, execute the script:

```bash
PARTY_ID=**YOUR_PARTY_ID** AGGREGATOR_ENDPOINT=**YOUR_AGGREGATOR_ENDPOINT** ./verify_aggregator_signature.sh
```

:::tip

Here is an example of the aforementioned command created with the example set for `pre-release-preview`:

```bash
PARTY_ID=pool1hp72sauk0g0yqm4dzllz0pz6j93gewhllkzphn4hykkfmne43y AGGREGATOR_ENDPOINT=https://aggregator.pre-release-preview.api.mithril.network/aggregator ./verify_aggregator_signature.sh
```

:::

If your aggregator is contributing, you should see this message:

```bash
>> Congrats, you have signed this certificate: https://aggregator.pre-release-preview.api.mithril.network/aggregator/certificate/el3p289b03a223244285b2ls10839846ae7a69f1e8362824a383f376f93f723f !
```

Otherwise, you should see this error message:

```bash
>> Oops, your party id was not found in the last 20 certificates. Please try again later.
```

### Verify your aggregator is producing new certificates

TODO

### Register your aggregator for automatic discovery

TODO

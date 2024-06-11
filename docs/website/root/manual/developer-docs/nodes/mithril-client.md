---
sidebar_position: 3
---

import NetworksMatrix from '../../../networks-matrix.md';
import CompiledBinaries from '../../../compiled-binaries.md'

# Mithril client node

:::info

Mithril client is responsible for restoring the **Cardano** blockchain on an empty node from a certified snapshot.

:::

:::tip

* For more information about the **Mithril network**, please see
  the [architecture](../../../mithril/mithril-network/architecture.md) overview.

* For more information about the **Mithril client** node, please
  see [this overview](../../../mithril/mithril-network/client.md).

* Check out the [`Bootstrap a Cardano node`](../../getting-started/bootstrap-cardano-node.md) guide.

:::

:::note Mithril networks

<NetworksMatrix />

:::

## Resources

| Node | Source repository | Rust documentation | Docker packages |
|:-:|:-----------------:|:------------------:|:---------------:|
**Mithril client CLI** | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/tree/main/mithril-client-cli) | [:arrow_upper_right:](https://mithril.network/rust-doc/mithril_client_cli/index.html) | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/pkgs/container/mithril-client)

## Pre-requisites

* Install the latest stable version of the [correctly configured](https://www.rust-lang.org/learn/get-started) Rust
  toolchain.

* Install Build Tools `build-essential` and `m4`. For example, on Ubuntu/Debian/Mint, run `sudo apt install build-essential m4`.

* Install OpenSSL development libraries. For example, on Ubuntu/Debian/Mint, run `apt install libssl-dev`

## Download the source file

Download the source file from GitHub (HTTPS):

```bash
git clone https://github.com/input-output-hk/mithril.git
```

Or (SSH):

```bash
git clone git@github.com:input-output-hk/mithril.git
```

Switch to the desired branch/tag:

```bash
# Replace **YOUR_BUILD_BRANCH_OR_TAG** with the appropriate branch or tag name
# Please refer to the **Build from** column of the **Mithril networks** table above
git checkout **YOUR_BUILD_BRANCH_OR_TAG**
```

Change the directory:

```bash
cd mithril/mithril-client-cli
```

## Development testing and building

Run tests:

```bash
make test
```

Create the help menu:

```bash
make help
```

Generate the Rust documentation:

```bash
make doc
```

Run in debug mode with the default configuration:

```bash
make debug
```

## Release the build and run the binary

Build and run in release mode with the default configuration:

```bash
make run
```

Or, build only in release mode:

```bash
make build
```

Display the help menu:

```bash
./mithril-client-cli --help
```

You should see:

```bash
This program shows, downloads and verifies certified blockchain artifacts.

Usage: mithril-client [OPTIONS] <COMMAND>

Commands:
  cardano-db                  Cardano db management (alias: cdb)
  mithril-stake-distribution  Mithril Stake Distribution management (alias: msd)
  cardano-transaction         [unstable] Cardano transactions management (alias: ctx)
  help                        Print this message or the help of the given subcommand(s)

Options:
      --run-mode <RUN_MODE>
          Run Mode [env: RUN_MODE=] [default: dev]
  -v, --verbose...
          Verbosity level (-v=warning, -vv=info, -vvv=debug)
      --config-directory <CONFIG_DIRECTORY>
          Directory where configuration file is located [default: ./config]
      --aggregator-endpoint <AGGREGATOR_ENDPOINT>
          Override configuration Aggregator endpoint URL [env: AGGREGATOR_ENDPOINT=]
      --log-format-json
          Enable JSON output for logs displayed according to verbosity level
      --log-output <LOG_OUTPUT>
          Redirect the logs to a file
      --unstable
          Enable unstable commands (such as Cardano Transactions)
  -h, --help
          Print help
  -V, --version
          Print version
```

Run in release mode with the default configuration:

```bash
./mithril-client-cli
```

Run in release mode with a specific mode:

```bash
./mithril-client-cli --run-mode preview
```

Run in release mode with a custom configuration using environment variables:

```bash
GENESIS_VERIFICATION_KEY=$(wget -q -O - **YOUR_GENESIS_VERIFICATION_KEY**) AGGREGATOR_ENDPOINT=**YOUR_AGGREGATOR_ENDPOINT** ./mithril-client
```

:::tip

To display results in JSON format for the `list` and `show` commands, simply use the `--json` (or `-j`) option:

```bash
./mithril-client-cli cardano-db snapshot list --json
```

:::

:::tip

If you wish to delve deeper and access several levels of logs from the Mithril client, use the following:

* Add `-v` for some logs (WARN)
* Add `-vv` for more logs (INFO)
* Add `-vvv` for even more logs (DEBUG)
* Add `-vvvv` for all logs (TRACE)

:::

## Download the pre-built binary

<CompiledBinaries />

## Run a Docker container

### Registry image

A list of available images on the registry can be
found [here](https://github.com/input-output-hk/mithril/pkgs/container/mithril-client).

To prepare the environment variables, retrieve the values from the above **Mithril networks** table.

```bash
export MITHRIL_IMAGE_ID=**YOUR_MITHRIL_IMAGE_ID**
export AGGREGATOR_ENDPOINT=**YOUR_AGGREGATOR_ENDPOINT**
export GENESIS_VERIFICATION_KEY=$(wget -q -O - **YOUR_GENESIS_VERIFICATION_KEY**)
export SNAPSHOT_DIGEST=latest
```

Here is an example configuration for the `release-preprod` network and the `latest` stable Docker image:

```bash
export MITHRIL_IMAGE_ID=latest
export AGGREGATOR_ENDPOINT=https://aggregator.release-preprod.api.mithril.network/aggregator
export GENESIS_VERIFICATION_KEY=$(wget -q -O - https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/genesis.vkey)
export SNAPSHOT_DIGEST=latest
```

Proceed by creating a shell function for the Mithril client:

```bash
mithril_client () {
  docker run --rm -e GENESIS_VERIFICATION_KEY=$GENESIS_VERIFICATION_KEY -e AGGREGATOR_ENDPOINT=$AGGREGATOR_ENDPOINT --name='mithril-client' -v $(pwd):/app/data -u $(id -u) ghcr.io/input-output-hk/mithril-client:$MITHRIL_IMAGE_ID $@
}
```

Now you can use the `mithril_client` functions:

```bash
# 1- Help
mithril_client help

# 2- List cardano db snapshots
mithril_client cardano-db snapshot list

# 3- Show detailed information about a cardano db snapshot
mithril_client cardano-db snapshot show $CARDANO_DB_DIGEST

# 4- Download the given cardano db and verify the certificate
mithril_client cardano-db download $CARDANO_DB_DIGEST

# 5- List Mithril stake distributions
mithril_client mithril-stake-distribution list

# 6- Download and verify the given Mithril stake distribution
mithril_client mithril-stake-distribution download $MITHRIL_STAKE_DISTRIBUTION_ARTIFACT_HASH

# 7- List Cardano transaction snapshots
mithril_client --unstable cardano-transaction snapshot list

# 8- Show detailed information about a Cardano transaction snapshot
mithril_client --unstable cardano-transaction snapshot show $CARDANO_TRANSACTION_SNAPSHOT_HASH

# 9- Certify that given list of transactions hashes are included in the Cardano transactions set
mithril_client --unstable cardano-transaction certify $TRANSACTION_HASH_1,$TRANSACTION_HASH_2
```

### Local image

Build a local Docker image:

```bash
make docker-build
```

Run a local Docker container:

```bash
make docker-run
```

## Subcommands

Here are the subcommands available:

### Cardano DB

| Subcommand | Performed action |
|------------|------------------|
| **download** | Downloads and restores a cardano-db snapshot|
| **help** | Prints this message or the help for the given subcommand(s)|
| **snapshot list** | Lists available cardano-db snapshots|
| **snapshot show** | Shows information about a cardano-db snapshot|

### Mithril stake distribution

| Subcommand | Performed action |
|------------|------------------|
| **download** | Downloads and verifies Mithril stake distribution|
| **help** | Prints this message or the help for the given subcommand(s)|
| **list** | Lists available Mithril stake distributions|

### Cardano transactions

| Subcommand | Performed action |
|------------|------------------|
| **certify** | Certifies that given list of transactions hashes are included in the Cardano transactions set|
| **snapshot list** | Lists available Cardano transactions snapshots|
| **snapshot show** | Shows information about a Cardano transactions snapshot|
| **help** | Prints this message or the help for the given subcommand(s)|

## Configuration parameters

The configuration parameters can be set in either of the following ways:

1. In a configuration file, depending on the `--run-mode` parameter. If the runtime mode is `testnet`, the file is
   located in `./conf/testnet.json`.

2. The value can be overridden by an environment variable with the parameter name in uppercase.

Here is a list of the available parameters:

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `verbose` | `--verbose` | `-v` | `VERBOSE` | Verbosity level | - | Parsed from the number of occurrences: `-v` for `Warning`, `-vv` for `Info`, `-vvv` for `Debug` and `-vvvv` for `Trace` | :heavy_check_mark: |
| `unstable` | `--unstable` | - | - | Enable unstable commands | - | - | - |
| `run_mode` | `--run-mode` | - | `RUN_MODE` | Runtime mode | `dev` | - | :heavy_check_mark: |
| `aggregator_endpoint` | `--aggregator-endpoint` | - | `AGGREGATOR_ENDPOINT` | Aggregator node endpoint | - | `https://aggregator.pre-release-preview.api.mithril.network/aggregator` | :heavy_check_mark: |
| `genesis_verification_key` | - | - | `GENESIS_VERIFICATION_KEY` | Genesis verification key | - | - | :heavy_check_mark: |
| `log_format_json` | `--log-format-json` | - | - | Enable JSON output for logs | - | - | - |
| `log_output` | `--log-output` | `-o` | - | Redirect the logs to a file | - | `./mithril-client.log` | - |

`cardano-db snapshot show` command:

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `digest` | `--digest` | - | `DIGEST` | Cardano DB digest or `latest` for the latest digest | - | - | :heavy_check_mark: |
| `json` | `--json` | - | - | Enable JSON output for command results | - | - | - |

`cardano-db snapshot list` command:

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `json` | `--json` | - | - | Enable JSON output for command results | - | - | - |

`cardano-db download` command:

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `digest` | `--digest` | - | `DIGEST` | Cardano DB digest or `latest` for the latest digest | - | - | :heavy_check_mark: |
| `download_dir` | `--download-dir` | - | - | Directory where the Cardano DB will be downloaded | . | - | - |
| `json` | `--json` | - | - | Enable JSON output for progress logs | - | - | - |

`mithril-stake-distribution list` command:

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `json` | `--json` | - | - | Enable JSON output for command results | - | - | - |

`mithril-stake-distribution download` command:

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `artifact_hash` | `--artifact-hash` | - | - | Hash of the Mithril stake distribution artifact or `latest` for the latest artifact | - | - | :heavy_check_mark: |
| `download_dir` | `--download-dir` | - | - | Directory where the Mithril stake distribution will be downloaded | . | - | - |

`cardano-transaction snapshot show` command:

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `hash` | `--hash` | - | `HASH` | Cardano transaction snapshot hash or `latest` for the latest Cardano transaction snapshot | - | - | :heavy_check_mark: |
| `json` | `--json` | - | - | Enable JSON output for command results | - | - | - |

`cardano-transaction snapshot list` command:

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `json` | `--json` | - | - | Enable JSON output for command results | - | - | - |

`cardano-transaction certify` command:

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `transactions_hashes` | `--transactions_hashes` | - | `TRANSACTIONS_HASHES` | Cardano transactions hashes separated by commas | - | - | :heavy_check_mark: |
| `json` | `--json` | - | - | Enable JSON output for progress logs | - | - | - |

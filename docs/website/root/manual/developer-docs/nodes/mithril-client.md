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

* For more information about the **Mithril network**, please see the [architecture](../../../mithril/mithril-network/architecture.md) overview.

* For more information about the **Mithril client** node, please see [this overview](../../../mithril/mithril-network/client.md).

* Check out the [`Bootstrap a Cardano node`](../../getting-started/bootstrap-cardano-node.md) guide.

:::

:::note Mithril networks

<NetworksMatrix />

:::


## Resources

| Node | Source repository | Rust documentation | Docker packages |
|:-:|:-----------------:|:------------------:|:---------------:|
**Mithril client** | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/tree/main/mithril-client) | [:arrow_upper_right:](https://mithril.network/mithril-client/doc/mithril_client/index.html) | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/pkgs/container/mithril-client)

## Pre-requisites

* Install the latest stable version of the [correctly configured](https://www.rust-lang.org/learn/get-started) Rust toolchain.

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
cd mithril/mithril-client
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
./mithril-client --help
```

You should see:

```bash
This program shows, downloads, and verifies certified blockchain artifacts.

Usage: mithril-client [OPTIONS] <COMMAND>

Commands:
  snapshot                    Snapshot commands
  mithril-stake-distribution  Mithril stake distribution management (alias: msd)
  help                        Print this message or the help for the given subcommand(s)

Options:
      --run-mode <RUN_MODE>
          Run Mode [env: RUN_MODE=] [default: dev]
  -v, --verbose...
          Verbosity level (-v=warning, -vv=info, -vvv=debug)
      --config-directory <CONFIG_DIRECTORY>
          Directory where configuration file is located [default: ./config]
      --aggregator-endpoint <AGGREGATOR_ENDPOINT>
          Override configuration Aggregator endpoint URL
  -h, --help
          Print help
  -V, --version
          Print version


```

Run in release mode with the default configuration:

```bash
./mithril-client
```

Run in release mode with a specific mode:

```bash
./mithril-client --run-mode preview
```

Run in release mode with a custom configuration using environment variables:

```bash
GENESIS_VERIFICATION_KEY=$(wget -q -O - **YOUR_GENESIS_VERIFICATION_KEY**) NETWORK=**YOUR_CARDANO_NETWORK** AGGREGATOR_ENDPOINT=**YOUR_AGGREGATOR_ENDPOINT** ./mithril-client
```

:::tip

To display results in JSON format for the `list` and `show` commands, simply use the `--json` (or `-j`) option:

```bash
./mithril-client snapshot list --json
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

A list of available images on the registry can be found [here](https://github.com/input-output-hk/mithril/pkgs/container/mithril-client).

To prepare the environment variables, retrieve the values from the above **Mithril networks** table.

```bash
export MITHRIL_IMAGE_ID=**YOUR_MITHRIL_IMAGE_ID**
export NETWORK=**YOUR_CARDANO_NETWORK**
export AGGREGATOR_ENDPOINT=**YOUR_AGGREGATOR_ENDPOINT**
export GENESIS_VERIFICATION_KEY=$(wget -q -O - **YOUR_GENESIS_VERIFICATION_KEY**)
export SNAPSHOT_DIGEST=$(curl -sL $AGGREGATOR_ENDPOINT/artifact/snapshots | jq -r '.[0].digest')
```

Here is an example configuration for the `release-preprod` network and the `latest` stable Docker image:

```bash
export MITHRIL_IMAGE_ID=latest
export NETWORK=preprod
export AGGREGATOR_ENDPOINT=https://aggregator.release-preprod.api.mithril.network/aggregator
export GENESIS_VERIFICATION_KEY=$(wget -q -O - https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/genesis.vkey)
export SNAPSHOT_DIGEST=$(curl -sL $AGGREGATOR_ENDPOINT/artifact/snapshots | jq -r '.[0].digest')
```

Proceed by creating a shell function for the Mithril client:

```bash
mithril_client () {
  docker run --rm -e NETWORK=$NETWORK -e GENESIS_VERIFICATION_KEY=$GENESIS_VERIFICATION_KEY -e AGGREGATOR_ENDPOINT=$AGGREGATOR_ENDPOINT --name='mithril-client' -v $(pwd):/app/data -u $(id -u) ghcr.io/input-output-hk/mithril-client:$MITHRIL_IMAGE_ID $@
}
```

Now you can use the `mithril_client` functions:

```bash
# 1- Help
mithril_client help

# 2- List snapshots
mithril_client snapshot list

# 3- Show detailed information about a snapshot
mithril_client snapshot show $SNAPSHOT_DIGEST

# 4- Download the given snapshot and verify the certificate
mithril_client snapshot download $SNAPSHOT_DIGEST

# 5- List Mithril stake distributions
mithril_client mithril-stake-distribution list

# 6- Download and verify the given Mithril stake distribution
mithril_client mithril-stake-distribution download $MITHRIL_STAKE_DISTRIBUTION_ARTIFACT_HASH
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

### Snapshot

| Subcommand | Performed action |
|------------|------------------|
| **download** | Downloads and restores a snapshot|
| **help** | Prints this message or the help for the given subcommand(s)|
| **list** | Lists available snapshots|
| **show** | Shows information about a snapshot|

### Mithril stake distribution

| Subcommand | Performed action |
|------------|------------------|
| **download** | Downloads and verifies Mithril stake distribution|
| **help** | Prints this message or the help for the given subcommand(s)|
| **list** | Lists available Mithril stake distributions|

## Configuration parameters

The configuration parameters can be set in either of the following ways:

1. In a configuration file, depending on the `--run-mode` parameter. If the runtime mode is `testnet`, the file is located in `./conf/testnet.json`.

2. The value can be overridden by an environment variable with the parameter name in uppercase.

Here is a list of the available parameters:

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `verbose` | `--verbose` | `-v` | `VERBOSE` | Verbosity level | - | Parsed from the number of occurrences: `-v` for `Warning`, `-vv` for `Info`, `-vvv` for `Debug` and `-vvvv` for `Trace` | :heavy_check_mark: |
| `run_mode` | `--run-mode` | - | `RUN_MODE` | Runtime mode | `dev` | - | :heavy_check_mark: |
| `network` | - | - | `NETWORK` | Cardano network | - | `testnet` or `mainnet` or `devnet` | :heavy_check_mark: |
| `aggregator_endpoint` | `--aggregator-endpoint` | - | `AGGREGATOR_ENDPOINT` | Aggregator node endpoint | - | `https://aggregator.pre-release-preview.api.mithril.network/aggregator` | :heavy_check_mark: |
| `genesis_verification_key` | - | - | `GENESIS_VERIFICATION_KEY` | Genesis verification key | - | - | :heavy_check_mark: |
| `json_output` | `--json` | `-j` | - | Enable JSON output | no | - | - |

`snapshot show` command:

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `digest` | `--digest` | - | `DIGEST` | Snapshot digest or `latest` for the latest digest | - | - | :heavy_check_mark: |

`snapshot download` command:

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `digest` | `--digest` | - | `DIGEST` | Snapshot digest or `latest` for the latest digest | - | - | :heavy_check_mark: |
| `download_dir` | `--download-dir` | - | - | Directory where the snapshot will be downloaded | . | - | - |

`mithril-stake-distribution download` command:

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `artifact_hash` | `--artifact-hash` | - | - | Hash of the Mithril stake distribution artifact or `latest` for the latest artifact | - | - | :heavy_check_mark: |
| `download_dir` | `--download-dir` | - | - | Directory where the Mithril stake distribution will be downloaded | . | - | - |

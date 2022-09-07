---
sidebar_position: 2
---

# Mithril Signer Node

:::info

This is the node of the **Mithril Network** responsible for producing individual signatures that are collected and aggregated by the **Mithril Aggregator**.

:::

:::tip

* For more information about the **Mithril Network**, please refer to the [Architecture](../../../mithril/mithril-network/architecture.md) page.

* For more information about the **Mithril Signer**, please refer to the [Signer Node](../../../mithril/mithril-network/signer.md) page.

* Checkout the [`Run a Mithril Signer node (SPO)`](../../getting-started/run-mithril-devnet.md) guide.

:::

## Resources

| Node | Source Repository | Rust Documentation | Docker Packages |
|:-:|:-----------------:|:------------------:|:---------------:|
**Mithril Signer** | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/tree/main/mithril-signer) | [:arrow_upper_right:](https://mithril.network/mithril-signer/doc/mithril_signer/index.html) | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/pkgs/container/mithril-signer)

## Pre-requisites

* Install a [correctly configured](https://www.rust-lang.org/learn/get-started) Rust toolchain (latest stable version)

* Install OpenSSL development libraries, for example on Ubuntu/Debian/Mint run `apt install libssl-dev`

## Download source

Download from Github (HTTPS)

```bash
git clone https://github.com/input-output-hk/mithril.git
```

Or (SSH)

```bash
git clone git@github.com:input-output-hk/mithril.git
```

Change directory

```bash
cd mithril/mithril-signer
```

## Development test and build

Run tests

```bash
make test
```

Create the help menu

```bash
make help
```

Generate the Rust documentation

```bash
make doc
```

Run in debug mode with default configuration

```bash
make debug
```

## Release build and run binary

Build and run in release with default configuration

```bash
make run
```

Or, build only in release

```bash
make build
```

Display the help menu

```bash
./mithril-signer --help
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

Run in release with default configuration

```bash
./mithril-signer
```

Run in release with a specific mode

```bash
./mithril-signer -r testnet
```

Run in release with a custom configuration via env vars

```bash
NETWORK=testnet AGGREGATOR_ENDPOINT=https://aggregator.api.mithril.network/aggregator ./mithril-signer
```

:::tip

If you want to dig deeper, you can get access to several level of logs from the Mithril Signer:

* Add `-v` for some logs (WARN)
* Add `-vv` for more logs (INFO)
* Add `-vvv` for even more logs (DEBUG)
* Add `-vvvv` for all logs (TRACE)

:::

## Build and run Docker container

Build a local Docker image

```bash
make docker-build
```

Run a local Docker container

```bash
make docker-run
```

## Configuration parameters

The configuration parameters are set either:

* In a configuration file (depending on the `--run-mode` parameter). If runtime mode is `testnet` the file is located in `./conf/testnet.json`.
* The value can be overriden by an environment variable whose name is the parameter name uppercased.

Here is a list of the available parameters:

| Parameter | Command Line (long) |  Command Line (short) | Environment Variable | Description | Default Value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `verbose` | `--verbose` | `-v` | `VERBOSE` | Verbosity level | - | Parsed from number of occurences: `-v` for `Warning`, `-vv` for `Info`, `-vvv` for `Debug` and `-vvvv` for `Trace` | :heavy_check_mark: |
| `run_mode` | `--run-mode` | `-r` | `RUN_MODE` | Runtime mode | `dev` | - | :heavy_check_mark: |
| `db_directory` | `--db-directory` | - | `DB_DIRECTORY` | Directory to snapshot from the **Cardano Node** | `/db` | - | :heavy_check_mark: |
| `network` | - | - | `NETWORK` | Cardano network | - | `testnet` or `mainnet` or `devnet` | :heavy_check_mark: |
`network_magic` | - | - | `NETWORK_MAGIC` | Cardano Network Magic number (for `testnet` and `devnet`) | - | `1097911063` or `42` | - |
| `party_id` | - | - | `PARTY_ID` | Party Id of the signer, usually the `PoolId` of the SPO | - | `pool1pxaqe80sqpde7902er5kf6v0c7y0sv6d5g676766v2h829fvs3x` | :heavy_check_mark: |
| `run_interval` | - | - | `RUN_INTERVAL` | Interval between two runtime cycles in ms | - | `60000` | :heavy_check_mark: |
| `aggregator_endpoint` | - | - | `AGGREGATOR_ENDPOINT` | Aggregator node endpoint | - | `https://aggregator.api.mithril.network/aggregator` | :heavy_check_mark: |
| `data_stores_directory` | - | - | `DATA_STORES_DIRECTORY` | Directory to store signer data (Stakes, Protocol initializers, ...) | - | `./mithril-signer/stores` | :heavy_check_mark: |

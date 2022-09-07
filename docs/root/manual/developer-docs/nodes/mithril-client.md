---
sidebar_position: 3
---

# Mithril Client Node

:::info

This is the node of the **Mithril Network** responsible for restoring the **Cardano** blockchain on an empty node from a certified snapshot.

:::

:::tip

* For more information about the **Mithril Network**, please refer to the [Architecture](../../../mithril/mithril-network/architecture.md) page.

* For more information about the **Mithril Client**, please refer to the [Client Node](../../../mithril/mithril-network/client.md) page.

* Checkout the [`Bootstrap a Cardano Node`](../../getting-started/bootstrap-cardano-node.md) guide.

:::

## Resources

| Node | Source Repository | Rust Documentation | Docker Packages |
|:-:|:-----------------:|:------------------:|:---------------:|
**Mithril Client** | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/tree/main/mithril-client) | [:arrow_upper_right:](https://mithril.network/mithril-client/doc/mithril_client/index.html) | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/pkgs/container/mithril-client)

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
cd mithril/mithril-client
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
./mithril-client --help
```

You should see

```bash
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

Run in release with default configuration

```bash
./mithril-client
```

Run in release with a specific mode

```bash
./mithril-client -r testnet
```

Run in release with a custom configuration via env vars

```bash
NETWORK=testnet AGGREGATOR_ENDPOINT=https://aggregator.api.mithril.network/aggregator ./mithril-client
```

:::tip

If you want to dig deeper, you can get access to several level of logs from the Mithril Client:

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

## Subcommands

Here are the subcommands available:

| Subcommand | Performed action |
|------------|------------------|
| **download** | Download a snapshot|
| **help** | Print this message or the help of the given subcommand(s)|
| **list** | List available snapshots|
| **restore** | Restore a snapshot|
| **show** | Infos about a snapshot|

## Configuration parameters

The configuration parameters are set either:

* In a configuration file (depending on the `--run-mode` parameter). If runtime mode is `testnet` the file is located in `./conf/testnet.json`.
* The value can be overriden by an environment variable whose name is the parameter name uppercased.

Here is a list of the available parameters:

| Parameter | Command Line (long) |  Command Line (short) | Environment Variable | Description | Default Value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `verbose` | `--verbose` | `-v` | `VERBOSE` | Verbosity level | - | Parsed from number of occurences: `-v` for `Warning`, `-vv` for `Info`, `-vvv` for `Debug` and `-vvvv` for `Trace` | :heavy_check_mark: |
| `run_mode` | `--run-mode` | `-r` | `RUN_MODE` | Runtime mode | `dev` | - | :heavy_check_mark: |
| `network` | - | - | `NETWORK` | Cardano network | - | `testnet` or `mainnet` or `devnet` | :heavy_check_mark: |
| `aggregator_endpoint` | - | - | `AGGREGATOR_ENDPOINT` | Aggregator node endpoint | - | `https://aggregator.api.mithril.network/aggregator` | :heavy_check_mark: |

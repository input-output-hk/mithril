---
sidebar_position: 1
---

# Mithril Aggregator Node

:::info

This is the node of the **Mithril Network** responsible for collecting individual signatures from the **Mithril Signers** and aggregate them into a multisignature. The **Mithril Aggregator** uses this ability to provide certified snapshots of the **Cardano** blockchain.

:::

:::tip

* For more information about the **Mithril Network**, please refer to the [Architecture](../../../mithril/mithril-network/architecture.md) page.

* For more information about the **Mithril Aggregator**, please refer to the [Aggregator Node](../../../mithril/mithril-network/aggregator.md) page.

:::

## Resources

| Node | Source Repository | Rust Documentation | Docker Packages | REST API
|:-:|:-----------------:|:------------------:|:---------------:|
**Mithril Aggregator** | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/tree/main/mithril-aggregator) | [:arrow_upper_right:](https://mithril.network/mithril-aggregator/doc/mithril_aggregator/index.html) | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/pkgs/container/mithril-aggregator) | [:arrow_upper_right:](/aggregator-api)

## Pre-requisites

* Install a [correctly configured](https://www.rust-lang.org/learn/get-started) Rust toolchain (version 1.62.0+).
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
cd mithril/mithril-aggregator
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
./mithril-aggregator --help
```

You should see

```bash
mithril-aggregator 
Node args

USAGE:
    mithril-aggregator [OPTIONS]

OPTIONS:
        --db-directory <DB_DIRECTORY>
            Directory to snapshot [default: /db]

    -h, --help
            Print help information

    -r, --run-mode <RUN_MODE>
            Run mode [default: dev]

        --server-ip <SERVER_IP>
            Server listening IP [default: 0.0.0.0]

        --server-port <SERVER_PORT>
            Server listening port [default: 8080]

        --snapshot-directory <SNAPSHOT_DIRECTORY>
            Directory to store snapshot Defaults to work folder [default: .]

    -v, --verbose
            Verbosity level
```

Run in release with default configuration

```bash
./mithril-aggregator
```

Run in release with a specific mode

```bash
./mithril-aggregator -r testnet
```

Run in release with a custom configuration via env vars

```bash
RUN_INTERVAL=60000 NETWORK=testnet ./mithril-aggregator
```

:::tip

If you want to dig deeper, you can get access to several level of logs from the Mithril Aggregators:

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
| `server_ip` | `--server-ip` | - | `SERVER_IP` | Listening server IP | `0.0.0.0` | - | :heavy_check_mark: |  
| `server_port` | `--server-port` | - | `SERVER_PORT` | Listening server port | `8080` | - | :heavy_check_mark: |
| `verbose` | `--verbose` | `-v` | `VERBOSE` | Verbosity level | - | Parsed from number of occurences: `-v` for `Warning`, `-vv` for `Info`, `-vvv` for `Debug` and `-vvvv` for `Trace` | :heavy_check_mark: |
| `run_mode` | `--run-mode` | `-r` | `RUN_MODE` | Runtime mode | `dev` | - | :heavy_check_mark: |
| `db_directory` | `--db-directory` | - | `DB_DIRECTORY` | Directory to snapshot from the **Cardano Node** | `/db` | - | :heavy_check_mark: |
| `snapshot_directory` | `--snapshot-directory` | - | `SNAPSHOT_DIRECTORY` | Directory to store local snapshots of the **Cardano Node** | `.` | - | :heavy_check_mark: |
| `network` | - | - | `NETWORK` | Cardano network | - | `testnet` or `mainnet` or `devnet` | :heavy_check_mark: |
`network_magic` | - | - | `NETWORK_MAGIC` | Cardano Network Magic number (for `testnet` and `devnet`) | - | `1097911063` or `42` | - |
| `snapshot_store_type` | - | - | `SNAPSHOT_STORE_TYPE` | Type of snapshot store to use | - | `gcp` or `local` | :heavy_check_mark: |
| `snapshot_uploader_type` | - | - | `SNAPSHOT_UPLOADER_TYPE` | Type of snapshot uploader to use | - | `gcp` or `local` | :heavy_check_mark: |
| `run_interval` | - | - | `RUN_INTERVAL` | Interval between two runtime cycles in ms | - | `60000` | :heavy_check_mark: |
| `snapshot_store_directory` | - | - | `SNAPSHOT_STORE_DIRECTORY` | Directory to store snapshot records | - | `./mithril/snapshots_db` | :heavy_check_mark: |
| `pending_certificate_store_directory` | - | - | `PENDING_CERTIFICATE_STORE_DIRECTORY` | Directory to store pending certificates | - | `./mithril/pending_cert_db` | :heavy_check_mark: |
| `certificate_store_directory` | - | - | `CERTIFICATE_STORE_DIRECTORY` | Directory to store certificates | - | `./mithril/cert_db` | :heavy_check_mark: |
| `verification_key_store_directory` | - | - | `VERIFICATION_KEY_STORE_DIRECTORY` | Directory to store verification keys | - | `./mithril/verification_key_db` | :heavy_check_mark: |
| `stake_store_directory` | - | - | `STAKE_STORE_DIRECTORY` | Directory to store stakes | - | `./mithril/stake_db` | :heavy_check_mark: |
| `single_signature_store_directory` | - | - | `SINGLE_SIGNATURE_STORE_DIRECTORY` | Directory to store single signatures | - | `./mithril/single_signature_db` | :heavy_check_mark: |
| `cardano_cli_path` | - | - | `CARDANO_CLI_PATH` | Cardano CLI tool path | - | `cardano-cli` | :heavy_check_mark: |
| `cardano_node_socket_path` | - | - | `CARDANO_NODE_SOCKET_PATH` | Path of the socket used by the Cardano CLI tool to communicate with the Cardano node | - | `/tmp/cardano.sock` | :heavy_check_mark: |
| `url_snapshot_manifest` | - | - | `URL_SNAPSHOT_MANIFEST` | Snapshots manifest location | - | Only if `snapshot_store_type` is `gcp`, else it should be `` | :heavy_check_mark: |

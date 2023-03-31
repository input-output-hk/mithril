---
sidebar_position: 1
---

import NetworksMatrix from '../../../networks-matrix.md';
import CompiledBinaries from '../../../compiled-binaries.md'

# Mithril Aggregator Node

:::info

This is the node of the **Mithril Network** responsible for collecting individual signatures from the **Mithril Signers** and aggregate them into a multi-signature. The **Mithril Aggregator** uses this ability to provide certified snapshots of the **Cardano** blockchain.

:::

:::tip

* For more information about the **Mithril Network**, please refer to the [Architecture](../../../mithril/mithril-network/architecture.md) page.

* For more information about the **Mithril Aggregator**, please refer to the [Aggregator Node](../../../mithril/mithril-network/aggregator.md) page.

:::

:::note Mithril Networks

<NetworksMatrix />

:::

## Resources

| Node | Source Repository | Rust Documentation | Docker Packages | REST API
|:-:|:-----------------:|:------------------:|:---------------:|
**Mithril Aggregator** | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/tree/main/mithril-aggregator) | [:arrow_upper_right:](https://mithril.network/mithril-aggregator/doc/mithril_aggregator/index.html) | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/pkgs/container/mithril-aggregator) | [:arrow_upper_right:](/aggregator-api)

## Pre-requisites

* Install a [correctly configured](https://www.rust-lang.org/learn/get-started) Rust toolchain (latest stable version)

* Install OpenSSL development libraries, for example on Ubuntu/Debian/Mint run `apt install libssl-dev`

* Ensure SQLite3 library is installed on your system and its version is at least `3.40`. Run `sqlite3 --version` to check your version.

## Download source

Download from GitHub (HTTPS)

```bash
git clone https://github.com/input-output-hk/mithril.git
```

Or (SSH)

```bash
git clone git@github.com:input-output-hk/mithril.git
```

Switch to build branch / tag

```bash
# **YOUR_BUILD_BRANCH_OR_TAG** depends on the Mithril network you target, 
# please refer to the **Build From** column of the above **Mithril Networks** table
git switch **YOUR_BUILD_BRANCH_OR_TAG**
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

## Release build and run binary 'serve' command

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
Mithril Aggregator Node

Usage: mithril-aggregator [OPTIONS] <COMMAND>

Commands:
  genesis  Genesis tools
  era      Era tools
  serve    Server runtime mode
  help     Print this message or the help of the given subcommand(s)

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

Run 'serve' command in release with default configuration

```bash
./mithril-aggregator serve
```

Run 'serve' command in release with a specific mode

```bash
./mithril-aggregator -r preview serve
```

Run 'serve' command in release with a custom configuration via env vars

```bash
GENESIS_VERIFICATION_KEY=$(wget -q -O - **YOUR_GENESIS_VERIFICATION_KEY**) RUN_INTERVAL=60000 NETWORK=**YOUR_CARDANO_NETWORK** ./mithril-aggregator serve
```

## Release build and run binary 'genesis' command

Build in release with default configuration

```bash
make build
```

Display the help menu

```bash
./mithril-aggregator genesis --help
```

You should see

```bash
mithril-aggregator-genesis 
Aggregator runs in Genesis tools mode
USAGE:
    mithril-aggregator genesis <SUBCOMMAND>
OPTIONS:
    -h, --help    Print help information
SUBCOMMANDS:
    bootstrap    Bootstrap a genesis certificate Test only usage
    export       Export payload to sign with genesis secret key
    help         Print this message or the help of the given subcommand(s)
    import       Import payload signed with genesis secret key and create & import a genesis certificate
```

Run 'genesis bootstrap' command in release with default configuration, **only in test mode**.
This allows the Mithril Aggregator node to bootstrap a `Genesis Certificate`. After this operation, the Mithril Aggregator will be able to produce new snapshots and certificates.

```bash
./mithril-aggregator genesis bootstrap
```

Or with a specific `Genesis Secret Key`, **only in test mode**.

```bash
./mithril-aggregator genesis bootstrap --genesis-secret-key **YOUR_SECRET_KEY*
```

Run 'genesis export' command in release with default configuration.
This allows the Mithril Aggregator node to export the `Genesis Payload` that needs to be signed (and later reimported) of the `Genesis Certificate`. The signature of the `Genesis Payload` must be done manually with the owner of the `Genesis Secret Key`.

```bash
./mithril-aggregator genesis export
```

Or with a custom export path (to override the default value `./mithril-genesis-payload.txt`)

```bash
./mithril-aggregator genesis export --target-path **YOUR_TARGET_PATH**
```

Run 'genesis import' command in release with default configuration.
This allows the Mithril Aggregator node to import the signed payload of the `Genesis Certificate` and create it in the store. After this operation, the Mithril Aggregator will be able to produce new snapshots and certificates.

```bash
./mithril-aggregator genesis import
```

Or with a custom export path (to override the default value `./mithril-genesis-signed-payload.txt`)

```bash
./mithril-aggregator genesis import --signed-payload-path **YOUR_SIGNED_PAYLOAD_PATH**
```

Run 'genesis import' command in release with a custom configuration via env vars

```bash
GENESIS_VERIFICATION_KEY=$(wget -q -O - **YOUR_GENESIS_VERIFICATION_KEY**) RUN_INTERVAL=60000 NETWORK=**YOUR_CARDANO_NETWORK** ./mithril-aggregator genesis import
```

## Release build and run binary 'era' command

Build in release with default configuration

```bash
make build
```

Display the help menu

```bash
./mithril-aggregator era --help
```

You should see

```bash
Era tools

Usage: mithril-aggregator era <COMMAND>

Commands:
  list               Era list command
  generate-tx-datum  Era tx datum generate command
  help               Print this message or the help of the given subcommand(s)

Options:
  -h, --help  Print help
```

Run 'era list' to list the supported eras embedded in the binary

```bash
./mithril-aggregator era list
```

You should see something like

```bash
Supported Eras:
[
    Thales,
]
```

:::tip

You can use the `--json` option in order to display results in `JSON` format for the `list` command:

```bash
./mithril-aggregator era list --json
```

You should see something like

```bash
["thales"]
```

:::

Run 'era generate-tx-datum' to generate the transaction datum file to be stored on the Cardano chain that will provide era markers to the 'cardano-chain' era reader adapter

**Case 1**: There is only one supported era in the code, create the datum file with

```bash
./mithril-aggregator era generate-tx-datum --current-era-epoch **EPOCH_AT_WHICH_CURRENT_ERA_STARTS** --era-markers-secret-key **YOUR_ERA_ACTIVATION_SECRET_KEY**
```

You should see something like

```bash
{"constructor":0,"fields":[{"bytes":"5b7b226e223a227468616c6573222c2265223a317d5d"},{"bytes":"a58fe8e336f465ded3bba7c5a7afe5b5a26f2fb65b7c4e6e742e680645f13df28bf2b63a61cc72d9c826be490e2c1f1098d955df503580a4e899b5173884e30e"}]}
```

**Case 2**: There are two supported era in the code, in order to announce the upcoming era (i.e. the activation epoch of this era is not known yet), run the command

```bash
./mithril-aggregator era generate-tx-datum --current-era-epoch **EPOCH_AT_WHICH_CURRENT_ERA_STARTS** --era-markers-secret-key **YOUR_ERA_ACTIVATION_SECRET_KEY**
```

**Case 3**: There are two supported era in the code, in order to activate the era switch at a following epoch (i.e. the activation epoch of this era known), run the command

```bash
./mithril-aggregator era generate-tx-datum --current-era-epoch **EPOCH_AT_WHICH_CURRENT_ERA_STARTS** --next-era-epoch **EPOCH_AT_WHICH_NEXT_ERA_STARTS** --era-markers-secret-key **YOUR_ERA_ACTIVATION_SECRET_KEY**
```

:::tip

If you want to dig deeper, you can get access to several level of logs from the Mithril Aggregator:

* Add `-v` for some logs (WARN)
* Add `-vv` for more logs (INFO)
* Add `-vvv` for even more logs (DEBUG)
* Add `-vvvv` for all logs (TRACE)

:::


<CompiledBinaries />

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
| **serve** | Aggregator runs its HTTP server in nominal mode and orchestrates multi signatures production |
| **help** | Print this message or the help of the given subcommand(s) |
| **genesis export** | Export genesis payload to sign with genesis secret key |
| **genesis import** | Import genesis signature (payload signed with genesis secret key) and create & import a genesis certificate in the store |
| **genesis bootstrap** | Bootstrap a genesis certificate (test only usage) |
| **era list** | List the supported eras |
| **era generate-tx-datum** | Generate era markers transaction datum to be stored on chain |

## Configuration parameters

The configuration parameters are set either:

* In a configuration file (depending on the `--run-mode` parameter). If runtime mode is `testnet` the file is located in `./conf/testnet.json`.
* The value can be overridden by an environment variable whose name is the parameter name uppercased.

Here is a list of the available parameters.

General parameters:

| Parameter | Command Line (long) |  Command Line (short) | Environment Variable | Description | Default Value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `cardano_cli_path` | - | - | `CARDANO_CLI_PATH` | Cardano CLI tool path | - | `cardano-cli` | :heavy_check_mark: |
| `cardano_node_socket_path` | - | - | `CARDANO_NODE_SOCKET_PATH` | Path of the socket used by the Cardano CLI tool to communicate with the Cardano node | - | `/tmp/cardano.sock` | :heavy_check_mark: |
| `config_directory` | `--config-directory` | - | - | Directory of the configuration file | `./config` | - | - |
| `data_stores_directory` | - | - | `data_stores_directory` | Directory to store Aggregator data (Certificates, Snapshots, Protocol Parameters, ...) | - | `./mithril-aggregator/stores` | :heavy_check_mark: |
| `db_directory` | `--db-directory` | - | `DB_DIRECTORY` | Directory of the **Cardano Node** stores | `/db` | - | :heavy_check_mark: |
| `genesis_verification_key` | - | - | `GENESIS_VERIFICATION_KEY` | Genesis verification key | - | - | :heavy_check_mark: |
| `network` | - | - | `NETWORK` | Cardano network | - | `testnet` or `mainnet` or `devnet` | :heavy_check_mark: |
| `network_magic` | - | - | `NETWORK_MAGIC` | Cardano Network Magic number (for `testnet` and `devnet`) | - | `1097911063` or `42` | - |
| `protocol_parameters` | - | - | `PROTOCOL_PARAMETERS__K`, `PROTOCOL_PARAMETERS__M`, and `PROTOCOL_PARAMETERS__PHI_F` | Mithril Protocol Parameters | - | `{ k: 5, m: 100, phi_f: 0.65 }` | :heavy_check_mark: |
| `run_mode` | `--run-mode` | `-r` | `RUN_MODE` | Runtime mode | `dev` | - | :heavy_check_mark: |
| `store_retention_limit` | - | - | `STORE_RETENTION_LIMIT` | Maximum number of records in stores. If not set, no limit is set. | - | - | - |
| `verbose` | `--verbose` | `-v` | `VERBOSE` | Verbosity level | - | Parsed from number of occurrences: `-v` for `Warning`, `-vv` for `Info`, `-vvv` for `Debug` and `-vvvv` for `Trace` | :heavy_check_mark: |

`serve` command:

| Parameter | Command Line (long) |  Command Line (short) | Environment Variable | Description | Default Value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `server_ip` | `--server-ip` | - | `SERVER_IP` | Listening server IP | `0.0.0.0` | - | :heavy_check_mark: |  
| `server_port` | `--server-port` | - | `SERVER_PORT` | Listening server port | `8080` | - | :heavy_check_mark: |
| `snapshot_directory` | `--snapshot-directory` | - | `SNAPSHOT_DIRECTORY` | Directory to store local snapshots of the **Cardano Node** | `.` | - | :heavy_check_mark: |
| `snapshot_store_type` | - | - | `SNAPSHOT_STORE_TYPE` | Type of snapshot store to use | - | `gcp` or `local` | :heavy_check_mark: |
| `snapshot_uploader_type` | - | - | `SNAPSHOT_UPLOADER_TYPE` | Type of snapshot uploader to use | - | `gcp` or `local` | :heavy_check_mark: |
| `snapshot_bucket_name` | - | - | `SNAPSHOT_BUCKET_NAME` | Name of the bucket where the snapshots are stored  | - | `snapshot-bucket` | :heavy_check_mark: | Required if `snapshot_uploader_type` is `gcp`
| `run_interval` | - | - | `RUN_INTERVAL` | Interval between two runtime cycles in ms | - | `60000` | :heavy_check_mark: |
| `url_snapshot_manifest` | - | - | `URL_SNAPSHOT_MANIFEST` | Snapshots manifest location | - | Only if `snapshot_store_type` is `gcp`, else it should be `` | :heavy_check_mark: |
| `era_reader_adapter_type` | `--era-reader-adapter-type` | - | `ERA_READER_ADAPTER_TYPE` | Era reader adapter type that can be `cardano-chain`, `file` or `bootstrap`. | `bootstrap` | - | - |
| `era_reader_adapter_params` | `--era-reader-adapter-params` | - | `ERA_READER_ADAPTER_PARAMS` | Era reader adapter params that is an optional JSON encoded parameters structure that is expected depending on the `era_reader_adapter_type` parameter | - | - | - |

`genesis bootstrap` command:

| Parameter | Command Line (long) |  Command Line (short) | Environment Variable | Description | Default Value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `genesis_secret_key` | - | - | `GENESIS_SECRET_KEY` | Genesis secret key, :warning: for test only | - | - | - |

`genesis import` command:

| Parameter | Command Line (long) |  Command Line (short) | Environment Variable | Description | Default Value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `signed_payload_path` | `--signed-payload-path` | - | - | Path of the payload to import. | - | - | - | - |

`genesis export` command:

| Parameter | Command Line (long) |  Command Line (short) | Environment Variable | Description | Default Value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `target_path` | `--target-path` | - | - | Path of the file to export the payload to. | - | - | - | - |

`era list` command:

| Parameter | Command Line (long) |  Command Line (short) | Environment Variable | Description | Default Value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `json` | `--json` | - | - | Export the supported era list to JSON format. | - | - | - | - |

`era generate-tx-datum` command:

| Parameter | Command Line (long) |  Command Line (short) | Environment Variable | Description | Default Value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `current_era_epoch` | `--current-era-epoch` | - | `CURRENT_ERA_EPOCH` | Epoch at which current era starts. | - | - | - | :heavy_check_mark: |
| `next_era_epoch` | `--next-era-epoch` | - | `NEXT_ERA_EPOCH` | Epoch at which the next era starts. If not specified and an upcoming era is available, it will announce the next era. If specified, it must be strictly greater than `current-epoch-era` | - | - | - | - |
| `era_markers_secret_key` | `--era-markers-secret-key` | - | `ERA_MARKERS_SECRET_KEY` | Era Markers Secret Key that is used to verify the authenticity of the era markers on chain. | - | - | - | :heavy_check_mark: |
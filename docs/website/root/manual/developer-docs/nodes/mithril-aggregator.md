---
sidebar_position: 1
---

import NetworksMatrix from '../../../networks-matrix.md';
import CompiledBinaries from '../../../compiled-binaries.md'

# Mithril aggregator node

:::info

Mithril aggregator is responsible for collecting individual signatures from the Mithril signers and aggregating them into a multi-signature. With this capability, the Mithril aggregator can provide certified snapshots of the Cardano blockchain.

:::

:::tip

* For more information about the **Mithril network**, please see the [architecture](../../../mithril/mithril-network/architecture.md) overview.

* For more information about the **Mithril aggregator**, please see the [aggregator node](../../../mithril/mithril-network/aggregator.md) overview.

:::

:::note Mithril networks

<NetworksMatrix />

:::

## Resources

| Node | Source repository | Rust documentation | Docker packages | REST API
|:----:|:-----------------:|:------------------:|:---------------:|:--------:|
**Mithril aggregator** | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/tree/main/mithril-aggregator) | [:arrow_upper_right:](https://mithril.network/rust-doc/mithril_aggregator/index.html) | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/pkgs/container/mithril-aggregator) | [:arrow_upper_right:](/doc/aggregator-api)

## Pre-requisites

* Install the latest stable version of the [correctly configured](https://www.rust-lang.org/learn/get-started) Rust toolchain.

* Install Build Tools `build-essential` and `m4`. For example, on Ubuntu/Debian/Mint, run `sudo apt install build-essential m4`.

* Install OpenSSL development libraries. For example, on Ubuntu/Debian/Mint, run `apt install libssl-dev`.

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
cd mithril/mithril-aggregator
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

To run in debug mode with the default configuration, use the following command:

```bash
make debug
```

## Release the build and run the binary 'serve' command

To build and run in release mode with the default configuration:

```bash
make run
```

Or, build only in release mode:

```bash
make build
```

Display the help menu:

```bash
./mithril-aggregator --help
```

You should see:

```bash
Mithril aggregator Node

Usage: mithril-aggregator [OPTIONS] <COMMAND>

Commands:
  genesis  Genesis tools
  era      Era tools
  serve    Server runtime mode
  tools    List of tools to upkeep the aggregator
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

Run the 'serve' command in release mode with the default configuration:

```bash
./mithril-aggregator serve
```

Run the 'serve' command in release with a specific mode:

```bash
./mithril-aggregator -r preview serve
```

Run the 'serve' command in release mode with a custom configuration using environment variables:

```bash
GENESIS_VERIFICATION_KEY=$(wget -q -O - **YOUR_GENESIS_VERIFICATION_KEY**) RUN_INTERVAL=60000 NETWORK=**YOUR_CARDANO_NETWORK** ./mithril-aggregator serve
```

## Release the build and run the binary 'genesis' command

Build in release mode with the default configuration:

```bash
make build
```

Display the help menu:

```bash
./mithril-aggregator genesis --help
```

You should see:

```bash
mithril-aggregator-genesis 
The aggregator runs in Genesis tools mode
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

### Bootstrap sub-command (test-only)

You can run the 'genesis bootstrap' command in release mode with the default configuration, but **only in test mode**. This will enable the Mithril aggregator node to bootstrap a `genesis certificate`. After completing this operation, the Mithril aggregator will be capable of producing new snapshots and certificates.

```bash
./mithril-aggregator genesis bootstrap
```

You can use a specific `genesis secret key` (only in test mode):

```bash
./mithril-aggregator genesis bootstrap --genesis-secret-key **YOUR_SECRET_KEY*
```

### Export sub-command

You can run the 'genesis export' command in release mode. This allows the Mithril aggregator node to export the `genesis payload` that needs to be signed (and later reimported) for the `genesis certificate`. The signature of the `genesis payload` must be done manually by the owner of the `genesis secret key`.

```bash
./mithril-aggregator genesis export --target-path **YOUR_TARGET_PATH**
```

### Sign sub-command

You can run the 'genesis sign' command in release mode. This allows the Mithril aggregator node to sign the `genesis payload` that needs to be reimported. The signature of the `genesis payload` must be done manually by the owner of the `genesis secret key`.

```bash
./mithril-aggregator genesis sign --to-sign-payload-path **TO_SIGN_PAYLOAD_PATH** --target-signed-payload-path **TARGET_SIGNED_PAYLOAD_PATH** --genesis-secret-key-path **GENESIS_SECRET_KEY_PATH**
```

### Import sub-command

Run the 'genesis import' command in release mode. This allows the aggregator node to import the signed payload of the `genesis certificate` and store it. After this operation, the  aggregator will be able to produce new snapshots and certificates.

```bash
./mithril-aggregator genesis import --signed-payload-path **YOUR_SIGNED_PAYLOAD_PATH**
```

Run the 'genesis import' command in release mode with a custom configuration using environment variables: 

```bash
GENESIS_VERIFICATION_KEY=$(wget -q -O - **YOUR_GENESIS_VERIFICATION_KEY**) RUN_INTERVAL=60000 NETWORK=**YOUR_CARDANO_NETWORK** ./mithril-aggregator genesis import
```

## Release the build and run the binary 'era' command

Build in release mode using the default configuration:

```bash
make build
```

Display the help menu:

```bash
./mithril-aggregator era --help
```

You should see:

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

Run the 'era list' command to list the supported eras embedded in the binary:

```bash
./mithril-aggregator era list
```

You should see something like:

```bash
Supported Eras:
[
    Thales,
]
```

:::tip

To display results in `JSON` format for the `list` command, simply use the `--json` option:

```bash
./mithril-aggregator era list --json
```

You should see something like:

```bash
["thales"]
```

:::

You can run 'era generate-tx-datum' to create the transaction datum file that will be stored on the Cardano chain, providing era markers to the 'cardano-chain' era reader adapter.

**Case 1**: If there is only one supported era in the code, create the datum file:

```bash
./mithril-aggregator era generate-tx-datum --current-era-epoch **EPOCH_AT_WHICH_CURRENT_ERA_STARTS** --era-markers-secret-key **YOUR_ERA_ACTIVATION_SECRET_KEY** --target-path **TARGET_PATH**
```

You should see something like:

```bash
{"constructor":0,"fields":[{"bytes":"5b7b226e223a227468616c6573222c2265223a317d5d"},{"bytes":"a58fe8e336f465ded3bba7c5a7afe5b5a26f2fb65b7c4e6e742e680645f13df28bf2b63a61cc72d9c826be490e2c1f1098d955df503580a4e899b5173884e30e"}]}
```

**Case 2**: If there are two supported eras in the code and the activation epoch of the upcoming era is not yet known, run the command:

```bash
./mithril-aggregator era generate-tx-datum --current-era-epoch **EPOCH_AT_WHICH_CURRENT_ERA_STARTS** --era-markers-secret-key **YOUR_ERA_ACTIVATION_SECRET_KEY** --target-path **TARGET_PATH**
```

**Case 3**: If there are two supported eras in the code and the activation epoch of the era switch is known to be at the following epoch, run the command:

```bash
./mithril-aggregator era generate-tx-datum --current-era-epoch **EPOCH_AT_WHICH_CURRENT_ERA_STARTS** --next-era-epoch **EPOCH_AT_WHICH_NEXT_ERA_STARTS** --era-markers-secret-key **YOUR_ERA_ACTIVATION_SECRET_KEY** --target-path **TARGET_PATH**
```

## Release the build and run the binary 'tools' command

Build in release mode using the default configuration:

```bash
make build
```

Display the help menu:

```bash
./mithril-aggregator tools --help
```

You should see:

```bash
List of tools to upkeep the aggregator

Usage: mithril-aggregator tools <COMMAND>

Commands:
  recompute-certificates-hash  Load all certificates in the database to recompute their hash and update all related entities
  help                         Print this message or the help of the given subcommand(s)

Options:
  -h, --help  Print help
```

Run the 'tools recompute-certificates-hash' command in release mode with the default configuration. This allows the Mithril aggregator node to recompute all of its certificate hashes, which is useful to avoid a chain re-genesis after an update that changes the structure of the certificates.

```bash
./mithril-aggregator tools recompute-certificates-hash
```

:::tip

If you wish to delve deeper and access several levels of logs from the Mithril aggregator, use the following:

* Add `-v` for some logs (WARN)
* Add `-vv` for more logs (INFO)
* Add `-vvv` for even more logs (DEBUG)
* Add `-vvvv` for all logs (TRACE)

:::


## Download the pre-built binary

<CompiledBinaries />

## Build and run the Docker container

Build a local Docker image:

```bash
make docker-build
```

Run a local Docker container:

```bash
make docker-run
```

## Subcommands

Here are the available subcommands:

| Subcommand | Performed action |
|------------|------------------|
| **serve** | The aggregator runs its HTTP server in nominal mode and orchestrates multi-signature production |
| **help** | Prints this message or the help of the given subcommand(s) |
| **genesis export** | Exports genesis payload to sign with genesis secret key |
| **genesis sign** | Signs the genesis payload with the genesis secret key |
| **genesis import** | Imports the genesis signature (the payload signed with the genesis secret key) and creates and imports a genesis certificate in the store |
| **genesis bootstrap** | Bootstraps a genesis certificate (test only usage) |
| **era list** | Lists the supported eras |
| **era generate-tx-datum** | Generates the era markers transaction datum to be stored on-chain |
| **tools recompute-certificates-hash** | Loads all certificates in the database, recomputing their hash, and updating all related entities |

## Configuration parameters

The configuration parameters can be set in either of the following ways:

1. In a configuration file, depending on the `--run-mode` parameter. If the runtime mode is `testnet`, the file is located in `./conf/testnet.json`.

2. The value can be overridden by an environment variable with the parameter name in uppercase.

Here is a list of the available parameters:

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `cardano_cli_path` | - | - | `CARDANO_CLI_PATH` | Cardano CLI tool path | - | `cardano-cli` | :heavy_check_mark: |
| `cardano_node_socket_path` | - | - | `CARDANO_NODE_SOCKET_PATH` | Path of the socket used by the Cardano CLI tool to communicate with the Cardano node | - | `/tmp/cardano.sock` | :heavy_check_mark: |
| `config_directory` | `--config-directory` | - | - | Directory of the configuration file | `./config` | - | - |
| `data_stores_directory` | - | - | `data_stores_directory` | Directory to store aggregator data (certificates, snapshots, protocol parameters, ...) | - | `./mithril-aggregator/stores` | :heavy_check_mark: |
| `db_directory` | `--db-directory` | - | `DB_DIRECTORY` | Directory of the **Cardano node** stores | `/db` | - | :heavy_check_mark: |
| `genesis_verification_key` | - | - | `GENESIS_VERIFICATION_KEY` | Genesis verification key | - | - | :heavy_check_mark: |
| `network` | - | - | `NETWORK` | Cardano network | - | `testnet` or `mainnet` or `devnet` | :heavy_check_mark: |
| `network_magic` | - | - | `NETWORK_MAGIC` | Cardano network magic number (for `testnet` and `devnet`) | - | `1097911063` or `42` | - |
| `protocol_parameters` | - | - | `PROTOCOL_PARAMETERS__K`, `PROTOCOL_PARAMETERS__M`, and `PROTOCOL_PARAMETERS__PHI_F` | Mithril protocol parameters | - | `{ k: 5, m: 100, phi_f: 0.65 }` | :heavy_check_mark: |
| `run_mode` | `--run-mode` | `-r` | `RUN_MODE` | Runtime mode | `dev` | - | :heavy_check_mark: |
| `store_retention_limit` | - | - | `STORE_RETENTION_LIMIT` | Maximum number of records in stores. If not set, no limit is set. | - | - | - |
| `verbose` | `--verbose` | `-v` | `VERBOSE` | Verbosity level | - | Parsed from the number of occurrences: `-v` for `Warning`, `-vv` for `Info`, `-vvv` for `Debug` and `-vvvv` for `Trace` | :heavy_check_mark: |

`serve` command:

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `server_ip` | `--server-ip` | - | `SERVER_IP` | Listening server IP | `0.0.0.0` | - | :heavy_check_mark: |  
| `server_port` | `--server-port` | - | `SERVER_PORT` | Listening server port | `8080` | - | :heavy_check_mark: |
| `snapshot_directory` | `--snapshot-directory` | - | `SNAPSHOT_DIRECTORY` | Directory to store local snapshots of the **Cardano node** | `.` | - | :heavy_check_mark: |
| `snapshot_store_type` | - | - | `SNAPSHOT_STORE_TYPE` | Type of snapshot store to use | - | `gcp` or `local` | :heavy_check_mark: |
| `snapshot_uploader_type` | - | - | `SNAPSHOT_UPLOADER_TYPE` | Type of snapshot uploader to use | - | `gcp` or `local` | :heavy_check_mark: |
| `snapshot_bucket_name` | - | - | `SNAPSHOT_BUCKET_NAME` | Name of the bucket where the snapshots are stored  | - | `snapshot-bucket` | :heavy_check_mark: | Required if `snapshot_uploader_type` is `gcp`
| `snapshot_use_cdn_domain` | - | - | `SNAPSHOT_USE_CDN_DOMAIN` | Use CDN domain for constructing snapshot url  | `false` | - | - | To be used if `snapshot_uploader_type` is `gcp`
| `run_interval` | - | - | `RUN_INTERVAL` | Interval between two runtime cycles in ms | - | `60000` | :heavy_check_mark: |
| `chain_observer_type` | `--chain-observer-type` | - | `CHAIN_OBSERVER_TYPE` | Chain observer type that can be `cardano-cli`, `pallas` or `fake`. | `pallas` | - | - |
| `era_reader_adapter_type` | `--era-reader-adapter-type` | - | `ERA_READER_ADAPTER_TYPE` | Era reader adapter type that can be `cardano-chain`, `file` or `bootstrap`. | `bootstrap` | - | - |
| `era_reader_adapter_params` | `--era-reader-adapter-params` | - | `ERA_READER_ADAPTER_PARAMS` | Era reader adapter params that is an optional JSON encoded parameters structure that is expected depending on the `era_reader_adapter_type` parameter | - | - | - |
| `signed_entity_types` | `--signed-entity-types` | - | `SIGNED_ENTITY_TYPES` | Signed entity types parameters (discriminants names in an ordered comma separated list) | - | `MithrilStakeDistribution,CardanoImmutableFilesFull,CardanoStakeDistribution` | - |
| `snapshot_compression_algorithm` | `--snapshot-compression-algorithm` | - | `SNAPSHOT_COMPRESSION_ALGORITHM` | Compression algorithm of the snapshot archive | `zstandard` | `gzip` or `zstandard` | - |
| `zstandard_parameters` | - | - | `ZSTANDARD_PARAMETERS__LEVEL` and `ZSTANDARD_PARAMETERS__NUMBER_OF_WORKERS` | Zstandard specific parameters | - | `{ level: 9, number_of_workers: 4 }` | - |
| `allow_unparsable_block` | `--allow-unparsable-block` | - | `ALLOW_UNPARSABLE_BLOCK` | If set no error is returned in case of unparsable block and an error log is written instead. Will be ignored on (pre)production networks. | `false` | - | - |
| `cardano_transactions_signing_config` | - | - | `CARDANO_TRANSACTIONS_SIGNING_CONFIG__SECURITY_PARAMETER` and `CARDANO_TRANSACTIONS_SIGNING_CONFIG__STEP` | Cardano transactions signing configuration | - | `{ security_parameter: 3000, step: 120 }` | - |
| `cardano_transactions_prover_cache_pool_size` | `--cardano-transactions-prover-cache-pool-size` | - | `CARDANO_TRANSACTIONS_PROVER_CACHE_POOL_SIZE` | Cardano transactions prover cache pool size | `10` | `10` | - |
| `cardano_transactions_database_connection_pool_size` | `--cardano-transactions-database-connection-pool-size` | - | `CARDANO_TRANSACTIONS_DATABASE_CONNECTION_POOL_SIZE` | Cardano transactions database connection pool size | `10` | `10` | - |
| `cardano_transactions_prover_max_hashes_allowed_by_request` | `--cardano-transactions-prover-max-hashes-allowed-by-request` | - | `CARDANO_TRANSACTIONS_PROVER_MAX_HASHES_ALLOWED_BY_REQUEST` | Maximum number of transactions hashes allowed by request to the prover of the Cardano transactions | `100` | `100` | - |
| `cardano_transactions_block_streamer_max_roll_forwards_per_poll` | `--cardano-transactions-block-streamer-max-roll-forwards-per-poll` | - | `CARDANO_TRANSACTIONS_BLOCK_STREAMER_MAX_ROLL_FORWARDS_PER_POLL` | Maximum number of roll forwards during a poll of the block streamer when importing transactions | `1000` | `1000` | - |
| `cardano_transactions_signing_config` | `--cardano-transactions-signing-config` | - | `CARDANO_TRANSACTIONS_SIGNING_CONFIG` | Cardano transactions signing configuration | `{ "security_parameter": 3000, "step": 120 }` | `{ "security_parameter": 3000, "step": 120 }` | - | 


`genesis bootstrap` command:

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `genesis_secret_key` | - | - | `GENESIS_SECRET_KEY` | Genesis secret key, :warning: for test only | - | - | - |

`genesis export` command:

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `target_path` | `--target-path` | - | - | Path of the file to export the payload to. | - | - | - | - |

`genesis import` command:

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `signed_payload_path` | `--signed-payload-path` | - | - | Path of the payload to import. | - | - | - | - |

`genesis sign` command:

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `to_sign_payload_path` | `--to-sign-payload-path` | - | - | Path of the payload to sign. | - | - | - | - |
| `target_signed_payload_path` | `--target-signed-payload-path` | - | - | Path of the signed payload to export. | - | - | - | - |
| `genesis_secret_key_path` | `--genesis-secret-key-path` | - | - | Path of the genesis secret key. | - | - | - |

`era list` command:

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `json` | `--json` | - | - | Export the supported era list to JSON format. | - | - | - | - |

`era generate-tx-datum` command:

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `current_era_epoch` | `--current-era-epoch` | - | `CURRENT_ERA_EPOCH` | Epoch at which current era starts. | - | - | - | :heavy_check_mark: |
| `next_era_epoch` | `--next-era-epoch` | - | `NEXT_ERA_EPOCH` | Epoch at which the next era starts. If not specified and an upcoming era is available, it will announce the next era. If specified, it must be strictly greater than `current-epoch-era` | - | - | - | - |
| `era_markers_secret_key` | `--era-markers-secret-key` | - | `ERA_MARKERS_SECRET_KEY` | Era markers secret key that is used to verify the authenticity of the era markers on the chain. | - | - | - | :heavy_check_mark: |
| `target_path` | `--target-path` | - | - | Path of the file to export the payload to. | - | - | - | - |

The `tools recompute-certificates-hash` command has no dedicated parameters. 

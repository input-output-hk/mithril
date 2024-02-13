

### mithril-aggregator

Mithril Aggregator Node
```bash
Mithril Aggregator Node

Usage: mithril-aggregator [OPTIONS] <COMMAND>

Commands:
  genesis       Genesis tools
  era           Era tools
  serve         Server runtime mode
  tools         List of tools to upkeep the aggregator
  generate-doc  Generate documentation
  help          Print this message or the help of the given subcommand(s)

Options:
  -r, --run-mode <RUN_MODE>
          Run Mode
          
          [default: dev]

  -v, --verbose...
          Verbosity level

      --db-directory <DB_DIRECTORY>
          Directory of the Cardano node files

      --config-directory <CONFIG_DIRECTORY>
          Directory where configuration file is located
          
          [default: ./config]

  -h, --help
          Print help

  -V, --version
          Print version

```
| Subcommand | Aliases | Performed action |
|------------|---------|------------------|
| **genesis** |  | Genesis tools |
| **era** |  | Era tools |
| **serve** |  | Server runtime mode |
| **tools** |  | List of tools to upkeep the aggregator |
| **generate-doc** | doc | Generate documentation |
| **help** |  | Print this message or the help of the given subcommand(s) |

The configuration parameters can be set in either of the following ways:

1. In a configuration file, depending on the `--run-mode` parameter. If the runtime mode is `testnet`, the file is located in `./conf/testnet.json`.

2. The value can be overridden by an environment variable with the parameter name in uppercase.

| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `cardano_cli_path` | - | - | `CARDANO_CLI_PATH` | Cardano CLI tool path | - | `cardano-cli` | - |
| `cardano_node_socket_path` | - | - | `CARDANO_NODE_SOCKET_PATH` | Path of the socket used by the Cardano CLI tool<br>to communicate with the Cardano node | - | `/tmp/cardano.sock` | - |
| `cardano_node_version` | - | - | `CARDANO_NODE_VERSION` | Cardano node version.<br><br>**NOTE**: This cannot be verified for now (see [this<br>issue](https://github.com/input-output-hk/cardano-cli/issues/224)). This<br>is why it has to be manually given to the Aggregator | - | - | - |
| `cexplorer_pools_url` | - | - | `CEXPLORER_POOLS_URL` | Url to CExplorer list of pools to import as signer in the database. | - | - | - |
| `chain_observer_type` | - | - | `CHAIN_OBSERVER_TYPE` | Cardano chain observer type | - | - | - |
| `config_directory` | `--config-directory` | - | `CONFIG_DIRECTORY` | Directory where configuration file is located | `./config` | - | - |
| `data_stores_directory` | - | - | `DATA_STORES_DIRECTORY` | Directory to store aggregator data (Certificates, Snapshots, Protocol Parameters, ...) | - | `./mithril-aggregator/stores` | - |
| `db_directory` | `--db-directory` | - | `DB_DIRECTORY` | Directory of the Cardano node files | `/db` | - | - |
| `disable_digests_cache` | - | - | `DISABLE_DIGESTS_CACHE` | Use the digest caching strategy | `false` | - | - |
| `environment` | - | - | `ENVIRONMENT` | What kind of runtime environment the configuration is meant to. | `Production` | - | - |
| `era_reader_adapter_params` | - | - | `ERA_READER_ADAPTER_PARAMS` | Era reader adapter parameters | - | - | - |
| `era_reader_adapter_type` | - | - | `ERA_READER_ADAPTER_TYPE` | Era reader adapter type | `bootstrap` | - | - |
| `genesis_verification_key` | - | - | `GENESIS_VERIFICATION_KEY` | Genesis verification key | - | - | - |
| `help` | `--help` | `-h` | `HELP` | Print help | - | - | - |
| `network` | - | - | `NETWORK` | Cardano network | - | `testnet` or `mainnet` or `devnet` | - |
| `network_magic` | - | - | `NETWORK_MAGIC` | Cardano Network Magic number<br><br>useful for TestNet & DevNet | - | `1097911063` or `42` | - |
| `protocol_parameters` | - | - | `PROTOCOL_PARAMETERS` | Protocol parameters | - | `{ k: 5, m: 100, phi_f: 0.65 }` | - |
| `reset_digests_cache` | - | - | `RESET_DIGESTS_CACHE` | Should the immutable cache be reset or not | `false` | - | - |
| `run_interval` | - | - | `RUN_INTERVAL` | Run Interval is the interval between two runtime cycles in ms | - | `60000` | - |
| `run_mode` | `--run-mode` | `-r` | `RUN_MODE` | Run Mode | `dev` | - | - |
| `server_ip` | - | - | `SERVER_IP` | Server listening IP | `0.0.0.0` | - | - |
| `server_port` | - | - | `SERVER_PORT` | Server listening port | `8080` | - | - |
| `signed_entity_types` | - | - | `SIGNED_ENTITY_TYPES` | Signed entity types parameters (discriminants names in an ordered comma separated list).     | - | `MithrilStakeDistribution,CardanoImmutableFilesFull,CardanoStakeDistribution` | - |
| `signer_importer_run_interval` | - | - | `SIGNER_IMPORTER_RUN_INTERVAL` | Time interval at which the signers in [Self::cexplorer_pools_url] will be imported (in minutes). | `720` | - | - |
| `snapshot_bucket_name` | - | - | `SNAPSHOT_BUCKET_NAME` | Bucket name where the snapshots are stored if snapshot_uploader_type is Gcp | - | - | - |
| `snapshot_compression_algorithm` | - | - | `SNAPSHOT_COMPRESSION_ALGORITHM` | Compression algorithm used for the snapshot archive artifacts. | `zstandard` | `gzip` or `zstandard` | - |
| `snapshot_directory` | - | - | `SNAPSHOT_DIRECTORY` | Directory to store snapshot | `.` | - | - |
| `snapshot_store_type` | - | - | `SNAPSHOT_STORE_TYPE` | Type of snapshot store to use | `local` | `gcp` or `local` | - |
| `snapshot_uploader_type` | - | - | `SNAPSHOT_UPLOADER_TYPE` | Type of snapshot uploader to use | `gcp` | `gcp` or `local` | - |
| `snapshot_use_cdn_domain` | - | - | `SNAPSHOT_USE_CDN_DOMAIN` | Use CDN domain to construct snapshot urls if snapshot_uploader_type is Gcp | `false` | - | - |
| `store_retention_limit` | - | - | `STORE_RETENTION_LIMIT` | Max number of records in stores.<br>When new records are added, oldest records are automatically deleted so<br>there can always be at max the number of records specified by this<br>setting. | - | - | - |
| `verbose` | `--verbose` | `-v` | `VERBOSE` | Verbosity level | `0` | - | - |
| `version` | `--version` | `-V` | `VERSION` | Print version | - | - | - |
| `zstandard_parameters` | - | - | `ZSTANDARD_PARAMETERS` | Specific parameters when [snapshot_compression_algorithm][Self::snapshot_compression_algorithm]<br>is set to [zstandard][CompressionAlgorithm::Zstandard]. | - | `{ level: 9, number_of_workers: 4 }` | - |
####  mithril-aggregator genesis

Genesis tools
```bash
Genesis tools

Usage: genesis <COMMAND>

Commands:
  export     Genesis certificate export command
  import     Genesis certificate import command
  sign       Genesis certificate sign command
  bootstrap  Genesis certificate bootstrap command
  help       Print this message or the help of the given subcommand(s)

Options:
  -h, --help
          Print help

```
| Subcommand | Aliases | Performed action |
|------------|---------|------------------|
| **export** |  | Genesis certificate export command |
| **import** |  | Genesis certificate import command |
| **sign** |  | Genesis certificate sign command |
| **bootstrap** |  | Genesis certificate bootstrap command |
| **help** |  | Print this message or the help of the given subcommand(s) |

#####  mithril-aggregator  genesis export

Genesis certificate export command
```bash
Genesis certificate export command

Usage: export --target-path <TARGET_PATH>

Options:
      --target-path <TARGET_PATH>
          Target Path

  -h, --help
          Print help

```


The configuration parameters can be set in either of the following ways:

1. In a configuration file, depending on the `--run-mode` parameter. If the runtime mode is `testnet`, the file is located in `./conf/testnet.json`.

2. The value can be overridden by an environment variable with the parameter name in uppercase.

| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `target_path` | `--target-path` | - | `TARGET_PATH` | Target Path | - | - | :heavy_check_mark: |
| `help` | `--help` | `-h` | `HELP` | Print help | - | - | - |

#####  mithril-aggregator  genesis import

Genesis certificate import command
```bash
Genesis certificate import command

Usage: import --signed-payload-path <SIGNED_PAYLOAD_PATH>

Options:
      --signed-payload-path <SIGNED_PAYLOAD_PATH>
          Signed Payload Path

  -h, --help
          Print help

```


The configuration parameters can be set in either of the following ways:

1. In a configuration file, depending on the `--run-mode` parameter. If the runtime mode is `testnet`, the file is located in `./conf/testnet.json`.

2. The value can be overridden by an environment variable with the parameter name in uppercase.

| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `signed_payload_path` | `--signed-payload-path` | - | `SIGNED_PAYLOAD_PATH` | Signed Payload Path | - | - | :heavy_check_mark: |
| `help` | `--help` | `-h` | `HELP` | Print help | - | - | - |

#####  mithril-aggregator  genesis sign

Genesis certificate sign command
```bash
Genesis certificate sign command

Usage: sign --to-sign-payload-path <TO_SIGN_PAYLOAD_PATH> --target-signed-payload-path <TARGET_SIGNED_PAYLOAD_PATH> --genesis-secret-key-path <GENESIS_SECRET_KEY_PATH>

Options:
      --to-sign-payload-path <TO_SIGN_PAYLOAD_PATH>
          To Sign Payload Path

      --target-signed-payload-path <TARGET_SIGNED_PAYLOAD_PATH>
          Target Signed Payload Path

      --genesis-secret-key-path <GENESIS_SECRET_KEY_PATH>
          Genesis Secret Key Path

  -h, --help
          Print help

```


The configuration parameters can be set in either of the following ways:

1. In a configuration file, depending on the `--run-mode` parameter. If the runtime mode is `testnet`, the file is located in `./conf/testnet.json`.

2. The value can be overridden by an environment variable with the parameter name in uppercase.

| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `to_sign_payload_path` | `--to-sign-payload-path` | - | `TO_SIGN_PAYLOAD_PATH` | To Sign Payload Path | - | - | :heavy_check_mark: |
| `target_signed_payload_path` | `--target-signed-payload-path` | - | `TARGET_SIGNED_PAYLOAD_PATH` | Target Signed Payload Path | - | - | :heavy_check_mark: |
| `genesis_secret_key_path` | `--genesis-secret-key-path` | - | `GENESIS_SECRET_KEY_PATH` | Genesis Secret Key Path | - | - | :heavy_check_mark: |
| `help` | `--help` | `-h` | `HELP` | Print help | - | - | - |

#####  mithril-aggregator  genesis bootstrap

Genesis certificate bootstrap command
```bash
Genesis certificate bootstrap command

Usage: bootstrap --genesis-secret-key <GENESIS_SECRET_KEY>

Options:
      --genesis-secret-key <GENESIS_SECRET_KEY>
          Genesis Secret Key (test only)
          
          [env: GENESIS_SECRET_KEY=]

  -h, --help
          Print help

```


The configuration parameters can be set in either of the following ways:

1. In a configuration file, depending on the `--run-mode` parameter. If the runtime mode is `testnet`, the file is located in `./conf/testnet.json`.

2. The value can be overridden by an environment variable with the parameter name in uppercase.

| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `genesis_secret_key` | `--genesis-secret-key` | - | `GENESIS_SECRET_KEY` | Genesis Secret Key (test only) | - | - | :heavy_check_mark: |
| `help` | `--help` | `-h` | `HELP` | Print help | - | - | - |

####  mithril-aggregator era

Era tools
```bash
Era tools

Usage: era <COMMAND>

Commands:
  list               Era list command
  generate-tx-datum  Era tx datum generate command
  help               Print this message or the help of the given subcommand(s)

Options:
  -h, --help
          Print help

```
| Subcommand | Aliases | Performed action |
|------------|---------|------------------|
| **list** |  | Era list command |
| **generate-tx-datum** |  | Era tx datum generate command |
| **help** |  | Print this message or the help of the given subcommand(s) |

#####  mithril-aggregator  era list

Era list command
```bash
Era list command

Usage: list [OPTIONS]

Options:
      --json
          Enable JSON output

  -h, --help
          Print help

```


The configuration parameters can be set in either of the following ways:

1. In a configuration file, depending on the `--run-mode` parameter. If the runtime mode is `testnet`, the file is located in `./conf/testnet.json`.

2. The value can be overridden by an environment variable with the parameter name in uppercase.

| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `json` | `--json` | - | `JSON` | Enable JSON output | `false` | - | - |
| `help` | `--help` | `-h` | `HELP` | Print help | - | - | - |

#####  mithril-aggregator  era generate-tx-datum

Era tx datum generate command
```bash
Era tx datum generate command

Usage: generate-tx-datum [OPTIONS] --current-era-epoch <CURRENT_ERA_EPOCH> --era-markers-secret-key <ERA_MARKERS_SECRET_KEY> --target-path <TARGET_PATH>

Options:
      --current-era-epoch <CURRENT_ERA_EPOCH>
          Current Era epoch
          
          [env: CURRENT_ERA_EPOCH=]

      --next-era-epoch <NEXT_ERA_EPOCH>
          Next Era epoch start, if exists
          
          [env: NEXT_ERA_EPOCH=]

      --era-markers-secret-key <ERA_MARKERS_SECRET_KEY>
          Era Markers Secret Key
          
          [env: ERA_MARKERS_SECRET_KEY=]

      --target-path <TARGET_PATH>
          Target Path

  -h, --help
          Print help

```


The configuration parameters can be set in either of the following ways:

1. In a configuration file, depending on the `--run-mode` parameter. If the runtime mode is `testnet`, the file is located in `./conf/testnet.json`.

2. The value can be overridden by an environment variable with the parameter name in uppercase.

| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `current_era_epoch` | `--current-era-epoch` | - | `CURRENT_ERA_EPOCH` | Current Era epoch | - | - | :heavy_check_mark: |
| `next_era_epoch` | `--next-era-epoch` | - | `NEXT_ERA_EPOCH` | Next Era epoch start, if exists | - | - | - |
| `era_markers_secret_key` | `--era-markers-secret-key` | - | `ERA_MARKERS_SECRET_KEY` | Era Markers Secret Key | - | - | :heavy_check_mark: |
| `target_path` | `--target-path` | - | `TARGET_PATH` | Target Path | - | - | :heavy_check_mark: |
| `help` | `--help` | `-h` | `HELP` | Print help | - | - | - |

####  mithril-aggregator serve

Server runtime mode
```bash
Server runtime mode

Usage: serve [OPTIONS]

Options:
      --server-ip <SERVER_IP>
          Server listening IP

      --server-port <SERVER_PORT>
          Server TCP port

      --snapshot-directory <SNAPSHOT_DIRECTORY>
          Directory to store snapshot Defaults to work folder

      --disable-digests-cache
          Disable immutables digests cache

      --reset-digests-cache
          If set the existing immutables digests cache will be reset.
          
          Will be ignored if set in conjunction with `--disable-digests-cache`.

  -h, --help
          Print help (see a summary with '-h')

```


The configuration parameters can be set in either of the following ways:

1. In a configuration file, depending on the `--run-mode` parameter. If the runtime mode is `testnet`, the file is located in `./conf/testnet.json`.

2. The value can be overridden by an environment variable with the parameter name in uppercase.

| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `server_ip` | `--server-ip` | - | `SERVER_IP` | Server listening IP | - | - | - |
| `server_port` | `--server-port` | - | `SERVER_PORT` | Server TCP port | - | - | - |
| `snapshot_directory` | `--snapshot-directory` | - | `SNAPSHOT_DIRECTORY` | Directory to store snapshot Defaults to work folder | - | - | - |
| `disable_digests_cache` | `--disable-digests-cache` | - | `DISABLE_DIGESTS_CACHE` | Disable immutables digests cache | `false` | - | - |
| `reset_digests_cache` | `--reset-digests-cache` | - | `RESET_DIGESTS_CACHE` | If set the existing immutables digests cache will be reset | `false` | - | - |
| `help` | `--help` | `-h` | `HELP` | Print help (see more with '--help') | - | - | - |

####  mithril-aggregator tools

List of tools to upkeep the aggregator
```bash
List of tools to upkeep the aggregator

Usage: tools <COMMAND>

Commands:
  recompute-certificates-hash  Load all certificates in the database to recompute their hash and update all related entities
  help                         Print this message or the help of the given subcommand(s)

Options:
  -h, --help
          Print help

```
| Subcommand | Aliases | Performed action |
|------------|---------|------------------|
| **recompute-certificates-hash** |  | Load all certificates in the database to recompute their hash and update all related entities |
| **help** |  | Print this message or the help of the given subcommand(s) |

#####  mithril-aggregator  tools recompute-certificates-hash

Load all certificates in the database to recompute their hash and update all related entities
```bash
Load all certificates in the database to recompute their hash and update all related entities.

Since it will modify the aggregator sqlite database it's strongly recommended to backup it before running this command.

Usage: recompute-certificates-hash

Options:
  -h, --help
          Print help (see a summary with '-h')

```



####  mithril-aggregator generate-doc

Generate documentation
```bash
Generate documentation

Usage: generate-doc [OPTIONS]

Options:
      --output <OUTPUT>
          Generated documentation file
          
          [default: "[PROGRAM NAME]-command-line.md"]

  -h, --help
          Print help

```


The configuration parameters can be set in either of the following ways:

1. In a configuration file, depending on the `--run-mode` parameter. If the runtime mode is `testnet`, the file is located in `./conf/testnet.json`.

2. The value can be overridden by an environment variable with the parameter name in uppercase.

| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `output` | `--output` | - | `OUTPUT` | Generated documentation file | `[PROGRAM NAME]-command-line.md` | - | - |
| `help` | `--help` | `-h` | `HELP` | Print help | - | - | - |


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

Here is a list of the available parameters:
### Configuration parameters

| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `run_mode` | `--run-mode` | `-r` |  | Run Mode | `dev` | ? | - |
| `verbose` | `--verbose` | `-v` |  | Verbosity level | `0` | ? | - |
| `db_directory` | `--db-directory` |  |  | Directory of the Cardano node files |  | ? | - |
| `config_directory` | `--config-directory` |  |  | Directory where configuration file is located | `./config` | ? | - |
| `help` | `--help` | `-h` |  | Print help |  | ? | - |
| `version` | `--version` | `-V` |  | Print version |  | ? | - |

###  mithril-aggregator genesis

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

###  mithril-aggregator  genesis export

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

Here is a list of the available parameters:
### Configuration parameters

| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `target_path` | `--target-path` |  |  | Target Path |  | ? | :heavy_check_mark: |
| `help` | `--help` | `-h` |  | Print help |  | ? | - |


###  mithril-aggregator  genesis import

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

Here is a list of the available parameters:
### Configuration parameters

| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `signed_payload_path` | `--signed-payload-path` |  |  | Signed Payload Path |  | ? | :heavy_check_mark: |
| `help` | `--help` | `-h` |  | Print help |  | ? | - |


###  mithril-aggregator  genesis sign

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

Here is a list of the available parameters:
### Configuration parameters

| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `to_sign_payload_path` | `--to-sign-payload-path` |  |  | To Sign Payload Path |  | ? | :heavy_check_mark: |
| `target_signed_payload_path` | `--target-signed-payload-path` |  |  | Target Signed Payload Path |  | ? | :heavy_check_mark: |
| `genesis_secret_key_path` | `--genesis-secret-key-path` |  |  | Genesis Secret Key Path |  | ? | :heavy_check_mark: |
| `help` | `--help` | `-h` |  | Print help |  | ? | - |


###  mithril-aggregator  genesis bootstrap

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

Here is a list of the available parameters:
### Configuration parameters

| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `genesis_secret_key` | `--genesis-secret-key` |  | `GENESIS_SECRET_KEY` | Genesis Secret Key (test only) |  | ? | :heavy_check_mark: |
| `help` | `--help` | `-h` |  | Print help |  | ? | - |


###  mithril-aggregator era

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

###  mithril-aggregator  era list

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

Here is a list of the available parameters:
### Configuration parameters

| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `json` | `--json` |  |  | Enable JSON output | `false` | ? | - |
| `help` | `--help` | `-h` |  | Print help |  | ? | - |


###  mithril-aggregator  era generate-tx-datum

Era tx datum generate command
```bash
Era tx datum generate command

Usage: generate-tx-datum [OPTIONS] --current-era-epoch <CURRENT_ERA_EPOCH> --era-markers-secret-key <ERA_MARKERS_SECRET_KEY>

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

  -h, --help
          Print help

```


The configuration parameters can be set in either of the following ways:

1. In a configuration file, depending on the `--run-mode` parameter. If the runtime mode is `testnet`, the file is located in `./conf/testnet.json`.

2. The value can be overridden by an environment variable with the parameter name in uppercase.

Here is a list of the available parameters:
### Configuration parameters

| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `current_era_epoch` | `--current-era-epoch` |  | `CURRENT_ERA_EPOCH` | Current Era epoch |  | ? | :heavy_check_mark: |
| `next_era_epoch` | `--next-era-epoch` |  | `NEXT_ERA_EPOCH` | Next Era epoch start, if exists |  | ? | - |
| `era_markers_secret_key` | `--era-markers-secret-key` |  | `ERA_MARKERS_SECRET_KEY` | Era Markers Secret Key |  | ? | :heavy_check_mark: |
| `help` | `--help` | `-h` |  | Print help |  | ? | - |


###  mithril-aggregator serve

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

Here is a list of the available parameters:
### Configuration parameters

| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `server_ip` | `--server-ip` |  |  | Server listening IP |  | ? | - |
| `server_port` | `--server-port` |  |  | Server TCP port |  | ? | - |
| `snapshot_directory` | `--snapshot-directory` |  |  | Directory to store snapshot Defaults to work folder |  | ? | - |
| `disable_digests_cache` | `--disable-digests-cache` |  |  | Disable immutables digests cache | `false` | ? | - |
| `reset_digests_cache` | `--reset-digests-cache` |  |  | If set the existing immutables digests cache will be reset | `false` | ? | - |
| `help` | `--help` | `-h` |  | Print help (see more with '--help') |  | ? | - |


###  mithril-aggregator tools

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

###  mithril-aggregator  tools recompute-certificates-hash

Load all certificates in the database to recompute their hash and update all related entities
```bash
Load all certificates in the database to recompute their hash and update all related entities.

Since it will modify the aggregator sqlite database it's strongly recommended to backup it before running this command.

Usage: recompute-certificates-hash

Options:
  -h, --help
          Print help (see a summary with '-h')

```



###  mithril-aggregator generate-doc

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

Here is a list of the available parameters:
### Configuration parameters

| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `output` | `--output` |  |  | Generated documentation file | `[PROGRAM NAME]-command-line.md` | ? | - |
| `help` | `--help` | `-h` |  | Print help |  | ? | - |


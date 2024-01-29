

### mithril-signer

An implementation of a Mithril Signer
```bash
An implementation of a Mithril Signer

Usage: mithril-signer [OPTIONS] [COMMAND]

Commands:
  generate-doc  Generate documentation
  help          Print this message or the help of the given subcommand(s)

Options:
  -r, --run-mode <RUN_MODE>
          Run Mode
          
          [env: RUN_MODE=]
          [default: dev]

  -v, --verbose...
          Verbosity level, add more v to increase

  -c, --configuration-dir <CONFIGURATION_DIR>
          Directory where the configuration file is located
          
          [default: ./config]

      --disable-digests-cache
          Disable immutables digests cache

      --reset-digests-cache
          If set the existing immutables digests cache will be reset.
          
          Will be ignored if set in conjunction with `--disable-digests-cache`.

  -h, --help
          Print help (see a summary with '-h')

  -V, --version
          Print version

```
| Subcommand | Aliases | Performed action |
|------------|---------|------------------|
| **generate-doc** | doc | Generate documentation |
| **help** |  | Print this message or the help of the given subcommand(s) |

The configuration parameters can be set in either of the following ways:

1. In a configuration file, depending on the `--run-mode` parameter. If the runtime mode is `testnet`, the file is located in `./conf/testnet.json`.

2. The value can be overridden by an environment variable with the parameter name in uppercase.

| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `data_stores_directory` | - | - | `DATA_STORES_DIRECTORY` | Directory to store signer data (Stakes, Protocol initializers, ...) | - | `./mithril-signer/stores` | - |
| `network_magic` | - | - | `NETWORK_MAGIC` | Cardano Network Magic number<br>useful for TestNet & DevNet | - | `1097911063` or `42` | - |
| `cardano_cli_path` | - | - | `CARDANO_CLI_PATH` | Cardano CLI tool path | - | `cardano-cli` | - |
| `db_directory` | - | - | `DB_DIRECTORY` | Directory to snapshot | - | - | - |
| `disable_digests_cache` | `--disable-digests-cache` | - | `DISABLE_DIGESTS_CACHE` | Disable immutables digests cache | `false` | - | - |
| `version` | `--version` | `-V` | `VERSION` | Print version | - | - | - |
| `reset_digests_cache` | `--reset-digests-cache` | - | `RESET_DIGESTS_CACHE` | If set the existing immutables digests cache will be reset | `false` | - | - |
| `era_reader_adapter_params` | - | - | `ERA_READER_ADAPTER_PARAMS` | Era reader adapter parameters | - | - | - |
| `configuration_dir` | `--configuration-dir` | `-c` | `CONFIGURATION_DIR` | Directory where the configuration file is located | `./config` | - | - |
| `party_id` | - | - | `PARTY_ID` | Party Id | - | `pool1pxaqe80sqpde7902er5kf6v0c7y0sv6d5g676766v2h829fvs3x` | - |
| `era_reader_adapter_type` | - | - | `ERA_READER_ADAPTER_TYPE` | Era reader adapter type | `bootstrap` | - | - |
| `operational_certificate_path` | - | - | `OPERATIONAL_CERTIFICATE_PATH` | File path to the operational certificate of the pool | - | - | - |
| `command` | - | - | `COMMAND` | Available commands | - | - | - |
| `cardano_node_socket_path` | - | - | `CARDANO_NODE_SOCKET_PATH` | Path of the socket used by the Cardano CLI tool<br>to communicate with the Cardano node | - | `/tmp/cardano.sock` | - |
| `verbose` | `--verbose` | `-v` | `VERBOSE` | Verbosity level, add more v to increase | `0` | Parsed from the number of occurrences: `-v` for `Warning`, `-vv` for `Info`, `-vvv` for `Debug` and `-vvvv` for `Trace` | - |
| `store_retention_limit` | - | - | `STORE_RETENTION_LIMIT` | Store retention limit. If set to None, no limit will be set. | - | - | - |
| `network` | - | - | `NETWORK` | Cardano network | - | `testnet` or `mainnet` or `devnet` | - |
| `kes_secret_key_path` | - | - | `KES_SECRET_KEY_PATH` | File path to the KES secret key of the pool | - | - | - |
| `aggregator_endpoint` | - | - | `AGGREGATOR_ENDPOINT` | Aggregator endpoint | - | `https://aggregator.pre-release-preview.api.mithril.network/aggregator` | - |
| `run_interval` | - | - | `RUN_INTERVAL` | Run Interval | - | `60000` | - |
| `run_mode` | `--run-mode` | `-r` | `RUN_MODE` | Run Mode | `dev` | - | - |
| `relay_endpoint` | - | - | `RELAY_ENDPOINT` | Relay endpoint | - | - | - |
| `help` | `--help` | `-h` | `HELP` | Print help (see more with '--help') | - | - | - |
###  mithril-signer generate-doc

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

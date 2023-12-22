Generated doc

### mithril-client

This program shows, downloads and verifies certified blockchain artifacts.
```bash
This program shows, downloads and verifies certified blockchain artifacts.

Usage: mithril-client [OPTIONS] <COMMAND>

Commands:
  snapshot                    Snapshot management
  mithril-stake-distribution  Mithril Stake Distribution management (alias: msd)
  help                        Print this message or the help of the given subcommand(s)

Options:
      --run-mode <RUN_MODE>
          Run Mode
          
          [env: RUN_MODE=]
          [default: dev]

  -v, --verbose...
          Verbosity level (-v=warning, -vv=info, -vvv=debug)

      --config-directory <CONFIG_DIRECTORY>
          Directory where configuration file is located
          
          [default: ./config]

      --aggregator-endpoint <AGGREGATOR_ENDPOINT>
          Override configuration Aggregator endpoint URL
          
          [env: AGGREGATOR_ENDPOINT=]

      --log-format-json
          Enable JSON output for logs displayed according to verbosity level

      --log-output <LOG_OUTPUT>
          Redirect the logs to a file

  -h, --help
          Print help

  -V, --version
          Print version

```
| Subcommand | Performed action |
|------------|------------------|
| **snapshot** | Snapshot management |
| **mithril-stake-distribution** | Mithril Stake Distribution management (alias: msd) |


The configuration parameters can be set in either of the following ways:

1. In a configuration file, depending on the `--run-mode` parameter. If the runtime mode is `testnet`, the file is located in `./conf/testnet.json`.

2. The value can be overridden by an environment variable with the parameter name in uppercase.

Here is a list of the available parameters:
### Configuration parameters

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `run_mode` | `--run-mode` |  | `RUN_MODE` | ? | `dev` | Run Mode | - |
| `verbose` | `--verbose` | `-v` |  | ? |  | Verbosity level (-v=warning, -vv=info, -vvv=debug) | - |
| `config_directory` | `--config-directory` |  |  | ? | `./config` | Directory where configuration file is located | - |
| `aggregator_endpoint` | `--aggregator-endpoint` |  | `AGGREGATOR_ENDPOINT` | ? |  | Override configuration Aggregator endpoint URL | - |
| `log_format_json` | `--log-format-json` |  |  | ? |  | Enable JSON output for logs displayed according to verbosity level | - |
| `log_output` | `--log-output` |  |  | ? |  | Redirect the logs to a file | - |

###  mithril-client snapshot

Snapshot management
```bash
Snapshot management

Usage: snapshot <COMMAND>

Commands:
  list      List available snapshots
  show      Show detailed informations about a snapshot
  download  Download the snapshot and verify the certificate
  help      Print this message or the help of the given subcommand(s)

Options:
  -h, --help
          Print help

```
| Subcommand | Performed action |
|------------|------------------|
| **list** | List available snapshots |
| **show** | Show detailed informations about a snapshot |
| **download** | Download the snapshot and verify the certificate |


###  mithril-client  snapshot list

List available snapshots
```bash
List available snapshots

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

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `json` | `--json` |  |  | ? |  | Enable JSON output | - |


###  mithril-client  snapshot show

Show detailed informations about a snapshot
```bash
Show detailed informations about a snapshot

Usage: show [OPTIONS] <DIGEST>

Arguments:
  <DIGEST>
          Snapshot digest.
          
          If `latest` is specified as digest, the command will return the latest snapshot.

Options:
      --json
          Enable JSON output

  -h, --help
          Print help (see a summary with '-h')

```


The configuration parameters can be set in either of the following ways:

1. In a configuration file, depending on the `--run-mode` parameter. If the runtime mode is `testnet`, the file is located in `./conf/testnet.json`.

2. The value can be overridden by an environment variable with the parameter name in uppercase.

Here is a list of the available parameters:
### Configuration parameters

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `json` | `--json` |  |  | ? |  | Enable JSON output | - |
| `digest` |  |  |  | ? |  | Snapshot digest | :heavy_check_mark: |


###  mithril-client  snapshot download

Download the snapshot and verify the certificate
```bash
Download the snapshot and verify the certificate

Usage: download [OPTIONS] <DIGEST>

Arguments:
  <DIGEST>
          Digest of the snapshot to download. Use the `list` command to get that information.
          
          If `latest` is specified as digest, the command will return the latest snapshot.

Options:
      --json
          Enable JSON output

      --download-dir <DOWNLOAD_DIR>
          Directory where the snapshot will be downloaded. By default, a subdirectory will be created in this directory to extract and verify the certificate

      --genesis-verification-key <GENESIS_VERIFICATION_KEY>
          Genesis Verification Key to check the certifiate chain
          
          [env: GENESIS_VERIFICATION_KEY=]

  -h, --help
          Print help (see a summary with '-h')

```


The configuration parameters can be set in either of the following ways:

1. In a configuration file, depending on the `--run-mode` parameter. If the runtime mode is `testnet`, the file is located in `./conf/testnet.json`.

2. The value can be overridden by an environment variable with the parameter name in uppercase.

Here is a list of the available parameters:
### Configuration parameters

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `json` | `--json` |  |  | ? |  | Enable JSON output | - |
| `digest` |  |  |  | ? |  | Digest of the snapshot to download. Use the `list` command to get that information | :heavy_check_mark: |
| `download_dir` | `--download-dir` |  |  | ? |  | Directory where the snapshot will be downloaded. By default, a subdirectory will be created in this directory to extract and verify the certificate | - |
| `genesis_verification_key` | `--genesis-verification-key` |  | `GENESIS_VERIFICATION_KEY` | ? |  | Genesis Verification Key to check the certifiate chain | - |


###  mithril-client mithril-stake-distribution

Mithril Stake Distribution management (alias: msd)
```bash
Mithril Stake Distribution management (alias: msd)

Usage: mithril-stake-distribution <COMMAND>

Commands:
  list      List certified stake distributions
  download  Download and verify the given Mithril Stake Distribution
  help      Print this message or the help of the given subcommand(s)

Options:
  -h, --help
          Print help

```
| Subcommand | Performed action |
|------------|------------------|
| **list** | List certified stake distributions |
| **download** | Download and verify the given Mithril Stake Distribution |


###  mithril-client  mithril-stake-distribution list

List certified stake distributions
```bash
List certified stake distributions

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

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `json` | `--json` |  |  | ? |  | Enable JSON output | - |


###  mithril-client  mithril-stake-distribution download

Download and verify the given Mithril Stake Distribution
```bash
Download and verify the given Mithril Stake Distribution

Usage: download [OPTIONS] <ARTIFACT_HASH>

Arguments:
  <ARTIFACT_HASH>
          Hash of the Mithril Stake Distribution artifact.
          
          If `latest` is specified as artifact_hash, the command will return the latest stake distribution.

Options:
      --download-dir <DOWNLOAD_DIR>
          Directory where the Mithril Stake Distribution will be downloaded. By default, a subdirectory will be created in this directory to extract and verify the certificate

      --genesis-verification-key <GENESIS_VERIFICATION_KEY>
          Genesis Verification Key to check the certifiate chain
          
          [env: GENESIS_VERIFICATION_KEY=]

  -h, --help
          Print help (see a summary with '-h')

```


The configuration parameters can be set in either of the following ways:

1. In a configuration file, depending on the `--run-mode` parameter. If the runtime mode is `testnet`, the file is located in `./conf/testnet.json`.

2. The value can be overridden by an environment variable with the parameter name in uppercase.

Here is a list of the available parameters:
### Configuration parameters

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `artifact_hash` |  |  |  | ? |  | Hash of the Mithril Stake Distribution artifact | :heavy_check_mark: |
| `download_dir` | `--download-dir` |  |  | ? |  | Directory where the Mithril Stake Distribution will be downloaded. By default, a subdirectory will be created in this directory to extract and verify the certificate | - |
| `genesis_verification_key` | `--genesis-verification-key` |  | `GENESIS_VERIFICATION_KEY` | ? |  | Genesis Verification Key to check the certifiate chain | - |




### mithril-client

This program shows, downloads and verifies certified blockchain artifacts.
```bash
This program shows, downloads and verifies certified blockchain artifacts.

Usage: mithril-client [OPTIONS] <COMMAND>

Commands:
  snapshot                    Snapshot management
  mithril-stake-distribution  Mithril Stake Distribution management (alias: msd)
  generate-doc                Generate documentation
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
| Subcommand | Aliases | Performed action |
|------------|---------|------------------|
| **snapshot** |  | Snapshot management |
| **mithril-stake-distribution** | msd | Mithril Stake Distribution management (alias: msd) |
| **generate-doc** | doc | Generate documentation |
| **help** |  | Print this message or the help of the given subcommand(s) |

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
| Subcommand | Aliases | Performed action |
|------------|---------|------------------|
| **list** |  | List available snapshots |
| **show** |  | Show detailed informations about a snapshot |
| **download** |  | Download the snapshot and verify the certificate |
| **help** |  | Print this message or the help of the given subcommand(s) |

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

| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `json` | `--json` | - | `JSON` | Enable JSON output | `false` | ? | - |
| `help` | `--help` | `-h` | `HELP` | Print help |  | ? | - |

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

| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `json` | `--json` | - | `JSON` | Enable JSON output | `false` | ? | - |
| `digest` | - | - | `DIGEST` | Snapshot digest |  | ? | :heavy_check_mark: |
| `help` | `--help` | `-h` | `HELP` | Print help (see more with '--help') |  | ? | - |

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

| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `json` | `--json` | - | `JSON` | Enable JSON output | `false` | ? | - |
| `digest` | - | - | `DIGEST` | Digest of the snapshot to download. Use the `list` command to get that information |  | ? | :heavy_check_mark: |
| `download_dir` | `--download-dir` | - | `DOWNLOAD_DIR` | Directory where the snapshot will be downloaded. By default, a subdirectory will be created in this directory to extract and verify the certificate |  | ? | - |
| `genesis_verification_key` | `--genesis-verification-key` | - | `GENESIS_VERIFICATION_KEY` | Genesis Verification Key to check the certifiate chain |  | ? | - |
| `help` | `--help` | `-h` | `HELP` | Print help (see more with '--help') |  | ? | - |

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
| Subcommand | Aliases | Performed action |
|------------|---------|------------------|
| **list** |  | List certified stake distributions |
| **download** |  | Download and verify the given Mithril Stake Distribution |
| **help** |  | Print this message or the help of the given subcommand(s) |

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

| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `json` | `--json` | - | `JSON` | Enable JSON output | `false` | ? | - |
| `help` | `--help` | `-h` | `HELP` | Print help |  | ? | - |

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

| Parameter | Command line (long) | Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:--------------------:|----------------------|-------------|---------------|---------|:---------:|
| `artifact_hash` | - | - | `ARTIFACT_HASH` | Hash of the Mithril Stake Distribution artifact |  | ? | :heavy_check_mark: |
| `download_dir` | `--download-dir` | - | `DOWNLOAD_DIR` | Directory where the Mithril Stake Distribution will be downloaded. By default, a subdirectory will be created in this directory to extract and verify the certificate |  | ? | - |
| `genesis_verification_key` | `--genesis-verification-key` | - | `GENESIS_VERIFICATION_KEY` | Genesis Verification Key to check the certifiate chain |  | ? | - |
| `help` | `--help` | `-h` | `HELP` | Print help (see more with '--help') |  | ? | - |

###  mithril-client generate-doc

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
| `output` | `--output` | - | `OUTPUT` | Generated documentation file | `[PROGRAM NAME]-command-line.md` | ? | - |
| `help` | `--help` | `-h` | `HELP` | Print help |  | ? | - |

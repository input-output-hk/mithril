Generated doc

###  mithril-client

This program shows, downloads and verifies certified blockchain artifacts.
| Subcommand | Performed action |
|------------|------------------|
| **snapshot** | Snapshot management |
| **mithril-stake-distribution** | Mithril Stake Distribution management (alias: msd) |

### Configuration parameters

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `run_mode` | `--run-mode` |  | `RUN_MODE` | ? | `dev` | Run Mode | - |
| `verbose` | `--verbose` | `-v` |  | ? |  | Verbosity level (-v=warning, -vv=info, -vvv=debug) | - |
| `config_directory` | `--config-directory` |  |  | ? | `./config` | Directory where configuration file is located | - |
| `aggregator_endpoint` | `--aggregator-endpoint` |  | `AGGREGATOR_ENDPOINT` | ? |  | Override configuration Aggregator endpoint URL | - |
| `log_format_json` | `--log-format-json` |  |  | ? |  | Enable JSON output for logs displayed according to verbosity level | - |
| `log_output` | `--log-output` |  |  | ? |  | Redirect the logs to a file | - |

### mithril-client  snapshot

Snapshot management
| Subcommand | Performed action |
|------------|------------------|
| **list** | List available snapshots |
| **show** | Show detailed informations about a snapshot |
| **download** | Download the snapshot and verify the certificate |


### mithril-client snapshot  list

List available snapshots

### Configuration parameters

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `json` | `--json` |  |  | ? |  | Enable JSON output | - |


### mithril-client snapshot  show

Show detailed informations about a snapshot

### Configuration parameters

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `json` | `--json` |  |  | ? |  | Enable JSON output | - |
| `digest` |  |  |  | ? |  | Snapshot digest | :heavy_check_mark: |


### mithril-client snapshot  download

Download the snapshot and verify the certificate

### Configuration parameters

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `json` | `--json` |  |  | ? |  | Enable JSON output | - |
| `digest` |  |  |  | ? |  | Digest of the snapshot to download. Use the `list` command to get that information | :heavy_check_mark: |
| `download_dir` | `--download-dir` |  |  | ? |  | Directory where the snapshot will be downloaded. By default, a subdirectory will be created in this directory to extract and verify the certificate | - |
| `genesis_verification_key` | `--genesis-verification-key` |  | `GENESIS_VERIFICATION_KEY` | ? |  | Genesis Verification Key to check the certifiate chain | - |


### mithril-client  mithril-stake-distribution

Mithril Stake Distribution management (alias: msd)
| Subcommand | Performed action |
|------------|------------------|
| **list** | List certified stake distributions |
| **download** | Download and verify the given Mithril Stake Distribution |


### mithril-client mithril-stake-distribution  list

List certified stake distributions

### Configuration parameters

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `json` | `--json` |  |  | ? |  | Enable JSON output | - |


### mithril-client mithril-stake-distribution  download

Download and verify the given Mithril Stake Distribution

### Configuration parameters

| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `artifact_hash` |  |  |  | ? |  | Hash of the Mithril Stake Distribution artifact | :heavy_check_mark: |
| `download_dir` | `--download-dir` |  |  | ? |  | Directory where the Mithril Stake Distribution will be downloaded. By default, a subdirectory will be created in this directory to extract and verify the certificate | - |
| `genesis_verification_key` | `--genesis-verification-key` |  | `GENESIS_VERIFICATION_KEY` | ? |  | Genesis Verification Key to check the certifiate chain | - |


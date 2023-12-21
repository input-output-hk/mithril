###  mithril-client

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
### mithril-client  snapshot

| Subcommand | Performed action |
|------------|------------------|
| **list** | List available snapshots |
| **show** | Show detailed informations about a snapshot |
| **download** | Download the snapshot and verify the certificate |

### mithril-client snapshot  list

### Configuration parameters
| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `json` | `--json` |  |  | ? |  | Enable JSON output | - |
### mithril-client snapshot  show

### Configuration parameters
| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `json` | `--json` |  |  | ? |  | Enable JSON output | - |
| `digest` |  |  |  | ? |  | Snapshot digest | :heavy_check_mark: |
### mithril-client snapshot  download

### Configuration parameters
| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `json` | `--json` |  |  | ? |  | Enable JSON output | - |
| `digest` |  |  |  | ? |  | Digest of the snapshot to download. Use the `list` command to get that information | :heavy_check_mark: |
| `download_dir` | `--download-dir` |  |  | ? |  | Directory where the snapshot will be downloaded. By default, a subdirectory will be created in this directory to extract and verify the certificate | - |
| `genesis_verification_key` | `--genesis-verification-key` |  | `GENESIS_VERIFICATION_KEY` | ? |  | Genesis Verification Key to check the certifiate chain | - |
### mithril-client  mithril-stake-distribution

| Subcommand | Performed action |
|------------|------------------|
| **list** | List certified stake distributions |
| **download** | Download and verify the given Mithril Stake Distribution |

### mithril-client mithril-stake-distribution  list

### Configuration parameters
| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `json` | `--json` |  |  | ? |  | Enable JSON output | - |
### mithril-client mithril-stake-distribution  download

### Configuration parameters
| Parameter | Command line (long) |  Command line (short) | Environment variable | Description | Default value | Example | Mandatory |
|-----------|---------------------|:---------------------:|----------------------|-------------|---------------|---------|:---------:|
| `artifact_hash` |  |  |  | ? |  | Hash of the Mithril Stake Distribution artifact | :heavy_check_mark: |
| `download_dir` | `--download-dir` |  |  | ? |  | Directory where the Mithril Stake Distribution will be downloaded. By default, a subdirectory will be created in this directory to extract and verify the certificate | - |
| `genesis_verification_key` | `--genesis-verification-key` |  | `GENESIS_VERIFICATION_KEY` | ? |  | Genesis Verification Key to check the certifiate chain | - |
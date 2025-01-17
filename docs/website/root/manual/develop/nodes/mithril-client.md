---
sidebar_position: 3
---

import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';

import CompiledBinaries from '../../../compiled-binaries.mdx'

# Mithril client node

:::info

Mithril client is responsible for restoring the **Cardano** blockchain on an empty node from a certified snapshot.

:::

:::tip

- For more information about the **Mithril network**, please see
  the [architecture](../../../mithril/advanced/mithril-network/architecture.md) overview

- For more information about the **Mithril client** node, please
  see [this overview](../../../mithril/advanced/mithril-network/client.md)

- Check out the [`Bootstrap a Cardano node`](../../getting-started/bootstrap-cardano-node.md) guide.

:::

:::info

The Mithril network configurations are available in the [**Network configurations**](../../getting-started/network-configurations.md) section of the user manual.

:::

## Resources

|          Node          |                                       Source repository                                        |                                  Rust documentation                                   |                                         Docker packages                                         |                         Network configurations                         |
| :--------------------: | :--------------------------------------------------------------------------------------------: | :-----------------------------------------------------------------------------------: | :---------------------------------------------------------------------------------------------: | :--------------------------------------------------------------------: |
| **Mithril client CLI** | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/tree/main/mithril-client-cli) | [:arrow_upper_right:](https://mithril.network/rust-doc/mithril_client_cli/index.html) | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/pkgs/container/mithril-client) | [:arrow_upper_right:](../../getting-started/network-configurations.md) |

## Prerequisites

<Tabs groupId="system" queryString>
<TabItem value="linux-mac" label="Linux / Mac">
- Install the latest stable version of the [correctly configured](https://www.rust-lang.org/learn/get-started) Rust
  toolchain

- Install build tools `build-essential` and `m4`; for example, on Ubuntu/Debian/Mint, run `sudo apt install build-essential m4`

- Install OpenSSL development libraries; for example, on Ubuntu/Debian/Mint, run `apt install libssl-dev`.

</TabItem>
<TabItem value="windows" label="Windows">
:::info
All Windows commands below are run on PowerShell 5.
:::

- Install [Visual Studio C++ Build tools](https://visualstudio.microsoft.com/visual-cpp-build-tools/) (required by the Rust toolchain).

- Install the latest stable version of the [correctly configured](https://www.rust-lang.org/learn/get-started) Rust
  toolchain

</TabItem>
</Tabs>

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
# Please refer to the [**Network configurations**](http://mithril.network/manual/getting-started/network-configurations) section of the user manual
git checkout **YOUR_BUILD_BRANCH_OR_TAG**
```

Change the directory:

<Tabs groupId="system" queryString>
  <TabItem value="linux-mac" label="Linux / Mac">
  ```bash
  cd mithril/mithril-client-cli
  ```
  </TabItem>
  <TabItem value="windows" label="Windows">
  ```powershell
  cd mithril\mithril-client-cli
  ```
  </TabItem>
</Tabs>

## Development testing and building

Run tests:

<Tabs groupId="system" queryString>
  <TabItem value="linux-mac" label="Linux / Mac">
  ```bash
  make test
  ```
  </TabItem>
  <TabItem value="windows" label="Windows">
  ```powershell
  cargo test
  ```
  </TabItem>
</Tabs>

Create the help menu:

<Tabs groupId="system" queryString>
  <TabItem value="linux-mac" label="Linux / Mac">
  ```bash
  make help
  ```
  </TabItem>
  <TabItem value="windows" label="Windows">
  ```powershell
  cargo run -- -h
  ```
  </TabItem>
</Tabs>

Generate the Rust documentation:

<Tabs groupId="system" queryString>
  <TabItem value="linux-mac" label="Linux / Mac">
  ```bash
  make doc
  ```
  </TabItem>
  <TabItem value="windows" label="Windows">
  ```powershell
  cargo doc --no-deps --open
  ```
  </TabItem>
</Tabs>

Run in debug mode with the default configuration:

<Tabs groupId="system" queryString>
  <TabItem value="linux-mac" label="Linux / Mac">
  ```bash
  make debug
  ```
  </TabItem>
  <TabItem value="windows" label="Windows">
  ```powershell
  cargo run
  ```
  </TabItem>
</Tabs>

## Release the build and run the binary

Build and run in release mode with the default configuration:

<Tabs groupId="system" queryString>
  <TabItem value="linux-mac" label="Linux / Mac">
  ```bash
  make run
  ```
  </TabItem>
  <TabItem value="windows" label="Windows">
  ```powershell
  cargo build --release; copy ..\target\release\mithril-client.exe .; .\mithril-client
  ```
  </TabItem>
</Tabs>

Or, build only in release mode:

<Tabs groupId="system" queryString>
  <TabItem value="linux-mac" label="Linux / Mac">
  ```bash
  make build
  ```
  </TabItem>
  <TabItem value="windows" label="Windows">
  ```powershell
  cargo build --release; copy ..\target\release\mithril-client.exe .
  ```
  </TabItem>
</Tabs>

Display the help menu:

<Tabs groupId="system" queryString>
  <TabItem value="linux-mac" label="Linux / Mac">
  ```bash
  ./mithril-client --help
  ```
  </TabItem>
  <TabItem value="windows" label="Windows">
  ```powershell
  .\mithril-client --help
  ```
  </TabItem>
</Tabs>

You should see:

```bash
This program shows, downloads, and verifies certified blockchain artifacts.

Usage: mithril-client [OPTIONS] <COMMAND>

Commands:
  cardano-db                  Cardano db management (alias: cdb)
  mithril-stake-distribution  Mithril stake distribution management (alias: msd)
  cardano-transaction         Cardano transactions management (alias: ctx)
  cardano-stake-distribution  Cardano stake distribution management (alias: csd)
  help                        Print this message or the help of the given subcommand(s)

Options:
      --run-mode <RUN_MODE>
          Run Mode [env: RUN_MODE=] [default: dev]
  -v, --verbose...
          Verbosity level (-v=warning, -vv=info, -vvv=debug)
      --config-directory <CONFIG_DIRECTORY>
          Directory where configuration file is located [default: ./config]
      --aggregator-endpoint <AGGREGATOR_ENDPOINT>
          Override configuration Aggregator endpoint URL [env: AGGREGATOR_ENDPOINT=]
      --log-format-json
          Enable JSON output for logs displayed according to verbosity level
      --log-output <LOG_OUTPUT>
          Redirect the logs to a file
      --unstable
          Enable unstable commands
  -h, --help
          Print help
  -V, --version
          Print version
```

Run in release mode with the default configuration:

<Tabs groupId="system" queryString>
  <TabItem value="linux-mac" label="Linux / Mac">
  ```bash
  ./mithril-client
  ```
  </TabItem>
  <TabItem value="windows" label="Windows">
  ```powershell
  .\mithril-client
  ```
  </TabItem>
</Tabs>

Run in release mode with a specific mode:

<Tabs groupId="system" queryString>
  <TabItem value="linux-mac" label="Linux / Mac">
  ```bash
  ./mithril-client --run-mode preview
  ```
  </TabItem>
  <TabItem value="windows" label="Windows">
  ```powershell
  .\mithril-client --run-mode preview
  ```
  </TabItem>
</Tabs>

Run in release mode with a custom configuration using environment variables:

<Tabs groupId="system" queryString>
  <TabItem value="linux-mac" label="Linux / Mac">
  ```bash
  GENESIS_VERIFICATION_KEY=$(wget -q -O - **YOUR_GENESIS_VERIFICATION_KEY**) AGGREGATOR_ENDPOINT=**YOUR_AGGREGATOR_ENDPOINT** ./mithril-client
  ```
  </TabItem>
  <TabItem value="windows" label="Windows">
  ```powershell
  $env:GENESIS_VERIFICATION_KEY = curl.exe --silent - **YOUR_GENESIS_VERIFICATION_KEY**; $env:AGGREGATOR_ENDPOINT=**YOUR_AGGREGATOR_ENDPOINT**; .\mithril-client
  ```
  </TabItem>
</Tabs>

:::tip

To display results in JSON format for the `list` and `show` commands, simply use the `--json` (or `-j`) option:

<Tabs groupId="system" queryString>
  <TabItem value="linux-mac" label="Linux / Mac">
  ```bash
  ./mithril-client cardano-db snapshot list --json
  ```
  </TabItem>
  <TabItem value="windows" label="Windows">
  ```powershell
  .\mithril-client cardano-db snapshot list --json
  ```
  </TabItem>
</Tabs>

:::

:::tip

If you wish to delve deeper and access several levels of logs from the Mithril client, use the following:

- Add `-v` for some logs (WARN)
- Add `-vv` for more logs (INFO)
- Add `-vvv` for even more logs (DEBUG)
- Add `-vvvv` for all logs (TRACE).

:::

## Download the pre-built binary

<Tabs groupId="system" queryString>
  <TabItem value="linux-mac" label="Linux / Mac">
  <CompiledBinaries  node="mithril-client"/>
  </TabItem>
  <TabItem value="windows" label="Windows">
  :::info
  Each distribution (pre-)release comes with pre-compiled binaries ready to use.
  You can download them from the distribution (pre-)release distribution page on GitHub that depends on the Mithril network you
  are targeting.
  These links are available in the **Build from** field in the [**network configurations**](/manual/getting-started/network-configurations) table.
  :::
  </TabItem>
</Tabs>

## Run a Docker container

### Registry image

A list of available images on the registry can be
found [here](https://github.com/input-output-hk/mithril/pkgs/container/mithril-client).

To prepare the environment variables, retrieve the values from the **Mithril networks** section.

<Tabs groupId="system" queryString>
  <TabItem value="linux-mac" label="Linux / Mac">
  ```bash
  export MITHRIL_IMAGE_ID=**YOUR_MITHRIL_IMAGE_ID**
  export AGGREGATOR_ENDPOINT=**YOUR_AGGREGATOR_ENDPOINT**
  export GENESIS_VERIFICATION_KEY=$(wget -q -O - **YOUR_GENESIS_VERIFICATION_KEY**)
  export SNAPSHOT_DIGEST=latest
  ```
  </TabItem>
  <TabItem value="windows" label="Windows">
  ```powershell
  $env:MITHRIL_IMAGE_ID="**YOUR_MITHRIL_IMAGE_ID**"
  $env:AGGREGATOR_ENDPOINT="**YOUR_AGGREGATOR_ENDPOINT**"
  $env:GENESIS_VERIFICATION_KEY = curl.exe --silent - **YOUR_GENESIS_VERIFICATION_KEY**
  $env:SNAPSHOT_DIGEST="latest"
  ```
  </TabItem>
</Tabs>

Here is an example configuration for the `release-preprod` network and the `latest` stable Docker image:

<Tabs groupId="system" queryString>
  <TabItem value="linux-mac" label="Linux / Mac">
  ```bash
  export MITHRIL_IMAGE_ID=latest
  export AGGREGATOR_ENDPOINT=https://aggregator.release-preprod.api.mithril.network/aggregator
  export GENESIS_VERIFICATION_KEY=$(wget -q -O - https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/genesis.vkey)
  export SNAPSHOT_DIGEST=latest
  ```
  </TabItem>
  <TabItem value="windows" label="Windows">
  ```powershell
  $env:MITHRIL_IMAGE_ID="latest"
  $env:AGGREGATOR_ENDPOINT="https://aggregator.release-preprod.api.mithril.network/aggregator"
  $env:GENESIS_VERIFICATION_KEY = curl.exe --silent - https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/genesis.vkey
  $env:SNAPSHOT_DIGEST="latest"
  ```
  </TabItem>
</Tabs>

Proceed by creating a shell function for the Mithril client:

<Tabs groupId="system" queryString>
  <TabItem value="linux-mac" label="Linux / Mac">
  ```bash
  mithril_client () {
    docker run --rm -e GENESIS_VERIFICATION_KEY=$GENESIS_VERIFICATION_KEY -e AGGREGATOR_ENDPOINT=$AGGREGATOR_ENDPOINT --name='mithril-client' -v $(pwd):/app/data -u $(id -u) ghcr.io/input-output-hk/mithril-client:$MITHRIL_IMAGE_ID $@
  }
  ```
  </TabItem>
  <TabItem value="windows" label="Windows">
  ```powershell
  function mithril_client {
    docker run --rm -e GENESIS_VERIFICATION_KEY=$env:GENESIS_VERIFICATION_KEY -e AGGREGATOR_ENDPOINT=$env:AGGREGATOR_ENDPOINT --name='mithril-client' -v ${PWD}:/app/data ghcr.io/input-output-hk/mithril-client:$env:MITHRIL_IMAGE_ID $args
  }
  ```
  </TabItem>
</Tabs>

Now you can use the `mithril_client` functions:

```bash
# 1- Help
mithril_client help

# 2- List Cardano db snapshots
mithril_client cardano-db snapshot list

# 3- Show detailed information about a Cardano db snapshot
mithril_client cardano-db snapshot show $CARDANO_DB_DIGEST

# 4- Download the given Cardano db and verify the certificate
mithril_client cardano-db download $CARDANO_DB_DIGEST

# 5- List Mithril stake distributions
mithril_client mithril-stake-distribution list

# 6- Download and verify the given Mithril stake distribution
mithril_client mithril-stake-distribution download $MITHRIL_STAKE_DISTRIBUTION_ARTIFACT_HASH

# 7- List Cardano transaction snapshots
mithril_client cardano-transaction snapshot list

# 8- Show detailed information about a Cardano transaction snapshot
mithril_client cardano-transaction snapshot show $CARDANO_TRANSACTION_SNAPSHOT_HASH

# 9- Certify that the given list of transactions hashes are included in the Cardano transactions set
mithril_client cardano-transaction certify $TRANSACTION_HASH_1,$TRANSACTION_HASH_2

# 10- List Cardano stake distributions
mithril_client cardano-stake-distribution list

# 11 - Download and verify the given Cardano stake distribution from its hash or epoch
mithril_client cardano-stake-distribution download $UNIQUE_IDENTIFIER
```

### Local image

Build a local Docker image:

<Tabs groupId="system" queryString>
  <TabItem value="linux-mac" label="Linux / Mac">
  ```bash
  make docker-build
  ```
  </TabItem>
  <TabItem value="windows" label="Windows">
  ```powershell
  pushd ..\; docker build -t mithril/mithril-client -f mithril-client-cli\Dockerfile . ; popd
  ```
  </TabItem>
</Tabs>

Run a local Docker container:

<Tabs groupId="system" queryString>
  <TabItem value="linux-mac" label="Linux / Mac">
  ```bash
  make docker-run
  ```
  </TabItem>
  <TabItem value="windows" label="Windows">
  ```powershell
  docker run --rm --name='mithril-client' mithril/mithril-client
  ```
  </TabItem>
</Tabs>

## Subcommands

Here are the subcommands available:

### Cardano DB

| Subcommand        | Performed action                                            |
| ----------------- | ----------------------------------------------------------- |
| **download**      | Downloads and restores a cardano-db snapshot                |
| **help**          | Prints this message or the help for the given subcommand(s) |
| **snapshot list** | Lists available cardano-db snapshots                        |
| **snapshot show** | Shows information about a cardano-db snapshot               |

### Mithril stake distribution

| Subcommand   | Performed action                                            |
| ------------ | ----------------------------------------------------------- |
| **download** | Downloads and verifies Mithril stake distribution           |
| **help**     | Prints this message or the help for the given subcommand(s) |
| **list**     | Lists available Mithril stake distributions                 |

### Cardano transactions

| Subcommand        | Performed action                                                                              |
| ----------------- | --------------------------------------------------------------------------------------------- |
| **certify**       | Certifies that given list of transactions hashes are included in the Cardano transactions set |
| **snapshot list** | Lists available Cardano transactions snapshots                                                |
| **snapshot show** | Shows information about a Cardano transactions snapshot                                       |
| **help**          | Prints this message or the help for the given subcommand(s)                                   |

### Cardano stake distribution

| Subcommand   | Performed action                                            |
| ------------ | ----------------------------------------------------------- |
| **download** | Downloads and verifies Cardano stake distribution           |
| **help**     | Prints this message or the help for the given subcommand(s) |
| **list**     | Lists available Cardano stake distributions                 |

## Configuration parameters

The configuration parameters can be set in either of the following ways:

1. In a configuration file, depending on the `--run-mode` parameter. If the runtime mode is `testnet`, the file is
   located in `./conf/testnet.json`.

2. The value can be overridden by an environment variable with the parameter name in uppercase.

Here is a list of the available parameters:

| Parameter                  | Command line (long)     | Command line (short) | Environment variable       | Description                 | Default value | Example                                                                                                                 |     Mandatory      |
| -------------------------- | ----------------------- | :------------------: | -------------------------- | --------------------------- | ------------- | ----------------------------------------------------------------------------------------------------------------------- | :----------------: |
| `verbose`                  | `--verbose`             |         `-v`         | `VERBOSE`                  | Verbosity level             | -             | Parsed from the number of occurrences: `-v` for `Warning`, `-vv` for `Info`, `-vvv` for `Debug` and `-vvvv` for `Trace` | :heavy_check_mark: |
| `unstable`                 | `--unstable`            |          -           | -                          | Enable unstable commands    | -             | -                                                                                                                       |         -          |
| `run_mode`                 | `--run-mode`            |          -           | `RUN_MODE`                 | Runtime mode                | `dev`         | -                                                                                                                       | :heavy_check_mark: |
| `aggregator_endpoint`      | `--aggregator-endpoint` |          -           | `AGGREGATOR_ENDPOINT`      | Aggregator node endpoint    | -             | `https://aggregator.pre-release-preview.api.mithril.network/aggregator`                                                 | :heavy_check_mark: |
| `genesis_verification_key` | -                       |          -           | `GENESIS_VERIFICATION_KEY` | Genesis verification key    | -             | -                                                                                                                       | :heavy_check_mark: |
| `log_format_json`          | `--log-format-json`     |          -           | -                          | Enable JSON output for logs | -             | -                                                                                                                       |         -          |
| `log_output`               | `--log-output`          |         `-o`         | -                          | Redirect the logs to a file | -             | `./mithril-client.log`                                                                                                  |         -          |

`cardano-db snapshot show` command:

| Parameter | Command line (long) | Command line (short) | Environment variable | Description                                         | Default value | Example |     Mandatory      |
| --------- | ------------------- | :------------------: | -------------------- | --------------------------------------------------- | ------------- | ------- | :----------------: |
| `digest`  | `--digest`          |          -           | `DIGEST`             | Cardano DB digest or `latest` for the latest digest | -             | -       | :heavy_check_mark: |
| `json`    | `--json`            |          -           | -                    | Enable JSON output for command results              | -             | -       |         -          |

`cardano-db snapshot list` command:

| Parameter | Command line (long) | Command line (short) | Environment variable | Description                            | Default value | Example | Mandatory |
| --------- | ------------------- | :------------------: | -------------------- | -------------------------------------- | ------------- | ------- | :-------: |
| `json`    | `--json`            |          -           | -                    | Enable JSON output for command results | -             | -       |     -     |

`cardano-db download` command:

| Parameter      | Command line (long) | Command line (short) | Environment variable | Description                                         | Default value | Example |     Mandatory      |
| -------------- | ------------------- | :------------------: | -------------------- | --------------------------------------------------- | ------------- | ------- | :----------------: |
| `digest`       | `--digest`          |          -           | `DIGEST`             | Cardano DB digest or `latest` for the latest digest | -             | -       | :heavy_check_mark: |
| `download_dir` | `--download-dir`    |          -           | -                    | Directory where the Cardano DB will be downloaded   | .             | -       |         -          |
| `json`         | `--json`            |          -           | -                    | Enable JSON output for progress logs                | -             | -       |         -          |

`mithril-stake-distribution list` command:

| Parameter | Command line (long) | Command line (short) | Environment variable | Description                            | Default value | Example | Mandatory |
| --------- | ------------------- | :------------------: | -------------------- | -------------------------------------- | ------------- | ------- | :-------: |
| `json`    | `--json`            |          -           | -                    | Enable JSON output for command results | -             | -       |     -     |

`mithril-stake-distribution download` command:

| Parameter       | Command line (long) | Command line (short) | Environment variable | Description                                                                         | Default value | Example |     Mandatory      |
| --------------- | ------------------- | :------------------: | -------------------- | ----------------------------------------------------------------------------------- | ------------- | ------- | :----------------: |
| `artifact_hash` | `--artifact-hash`   |          -           | -                    | Hash of the Mithril stake distribution artifact or `latest` for the latest artifact | -             | -       | :heavy_check_mark: |
| `download_dir`  | `--download-dir`    |          -           | -                    | Directory where the Mithril stake distribution will be downloaded                   | .             | -       |         -          |

`cardano-transaction snapshot show` command:

| Parameter | Command line (long) | Command line (short) | Environment variable | Description                                                                               | Default value | Example |     Mandatory      |
| --------- | ------------------- | :------------------: | -------------------- | ----------------------------------------------------------------------------------------- | ------------- | ------- | :----------------: |
| `hash`    | `--hash`            |          -           | `HASH`               | Cardano transaction snapshot hash or `latest` for the latest Cardano transaction snapshot | -             | -       | :heavy_check_mark: |
| `json`    | `--json`            |          -           | -                    | Enable JSON output for command results                                                    | -             | -       |         -          |

`cardano-transaction snapshot list` command:

| Parameter | Command line (long) | Command line (short) | Environment variable | Description                            | Default value | Example | Mandatory |
| --------- | ------------------- | :------------------: | -------------------- | -------------------------------------- | ------------- | ------- | :-------: |
| `json`    | `--json`            |          -           | -                    | Enable JSON output for command results | -             | -       |     -     |

`cardano-transaction certify` command:

| Parameter             | Command line (long)     | Command line (short) | Environment variable  | Description                                     | Default value | Example |     Mandatory      |
| --------------------- | ----------------------- | :------------------: | --------------------- | ----------------------------------------------- | ------------- | ------- | :----------------: |
| `transactions_hashes` | `--transactions_hashes` |          -           | `TRANSACTIONS_HASHES` | Cardano transactions hashes separated by commas | -             | -       | :heavy_check_mark: |
| `json`                | `--json`                |          -           | -                     | Enable JSON output for progress logs            | -             | -       |         -          |

`cardano-stake-distribution list` command:

| Parameter | Command line (long) | Command line (short) | Environment variable | Description                            | Default value | Example | Mandatory |
| --------- | ------------------- | :------------------: | -------------------- | -------------------------------------- | ------------- | ------- | :-------: |
| `json`    | `--json`            |          -           | -                    | Enable JSON output for command results | -             | -       |     -     |

`cardano-stake-distribution download` command:

| Parameter           | Command line (long)   | Command line (short) | Environment variable | Description                                                                                  | Default value | Example |     Mandatory      |
| ------------------- | --------------------- | :------------------: | -------------------- | -------------------------------------------------------------------------------------------- | ------------- | ------- | :----------------: |
| `unique_identifier` | `--unique-identifier` |          -           | -                    | Epoch or hash of the Cardano stake distribution artifact or `latest` for the latest artifact | -             | -       | :heavy_check_mark: |
| `download_dir`      | `--download-dir`      |          -           | -                    | Directory where the Cardano stake distribution will be downloaded                            | .             | -       |         -          |

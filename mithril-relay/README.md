# Mithril-relay ![CI workflow](https://github.com/input-output-hk/mithril/actions/workflows/ci.yml/badge.svg) [![Discord](https://img.shields.io/discord/500028886025895936.svg?logo=discord&style=flat-square)](https://discord.gg/5kaErDKDRq)

> [!WARNING]  
> **Do not use in production** 🔥

> [!NOTE]  
> **This is a work in progress** 🛠

The **Mithril relay** is an experimental implementation of a relay for Mithril signer and aggregator which supports partial P2P network layer for a Mithril network.

## Pre-requisites

- Install the latest stable version of the [correctly configured](https://www.rust-lang.org/learn/get-started) Rust toolchain.

- Install Build Tools `build-essential` and `m4`. For example, on Ubuntu/Debian/Mint, run `sudo apt install build-essential m4`.

- Install OpenSSL development libraries. For example, on Ubuntu/Debian/Mint, run `apt install libssl-dev`

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
cd mithril/mithril-relay
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

## Release the build and run the binary

Build only in release mode:

```bash
make build
```

Display the help menu:

```bash
./mithril-relay --help
```

You should see:

```bash
This program is a relay for Mithril nodes.

Usage: mithril-relay [OPTIONS] <COMMAND>

Commands:
  aggregator  Run a relay for a Mithril aggregator
  signer      Run a relay for a Mithril signer
  passive     Run a passive relay (just a peer in the P2P network)
  help        Print this message or the help of the given subcommand(s)

Options:
      --run-mode <RUN_MODE>
          Run Mode [env: RUN_MODE=] [default: dev]
  -v, --verbose...
          Verbosity level (-v=warning, -vv=info, -vvv=debug)
      --config-directory <CONFIG_DIRECTORY>
          Directory where configuration file is located [default: ./config]
  -h, --help
          Print help
  -V, --version
          Print version

```

Run a relay for an aggregator:

```bash
./mithril-relay aggregator --listen-port **P2P_LISTEN_PORT** --aggregator-endpoint **AGGREGATOR_ENDPOINT** --dial-to **OTHER_P2P_PEER_TO_CONNECT_TO**
```

Run a relay for a signer:

```bash
./mithril-relay signer --server-port **HTTP_SERVER_LISTENING_PORT** --listen-port **P2P_LISTEN_PORT** --aggregator-endpoint **AGGREGATOR_ENDPOINT** --dial-to **OTHER_P2P_PEER_TO_CONNECT_TO**
```

Run a passive relay:

```bash
./mithril-relay passive --dial-to **OTHER_P2P_PEER_TO_CONNECT_TO**
```

If you wish to delve deeper and access several levels of logs from the Mithril client, use the following:

- Add `-v` for some logs (WARN)
- Add `-vv` for more logs (INFO)
- Add `-vvv` for even more logs (DEBUG)
- Add `-vvvv` for all logs (TRACE)

## Run a Docker container

### Registry image

A list of available images on the registry can be found [here](https://github.com/input-output-hk/mithril/pkgs/container/mithril-relay).

To prepare the environment variables, retrieve the values from the above **Mithril networks** table.

```bash
export MITHRIL_IMAGE_ID=**YOUR_MITHRIL_IMAGE_ID**
export AGGREGATOR_ENDPOINT=**YOUR_AGGREGATOR_ENDPOINT**
```

Here is an example configuration for the `release-preprod` network and the `latest` stable Docker image:

```bash
export MITHRIL_IMAGE_ID=latest
export AGGREGATOR_ENDPOINT=https://aggregator.release-preprod.api.mithril.network/aggregator
```

Proceed by creating a shell function for the Mithril relay:

```bash
mithril_relay () {
  docker run --rm -e NETWORK=$NETWORK -e GENESIS_VERIFICATION_KEY=$GENESIS_VERIFICATION_KEY -e AGGREGATOR_ENDPOINT=$AGGREGATOR_ENDPOINT --name='mithril-relay' -v $(pwd):/app/data -u $(id -u) ghcr.io/input-output-hk/mithril-relay:$MITHRIL_IMAGE_ID $@
}
```

Now you can use the `mithril_relay` functions:

```bash
# 1- Help
mithril_relay --help

# 2- Aggregator
mithril_relay aggregator

# 3- Signer
mithril_relay signer

# 4- Passive
mithril_relay passive
```

### Local image

Build a local Docker image:

```bash
make docker-build
```

Run a local Docker container:

```bash
make docker-run
```

## Subcommands

Here are the subcommands available:

| Subcommand     | Performed action                                      |
| -------------- | ----------------------------------------------------- |
| **aggregator** | Runs a relay for a Mithril aggregator                 |
| **signer**     | Runs a relay for a Mithril signer                     |
| **passive**    | Runs a passive relay (just a peer in the P2P network) |

## Configuration parameters

The configuration parameters can be set in either of the following ways:

1. In a configuration file, depending on the `--run-mode` parameter. If the runtime mode is `testnet`, the file is located in `./conf/testnet.json`.

2. The value can be overridden by an environment variable with the parameter name in uppercase.

Here is a list of the available parameters:

| Parameter  | Command line (long) | Command line (short) | Environment variable | Description     | Default value | Example                                                                                                                 |     Mandatory      |
| ---------- | ------------------- | :------------------: | -------------------- | --------------- | ------------- | ----------------------------------------------------------------------------------------------------------------------- | :----------------: |
| `verbose`  | `--verbose`         |         `-v`         | `VERBOSE`            | Verbosity level | -             | Parsed from the number of occurrences: `-v` for `Warning`, `-vv` for `Info`, `-vvv` for `Debug` and `-vvvv` for `Trace` | :heavy_check_mark: |
| `run_mode` | `--run-mode`        |          -           | `RUN_MODE`           | Runtime mode    | `dev`         | -                                                                                                                       | :heavy_check_mark: |

`aggregator` command:

| Parameter             | Command line (long)     | Command line (short) | Environment variable  | Description                                                | Default value | Example                                                                 |     Mandatory      |
| --------------------- | ----------------------- | :------------------: | --------------------- | ---------------------------------------------------------- | ------------- | ----------------------------------------------------------------------- | :----------------: |
| `listen_port`         | `--listen-port`         |          -           | `LISTEN_PORT`         | P2P peer listening port                                    | 0             | `9090`                                                                  | :heavy_check_mark: |
| `dial_to`             | `--dial-to`             |          -           | `DIAL_TO`             | P2P peer address to connect to (not needed for first peer) | -             | `/ip4/0.0.0.0/tcp/1234`                                                 |         -          |
| `aggregator_endpoint` | `--aggregator-endpoint` |          -           | `AGGREGATOR_ENDPOINT` | Aggregator node endpoint                                   | -             | `https://aggregator.pre-release-preview.api.mithril.network/aggregator` | :heavy_check_mark: |

`signer` command:

| Parameter             | Command line (long)     | Command line (short) | Environment variable  | Description                                                | Default value | Example                                                                 |     Mandatory      |
| --------------------- | ----------------------- | :------------------: | --------------------- | ---------------------------------------------------------- | ------------- | ----------------------------------------------------------------------- | :----------------: |
| `listen_port`         | `--listen-port`         |          -           | `LISTEN_PORT`         | P2P peer listening port                                    | 0             | `9090`                                                                  | :heavy_check_mark: |
| `dial_to`             | `--dial-to`             |          -           | `DIAL_TO`             | P2P peer address to connect to (not needed for first peer) | -             | `/ip4/0.0.0.0/tcp/1234`                                                 |         -          |
| `server_port`         | `--server-port`         |          -           | `SERVER_PORT`         | HTTP server listening port                                 | 3132          | `8181`                                                                  | :heavy_check_mark: |
| `aggregator_endpoint` | `--aggregator-endpoint` |          -           | `AGGREGATOR_ENDPOINT` | Aggregator node endpoint                                   | -             | `https://aggregator.pre-release-preview.api.mithril.network/aggregator` | :heavy_check_mark: |

`passive` command:

| Parameter     | Command line (long) | Command line (short) | Environment variable | Description                                                | Default value | Example                 |     Mandatory      |
| ------------- | ------------------- | :------------------: | -------------------- | ---------------------------------------------------------- | ------------- | ----------------------- | :----------------: |
| `listen_port` | `--listen-port`     |          -           | `LISTEN_PORT`        | P2P peer listening port                                    | 0             | `9090`                  | :heavy_check_mark: |
| `dial_to`     | `--dial-to`         |          -           | `DIAL_TO`            | P2P peer address to connect to (not needed for first peer) | -             | `/ip4/0.0.0.0/tcp/1234` |         -          |

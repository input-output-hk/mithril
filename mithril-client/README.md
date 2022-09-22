# Mithril Network / Mithril Client

**This is a work in progress** :hammer_and_wrench:s

* A **Mithril Client** is able to download, verify authenticity and restore a snapshot into a **Cardano Node**.
* It allows user to bootstrap a **Cardano Node** in minutes instead of days.
* This cli implements a MVP version of a **Mithril Client**.

---

## Pre-requisites

**Install Rust**

* Install a [correctly configured](https://www.rust-lang.org/learn/get-started) Rust toolchain (latest stable version).
* Install Rust [Clippy](https://github.com/rust-lang/rust-clippy) component.

## Mithril test networks

The Mithril test networks are:

* `preview`: Test network with magic id `2`, implemented on the IOG hosted Mithril Aggregator
* `preprod`: Test network with magic id `1`, not implemented yet on the IOG hosted Mithril Aggregator
* `testnet`: Legacy test network with magic id `1097911063`, used to be on the IOG hosted Mithril Aggregator, now deprecated

In this documentation, we use the generic `testnet` identifier, but you need to replace it with the identifier of the network that runs on your Cardano node

## Download source code

```bash
# Download sources from github
git clone https://github.com/input-output-hk/mithril

# Go to sources directory
cd mithril-client
```

## Development test and build

```bash
# Test
make test

# Help
make help

# Doc
make doc

# Run with default configuration
make debug
```

## Release build and run binary

```bash
# Build and run in release with default configuration
make run list

# Or
# Build
make build

# Help
./mithril-client --help

# Run and show list of snapshots
./mithril-client list

# Run and show list of snapshots for the testnet config file
# Run in a specific mode
./mithril-client -r testnet list

# Run with custom configuration with env vars
GENESIS_VERIFICATION_KEY=$(wget -q -O - https://raw.githubusercontent.com/input-output-hk/mithril/main/TEST_ONLY_genesis.vkey) NETWORK=testnet AGGREGATOR_ENDPOINT=https://aggregator.api.mithril.network/aggregator ./mithril-client
```

You can use the `--json` option in order to display results in `JSON` format for the `list` and `show` commands:

```bash
./mithril-client list --json
```

## Build and run Docker container

```bash
# Build Docker image
make docker-build

# Run Docker container
make docker-run
```

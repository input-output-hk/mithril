# Mithril Network / Mithril Client

**This is a work in progress** :hammer_and_wrench:s

* A **Mithril Client** is able to download, verify authenticity and restore a snapshot into a **Cardano Node**.
* It allows user to bootstrap a **Cardano Node** in minutes instead of days.
* This cli implements a MVP version of a **Mithril Client**.

---

## Pre-requisites

**Install Rust**

* Install a [correctly configured](https://www.rust-lang.org/learn/get-started) Rust toolchain (version 1.58.0+).
* Install Rust [Clippy](https://github.com/rust-lang/rust-clippy) component.

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
NETWORK=testnet AGGREGATOR_ENDPOINT=http://aggregator.api.mithril.network/aggregator ./mithril-client
```

## Build and run Docker container

```bash
# Build Docker image
make docker-build

# Run Docker container
make docker-run
```

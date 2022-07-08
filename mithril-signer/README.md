# Mithril Signer

**This is a work in progress** :hammer_and_wrench:

This is a first version of the Mithril Signer

---

## Pre-requisites

**Install Rust**

- Install a [correctly configured](https://www.rust-lang.org/learn/get-started) Rust toolchain (version 1.62.0+).

## Download source code

```bash
# Download sources from github
git clone https://github.com/input-output-hk/mithril

# Go to sources directory
cd mithril-signer
```

## Development test and build

```bash
# Test
make test

# Help
make help

# Doc
make doc

# Run in debug with default configuration
make debug
```

## Release build and run binary

```bash
# Build and run in release with default configuration
make run

# Or
# Build
make build

# Help
./mithril-signer --help

# Run
./mithril-signer

# Run in a specific mode
./mithril-signer -r testnet

# Run with custom configuration with env vars
NETWORK=testnet AGGREGATOR_ENDPOINT=http://aggregator.api.mithril.network/aggregator ./mithril-signer
```

## Build and run Docker container

```bash
# Build Docker image
make docker-build

# Run Docker container
make docker-run
```

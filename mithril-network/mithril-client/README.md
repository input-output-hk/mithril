# Mithril Network / Mithril Client

**This is a work in progress** :hammer_and_wrench:s

* A **Mithril Client** is able to download, verify authenticity and restore a snapshot into a **Cardano Node**.
* It allows user to bootstrap a **Cardano Node** in minutes instead of days.
* This cli implements a MVP version of a **Mithril Client**.

---
## Pre-requisites:

**Install Rust**

- Install a [correctly configured](https://www.rust-lang.org/learn/get-started) Rust toolchain (version 1.58.0+). 


## Download source code:
```bash
# Download sources from github
git clone https://github.com/input-output-hk/mithril

# Go to sources directory
cd mithril-network/mithril-client
```

## Development test and build:
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

## Release build and run binary:
```bash
# Build and run in release with default configuration
make run list

# Or
# Build
make build

# Help
./mithril-client --help

# Run
./mithril-client
```

## Build and run Docker container:

```bash
# Build Docker image
cd ../../
docker build -t mithril/mithril-client -f mithril-network/mithril-client/Dockerfile .

# Run Docker container
docker run --rm --name='mithril-client' mithril/mithril-client
```

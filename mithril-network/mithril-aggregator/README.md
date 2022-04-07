# Mithril Aggregator

**This is a work in progress** :hammer_and_wrench:

This is a first version of the Mithril Aggregagator

---
## Pre-requisites:

**Install Rust**

- Install a [correctly configured](https://www.rust-lang.org/learn/get-started) Rust toolchain (version 1.58.0+). 


## Download source code:
```bash
# Download sources from github
git clone https://github.com/input-output-hk/mithril

# Go to sources directory
cd mithril-network/mithril-aggregator
```

## Development test and build:
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

## Release build and run binary:
```bash
# Build and run in release with default configuration
make run

# Or
# Build
make build

# Help
./mithril-aggregator --help

# Run
./mithril-aggregator
```

## Build and run Docker container:

```bash
# Build Docker image
cd ../../
docker build -t mithril/mithril-aggregator -f mithril-network/mithril-aggregator/Dockerfile .

# Run Docker container
docker run --rm --name='mithril-aggregator' mithril/mithril-aggregator
```

## Interact with the Mithril Aggregator
```bash
# Interact with the aggregator through the OpenAPI UI
open -u https://input-output-hk.github.io/mithril/openapi-ui/
```


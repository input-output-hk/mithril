# Protocol Demo / Rust cli

**This is a work in progress** :hammer_and_wrench:

This cli implements a very simple version of the Mithril protocol for demonstration only

---
## Pre-requisites:

**Install Rust**

- Install a [correctly configured](https://www.rust-lang.org/learn/get-started) Rust toolchain (version 1.58.0+). 


## Download source code:
```bash
# Download sources from github
git clone https://github.com/input-output-hk/mithril

# Go to sources directory
cd rust-demo
```

## Development test and build:
```bash
# Test
make test

# Help
make help

# Run with default configuration
make run
```

## Release build and run binary:
```bash
# Build
make build

# Help
./mithrildemo --help

# Run
./mithrildemo

# Run with custom configuration
./mithrildemo -k 5 -m 50 --phi-f 0.65 --nparties 5 --nmessages 2
```

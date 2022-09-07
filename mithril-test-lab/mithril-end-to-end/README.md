# Mithril End To End Tests Suite

* Integration test suite handling Mithril signature, certificate generation and snapshot verification process.

---

## Pre-requisites

**Install Rust**

* Install a [correctly configured](https://www.rust-lang.org/learn/get-started) Rust toolchain (latest stable version).
* Install Rust [Clippy](https://github.com/rust-lang/rust-clippy) component.

## Download source code

```bash
# Download sources from github
git clone https://github.com/input-output-hk/mithril

# Go to sources directory
cd mithril-test-lab/mithril-end-to-end
```

## build and run the test suite

```bash
# Build
make build

# Help
./mithril-end-to-end --help

# Run
./mithril-end-to-end --db-directory db/ --bin-directory ../../target/release
```

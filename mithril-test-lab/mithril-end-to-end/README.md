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

## Build and run the test suite

```bash
# Build
make build

# Help
./mithril-end-to-end --help

# Run
./mithril-end-to-end --db-directory db/ --bin-directory ../../target/release
```

## Build and run the stress tester

```bash
# Build
make build

# Help
./load-aggregator --help

# Run with 100 signers
./load-aggregator -vvv --cardano-cli-path script/mock-cardano-cli --aggregator-dir ../../target/release --num-signers=100
```

## Launch a monitor

```bash
# Run monitor
make monitor
```

You will have acces to the following dashboards:

| Dashboard | URL | Credentials
|------------|------------|------------
| **Grafana** | [http://0.0.0.0:3000](http://0.0.0.0:3000/dashboards) | mithril/mithril
| **Prometheus** | [http://0.0.0.0:9090](http://0.0.0.0:9090/) | -
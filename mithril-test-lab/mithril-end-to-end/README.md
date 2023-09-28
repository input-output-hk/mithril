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

## Build and run an aggregator stress test

```bash
# Build
make build

# Help
./load-aggregator --help

# Run with 100 signers and 0 clients
./load-aggregator -vvv --cardano-cli-path script/mock-cardano-cli --aggregator-dir ../../target/release --num-signers=100

# Run with 100 signers and 200 clients
./load-aggregator -vvv --cardano-cli-path script/mock-cardano-cli --aggregator-dir ../../target/release --num-signers=100 --num-clients=200
```

## Benchmark aggregator performances

```bash
# Build the load aggregator tool
make build

# Run a benchmark for [10, 20, 30 40, 50] signers x [100, 200, 300] clients
MIN_SIGNERS=10 MAX_SIGNERS=50 STEP_SIGNERS=10 MIN_CLIENTS=100 MAX_CLIENTS=300 STEP_CLIENTS=100 ./benchmark-aggregator.sh
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
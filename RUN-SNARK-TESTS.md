# Running SNARK Proofs Tests

This guide walks you through running SNARK proof tests for Mithril, covering both unit tests and the full end-to-end test suite.

## Prerequisites

- **Operating System**: Linux (Ubuntu 24.04 or later)
- **Rust toolchain**: latest stable version

## Getting Started

Clone the Mithril repository:

```bash
git clone git@github.com:input-output-hk/mithril.git
cd mithril
```

## Unit Tests

Run the full workspace unit tests with the `future_snark` feature enabled:

```bash
cargo test --all-features --features future_snark
```

## End-to-End Test

### Branch Setup

Switch to the dedicated SNARK end-to-end test branch:

```bash
git switch jpraynaud/3107-snark-e2e-test
```

### Working Directory Configuration

> **Important**: The working directory path must be kept short because the Cardano node uses local socket files with path length limitations. Use an absolute path such as the example below. **Do not** use `.` as the working directory, this could delete your entire repository.

Set the working directory environment variable:

```bash
WORK_DIRECTORY=~/Desktop/mithril/artifacts
```

### Running the Test

The following command compiles all required nodes and runs the end-to-end test with SNARK proofs. Expect the full process to take approximately **10 to 20 minutes** depending on your machine:

```bash
cargo build --release --features future_snark \
  -p mithril-aggregator \
  -p mithril-signer \
  -p mithril-client-cli \
  -p mithril-relay \
  -p mithril-end-to-end \
&& RUST_BACKTRACE=1 ./target/release/mithril-end-to-end -vvvv \
  --bin-directory ./target/release/ \
  --work-directory=$WORK_DIRECTORY \
  --devnet-scripts-directory=./mithril-test-lab/mithril-devnet \
  --mithril-era=lagrange \
  --aggregate-signature-type=Snark \
  --cardano-epoch-length=50 \
  --cardano-slot-length=1.0 \
  --mithril-run-interval 500
```

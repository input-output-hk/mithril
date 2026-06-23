# Running Cardano Blocks and Transactions certification

This guide walks you through running Cardano Blocks and Transactions certification tests for Mithril, covering both unit tests and the full end-to-end test suite.

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

Run the full workspace unit tests:

```bash
cargo test --all-features
```

## End-to-End Test

### Working Directory Configuration

> **Important**: The working directory path must be kept short because the Cardano node uses local socket files with path length limitations. Use an absolute path such as the example below. **Do not** use `.` as the working directory, this could delete your entire repository.

Set the working directory environment variable:

```bash
WORK_DIRECTORY=~/Desktop/mithril/artifacts
```

### Running the Test

The following command compiles all required nodes and runs the **full** end-to-end test for the certification of the Cardano Blocks and Transactions:

```bash
cargo build --release \
  -p mithril-aggregator \
  -p mithril-signer \
  -p mithril-client-cli \
  -p mithril-relay \
  -p mithril-end-to-end \
&& RUST_BACKTRACE=1 ./target/release/mithril-end-to-end -vvvv \
  --bin-directory ./target/release/ \
  --work-directory=$WORK_DIRECTORY \
  --devnet-scripts-directory=./mithril-test-lab/cardano-devnet \
  --cardano-node-version=11.0.1 \
  --mithril-era=pythagoras \
  --aggregate-signature-type Concatenation \
  full \
  --signed-entity-types=CardanoBlocksTransactions
```

The following command compiles all required nodes and runs the **run-only** end-to-end test for the certification of the Cardano Blocks and Transactions, which will keep the aggregator up and running until the test is manually stopped:

```bash
cargo build --release \
  -p mithril-aggregator \
  -p mithril-signer \
  -p mithril-client-cli \
  -p mithril-relay \
  -p mithril-end-to-end \
&& RUST_BACKTRACE=1 ./target/release/mithril-end-to-end -vvvv \
  --bin-directory ./target/release/ \
  --work-directory=$WORK_DIRECTORY \
  --devnet-scripts-directory=./mithril-test-lab/cardano-devnet \
  --cardano-node-version=11.0.1 \
  --mithril-era=pythagoras \
  --aggregate-signature-type Concatenation \
  --cardano-slot-length 1.0 \
  --cardano-epoch-length 30 \
  --mithril-run-interval 1000 \
  run-only \
  --signed-entity-types=CardanoBlocksTransactions
```

## Explorer

You can visualize the results of the end-to-end tests using the Mithril Explorer:

- Open [https://mithril.network/explorer/unstable/?aggregator=http%3A%2F%2Flocalhost%3A8080%2Faggregator](https://mithril.network/explorer/unstable/?aggregator=http%3A%2F%2Flocalhost%3A8080%2Faggregator) with your browser to see the result.

The certification of the Cardano Blocks and Transactions is also running on the `testing-preview` network:

- Open [https://mithril.network/explorer/unstable/?aggregator=https%3A%2F%2Faggregator.testing-preview.api.mithril.network%2Faggregator](https://mithril.network/explorer/unstable/?aggregator=https%3A%2F%2Faggregator.testing-preview.api.mithril.network%2Faggregator) with your browser to see the result.

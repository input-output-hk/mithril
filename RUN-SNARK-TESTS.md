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

### Branch Setup

Switch to the dedicated SNARK end-to-end test branch:

```bash
git switch jpraynaud/3107-snark-e2e-test
```

## Unit Tests

Run the full workspace unit tests with the `future_snark` feature enabled:

```bash
cargo test --all-features --features future_snark
```

## End-to-End Test

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
  --cardano-node-version=11.0.1 \
  --mithril-era=lagrange \
  --aggregate-signature-type=Snark \
  --cardano-epoch-length=50 \
  --cardano-slot-length=1.0 \
  --mithril-run-interval 500
```

## Devnet

### Run the devnet

Switch to the devnet directory:

```bash
cd mithril-test-lab/mithril-devnet
```

Start the devnet with the SNARK proofs configuration:

```bash
MITHRIL_NODE_DOCKER_BUILD_TYPE=ci \
MITHRIL_ERA=Lagrange \
MITHRIL_AGGREGATE_SIGNATURE_TYPE=Snark \
MITHRIL_RUN_INTERVAL=500 \
MITHRIL_PROTOCOL_PARAMETERS_K=5 \
MITHRIL_PROTOCOL_PARAMETERS_M=9 \
MITHRIL_PROTOCOL_PARAMETERS_PHI_F=0.95 \
CARDANO_NODE_VERSION=11.0.1 \
SLOT_LENGTH=1.0 \
EPOCH_LENGTH=50 \
./devnet-run.sh
```

## Explorer

In a separate terminal, you can run the explorer to visualize the devnet by switching to the `mithril-explorer` directory:

```bash
cd mithril-explorer
```

Then build the WASM client with the `future_snark` feature enabled:

```bash
WASM_PACK_ARGS="-- --features future_snark" make -C ../mithril-client-wasm build
```

Then, run the development server:

```bash
make dev
```

Open [http://localhost:3000/explorer/?aggregator=https%3A%2F%2Faggregator.dev-follower-preview.api.mithril.network%2Faggregator](http://localhost:3000/explorer/?aggregator=https%3A%2F%2Faggregator.dev-follower-preview.api.mithril.network%2Faggregator) with your browser to see the result.

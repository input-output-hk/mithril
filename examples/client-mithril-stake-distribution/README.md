# Mithril client library example: Mithril stake distribution

## Description

This example shows how to implement a Mithril client and use the features related to the `Mithril stake distribution` type.

In this example, the client interacts by default with a real aggregator on the network `release-preprod` to:

- list the available Mithril stake distributions
- get a single Mithril stake distribution
- verify a certificate chain
- compute a message for a Mithril stake distribution
- verify that the certificate signs the computed message

The crates [`slog`](https://docs.rs/slog/latest/slog/) and [`slog_term`](https://docs.rs/slog-term/latest/slog_term/) are used to report the progress to the console.

## Build and run the example

```bash
# Switch to the latest release tag
git checkout tags/$(curl -sSL https://api.github.com/repos/input-output-hk/mithril/releases/latest | jq -r '.tag_name')

# Build from the crate directory
cargo build

# Run from the crate directory
cargo run

# Run with your custom network configuration
AGGREGATOR_ENDPOINT=YOUR_AGGREGATOR_ENDPOINT GENESIS_VERIFICATION_KEY=YOUR_GENESIS_VERIFICATION_KEY cargo run

# Example with 'release-preprod' network
AGGREGATOR_ENDPOINT=https://aggregator.release-preprod.api.mithril.network/aggregator GENESIS_VERIFICATION_KEY=$(curl -s https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/genesis.vkey) cargo run
```

## Links

- **Developer documentation**: https://docs.rs/mithril-client/latest/mithril_client/
- **Crates.io**: https://crates.io/crates/mithril-client

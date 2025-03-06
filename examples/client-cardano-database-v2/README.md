# Mithril client library example: Cardano database

## Description

This example shows how to implement a Mithril client and use its features related to the `Cardano database` type.

In this example, the client interacts by default with a real aggregator on the network `testing-preview` to:

- list the available snapshots
- get a single snapshot
- download and unpack snapshot archives tailored for a specific range
- verify the associated certificate and its chain
- compute a message for the retrieved artifact files
- verify that the certificate signs the computed message
- increments snapshot download statistics

The crate [indicatif](https://docs.rs/indicatif/latest/indicatif/) is used to nicely report the progress to the console.

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
AGGREGATOR_ENDPOINT=https://aggregator.testing-preview.api.mithril.network/aggregator GENESIS_VERIFICATION_KEY=$(curl -s https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/testing-preview/genesis.vkey) cargo run
```

## Links

- **Developer documentation**: https://docs.rs/mithril-client/latest/mithril_client/
- **Crates.io**: https://crates.io/crates/mithril-client

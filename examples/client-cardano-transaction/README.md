# Mithril client library example: Cardano transaction

> [!WARNING]
> This example makes use of unstable features of the Mithril client library.
> Use them at your own risk, and expect possible modifications in future releases.
> Please refer to the links provided at the end of this file for the most up-to-date developer documentation.

## Description

This example shows how to implement a Mithril client and use its features related to the `Cardano Transaction` type.

In this example, the client interacts with an aggregator and performs the following operations:

- Retrieve cryptographic proofs of membership of the Cardano transactions set for a list of transactions passed as arguments
- Verify the validity of the proofs
- Verify the validity of the validity of the certificate chain attached to the proofs
- Verify that the certificate chain signs a message computed from the proof

## Build and run the example

```bash
# Switch to the latest release tag
git checkout tags/$(curl -sSL https://api.github.com/repos/input-output-hk/mithril/releases/latest | jq -r '.tag_name')

# Build from the crate directory
cargo build

# Run from the crate directory
AGGREGATOR_ENDPOINT=YOUR_AGGREGATOR_ENDPOINT GENESIS_VERIFICATION_KEY=YOUR_GENESIS_VERIFICATION_KEY cargo run CARDANO_TX_HASH1,CARDANO_TX_HASH2,CARDANO_TX_HASH3

# Example with from 'release-preprod' network
AGGREGATOR_ENDPOINT=https://aggregator.release-preprod.api.mithril.network/aggregator GENESIS_VERIFICATION_KEY=$(curl -s https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/release-preprod/genesis.vkey) cargo run 0ea207ab71493f012faab0d1f8151eaf931901141c1482ce6e9a501498076484,326b5b67d926937bf19c6113d0957a39f2eae9df94875ce8a96eff5c8521303b
```

## Links

- **Developer documentation**: https://docs.rs/mithril-client/latest/mithril_client/
- **Crates.io**: https://crates.io/crates/mithril-client

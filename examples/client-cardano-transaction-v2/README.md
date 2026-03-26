# Mithril client library example: Cardano transaction V2

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
# Build from the crate directory
cargo build

# Run from the crate directory
AGGREGATOR_ENDPOINT=YOUR_AGGREGATOR_ENDPOINT GENESIS_VERIFICATION_KEY=YOUR_GENESIS_VERIFICATION_KEY cargo run CARDANO_TX_HASH1,CARDANO_TX_HASH2,CARDANO_TX_HASH3

# Example with from 'testing-preview' network
AGGREGATOR_ENDPOINT=https://aggregator.testing-preview.api.mithril.network/aggregator GENESIS_VERIFICATION_KEY=$(curl -s https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/testing-preview/genesis.vkey) cargo run 472f2e8b2ed5965d8899118dc6eabc5d366d96f34cf6941ebd702ca9de88c265,40cebe503aa0137aff67d92c3c117bee57d5e0bb250848091aa9455a8169a900
```

## Links

- **Developer documentation**: https://docs.rs/mithril-client/latest/mithril_client/
- **Crates.io**: https://crates.io/crates/mithril-client

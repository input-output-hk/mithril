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
# Build from the crate directory
cargo build

# Run from the crate directory
AGGREGATOR_ENDPOINT=YOUR_AGGREGATOR_ENDPOINT GENESIS_VERIFICATION_KEY=YOUR_GENESIS_VERIFICATION_KEY cargo run CARDANO_TX_HASH1,CARDANO_TX_HASH2,CARDANO_TX_HASH3

# Example with from 'testing-sanchonet' network
AGGREGATOR_ENDPOINT=https://aggregator.testing-sanchonet.api.mithril.network/aggregator GENESIS_VERIFICATION_KEY=$(curl -s https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/testing-sanchonet/genesis.vkey) cargo run db0dfab664045b117375a743a925385a7a3fa6a104f8bd95fa0f748088bcaff0,b457a094439cc5e371474f5758b4ecded3e1b035fe0717e39d78080e6fe169b2
```

## Links
- **Developer documentation**: https://docs.rs/mithril-client/latest/mithril_client/
- **Crates.io**: https://crates.io/crates/mithril-client
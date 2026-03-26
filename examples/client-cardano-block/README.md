# Mithril client library example: Cardano block

> [!WARNING]
> This example makes use of unstable features of the Mithril client library.
> Use them at your own risk, and expect possible modifications in future releases.
> Please refer to the links provided at the end of this file for the most up-to-date developer documentation.

## Description

This example shows how to implement a Mithril client and use its features related to the `Cardano Block` type.

In this example, the client interacts with an aggregator and performs the following operations:

- Retrieve cryptographic proofs of membership of the Cardano blocks set for a list of blocks passed as arguments
- Verify the validity of the proofs
- Verify the validity of the validity of the certificate chain attached to the proofs
- Verify that the certificate chain signs a message computed from the proof

## Build and run the example

```bash
# Build from the crate directory
cargo build

# Run from the crate directory
AGGREGATOR_ENDPOINT=YOUR_AGGREGATOR_ENDPOINT GENESIS_VERIFICATION_KEY=YOUR_GENESIS_VERIFICATION_KEY cargo run CARDANO_BLOCK_HASH1,CARDANO_BLOCK_HASH2,CARDANO_BLOCK_HASH3

# Example with from 'testing-preview' network
AGGREGATOR_ENDPOINT=https://aggregator.testing-preview.api.mithril.network/aggregator GENESIS_VERIFICATION_KEY=$(curl -s https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration/testing-preview/genesis.vkey) cargo run df4c5cc9e835a0be1c00dba0fd4bba2e83279b1e36fde31304d50ffae8953af1,343518f182e044fccd540ffa778e232eb18e8b67d2ba250f4c7c85cea9ffef36
```

## Links

- **Developer documentation**: https://docs.rs/mithril-client/latest/mithril_client/
- **Crates.io**: https://crates.io/crates/mithril-client

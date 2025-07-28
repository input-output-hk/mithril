# Mithril-client [![CI workflow](https://github.com/input-output-hk/mithril/actions/workflows/ci.yml/badge.svg)](https://github.com/input-output-hk/mithril/actions/workflows/ci.yml) [![crates.io](https://img.shields.io/crates/v/mithril-client.svg)](https://crates.io/crates/mithril-client) [![License](https://img.shields.io/badge/license-Apache%202.0-blue?style=flat-square)](https://github.com/input-output-hk/mithril/blob/main/LICENSE) [![Discord](https://img.shields.io/discord/500028886025895936.svg?logo=discord&style=flat-square)](https://discord.gg/5kaErDKDRq)

**This is a work in progress** 🛠

- `mithril-client` defines all the tooling necessary to manipulate Mithril certified types available from a Mithril aggregator.

- The different types of available data certified by Mithril are:
  - Cardano Database v1 (aka Snapshot): list, get, download archive and record statistics.
  - Cardano Database v2: list, get, download archive and record statistics.
  - Mithril stake distribution: list and get.
  - Cardano transactions: list & get snapshot, get proofs.
  - Cardano stake distribution: list, get and get by epoch.
  - Certificates: list, get, and chain validation.

## Example

Below is a basic example of how to use most of the functions exposed by the Mithril client library:

```rust
use mithril_client::{ClientBuilder, MessageBuilder};
use std::path::Path;

#[tokio::main]
async fn main() -> mithril_client::MithrilResult<()> {
    let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;

    let snapshots = client.cardano_database().list().await?;

    let last_digest = snapshots.first().unwrap().digest.as_ref();
    let snapshot = client.cardano_database().get(last_digest).await?.unwrap();

    let certificate = client
        .certificate()
        .verify_chain(&snapshot.certificate_hash)
        .await?;

    // Note: the directory must already exist, and the user running this code must have read/write access to it.
    let target_directory = Path::new("YOUR_TARGET_DIRECTORY");
    client
        .cardano_database()
        .download_unpack(&snapshot, target_directory)
        .await?;

    if let Err(e) = client.cardano_database().add_statistics(&snapshot).await {
        println!("Could not increment snapshot download statistics: {:?}", e);
    }

    let message = MessageBuilder::new()
        .compute_snapshot_message(&certificate, target_directory)
        .await?;
    assert!(certificate.match_message(&message));

    Ok(())
}
```

## Different arithmetic libraries

Under the hood mithril client could use different arithmetic libraries:

- [rug](https://crates.io/crates/rug)
- [num](https://crates.io/crates/num)

You can switch beetwen them by using `rug-backend` and `num-integer-backend` respectively.
For Windows and WASM target platforms only `num-integer-backend` is available,
for others `rug-backend` is a default option.

## Getting Help

First, check our [Developer documentation](https://mithril.network/doc/manual/developer-docs/nodes/mithril-client-library).

If you need more information, feel free to join IOG's Technical Community [discord server](https://discord.gg/5kaErDKDRq).

## Contributing

Thanks for considering contributing and help us on creating the Mithril protocol!

The best way to contribute right now is to try things out and provide feedback,
but we also accept contributions to the documentation and obviously to the
code itself.

When contributing to this project and interacting with others, please follow our [Code of Conduct](https://github.com/input-output-hk/mithril/blob/main/CODE-OF-CONDUCT.md) and our [Contributing Guidelines](https://github.com/input-output-hk/mithril/blob/main/CONTRIBUTING.md).

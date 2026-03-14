# Mithril-client [![CI workflow](https://github.com/input-output-hk/mithril/actions/workflows/ci.yml/badge.svg)](https://github.com/input-output-hk/mithril/actions/workflows/ci.yml) [![crates.io](https://img.shields.io/crates/v/mithril-client.svg)](https://crates.io/crates/mithril-client) [![License](https://img.shields.io/badge/license-Apache%202.0-blue?style=flat-square)](https://github.com/input-output-hk/mithril/blob/main/LICENSE) [![Discord](https://img.shields.io/discord/500028886025895936.svg?logo=discord&style=flat-square)](https://discord.gg/5kaErDKDRq)

**This is a work in progress** 🛠

- `mithril-client` defines all the tooling necessary to manipulate Mithril certified types available from a Mithril aggregator.

- The different types of available data certified by Mithril are:
  - Cardano Database: list, get, download archive and record statistics.
  - Mithril stake distribution: list and get.
  - Cardano transactions: list & get snapshot, get proofs.
  - Cardano stake distribution: list, get and get by epoch.
  - Certificates: list, get, and chain validation.

## Example

Below is a basic example of how to use most of the functions exposed by the Mithril client library:

```rust
use mithril_client::{ClientBuilder, MessageBuilder};
use mithril_client::cardano_database_client::{DownloadUnpackOptions, ImmutableFileRange};
use std::path::Path;

#[tokio::main]
async fn main() -> mithril_client::MithrilResult<()> {
    let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;

    let snapshots = client.cardano_database_v2().list().await?;

    let last_hash = snapshots.first().unwrap().hash.as_ref();
    let snapshot = client.cardano_database_v2().get(last_hash).await?.unwrap();

    let certificate = client
        .certificate()
        .verify_chain(&snapshot.certificate_hash)
        .await?;

    // Note: the directory must already exist, and the user running this code must have read/write access to it.
    let target_directory = Path::new("YOUR_TARGET_DIRECTORY");
    let immutable_file_range = ImmutableFileRange::Full;
    let download_unpack_options = DownloadUnpackOptions::default();
    client
        .cardano_database_v2()
        .download_unpack(
            &snapshot,
            &immutable_file_range,
            target_directory,
            download_unpack_options,
        )
        .await?;

    let verified_digests = client
        .cardano_database_v2()
        .download_and_verify_digests(&certificate, &snapshot)
        .await?;
    let merkle_proof = client
        .cardano_database_v2()
        .verify_cardano_database(
            &certificate,
            &snapshot,
            &immutable_file_range,
            false,
            target_directory,
            &verified_digests,
        )
        .await?;

    let full_restoration = immutable_file_range == ImmutableFileRange::Full;
    let include_ancillary = download_unpack_options.include_ancillary;
    let number_of_immutable_files_restored =
        immutable_file_range.length(snapshot.beacon.immutable_file_number);
    if let Err(e) = client
        .cardano_database_v2()
        .add_statistics(
            full_restoration,
            include_ancillary,
            number_of_immutable_files_restored,
        )
        .await
    {
        println!("Could not increment snapshot download statistics: {e:?}");
    }

    let message = MessageBuilder::new()
        .compute_cardano_database_message(&certificate, &merkle_proof)
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

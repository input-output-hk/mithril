---
sidebar_position: 4
---

# Mithril client library

:::info

Mithril client library can be used by Rust developers to use the Mithril network in their applications.

It is responsible for handling the different types of data certified by Mithril and available through a Mithril aggregator:

- [**Cardano transaction**](../../../glossary.md#cardano-transaction): list and get snapshots, get proofs
- [**Cardano stake distribution**](../../../glossary.md#stake-distribution): list, get, and get by epoch
- [**Cardano database**](../../../glossary.md#cardano-database): list, get, download the tarball, and record statistics
- [**Mithril stake distribution**](../../../glossary.md#stake-distribution): list and get
- [**Certificate**](../../../glossary.md#certificate): list, get, and chain validation.

:::

:::tip

- For more information about the **Mithril network**, please see the [architecture](../../../mithril/advanced/mithril-network/architecture.md) overview

- For more information about the **Mithril client** node, please see [this overview](../../../mithril/advanced/mithril-network/client.md)

- Check out the [`Bootstrap a Cardano node`](../../getting-started/bootstrap-cardano-node.md) guide.

:::

:::info

The Mithril network configurations are available in the [**Network configurations**](../../getting-started/network-configurations.md) section of the user manual.

:::

## Resources

|        Node        |                                     Source repository                                      |                                Rust documentation                                 |                         Network configurations                         |
| :----------------: | :----------------------------------------------------------------------------------------: | :-------------------------------------------------------------------------------: | :--------------------------------------------------------------------: |
| **Mithril client** | [:arrow_upper_right:](https://github.com/input-output-hk/mithril/tree/main/mithril-client) | [:arrow_upper_right:](https://mithril.network/rust-doc/mithril_client/index.html) | [:arrow_upper_right:](../../getting-started/network-configurations.md) |

## Prerequisites

- Install the latest stable version of the [correctly configured](https://www.rust-lang.org/learn/get-started) Rust toolchain

- Install build tools `build-essential` and `m4`; for example, on Ubuntu/Debian/Mint, run `sudo apt install build-essential m4`

- Install OpenSSL development libraries; for example, on Ubuntu/Debian/Mint, run `apt install libssl-dev`.

## Installation

In your project, use `cargo` to add [mithril-client](https://crates.io/crates/mithril-client) crate as a dependency:

```bash
cargo add mithril-client
```

:::info

Mithril client is an asynchronous library. You will need a runtime to execute your futures. We recommend using the crate [tokio](https://crates.io/crates/tokio), as the library has been tested with it.

:::

## Using the Mithril client library

### Cardano transactions

Here is a basic example of the code targetting the `release-preprod` network aggregator:

```rust title="/src/main.rs"
use mithril_client::{ClientBuilder, MessageBuilder, MithrilResult};

#[tokio::main]
async fn main() -> MithrilResult<()> {
    const AGGREGATOR_ENDPOINT: &str =
        "https://aggregator.release-preprod.api.mithril.network/aggregator";
    const GENESIS_VERIFICATION_KEY: &str = "5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d";
    let client = ClientBuilder::aggregator(AGGREGATOR_ENDPOINT, GENESIS_VERIFICATION_KEY)
        .with_origin_tag(Some("EXAMPLE".to_string()))
        .build()?;

    let transactions_hashes = [
        "f9b5221b3ead45d46c0ecae6bee18a0746c5694d0285281cca1b651bce5f49a5",
        "7769e8b78cc86890660ff5451c110b0a0d0413c8b8ebb17a64e017b4cd881777",
    ];
    let cardano_transaction_proof = client
        .cardano_transaction()
        .get_proofs(&transactions_hashes)
        .await
        .unwrap();

    let verified_transactions = cardano_transaction_proof.verify().unwrap();

    let certificate = client
        .certificate()
        .verify_chain(&cardano_transaction_proof.certificate_hash)
        .await
        .unwrap();

    let message = MessageBuilder::new()
        .compute_cardano_transactions_proofs_message(&certificate, &verified_transactions);
    assert!(certificate.match_message(&message));

    println!(
        r###"Cardano transactions with hashes "'{}'" have been successfully certified by Mithril."###,
        verified_transactions.certified_transactions().join(","),
    );
    if !cardano_transaction_proof
        .non_certified_transactions
        .is_empty()
    {
        println!(
            r###"No proof could be computed for Cardano transactions with hashes "'{}'".

            Mithril may not have signed those transactions yet, please try again later."###,
            cardano_transaction_proof
                .non_certified_transactions
                .join(","),
        );
    }

    Ok(())
}
```

:::info

An full example is available in the [Mithril repository](https://github.com/input-output-hk/mithril/tree/main/examples/client-cardano-transaction/src/main.rs). To run it, execute the following command:

```bash
cargo run -p client-cardano-transaction <TRANSACTIONS_HASHES>
```

or directly from the example crate directory:

```bash
cargo run
```

:::

### Cardano stake distribution

Here is a basic example of the code targetting the `release-preprod` network aggregator:

```rust title="/src/main.rs"
use mithril_client::{ClientBuilder, MessageBuilder, MithrilResult};

#[tokio::main]
async fn main() -> MithrilResult<()> {
    const AGGREGATOR_ENDPOINT: &str =
        "https://aggregator.release-preprod.api.mithril.network/aggregator";
    const GENESIS_VERIFICATION_KEY: &str = "5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d";
    let client = ClientBuilder::aggregator(AGGREGATOR_ENDPOINT, GENESIS_VERIFICATION_KEY)
        .with_origin_tag(Some("EXAMPLE".to_string()))
        .build()?;

    let cardano_stake_distributions = client.cardano_stake_distribution().list().await?;
    let last_epoch = cardano_stake_distributions.first().unwrap().epoch;

    let cardano_stake_distribution = client
        .cardano_stake_distribution()
        .get_by_epoch(last_epoch)
        .await?
        .unwrap();

    let certificate = client
        .certificate()
        .verify_chain(&cardano_stake_distribution.certificate_hash)
        .await?;

    let message = MessageBuilder::new()
        .compute_cardano_stake_distribution_message(&certificate, &cardano_stake_distribution)?;

    assert!(certificate.match_message(&message));

    Ok(())
}
```

:::info

An full example is available in the [Mithril repository](https://github.com/input-output-hk/mithril/tree/main/examples/client-cardano-stake-distribution/src/main.rs). To run it, execute the following command:

```bash
cargo run -p client-cardano-stake-distribution
```

or directly from the example crate directory:

```bash
cargo run
```

:::

### Cardano database

Here is a basic example of the code targetting the `release-preprod` network aggregator:

```rust title="/src/main.rs"
use mithril_client::{ClientBuilder, MessageBuilder};
use std::path::Path;

#[tokio::main]
async fn main() -> mithril_client::MithrilResult<()> {
    const AGGREGATOR_ENDPOINT: &str =
        "https://aggregator.release-preprod.api.mithril.network/aggregator";
    const GENESIS_VERIFICATION_KEY: &str = "5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d";
    const ANCILLARY_VERIFICATION_KEY: &str = "5b3138392c3139322c3231362c3135302c3131342c3231362c3233372c3231302c34352c31382c32312c3139362c3230382c3234362c3134362c322c3235322c3234332c3235312c3139372c32382c3135372c3230342c3134352c33302c31342c3232382c3136382c3132392c38332c3133362c33365d";
    let client = ClientBuilder::aggregator(AGGREGATOR_ENDPOINT, GENESIS_VERIFICATION_KEY)
        .set_ancillary_verification_key(ANCILLARY_VERIFICATION_KEY.to_string())
        .with_origin_tag(Some("EXAMPLE".to_string()))
        .build()?;
    let snapshots = client.cardano_database().list().await?;

    let last_digest = snapshots.first().unwrap().digest.as_ref();
    let snapshot = client.cardano_database().get(last_digest).await?.unwrap();

    let certificate = client
        .certificate()
        .verify_chain(&snapshot.certificate_hash)
        .await?;

    // Note: the directory must already exist, and the user running this code must have read/write access to it.
    let target_directory = Path::new(".");
    client
        .cardano_database()
        .download_unpack_full(&snapshot, target_directory)
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

:::info

Snapshot download and certificate chain validation can take quite some time, even with a fast computer and network. We have implemented a feedback mechanism for them; more details are available in the [feedback sub-module](https://mithril.network/rust-doc/mithril_client/feedback/index.html).

An example of implementation with the crate [indicatif](https://crates.io/crates/indicatif) is available in the [Mithril repository](https://github.com/input-output-hk/mithril/tree/main/examples/client-cardano-database/src/main.rs). To run it, execute the following command:

```bash
cargo run -p client-cardano-database
```

or directly from the example crate directory:

```bash
cargo run
```

:::

### Cardano database v2

Below is a basic example using the new `CardanoDatabase` functions. Make sure the target aggregator signs `CardanoDatabase` incremental snapshot.

:::tip

You can verify that the aggregator signs **CardanoDatabase** by running the command below:

```bash
wget -q -O - YOUR_AGGREGATOR_ENDPOINT | jq '.capabilities.signed_entity_types | contains(["CardanoDatabase"])'
```

For example, with the aggregator on the `testing-preview` Mithril network:

```bash
wget -q -O - https://aggregator.testing-preview.api.mithril.network/aggregator | jq '.capabilities.signed_entity_types | contains(["CardanoDatabase"])'
```

:::

```rust title="/src/main.rs"
use mithril_client::{
    cardano_database_client::{DownloadUnpackOptions, ImmutableFileRange},
    ClientBuilder, MessageBuilder,
};
use std::path::Path;

#[tokio::main]
async fn main() -> mithril_client::MithrilResult<()> {
    const AGGREGATOR_ENDPOINT: &str =
        "https://aggregator.testing-preview.api.mithril.network/aggregator";
    const GENESIS_VERIFICATION_KEY: &str = "5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d";
    const ANCILLARY_VERIFICATION_KEY: &str = "5b3138392c3139322c3231362c3135302c3131342c3231362c3233372c3231302c34352c31382c32312c3139362c3230382c3234362c3134362c322c3235322c3234332c3235312c3139372c32382c3135372c3230342c3134352c33302c31342c3232382c3136382c3132392c38332c3133362c33365d";
    let client = ClientBuilder::aggregator(AGGREGATOR_ENDPOINT, GENESIS_VERIFICATION_KEY)
        .set_ancillary_verification_key(ANCILLARY_VERIFICATION_KEY.to_string())
        .with_origin_tag(Some("EXAMPLE".to_string()))
        .build()?;

    let snapshots = client.cardano_database_v2().list().await?;

    let latest_hash = snapshots.first().unwrap().hash.as_ref();
    let snapshot = client
        .cardano_database_v2()
        .get(latest_hash)
        .await?
        .unwrap();

    let certificate = client
        .certificate()
        .verify_chain(&snapshot.certificate_hash)
        .await?;

    let immutable_file_range = ImmutableFileRange::From(15000);
    let download_unpack_options = DownloadUnpackOptions {
        allow_override: true,
        include_ancillary: false,
        ..DownloadUnpackOptions::default()
    };

    // Note: the directory must already exist, and the user running this code must have read/write access to it.
    let target_directory = Path::new(".");
    client
        .cardano_database_v2()
        .download_unpack(
            &snapshot,
            &immutable_file_range,
            &target_directory,
            download_unpack_options,
        )
        .await?;

    let verified_digests = client
        .cardano_database_v2()
        .download_and_verify_digests(
            &certificate,
            &snapshot
        )
        .await?;

    let allow_missing_immutables_files = false;
    let merkle_proof = client
        .cardano_database_v2()
        .verify_cardano_database(
            &certificate,
            &snapshot,
            &immutable_file_range,
            allow_missing_immutables_files,
            &target_directory,
            &verified_digests,
        )
        .await?;

    let message = MessageBuilder::new()
        .compute_cardano_database_message(&certificate, &merkle_proof)
        .await?;
    assert!(certificate.match_message(&message));

    Ok(())
}
```

:::info

An full example is available in the [Mithril repository](https://github.com/input-output-hk/mithril/tree/main/examples/client-cardano-database-v2/src/main.rs). To run it, execute the following command:

```bash
cargo run -p client-cardano-database-v2
```

or directly from the example crate directory:

```bash
cargo run
```

:::

:::tip

You can read the complete [developer documentation](https://mithril.network/rust-doc/mithril_client/index.html).

:::

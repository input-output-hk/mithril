//! A client to retrieve from an aggregator cryptographic proofs of membership for a subset of Cardano transactions.
//!
//! In order to do so it defines a [CardanoTransactionV2Client] which exposes the following features:
//!  - [get][CardanoTransactionV2Client::get_snapshot]: get a [Cardano transaction snapshot][CardanoBlocksTransactionsSnapshot]
//!    data from its hash.
//!  - [list][CardanoTransactionV2Client::list_snapshots]: get the list of the latest available Cardano transaction
//!    snapshot.
//!
//! # Get a Cardano transaction snapshot
//!
//! To get a Cardano transaction snapshot using the [ClientBuilder][crate::client::ClientBuilder].
//!
//! ```no_run
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::ClientBuilder;
//!
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//! let cardano_transaction_snapshot = client.cardano_transaction().get_snapshot("CARDANO_TRANSACTION_SNAPSHOT_HASH").await?.unwrap();
//!
//! println!("Cardano transaction snapshot hash={}, epoch={}", cardano_transaction_snapshot.hash, cardano_transaction_snapshot.epoch);
//! #    Ok(())
//! # }
//! ```
//!
//! # List available Cardano transaction snapshots
//!
//! To list latest available Cardano transaction snapshots using the [ClientBuilder][crate::client::ClientBuilder].
//!
//! ```no_run
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::ClientBuilder;
//!
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//! let cardano_transaction_snapshots = client.cardano_transaction().list_snapshots().await?;
//!
//! for cardano_transaction_snapshot in cardano_transaction_snapshots {
//!     println!("Cardano transaction snapshot hash={}, epoch={}", cardano_transaction_snapshot.hash, cardano_transaction_snapshot.epoch);
//! }
//! #    Ok(())
//! # }
//! ```

use std::sync::Arc;

use crate::{
    CardanoBlocksTransactionsSnapshot, CardanoBlocksTransactionsSnapshotListItem, MithrilResult,
};

/// HTTP client for CardanoTransactionsAPI from the aggregator
pub struct CardanoTransactionV2Client {
    aggregator_requester: Arc<dyn CardanoTransactionV2AggregatorRequest>,
}

/// Define the requests against an aggregator related to Cardano transactions.
#[cfg_attr(test, mockall::automock)]
#[cfg_attr(target_family = "wasm", async_trait::async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait::async_trait)]
pub trait CardanoTransactionV2AggregatorRequest: Send + Sync {
    /// Fetch the list of latest signed Cardano transactions snapshots from the aggregator
    async fn list_latest_snapshots(
        &self,
    ) -> MithrilResult<Vec<CardanoBlocksTransactionsSnapshotListItem>>;

    /// Fetch a Cardano transactions snapshot by its hash from the aggregator.
    async fn get_snapshot(
        &self,
        hash: &str,
    ) -> MithrilResult<Option<CardanoBlocksTransactionsSnapshot>>;
}

impl CardanoTransactionV2Client {
    /// Constructs a new `CardanoTransactionV2Client`.
    pub fn new(aggregator_requester: Arc<dyn CardanoTransactionV2AggregatorRequest>) -> Self {
        Self {
            aggregator_requester,
        }
    }

    /// Fetch a list of signed Cardano transaction snapshots.
    pub async fn list_snapshots(
        &self,
    ) -> MithrilResult<Vec<CardanoBlocksTransactionsSnapshotListItem>> {
        self.aggregator_requester.list_latest_snapshots().await
    }

    /// Get the given Cardano transaction snapshot data. If it cannot be found, a None is returned.
    pub async fn get_snapshot(
        &self,
        hash: &str,
    ) -> MithrilResult<Option<CardanoBlocksTransactionsSnapshot>> {
        self.aggregator_requester.get_snapshot(hash).await
    }
}

#[cfg(test)]
mod tests {
    use mockall::predicate::eq;

    use mithril_common::test::mock_extensions::MockBuilder;

    use crate::CardanoBlocksTransactionsSnapshot;
    use crate::common::test::Dummy;

    use super::*;

    #[tokio::test]
    async fn get_cardano_transactions_snapshot_list() {
        let aggregator_requester =
            MockBuilder::<MockCardanoTransactionV2AggregatorRequest>::configure(|mock| {
                let messages = vec![
                    CardanoBlocksTransactionsSnapshotListItem {
                        hash: "hash-123".to_string(),
                        ..Dummy::dummy()
                    },
                    CardanoBlocksTransactionsSnapshotListItem {
                        hash: "hash-456".to_string(),
                        ..Dummy::dummy()
                    },
                ];
                mock.expect_list_latest_snapshots().return_once(|| Ok(messages));
            });
        let client = CardanoTransactionV2Client::new(aggregator_requester);

        let items = client.list_snapshots().await.unwrap();

        assert_eq!(2, items.len());
        assert_eq!("hash-123".to_string(), items[0].hash);
        assert_eq!("hash-456".to_string(), items[1].hash);
    }

    #[tokio::test]
    async fn get_cardano_transactions_snapshot() {
        let aggregator_requester =
            MockBuilder::<MockCardanoTransactionV2AggregatorRequest>::configure(|mock| {
                let message = CardanoBlocksTransactionsSnapshot {
                    hash: "hash-123".to_string(),
                    merkle_root: "mk-123".to_string(),
                    ..Dummy::dummy()
                };
                mock.expect_get_snapshot()
                    .with(eq(message.hash.clone()))
                    .return_once(|_| Ok(Some(message)));
            });
        let client = CardanoTransactionV2Client::new(aggregator_requester);

        let cardano_transaction_snapshot = client
            .get_snapshot("hash-123")
            .await
            .unwrap()
            .expect("This test returns a cardano transaction V2 snapshot");

        assert_eq!("hash-123", &cardano_transaction_snapshot.hash);
        assert_eq!("mk-123", &cardano_transaction_snapshot.merkle_root);
    }
}

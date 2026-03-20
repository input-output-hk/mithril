//! A client to retrieve from an aggregator cryptographic proofs of membership for a subset of Cardano blocks.
//!
//! In order to do so it defines a [CardanoBlockClient] which exposes the following features:
//!  - [get_proofs][CardanoBlockClient::get_proofs]: get a [cryptographic proof][CardanoBlocksProofs]
//!    that the blocks with given hash are included in the global Cardano blocks set.
//!  - [get][CardanoBlockClient::get_snapshot]: get a [Cardano block snapshot][CardanoBlocksTransactionsSnapshot]
//!    data from its hash.
//!  - [list][CardanoBlockClient::list_snapshots]: get the list of the latest available Cardano block
//!    snapshot.
//!
//!  **Important:** Verifying a proof **only** means that its cryptography is valid, in order to certify that a Cardano
//! blocks subset is valid, the associated proof must be tied to a valid Mithril certificate (see the example below).
//!
//! # Get and verify Cardano block proof
//!
//! To get and verify a Cardano block proof using the [ClientBuilder][crate::client::ClientBuilder].
//!
//! ```no_run
//! #[cfg(feature = "unstable")]
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::{ClientBuilder, MessageBuilder};
//!
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//!
//! // 1 - Get a proof from the aggregator and verify it
//! let cardano_block_proof = client.cardano_block().get_proofs(&["block-1", "block-2"]).await?;
//! println!("Mithril could not certify the following blocks : {:?}", &cardano_block_proof.non_certified_transactions);
//!
//! let verified_blocks = cardano_block_proof.verify()?;
//!
//! // 2 - Verify its associated certificate chain
//! let certificate = client.certificate().verify_chain(&cardano_block_proof.certificate_hash).await?;
//!
//! // 3 - Ensure that the proof is indeed signed in the associated certificate
//! let message = MessageBuilder::new().compute_cardano_blocks_proofs_message(&certificate, &verified_blocks);
//! if certificate.match_message(&message) {
//!     // All green, Mithril certifies that those blocks are part of the Cardano blocks set.
//!     println!("Certified blocks : {:?}", verified_blocks.certified_transactions());
//! }
//! #    Ok(())
//! # }
//! ```
//!
//! # Get a Cardano block snapshot
//!
//! To get a Cardano block snapshot using the [ClientBuilder][crate::client::ClientBuilder].
//!
//! ```no_run
//!  #[cfg(feature = "unstable")]
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::ClientBuilder;
//!
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//! let cardano_block_snapshot = client.cardano_block().get_snapshot("CARDANO_BLOCK_SNAPSHOT_HASH").await?.unwrap();
//!
//! println!("Cardano block snapshot hash={}, epoch={}", cardano_block_snapshot.hash, cardano_block_snapshot.epoch);
//! #    Ok(())
//! # }
//! ```
//!
//! # List available Cardano block snapshots
//!
//! To list latest available Cardano block snapshots using the [ClientBuilder][crate::client::ClientBuilder].
//!
//! ```no_run
//!  #[cfg(feature = "unstable")]
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::ClientBuilder;
//!
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//! let cardano_block_snapshots = client.cardano_block().list_snapshots().await?;
//!
//! for cardano_block_snapshot in cardano_block_snapshots {
//!     println!("Cardano block snapshot hash={}, epoch={}", cardano_block_snapshot.hash, cardano_block_snapshot.epoch);
//! }
//! #    Ok(())
//! # }
//! ```

use anyhow::Context;
use std::sync::Arc;

use crate::{
    CardanoBlocksProofs, CardanoBlocksTransactionsSnapshot,
    CardanoBlocksTransactionsSnapshotListItem, MithrilResult,
};

/// HTTP client for CardanoBlocksAPI from the aggregator
pub struct CardanoBlockClient {
    aggregator_requester: Arc<dyn CardanoBlockAggregatorRequest>,
}

/// Define the requests against an aggregator related to Cardano blocks.
#[cfg_attr(test, mockall::automock)]
#[cfg_attr(target_family = "wasm", async_trait::async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait::async_trait)]
pub trait CardanoBlockAggregatorRequest: Send + Sync {
    /// Get a proof of membership for the given blocks hashes from the aggregator.
    async fn get_proof(&self, hashes: &[String]) -> MithrilResult<Option<CardanoBlocksProofs>>;

    /// Fetch the list of latest signed Cardano blocks snapshots from the aggregator
    async fn list_latest_snapshots(
        &self,
    ) -> MithrilResult<Vec<CardanoBlocksTransactionsSnapshotListItem>>;

    /// Fetch a Cardano blocks snapshot by its hash from the aggregator.
    async fn get_snapshot(
        &self,
        hash: &str,
    ) -> MithrilResult<Option<CardanoBlocksTransactionsSnapshot>>;
}

impl CardanoBlockClient {
    /// Constructs a new `CardanoBlockClient`.
    pub fn new(aggregator_requester: Arc<dyn CardanoBlockAggregatorRequest>) -> Self {
        Self {
            aggregator_requester,
        }
    }

    /// Get proofs that the given subset of blocks is included in the Cardano blocks set.
    pub async fn get_proofs<T: ToString>(
        &self,
        blocks_hashes: &[T],
    ) -> MithrilResult<CardanoBlocksProofs> {
        let blocks_hashes: Vec<String> = blocks_hashes.iter().map(|h| h.to_string()).collect();

        self.aggregator_requester
            .get_proof(&blocks_hashes)
            .await?
            .with_context(|| format!("No proof found for blocks hashes: {blocks_hashes:?}",))
    }

    /// Fetch a list of signed Cardano transaction snapshots.
    pub async fn list_snapshots(
        &self,
    ) -> MithrilResult<Vec<CardanoBlocksTransactionsSnapshotListItem>> {
        self.aggregator_requester.list_latest_snapshots().await
    }

    /// Get the given Cardano Blocks transaction snapshot data. If it cannot be found, a None is returned.
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

    use crate::common::test::Dummy;
    use crate::{CardanoBlock, MkSetProof};

    use super::*;

    #[tokio::test]
    async fn get_cardano_blocks_snapshot_list() {
        let aggregator_requester =
            MockBuilder::<MockCardanoBlockAggregatorRequest>::configure(|mock| {
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
        let client = CardanoBlockClient::new(aggregator_requester);

        let items = client.list_snapshots().await.unwrap();

        assert_eq!(2, items.len());
        assert_eq!("hash-123".to_string(), items[0].hash);
        assert_eq!("hash-456".to_string(), items[1].hash);
    }

    #[tokio::test]
    async fn get_cardano_blocks_snapshot() {
        let aggregator_requester =
            MockBuilder::<MockCardanoBlockAggregatorRequest>::configure(|mock| {
                let message = CardanoBlocksTransactionsSnapshot {
                    hash: "hash-123".to_string(),
                    merkle_root: "mk-123".to_string(),
                    ..Dummy::dummy()
                };
                mock.expect_get_snapshot()
                    .with(eq(message.hash.clone()))
                    .return_once(|_| Ok(Some(message)));
            });
        let client = CardanoBlockClient::new(aggregator_requester);

        let cardano_block_snapshot = client
            .get_snapshot("hash-123")
            .await
            .unwrap()
            .expect("This test returns a cardano block snapshot");
        assert_eq!("hash-123", &cardano_block_snapshot.hash);
        assert_eq!("mk-123", &cardano_block_snapshot.merkle_root);
    }

    #[tokio::test]
    async fn test_get_proof_ok() {
        let set_proof = MkSetProof::<CardanoBlock>::dummy();
        let expected_blocks_proofs = CardanoBlocksProofs {
            certified_blocks: Some(set_proof.clone()),
            ..Dummy::dummy()
        };

        let aggregator_requester =
            MockBuilder::<MockCardanoBlockAggregatorRequest>::configure(|mock| {
                let message = expected_blocks_proofs.clone();
                mock.expect_get_proof()
                    .with(eq(message
                        .certified_blocks
                        .iter()
                        .flat_map(|proof| proof.items.clone())
                        .map(|block| block.block_hash)
                        .collect::<Vec<_>>()))
                    .return_once(|_| Ok(Some(message)));
            });
        let client = CardanoBlockClient::new(aggregator_requester);

        let blocks_proofs = client
            .get_proofs(
                &set_proof
                    .items
                    .iter()
                    .map(|h| h.block_hash.clone())
                    .collect::<Vec<_>>(),
            )
            .await
            .unwrap();

        assert_eq!(expected_blocks_proofs, blocks_proofs);
    }

    #[tokio::test]
    async fn test_get_proof_ko() {
        let aggregator_requester =
            MockBuilder::<MockCardanoBlockAggregatorRequest>::configure(|mock| {
                mock.expect_get_proof()
                    .return_once(move |_| Err(anyhow::anyhow!("an error")));
            });
        let client = CardanoBlockClient::new(aggregator_requester);

        client
            .get_proofs(&["tx-123"])
            .await
            .expect_err("The certificate client should fail here.");
    }
}

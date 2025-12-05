//! A client to retrieve from an aggregator cryptographic proofs of membership for a subset of Cardano transactions.
//!
//! In order to do so it defines a [CardanoTransactionClient] which exposes the following features:
//!  - [get_proofs][CardanoTransactionClient::get_proofs]: get a [cryptographic proof][CardanoTransactionsProofs]
//!    that the transactions with given hash are included in the global Cardano transactions set.
//!  - [get][CardanoTransactionClient::get_snapshot]: get a [Cardano transaction snapshot][CardanoTransactionSnapshot]
//!    data from its hash.
//!  - [list][CardanoTransactionClient::list_snapshots]: get the list of the latest available Cardano transaction
//!    snapshot.
//!
//!  **Important:** Verifying a proof **only** means that its cryptography is valid, in order to certify that a Cardano
//! transactions subset is valid, the associated proof must be tied to a valid Mithril certificate (see the example below).
//!
//! # Get and verify Cardano transaction proof
//!
//! To get and verify a Cardano transaction proof using the [ClientBuilder][crate::client::ClientBuilder].
//!
//! ```no_run
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::{ClientBuilder, MessageBuilder};
//!
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//!
//! // 1 - Get a proof from the aggregator and verify it
//! let cardano_transaction_proof = client.cardano_transaction().get_proofs(&["tx-1", "tx-2"]).await?;
//! println!("Mithril could not certify the following transactions : {:?}", &cardano_transaction_proof.non_certified_transactions);
//!
//! let verified_transactions = cardano_transaction_proof.verify()?;
//!
//! // 2 - Verify its associated certificate chain
//! let certificate = client.certificate().verify_chain(&cardano_transaction_proof.certificate_hash).await?;
//!
//! // 3 - Ensure that the proof is indeed signed in the associated certificate
//! let message = MessageBuilder::new().compute_cardano_transactions_proofs_message(&certificate, &verified_transactions);
//! if certificate.match_message(&message) {
//!     // All green, Mithril certifies that those transactions are part of the Cardano transactions set.
//!     println!("Certified transactions : {:?}", verified_transactions.certified_transactions());
//! }
//! #    Ok(())
//! # }
//! ```
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

use anyhow::Context;
use std::sync::Arc;

use crate::{
    CardanoTransactionSnapshot, CardanoTransactionSnapshotListItem, CardanoTransactionsProofs,
    MithrilResult,
};

/// HTTP client for CardanoTransactionsAPI from the Aggregator
pub struct CardanoTransactionClient {
    aggregator_requester: Arc<dyn CardanoTransactionAggregatorRequest>,
}

/// Define the requests against an Aggregator related to Cardano transactions.
#[cfg_attr(test, mockall::automock)]
#[cfg_attr(target_family = "wasm", async_trait::async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait::async_trait)]
pub trait CardanoTransactionAggregatorRequest: Send + Sync {
    /// Get a proof of membership for the given transactions hashes from the Aggregator.
    async fn get_proof(
        &self,
        hashes: &[String],
    ) -> MithrilResult<Option<CardanoTransactionsProofs>>;

    /// Fetch the list of latest signed Cardano transaction snapshots from the Aggregator
    async fn list_latest_snapshots(&self)
    -> MithrilResult<Vec<CardanoTransactionSnapshotListItem>>;

    /// Fetch a Cardano transaction snapshot by its hash from the Aggregator.
    async fn get_snapshot(&self, hash: &str) -> MithrilResult<Option<CardanoTransactionSnapshot>>;
}

impl CardanoTransactionClient {
    /// Constructs a new `CardanoTransactionClient`.
    pub fn new(aggregator_requester: Arc<dyn CardanoTransactionAggregatorRequest>) -> Self {
        Self {
            aggregator_requester,
        }
    }

    /// Get proofs that the given subset of transactions is included in the Cardano transactions set.
    pub async fn get_proofs<T: ToString>(
        &self,
        transactions_hashes: &[T],
    ) -> MithrilResult<CardanoTransactionsProofs> {
        let transactions_hashes: Vec<String> =
            transactions_hashes.iter().map(|h| h.to_string()).collect();

        self.aggregator_requester
            .get_proof(&transactions_hashes)
            .await?
            .with_context(|| {
                format!("No proof found for transactions hashes: {transactions_hashes:?}",)
            })
    }

    /// Fetch a list of signed Cardano transaction snapshots.
    pub async fn list_snapshots(&self) -> MithrilResult<Vec<CardanoTransactionSnapshotListItem>> {
        self.aggregator_requester.list_latest_snapshots().await
    }

    /// Get the given Cardano transaction snapshot data. If it cannot be found, a None is returned.
    pub async fn get_snapshot(
        &self,
        hash: &str,
    ) -> MithrilResult<Option<CardanoTransactionSnapshot>> {
        self.aggregator_requester.get_snapshot(hash).await
    }
}

#[cfg(test)]
mod tests {
    use mockall::predicate::eq;

    use mithril_common::test::mock_extensions::MockBuilder;

    use crate::common::{BlockNumber, test::Dummy};
    use crate::{
        CardanoTransactionSnapshot, CardanoTransactionSnapshotListItem, CardanoTransactionsProofs,
        CardanoTransactionsSetProof,
    };

    use super::*;

    #[tokio::test]
    async fn get_cardano_transactions_snapshot_list() {
        let aggregator_requester =
            MockBuilder::<MockCardanoTransactionAggregatorRequest>::configure(|mock| {
                let messages = vec![
                    CardanoTransactionSnapshotListItem {
                        hash: "hash-123".to_string(),
                        ..Dummy::dummy()
                    },
                    CardanoTransactionSnapshotListItem {
                        hash: "hash-456".to_string(),
                        ..Dummy::dummy()
                    },
                ];
                mock.expect_list_latest_snapshots().return_once(|| Ok(messages));
            });
        let client = CardanoTransactionClient::new(aggregator_requester);

        let items = client.list_snapshots().await.unwrap();

        assert_eq!(2, items.len());
        assert_eq!("hash-123".to_string(), items[0].hash);
        assert_eq!("hash-456".to_string(), items[1].hash);
    }

    #[tokio::test]
    async fn get_cardano_transactions_snapshot() {
        let aggregator_requester =
            MockBuilder::<MockCardanoTransactionAggregatorRequest>::configure(|mock| {
                let message = CardanoTransactionSnapshot {
                    hash: "hash-123".to_string(),
                    merkle_root: "mk-123".to_string(),
                    ..Dummy::dummy()
                };
                mock.expect_get_snapshot()
                    .with(eq(message.hash.clone()))
                    .return_once(|_| Ok(Some(message)));
            });
        let client = CardanoTransactionClient::new(aggregator_requester);

        let cardano_transaction_snapshot = client
            .get_snapshot("hash-123")
            .await
            .unwrap()
            .expect("This test returns a cardano transaction snapshot");

        assert_eq!("hash-123", &cardano_transaction_snapshot.hash);
        assert_eq!("mk-123", &cardano_transaction_snapshot.merkle_root);
    }

    #[tokio::test]
    async fn test_get_proof_ok() {
        let certificate_hash = "cert-hash-123".to_string();
        let set_proof = CardanoTransactionsSetProof::dummy();
        let expected_transactions_proofs = CardanoTransactionsProofs::new(
            &certificate_hash,
            vec![set_proof.clone()],
            vec![],
            BlockNumber(99999),
        );

        let aggregator_requester =
            MockBuilder::<MockCardanoTransactionAggregatorRequest>::configure(|mock| {
                let message = expected_transactions_proofs.clone();
                mock.expect_get_proof()
                    .with(eq(message
                        .certified_transactions
                        .iter()
                        .flat_map(|tx| tx.transactions_hashes.clone())
                        .collect::<Vec<_>>()))
                    .return_once(|_| Ok(Some(message)));
            });
        let client = CardanoTransactionClient::new(aggregator_requester);

        let transactions_proofs = client
            .get_proofs(
                &set_proof
                    .transactions_hashes
                    .iter()
                    .map(|h| h.as_str())
                    .collect::<Vec<_>>(),
            )
            .await
            .unwrap();

        assert_eq!(expected_transactions_proofs, transactions_proofs);
    }

    #[tokio::test]
    async fn test_get_proof_ko() {
        let aggregator_requester =
            MockBuilder::<MockCardanoTransactionAggregatorRequest>::configure(|mock| {
                mock.expect_get_proof()
                    .return_once(move |_| Err(anyhow::anyhow!("an error")));
            });
        let client = CardanoTransactionClient::new(aggregator_requester);

        client
            .get_proofs(&["tx-123"])
            .await
            .expect_err("The certificate client should fail here.");
    }
}

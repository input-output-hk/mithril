//! A client to retrieve and verify proof of cardano transactions from an Aggregator.
//!

use crate::aggregator_client::{AggregatorClient, AggregatorClientError, AggregatorRequest};
use crate::{
    CardanoTransactionCommitment, CardanoTransactionCommitmentListItem, CardanoTransactionsProofs,
    MithrilResult,
};
use anyhow::Context;
use std::sync::Arc;

/// HTTP client for CardanoTransactionsAPI from the Aggregator
pub struct CardanoTransactionProofClient {
    aggregator_client: Arc<dyn AggregatorClient>,
}

impl CardanoTransactionProofClient {
    /// Constructs a new `CardanoTransactionProofClient`.
    pub fn new(aggregator_client: Arc<dyn AggregatorClient>) -> Self {
        Self { aggregator_client }
    }

    /// Get proofs that the given set of Cardano transactions is included in the global Cardano transactions set
    pub async fn get_proofs(
        &self,
        transactions_hashes: &[&str],
    ) -> MithrilResult<CardanoTransactionsProofs> {
        match self
            .aggregator_client
            .get_content(AggregatorRequest::GetTransactionsProofs {
                transactions_hashes: transactions_hashes.iter().map(|h| h.to_string()).collect(),
            })
            .await
        {
            Ok(content) => {
                let transactions_proofs: CardanoTransactionsProofs = serde_json::from_str(&content)
                    .with_context(|| {
                        "CardanoTransactionProof Client can not deserialize transactions proofs"
                    })?;

                Ok(transactions_proofs)
            }
            Err(e) => Err(e.into()),
        }
    }

    /// Fetch a list of signed Cardano transaction commitment
    pub async fn list(&self) -> MithrilResult<Vec<CardanoTransactionCommitmentListItem>> {
        let response = self
            .aggregator_client
            .get_content(AggregatorRequest::ListCardanoTransactionCommitments)
            .await
            .with_context(|| {
                "CardanoTransactionProofClient Client can not get the artifact list"
            })?;
        let items = serde_json::from_str::<Vec<CardanoTransactionCommitmentListItem>>(&response)
            .with_context(|| {
                "CardanoTransactionProofClient Client can not deserialize artifact list"
            })?;

        Ok(items)
    }

    /// Get the given Cardano transaction commitment data. If it cannot be found, a None is returned.
    pub async fn get(&self, hash: &str) -> MithrilResult<Option<CardanoTransactionCommitment>> {
        match self
            .aggregator_client
            .get_content(AggregatorRequest::GetCardanoTransactionCommitment {
                hash: hash.to_string(),
            })
            .await
        {
            Ok(content) => {
                let cardano_transaction_commitment: CardanoTransactionCommitment =
                    serde_json::from_str(&content).with_context(|| {
                        "CardanoTransactionProofClient Client can not deserialize artifact"
                    })?;

                Ok(Some(cardano_transaction_commitment))
            }
            Err(AggregatorClientError::RemoteServerLogical(_)) => Ok(None),
            Err(e) => Err(e.into()),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::aggregator_client::{AggregatorClientError, MockAggregatorHTTPClient};
    use crate::common::Beacon;
    use crate::{
        CardanoTransactionCommitment, CardanoTransactionCommitmentListItem,
        CardanoTransactionsProofs, CardanoTransactionsSetProof,
    };
    use anyhow::anyhow;
    use chrono::{DateTime, Utc};
    use std::sync::Arc;

    use super::*;

    fn fake_messages() -> Vec<CardanoTransactionCommitmentListItem> {
        vec![
            CardanoTransactionCommitmentListItem {
                merkle_root: "mk-123".to_string(),
                beacon: Beacon::new("network".to_string(), 1, 1),
                hash: "hash-123".to_string(),
                certificate_hash: "cert-hash-123".to_string(),
                created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                    .unwrap()
                    .with_timezone(&Utc),
            },
            CardanoTransactionCommitmentListItem {
                merkle_root: "mk-456".to_string(),
                beacon: Beacon::new("network".to_string(), 1, 2),
                hash: "hash-456".to_string(),
                certificate_hash: "cert-hash-456".to_string(),
                created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                    .unwrap()
                    .with_timezone(&Utc),
            },
        ]
    }

    #[tokio::test]
    async fn get_cardano_transaction_commitment_list() {
        let message = fake_messages();
        let mut http_client = MockAggregatorHTTPClient::new();
        http_client
            .expect_get_content()
            .return_once(move |_| Ok(serde_json::to_string(&message).unwrap()));
        let client = CardanoTransactionProofClient::new(Arc::new(http_client));
        let items = client.list().await.unwrap();

        assert_eq!(2, items.len());
        assert_eq!("hash-123".to_string(), items[0].hash);
        assert_eq!("hash-456".to_string(), items[1].hash);
    }

    #[tokio::test]
    async fn get_cardano_transaction_commitment() {
        let mut http_client = MockAggregatorHTTPClient::new();
        let message = CardanoTransactionCommitment {
            merkle_root: "mk-123".to_string(),
            beacon: Beacon::new("network".to_string(), 1, 1),
            hash: "hash-123".to_string(),
            certificate_hash: "cert-hash-123".to_string(),
            created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                .unwrap()
                .with_timezone(&Utc),
        };
        let expected = message.clone();
        http_client
            .expect_get_content()
            .return_once(move |_| Ok(serde_json::to_string(&message).unwrap()));
        let client = CardanoTransactionProofClient::new(Arc::new(http_client));
        let cardano_transaction_commitment = client
            .get("hash")
            .await
            .unwrap()
            .expect("This test returns a cardano transaction commitment");

        assert_eq!(expected, cardano_transaction_commitment);
    }

    #[tokio::test]
    async fn test_get_proof_ok() {
        let mut aggregator_client = MockAggregatorHTTPClient::new();
        let certificate_hash = "cert-hash-123".to_string();
        let set_proof = CardanoTransactionsSetProof::dummy();
        let transactions_proofs =
            CardanoTransactionsProofs::new(&certificate_hash, vec![set_proof.clone()], vec![]);
        let expected_transactions_proofs = transactions_proofs.clone();
        aggregator_client
            .expect_get_content()
            .return_once(move |_| Ok(serde_json::to_string(&transactions_proofs).unwrap()))
            .times(1);

        let cardano_tx_client = CardanoTransactionProofClient::new(Arc::new(aggregator_client));
        let transactions_proofs = cardano_tx_client
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
        let mut aggregator_client = MockAggregatorHTTPClient::new();
        aggregator_client
            .expect_get_content()
            .return_once(move |_| {
                Err(AggregatorClientError::RemoteServerTechnical(anyhow!(
                    "an error"
                )))
            })
            .times(1);

        let cardano_tx_client = CardanoTransactionProofClient::new(Arc::new(aggregator_client));
        cardano_tx_client
            .get_proofs(&["tx-123"])
            .await
            .expect_err("The certificate client should fail here.");
    }
}

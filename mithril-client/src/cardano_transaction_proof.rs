//! A client to retrieve and verify proof of cardano transactions from an Aggregator.
//!

use crate::aggregator_client::{AggregatorClient, AggregatorRequest};
use crate::{CardanoTransactionsProofs, MithrilResult};
use anyhow::Context;
use mithril_common::entities::ProtocolMessage;
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

    /// Verify the given proofs
    ///
    /// The following checks will be executed:
    /// 1 - Check for each merkle proof that its leaves belongs to their merkle root
    /// 2 - Obtain the merkle root for each proof
    /// 3 - Check that all merkle root are the same
    ///
    /// If every check is okay, a [ProtocolMessage] associated to the
    pub async fn verify_proofs(
        &self,
        _proofs: &CardanoTransactionsProofs,
    ) -> MithrilResult<String> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::aggregator_client::{AggregatorClientError, MockAggregatorHTTPClient};
    use crate::{CardanoTransactionsProofs, CardanoTransactionsSetProof};
    use anyhow::anyhow;
    use std::sync::Arc;

    use super::*;

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

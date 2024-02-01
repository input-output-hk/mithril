//! A client to retrieve and verify proof of cardano transactions from an Aggregator.
//!

use crate::aggregator_client::AggregatorClient;
use crate::{CardanoTransactionsProofs, MithrilResult};
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

    pub async fn get_proofs(
        &self,
        transactions_hashes: &[&str],
    ) -> MithrilResult<CardanoTransactionsProofs> {
        todo!()
    }

    pub async fn validate_proofs(
        &self,
        proofs: &CardanoTransactionsProofs,
    ) -> MithrilResult<ProtocolMessage> {
        todo!()
    }
}

#[cfg(test)]
mod tests {}

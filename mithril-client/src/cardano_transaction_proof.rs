//! A client to retrieve and verify proof of cardano transactions from an Aggregator.
//!

use crate::aggregator_client::AggregatorClient;
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
}

#[cfg(test)]
mod tests {}

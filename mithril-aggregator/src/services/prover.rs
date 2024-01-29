use async_trait::async_trait;

use mithril_common::{entities::TransactionHash, StdResult};

use crate::entities::TransactionsSetProof;

#[cfg(test)]
use mockall::automock;

/// Prover service is the cryptographic engine in charge of producing cryptographic proofs for transactions
#[async_trait]
#[cfg_attr(test, automock)]
pub trait ProverService: Sync + Send {
    /// Compute the cryptographic proof for the given transactions
    async fn compute_transactions_proof(
        &self,
        transaction_hashes: Vec<TransactionHash>,
    ) -> StdResult<Vec<TransactionsSetProof>>;
}
/// Mithril prover
#[derive(Default)]
pub struct MithrilProverService {}

#[async_trait]
impl ProverService for MithrilProverService {
    async fn compute_transactions_proof(
        &self,
        _transaction_hashes: Vec<TransactionHash>,
    ) -> StdResult<Vec<TransactionsSetProof>> {
        todo!("Implement Mithril prover")
    }
}

use async_trait::async_trait;

use mithril_common::{
    entities::{CardanoTransactionsSetProof, TransactionHash},
    StdResult,
};

#[cfg(test)]
use mockall::automock;

/// Prover service is the cryptographic engine in charge of producing cryptographic proofs for transactions
#[async_trait]
#[cfg_attr(test, automock)]
pub trait ProverService: Sync + Send {
    /// Compute the cryptographic proofs for the given transactions
    async fn compute_transactions_proofs(
        &self,
        transaction_hashes: &[TransactionHash],
    ) -> StdResult<Vec<CardanoTransactionsSetProof>>;
}
/// Mithril prover
#[derive(Default)]
pub struct MithrilProverService {}

#[async_trait]
impl ProverService for MithrilProverService {
    async fn compute_transactions_proofs(
        &self,
        _transaction_hashes: &[TransactionHash],
    ) -> StdResult<Vec<CardanoTransactionsSetProof>> {
        todo!("Implement Mithril prover")
    }
}

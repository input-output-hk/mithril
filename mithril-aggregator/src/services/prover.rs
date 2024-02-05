use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;

use mithril_common::{
    crypto_helper::{MKTree, MKTreeNode, MKTreeStore},
    entities::{Beacon, CardanoTransaction, CardanoTransactionsSetProof, TransactionHash},
    StdResult,
};

#[cfg(test)]
use mockall::automock;

/// Prover service is the cryptographic engine in charge of producing cryptographic proofs for transactions
#[cfg_attr(test, automock)]
#[async_trait]
pub trait ProverService: Sync + Send {
    /// Compute the cryptographic proofs for the given transactions
    async fn compute_transactions_proofs(
        &self,
        up_to: &Beacon,
        transaction_hashes: &[TransactionHash],
    ) -> StdResult<Vec<CardanoTransactionsSetProof>>;
}

/// Transactions retriever
#[cfg_attr(test, automock)]
#[async_trait]
pub trait TransactionsRetriever: Sync + Send {
    /// Get transactions up to given beacon
    async fn get_up_to(&self, beacon: &Beacon) -> StdResult<Vec<CardanoTransaction>>;
}

/// Mithril prover
pub struct MithrilProverService {
    transaction_retriever: Arc<dyn TransactionsRetriever>,
}

impl MithrilProverService {
    /// Create a new Mithril prover
    pub fn new(transaction_retriever: Arc<dyn TransactionsRetriever>) -> Self {
        Self {
            transaction_retriever,
        }
    }
}

#[async_trait]
impl ProverService for MithrilProverService {
    async fn compute_transactions_proofs(
        &self,
        up_to: &Beacon,
        transaction_hashes: &[TransactionHash],
    ) -> StdResult<Vec<CardanoTransactionsSetProof>> {
        let transactions = self.transaction_retriever.get_up_to(up_to).await?;
        let mk_leaves_all: Vec<MKTreeNode> =
            transactions.iter().map(|t| t.to_owned().into()).collect();
        let store = MKTreeStore::default();
        let mktree = MKTree::new(&mk_leaves_all, &store)
            .with_context(|| "MKTree creation should not fail")?;

        let mut transaction_hashes_certified = vec![];
        for transaction_hash in transaction_hashes {
            let mk_leaf = transaction_hash.to_string().into();
            if mktree.compute_proof(&[mk_leaf]).is_ok() {
                transaction_hashes_certified.push(transaction_hash.to_string());
            }
        }

        if !transaction_hashes_certified.is_empty() {
            let mk_leaves: Vec<MKTreeNode> = transaction_hashes_certified
                .iter()
                .map(|h| h.to_owned().into())
                .collect();
            let mk_proof = mktree.compute_proof(&mk_leaves)?;
            let transactions_set_proof_batch =
                CardanoTransactionsSetProof::new(transaction_hashes_certified, mk_proof);

            Ok(vec![transactions_set_proof_batch])
        } else {
            Ok(vec![])
        }
    }
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;
    use mithril_common::entities::CardanoTransaction;
    use mithril_common::test_utils::fake_data;
    use mockall::predicate::eq;

    use super::*;

    fn generate_transactions(
        total_transactions: usize,
    ) -> (Vec<TransactionHash>, Vec<CardanoTransaction>) {
        let mut hashes = vec![];
        let mut transactions = vec![];

        for i in 1..=total_transactions {
            let hash = format!("tx-{}", i);
            transactions.push(CardanoTransaction::new(&hash, 10 * i as u64, i as u64));
            hashes.push(hash);
        }

        (hashes, transactions)
    }

    #[tokio::test]
    async fn compute_proof_for_one_set_with_multiple_transactions() {
        let (transaction_hashes, transactions) = generate_transactions(3);
        let mut transaction_retriever = MockTransactionsRetriever::new();
        transaction_retriever
            .expect_get_up_to()
            .with(eq(fake_data::beacon()))
            .return_once(move |_| Ok(transactions));
        let prover = MithrilProverService::new(Arc::new(transaction_retriever));
        let transactions_set_proof = prover
            .compute_transactions_proofs(&fake_data::beacon(), &transaction_hashes)
            .await
            .unwrap();

        assert_eq!(transactions_set_proof.len(), 1);
        assert_eq!(
            transactions_set_proof[0].transactions_hashes(),
            transaction_hashes
        );
        transactions_set_proof[0].verify().unwrap();
    }

    #[tokio::test]
    async fn cant_compute_proof_for_unknown_transaction() {
        let (transaction_hashes, _transactions) = generate_transactions(3);
        let mut transaction_retriever = MockTransactionsRetriever::new();
        transaction_retriever
            .expect_get_up_to()
            .with(eq(fake_data::beacon()))
            .returning(|_| Ok(vec![]));
        let prover = MithrilProverService::new(Arc::new(transaction_retriever));
        let transactions_set_proof = prover
            .compute_transactions_proofs(&fake_data::beacon(), &transaction_hashes)
            .await
            .unwrap();

        assert_eq!(transactions_set_proof.len(), 0);
    }

    #[tokio::test]
    async fn compute_proof_for_one_set_of_three_known_transactions_and_two_unknowns() {
        let (transaction_hashes, transactions) = generate_transactions(5);
        // The last two are not in the "store"
        let transactions = transactions[0..=2].to_vec();
        let mut transaction_retriever = MockTransactionsRetriever::new();
        transaction_retriever
            .expect_get_up_to()
            .with(eq(fake_data::beacon()))
            .return_once(move |_| Ok(transactions));
        let prover = MithrilProverService::new(Arc::new(transaction_retriever));
        let transactions_set_proof = prover
            .compute_transactions_proofs(&fake_data::beacon(), &transaction_hashes)
            .await
            .unwrap();

        assert_eq!(transactions_set_proof.len(), 1);
        assert_eq!(
            transactions_set_proof[0].transactions_hashes(),
            &transaction_hashes[0..=2].to_vec()
        );
        transactions_set_proof[0].verify().unwrap();
    }

    // this one can't be done right now because we don't have a merkle tree of merkle tree yet
    // todo: compute_proof_for_multiple_set_with_multiple_transactions

    #[tokio::test]
    async fn cant_compute_proof_if_retriever_fail() {
        let (transaction_hashes, _transactions) = generate_transactions(3);
        let mut transaction_retriever = MockTransactionsRetriever::new();
        transaction_retriever
            .expect_get_up_to()
            .with(eq(fake_data::beacon()))
            .returning(|_| Err(anyhow!("Error")));

        let prover = MithrilProverService::new(Arc::new(transaction_retriever));
        prover
            .compute_transactions_proofs(&fake_data::beacon(), &transaction_hashes)
            .await
            .expect_err("Should have failed because of its retriever");
    }
}

use std::{collections::HashMap, sync::Arc};

use anyhow::Context;
use async_trait::async_trait;

use mithril_common::{
    crypto_helper::{MKMap, MKMapNode, MKTree, MKTreeNode},
    entities::{
        BlockRange, CardanoDbBeacon, CardanoTransaction, CardanoTransactionsSetProof,
        TransactionHash,
    },
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
        up_to: &CardanoDbBeacon,
        transaction_hashes: &[TransactionHash],
    ) -> StdResult<Vec<CardanoTransactionsSetProof>>;
}

/// Transactions retriever
#[cfg_attr(test, automock)]
#[async_trait]
pub trait TransactionsRetriever: Sync + Send {
    /// Get transactions up to given beacon using chronological order
    async fn get_up_to(&self, beacon: &CardanoDbBeacon) -> StdResult<Vec<CardanoTransaction>>;
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

    fn compute_merkle_map_from_transactions(
        &self,
        transactions: Vec<CardanoTransaction>,
    ) -> StdResult<MKMap<BlockRange, MKMapNode<BlockRange>>> {
        let mut transactions_by_block_ranges: HashMap<BlockRange, Vec<TransactionHash>> =
            HashMap::new();
        for transaction in transactions {
            let block_range = BlockRange::from_block_number(transaction.block_number);
            transactions_by_block_ranges
                .entry(block_range)
                .or_default()
                .push(transaction.transaction_hash);
        }
        let mk_hash_map = MKMap::new(
            transactions_by_block_ranges
                .into_iter()
                .try_fold(
                    vec![],
                    |mut acc, (block_range, transactions)| -> StdResult<Vec<(_, MKMapNode<_>)>> {
                        acc.push((block_range, MKTree::new(&transactions)?.into()));
                        Ok(acc)
                    },
                )?
                .as_slice(),
        )
        .with_context(|| "ProverService failed to compute the merkelized structure that proves ownership of the transaction")?;

        Ok(mk_hash_map)
    }
}

#[async_trait]
impl ProverService for MithrilProverService {
    async fn compute_transactions_proofs(
        &self,
        up_to: &CardanoDbBeacon,
        transaction_hashes: &[TransactionHash],
    ) -> StdResult<Vec<CardanoTransactionsSetProof>> {
        let transactions = self.transaction_retriever.get_up_to(up_to).await?;

        // 1 - Get transactions to prove per block range
        let transactions_to_prove = transactions
            .iter()
            .filter_map(|transaction| {
                let block_range = BlockRange::from_block_number(transaction.block_number);
                transaction_hashes
                    .contains(&transaction.transaction_hash)
                    .then_some((block_range, transaction.clone()))
            })
            .collect::<Vec<_>>();

        // 2 - Compute Transactions Merkle Tree
        let mk_map = self.compute_merkle_map_from_transactions(transactions)?;

        // 3 - Compute proof for each transaction to prove
        let mut transaction_hashes_certified = vec![];
        for (_block_range, transaction) in transactions_to_prove {
            let mk_tree_node_transaction_hash: MKTreeNode =
                transaction.transaction_hash.to_owned().into();
            if mk_map
                .compute_proof(&[mk_tree_node_transaction_hash])
                .is_ok()
            {
                transaction_hashes_certified.push(transaction.transaction_hash);
            }
        }

        if !transaction_hashes_certified.is_empty() {
            let mk_leaves: Vec<MKTreeNode> = transaction_hashes_certified
                .iter()
                .map(|h| h.to_owned().into())
                .collect();
            let mk_proof = mk_map.compute_proof(&mk_leaves)?;
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
            let hash = format!("tx-{i}");
            transactions.push(CardanoTransaction::new(
                &hash,
                10 * i as u64,
                100 * i as u64,
                format!("block_hash-{i}"),
                i as u64,
            ));
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

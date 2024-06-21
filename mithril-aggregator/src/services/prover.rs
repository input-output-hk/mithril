use async_trait::async_trait;
use rayon::prelude::*;
use slog::{debug, info, Logger};
use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    sync::Arc,
    time::Duration,
};

use mithril_common::{
    crypto_helper::{MKMap, MKMapNode, MKTree},
    entities::{
        BlockNumber, BlockRange, CardanoTransaction, CardanoTransactionsSetProof, TransactionHash,
    },
    resource_pool::ResourcePool,
    signable_builder::BlockRangeRootRetriever,
    StdResult,
};

/// Prover service is the cryptographic engine in charge of producing cryptographic proofs for transactions
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait ProverService: Sync + Send {
    /// Compute the cryptographic proofs for the given transactions
    async fn compute_transactions_proofs(
        &self,
        up_to: BlockNumber,
        transaction_hashes: &[TransactionHash],
    ) -> StdResult<Vec<CardanoTransactionsSetProof>>;

    /// Compute the cache
    async fn compute_cache(&self, up_to: BlockNumber) -> StdResult<()>;
}

/// Transactions retriever
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait TransactionsRetriever: Sync + Send {
    /// Get a list of transactions by hashes using chronological order
    async fn get_by_hashes(
        &self,
        hashes: Vec<TransactionHash>,
        up_to: BlockNumber,
    ) -> StdResult<Vec<CardanoTransaction>>;

    /// Get by block ranges
    async fn get_by_block_ranges(
        &self,
        block_ranges: Vec<BlockRange>,
    ) -> StdResult<Vec<CardanoTransaction>>;
}

#[cfg_attr(test, mockall::automock)]
trait TransactionsHashFilter: Sync + Send {
    fn apply(&self, transaction_hashes: &[TransactionHash]) -> Vec<TransactionHash>;
}

struct TransactionsHashFilterToProve {
    max_hashes: usize,
}

impl TransactionsHashFilterToProve {
    pub fn new(max_hashes: usize) -> Self {
        Self { max_hashes }
    }
}

impl Default for TransactionsHashFilterToProve {
    fn default() -> Self {
        Self::new(usize::MAX)
    }
}

impl TransactionsHashFilter for TransactionsHashFilterToProve {
    fn apply(&self, transaction_hashes: &[TransactionHash]) -> Vec<TransactionHash> {
        let mut transaction_hashes: Vec<TransactionHash> = transaction_hashes
            .iter()
            .filter(|hash| hash.len() == 64 && hash.chars().all(|c| c.is_ascii_hexdigit()))
            .take(self.max_hashes)
            .cloned()
            .collect();
        transaction_hashes.sort();
        transaction_hashes.dedup();
        transaction_hashes
    }
}

/// Mithril prover
pub struct MithrilProverService {
    transaction_retriever: Arc<dyn TransactionsRetriever>,
    block_range_root_retriever: Arc<dyn BlockRangeRootRetriever>,
    mk_map_pool: ResourcePool<MKMap<BlockRange, MKMapNode<BlockRange>>>,
    logger: Logger,
    filter: Box<dyn TransactionsHashFilter>,
}

impl MithrilProverService {
    /// Create a new Mithril prover
    ///
    /// The parameter `max_computable_transactions_hashes` corresponds to the maximum number
    /// of transactions hashes that can be computed by [Self::compute_transactions_proofs].
    pub fn new(
        transaction_retriever: Arc<dyn TransactionsRetriever>,
        block_range_root_retriever: Arc<dyn BlockRangeRootRetriever>,
        mk_map_pool_size: usize,
        logger: Logger,
        max_computable_transactions_hashes: usize,
    ) -> Self {
        Self::new_with_filter(
            transaction_retriever,
            block_range_root_retriever,
            mk_map_pool_size,
            logger,
            Box::new(TransactionsHashFilterToProve::new(
                max_computable_transactions_hashes,
            )),
        )
    }

    fn new_with_filter(
        transaction_retriever: Arc<dyn TransactionsRetriever>,
        block_range_root_retriever: Arc<dyn BlockRangeRootRetriever>,
        mk_map_pool_size: usize,
        logger: Logger,
        filter: Box<dyn TransactionsHashFilter>,
    ) -> Self {
        Self {
            transaction_retriever,
            block_range_root_retriever,
            mk_map_pool: ResourcePool::new(mk_map_pool_size, vec![]),
            logger,
            filter,
        }
    }

    async fn get_block_ranges(
        &self,
        transaction_hashes: &[TransactionHash],
        up_to: BlockNumber,
    ) -> StdResult<Vec<BlockRange>> {
        let transactions = self
            .transaction_retriever
            .get_by_hashes(transaction_hashes.to_vec(), up_to)
            .await?;
        let block_ranges = transactions
            .iter()
            .map(|t| BlockRange::from_block_number(t.block_number))
            .collect::<BTreeSet<_>>();

        Ok(block_ranges.into_iter().collect::<Vec<_>>())
    }

    /// Get all the transactions of the block ranges
    async fn get_all_transactions_for_block_ranges(
        &self,
        block_ranges: &[BlockRange],
    ) -> StdResult<HashMap<BlockRange, Vec<CardanoTransaction>>> {
        let mut block_ranges_map = HashMap::new();
        let transactions = self
            .transaction_retriever
            .get_by_block_ranges(block_ranges.to_vec())
            .await?;
        for transaction in transactions {
            let block_range = BlockRange::from_block_number(transaction.block_number);
            let block_range_transactions: &mut Vec<_> =
                block_ranges_map.entry(block_range).or_insert(vec![]);
            block_range_transactions.push(transaction)
        }

        Ok(block_ranges_map)
    }
}

#[async_trait]
impl ProverService for MithrilProverService {
    async fn compute_transactions_proofs(
        &self,
        up_to: BlockNumber,
        transaction_hashes: &[TransactionHash],
    ) -> StdResult<Vec<CardanoTransactionsSetProof>> {
        let transaction_hashes = self.filter.apply(transaction_hashes);

        // 1 - Compute the set of block ranges with transactions to prove
        let block_ranges_transactions = self.get_block_ranges(&transaction_hashes, up_to).await?;
        let block_range_transactions = self
            .get_all_transactions_for_block_ranges(&block_ranges_transactions)
            .await?;

        // 2 - Compute block ranges sub Merkle trees
        let mk_trees: StdResult<Vec<(BlockRange, MKTree)>> = block_range_transactions
            .into_iter()
            .map(|(block_range, transactions)| {
                let mk_tree = MKTree::new(&transactions)?;
                Ok((block_range, mk_tree))
            })
            .collect();
        let mk_trees = BTreeMap::from_iter(mk_trees?);

        // 3 - Compute block range roots Merkle map
        let acquire_timeout = Duration::from_millis(1000);
        let mut mk_map = self.mk_map_pool.acquire_resource(acquire_timeout)?;

        // 4 - Enrich the Merkle map with the block ranges Merkle trees
        for (block_range, mk_tree) in mk_trees {
            mk_map.insert(block_range, mk_tree.into())?;
        }

        // 5 - Compute the proof for all transactions
        if let Ok(mk_proof) = mk_map.compute_proof(&transaction_hashes) {
            self.mk_map_pool.give_back_resource_pool_item(mk_map)?;
            let mk_proof_leaves = mk_proof.leaves();
            let transaction_hashes_certified: Vec<TransactionHash> = transaction_hashes
                .iter()
                .filter(|hash| mk_proof_leaves.contains(&hash.as_str().into()))
                .cloned()
                .collect();

            Ok(vec![CardanoTransactionsSetProof::new(
                transaction_hashes_certified,
                mk_proof,
            )])
        } else {
            Ok(vec![])
        }
    }

    async fn compute_cache(&self, up_to: BlockNumber) -> StdResult<()> {
        let pool_size = self.mk_map_pool.size();
        info!(
            self.logger,
            "Prover starts computing the Merkle map pool resource of size {pool_size}"
        );
        let mk_map_cache = self
            .block_range_root_retriever
            .compute_merkle_map_from_block_range_roots(up_to)
            .await?;
        let discriminant_new = self.mk_map_pool.discriminant()? + 1;
        self.mk_map_pool.set_discriminant(discriminant_new)?;
        self.mk_map_pool.clear();
        (1..=pool_size)
            .into_par_iter()
            .map(|i| {
                debug!(
                    self.logger,
                    "Prover is computing the Merkle map pool resource {i}/{pool_size}"
                );
                self.mk_map_pool
                    .give_back_resource(mk_map_cache.clone(), discriminant_new)
            })
            .collect::<StdResult<()>>()?;
        info!(
            self.logger,
            "Prover completed computing the Merkle map pool resource of size {pool_size}"
        );

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;
    use mithril_common::crypto_helper::{MKMap, MKMapNode, MKTreeNode};
    use mithril_common::entities::CardanoTransaction;
    use mithril_common::test_utils::{
        assert_equivalent, equivalent_to, CardanoTransactionsBuilder,
    };
    use mockall::mock;
    use mockall::predicate::eq;
    use test_data::transactions_group_by_block_range;

    use super::*;

    mock! {
        pub BlockRangeRootRetrieverImpl { }

        #[async_trait]
        impl BlockRangeRootRetriever for BlockRangeRootRetrieverImpl {
            async fn retrieve_block_range_roots(
                &self,
                up_to_beacon: BlockNumber,
            ) -> StdResult<Box<dyn Iterator<Item = (BlockRange, MKTreeNode)>>>;

            async fn compute_merkle_map_from_block_range_roots(
                &self,
                up_to_beacon: BlockNumber,
            ) -> StdResult<MKMap<BlockRange, MKMapNode<BlockRange>>>;
        }
    }

    struct AcceptAllTransactionsFilter {}

    impl TransactionsHashFilter for AcceptAllTransactionsFilter {
        fn apply(&self, transaction_hashes: &[TransactionHash]) -> Vec<TransactionHash> {
            transaction_hashes.to_vec()
        }
    }

    mod test_data {
        use super::*;

        pub fn filter_transactions_for_indices(
            indices: &[usize],
            transactions: &[CardanoTransaction],
        ) -> Vec<CardanoTransaction> {
            transactions
                .iter()
                .enumerate()
                .filter(|(i, _)| indices.contains(i))
                .map(|(_, t)| t.to_owned())
                .collect()
        }

        pub fn map_to_transaction_hashes(
            transactions: &[CardanoTransaction],
        ) -> Vec<TransactionHash> {
            transactions
                .iter()
                .map(|t| t.transaction_hash.clone())
                .collect()
        }

        pub fn transactions_group_by_block_range(
            transactions: &[CardanoTransaction],
        ) -> BTreeMap<BlockRange, Vec<CardanoTransaction>> {
            let mut block_ranges_map = BTreeMap::new();
            for transaction in transactions {
                let block_range = BlockRange::from_block_number(transaction.block_number);
                let block_range_transactions: &mut Vec<_> =
                    block_ranges_map.entry(block_range).or_insert(vec![]);
                block_range_transactions.push(transaction.to_owned())
            }

            block_ranges_map
        }

        pub fn filter_transactions_for_block_ranges(
            block_ranges: &[BlockRange],
            transactions: &[CardanoTransaction],
        ) -> Vec<CardanoTransaction> {
            transactions
                .iter()
                .filter(|t| block_ranges.contains(&BlockRange::from_block_number(t.block_number)))
                .map(|t| t.to_owned())
                .collect()
        }

        pub fn compute_mk_map_from_block_ranges_map(
            block_ranges_map: BTreeMap<BlockRange, Vec<CardanoTransaction>>,
        ) -> MKMap<BlockRange, MKMapNode<BlockRange>> {
            MKMap::new_from_iter(
                block_ranges_map
                    .into_iter()
                    .map(|(block_range, transactions)| {
                        (
                            block_range,
                            MKMapNode::TreeNode(
                                MKTree::new(&transactions)
                                    .unwrap()
                                    .compute_root()
                                    .unwrap()
                                    .clone(),
                            ),
                        )
                    }),
            )
            .unwrap()
        }

        pub fn compute_beacon_from_transactions(
            transactions: &[CardanoTransaction],
        ) -> BlockNumber {
            let max_transaction = transactions.iter().max_by_key(|t| t.block_number).unwrap();
            max_transaction.block_number
        }

        pub struct TestData {
            pub transaction_hashes_to_prove: Vec<TransactionHash>,
            pub block_ranges_map: BTreeMap<BlockRange, Vec<CardanoTransaction>>,
            pub block_ranges_to_prove: Vec<BlockRange>,
            pub all_transactions_in_block_ranges_to_prove: Vec<CardanoTransaction>,
            pub beacon: BlockNumber,
        }

        pub fn build_test_data(
            transactions_to_prove: &[CardanoTransaction],
            transactions: &[CardanoTransaction],
        ) -> TestData {
            let transaction_hashes_to_prove = map_to_transaction_hashes(transactions_to_prove);
            let block_ranges_map = transactions_group_by_block_range(transactions);
            let block_ranges_map_to_prove =
                transactions_group_by_block_range(transactions_to_prove);
            let block_ranges_to_prove = block_ranges_map_to_prove
                .keys()
                .cloned()
                .collect::<Vec<_>>();
            let all_transactions_in_block_ranges_to_prove =
                filter_transactions_for_block_ranges(&block_ranges_to_prove, transactions);
            let beacon = compute_beacon_from_transactions(transactions);

            TestData {
                transaction_hashes_to_prove,
                block_ranges_map,
                block_ranges_to_prove,
                all_transactions_in_block_ranges_to_prove,
                beacon,
            }
        }
    }

    fn build_prover<F, G>(
        transaction_retriever_mock_config: F,
        block_range_root_retriever_mock_config: G,
    ) -> MithrilProverService
    where
        F: FnOnce(&mut MockTransactionsRetriever),
        G: FnOnce(&mut MockBlockRangeRootRetrieverImpl),
    {
        let mut transaction_retriever = MockTransactionsRetriever::new();
        transaction_retriever_mock_config(&mut transaction_retriever);
        let mut block_range_root_retriever = MockBlockRangeRootRetrieverImpl::new();
        block_range_root_retriever_mock_config(&mut block_range_root_retriever);
        let mk_map_pool_size = 1;
        let logger = slog_scope::logger();

        MithrilProverService::new_with_filter(
            Arc::new(transaction_retriever),
            Arc::new(block_range_root_retriever),
            mk_map_pool_size,
            logger,
            Box::new(AcceptAllTransactionsFilter {}),
        )
    }

    #[tokio::test]
    async fn filter_cardano_transactions_hashes() {
        let transactions_hashes = vec!["tx-hash-123".to_string(), "tx-hash-456".to_string()];

        let transactions_hash_filter = {
            let mut transactions_hash_filter = MockTransactionsHashFilter::new();
            transactions_hash_filter
                .expect_apply()
                .with(eq(transactions_hashes.clone()))
                .once()
                .return_once(|_t| vec!["tx-hash-123-returned".to_string()]);
            transactions_hash_filter
        };

        let transaction_retriever = {
            let mut transaction_retriever = MockTransactionsRetriever::new();
            transaction_retriever
                .expect_get_by_hashes()
                .withf(|transaction_hashes, _| {
                    transaction_hashes == &vec!["tx-hash-123-returned".to_string()]
                })
                .return_once(move |_, _| Ok(vec![]));

            transaction_retriever
                .expect_get_by_block_ranges()
                .return_once(move |_| Ok(vec![]));

            transaction_retriever
        };

        let block_range_root_retriever = {
            let mut block_range_root_retriever = MockBlockRangeRootRetrieverImpl::new();

            block_range_root_retriever
                .expect_compute_merkle_map_from_block_range_roots()
                .return_once(move |_| {
                    Ok(test_data::compute_mk_map_from_block_ranges_map(
                        transactions_group_by_block_range(&[]),
                    ))
                });
            block_range_root_retriever
        };

        let prover = MithrilProverService::new_with_filter(
            Arc::new(transaction_retriever),
            Arc::new(block_range_root_retriever),
            1,
            slog_scope::logger(),
            Box::new(transactions_hash_filter),
        );

        prover.compute_cache(123456).await.unwrap();

        prover
            .compute_transactions_proofs(99999, &transactions_hashes)
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn compute_proof_for_one_set_of_three_certified_transactions() {
        let transactions = CardanoTransactionsBuilder::new()
            .max_transactions_per_block(1)
            .blocks_per_block_range(3)
            .build_block_ranges(5);
        let transactions_to_prove =
            test_data::filter_transactions_for_indices(&[1, 2, 4], &transactions);
        let test_data = test_data::build_test_data(&transactions_to_prove, &transactions);
        let prover = build_prover(
            |transaction_retriever_mock| {
                let transaction_hashes_to_prove = test_data.transaction_hashes_to_prove.clone();
                let transactions_to_prove = transactions_to_prove.clone();
                transaction_retriever_mock
                    .expect_get_by_hashes()
                    .withf(move |transactions_hashes, beacon| {
                        equivalent_to(transactions_hashes, &transaction_hashes_to_prove)
                            && *beacon == test_data.beacon
                    })
                    .return_once(move |_, _| Ok(transactions_to_prove));

                let block_ranges_to_prove = test_data.block_ranges_to_prove.clone();
                let all_transactions_in_block_ranges_to_prove =
                    test_data.all_transactions_in_block_ranges_to_prove.clone();
                transaction_retriever_mock
                    .expect_get_by_block_ranges()
                    .with(eq(block_ranges_to_prove))
                    .return_once(move |_| Ok(all_transactions_in_block_ranges_to_prove));
            },
            |block_range_root_retriever_mock| {
                let block_ranges_map = test_data.block_ranges_map.clone();
                block_range_root_retriever_mock
                    .expect_compute_merkle_map_from_block_range_roots()
                    .return_once(|_| {
                        Ok(test_data::compute_mk_map_from_block_ranges_map(
                            block_ranges_map,
                        ))
                    });
            },
        );
        prover.compute_cache(test_data.beacon).await.unwrap();

        let transactions_set_proof = prover
            .compute_transactions_proofs(test_data.beacon, &test_data.transaction_hashes_to_prove)
            .await
            .unwrap();

        assert_eq!(transactions_set_proof.len(), 1);
        assert_equivalent(
            transactions_set_proof[0].transactions_hashes(),
            &test_data.transaction_hashes_to_prove,
        );
        transactions_set_proof[0].verify().unwrap();
    }

    #[tokio::test]
    async fn cant_compute_proof_for_not_yet_certified_transaction() {
        let transactions = CardanoTransactionsBuilder::new()
            .max_transactions_per_block(1)
            .blocks_per_block_range(3)
            .build_block_ranges(5);
        let transactions_to_prove =
            test_data::filter_transactions_for_indices(&[1, 2, 4], &transactions);
        let test_data = test_data::build_test_data(&transactions_to_prove, &transactions);
        let prover = build_prover(
            |transaction_retriever_mock| {
                let transaction_hashes_to_prove = test_data.transaction_hashes_to_prove.clone();
                transaction_retriever_mock
                    .expect_get_by_hashes()
                    .withf(move |transactions_hashes, beacon| {
                        equivalent_to(transactions_hashes, &transaction_hashes_to_prove)
                            && *beacon == test_data.beacon
                    })
                    .return_once(move |_, _| Ok(vec![]));
                transaction_retriever_mock
                    .expect_get_by_block_ranges()
                    .with(eq(vec![]))
                    .return_once(move |_| Ok(vec![]));
            },
            |block_range_root_retriever_mock| {
                let block_ranges_map = test_data.block_ranges_map.clone();
                block_range_root_retriever_mock
                    .expect_compute_merkle_map_from_block_range_roots()
                    .return_once(|_| {
                        Ok(test_data::compute_mk_map_from_block_ranges_map(
                            block_ranges_map,
                        ))
                    });
            },
        );
        prover.compute_cache(test_data.beacon).await.unwrap();

        let transactions_set_proof = prover
            .compute_transactions_proofs(test_data.beacon, &test_data.transaction_hashes_to_prove)
            .await
            .unwrap();

        assert_eq!(transactions_set_proof.len(), 0);
    }

    #[tokio::test]
    async fn cant_compute_proof_for_unknown_transaction() {
        let transactions = CardanoTransactionsBuilder::new()
            .max_transactions_per_block(1)
            .blocks_per_block_range(3)
            .build_block_ranges(5);
        let transactions_to_prove = test_data::filter_transactions_for_indices(&[], &transactions);
        let mut test_data = test_data::build_test_data(&transactions_to_prove, &transactions);
        test_data.transaction_hashes_to_prove = vec!["tx-unknown-123".to_string()];
        let prover = build_prover(
            |transaction_retriever_mock| {
                let transaction_hashes_to_prove = test_data.transaction_hashes_to_prove.clone();
                let transactions_to_prove = transactions_to_prove.clone();
                transaction_retriever_mock
                    .expect_get_by_hashes()
                    .withf(move |transactions_hashes, beacon| {
                        equivalent_to(transactions_hashes, &transaction_hashes_to_prove)
                            && *beacon == test_data.beacon
                    })
                    .return_once(move |_, _| Ok(transactions_to_prove));

                let block_ranges_to_prove = test_data.block_ranges_to_prove.clone();
                let all_transactions_in_block_ranges_to_prove =
                    test_data.all_transactions_in_block_ranges_to_prove.clone();
                transaction_retriever_mock
                    .expect_get_by_block_ranges()
                    .with(eq(block_ranges_to_prove))
                    .return_once(move |_| Ok(all_transactions_in_block_ranges_to_prove));
            },
            |block_range_root_retriever_mock| {
                let block_ranges_map = test_data.block_ranges_map.clone();
                block_range_root_retriever_mock
                    .expect_compute_merkle_map_from_block_range_roots()
                    .return_once(|_| {
                        Ok(test_data::compute_mk_map_from_block_ranges_map(
                            block_ranges_map,
                        ))
                    });
            },
        );
        prover.compute_cache(test_data.beacon).await.unwrap();

        let transactions_set_proof = prover
            .compute_transactions_proofs(test_data.beacon, &test_data.transaction_hashes_to_prove)
            .await
            .unwrap();

        assert_eq!(transactions_set_proof.len(), 0);
    }

    #[tokio::test]
    async fn compute_proof_for_one_set_of_three_certified_transactions_and_two_unknowns() {
        let transactions = CardanoTransactionsBuilder::new()
            .max_transactions_per_block(1)
            .blocks_per_block_range(3)
            .build_block_ranges(5);
        let transactions_to_prove =
            test_data::filter_transactions_for_indices(&[1, 2, 4], &transactions);
        let transaction_hashes_unknown =
            vec!["tx-unknown-123".to_string(), "tx-unknown-456".to_string()];
        let mut test_data = test_data::build_test_data(&transactions_to_prove, &transactions);
        let transaction_hashes_known = test_data.transaction_hashes_to_prove.clone();
        test_data.transaction_hashes_to_prove = [
            test_data.transaction_hashes_to_prove.clone(),
            transaction_hashes_unknown,
        ]
        .concat();
        let prover = build_prover(
            |transaction_retriever_mock| {
                let transactions_hashes_to_prove = test_data.transaction_hashes_to_prove.clone();
                let transactions_to_prove = transactions_to_prove.clone();
                transaction_retriever_mock
                    .expect_get_by_hashes()
                    .withf(move |hashes, _| equivalent_to(hashes, &transactions_hashes_to_prove))
                    .return_once(move |_, _| Ok(transactions_to_prove));

                let block_ranges_to_prove = test_data.block_ranges_to_prove.clone();
                let all_transactions_in_block_ranges_to_prove =
                    test_data.all_transactions_in_block_ranges_to_prove.clone();
                transaction_retriever_mock
                    .expect_get_by_block_ranges()
                    .with(eq(block_ranges_to_prove))
                    .return_once(move |_| Ok(all_transactions_in_block_ranges_to_prove));
            },
            |block_range_root_retriever_mock| {
                let block_ranges_map = test_data.block_ranges_map.clone();
                block_range_root_retriever_mock
                    .expect_compute_merkle_map_from_block_range_roots()
                    .return_once(|_| {
                        Ok(test_data::compute_mk_map_from_block_ranges_map(
                            block_ranges_map,
                        ))
                    });
            },
        );
        prover.compute_cache(test_data.beacon).await.unwrap();

        let transactions_set_proof = prover
            .compute_transactions_proofs(test_data.beacon, &test_data.transaction_hashes_to_prove)
            .await
            .unwrap();

        assert_eq!(transactions_set_proof.len(), 1);
        assert_equivalent(
            transactions_set_proof[0].transactions_hashes(),
            &transaction_hashes_known,
        );
        transactions_set_proof[0].verify().unwrap();
    }

    #[tokio::test]
    async fn cant_compute_proof_if_transaction_retriever_fails() {
        let transactions = CardanoTransactionsBuilder::new()
            .max_transactions_per_block(1)
            .blocks_per_block_range(3)
            .build_block_ranges(5);
        let transactions_to_prove =
            test_data::filter_transactions_for_indices(&[1, 2, 4], &transactions);
        let test_data = test_data::build_test_data(&transactions_to_prove, &transactions);
        let prover = build_prover(
            |transaction_retriever_mock| {
                transaction_retriever_mock
                    .expect_get_by_hashes()
                    .returning(|_, _| Err(anyhow!("Error")));
            },
            |block_range_root_retriever_mock| {
                block_range_root_retriever_mock
                    .expect_compute_merkle_map_from_block_range_roots()
                    .return_once(|_| MKMap::new(&[]));
            },
        );
        prover.compute_cache(test_data.beacon).await.unwrap();

        prover
            .compute_transactions_proofs(test_data.beacon, &test_data.transaction_hashes_to_prove)
            .await
            .expect_err("Should have failed because of transaction retriever failure");
    }

    #[tokio::test]
    async fn cant_compute_proof_if_block_range_root_retriever_fails() {
        let transactions = CardanoTransactionsBuilder::new()
            .max_transactions_per_block(1)
            .blocks_per_block_range(3)
            .build_block_ranges(5);
        let transactions_to_prove =
            test_data::filter_transactions_for_indices(&[1, 2, 4], &transactions);
        let test_data = test_data::build_test_data(&transactions_to_prove, &transactions);
        let prover = build_prover(
            |transaction_retriever_mock| {
                let transactions_to_prove = transactions_to_prove.clone();
                transaction_retriever_mock
                    .expect_get_by_hashes()
                    .return_once(move |_, _| Ok(transactions_to_prove));

                let all_transactions_in_block_ranges_to_prove =
                    test_data.all_transactions_in_block_ranges_to_prove.clone();
                transaction_retriever_mock
                    .expect_get_by_block_ranges()
                    .return_once(move |_| Ok(all_transactions_in_block_ranges_to_prove));
            },
            |block_range_root_retriever_mock| {
                block_range_root_retriever_mock
                    .expect_compute_merkle_map_from_block_range_roots()
                    .return_once(|_| Err(anyhow!("Error")));
            },
        );

        prover
            .compute_transactions_proofs(test_data.beacon, &test_data.transaction_hashes_to_prove)
            .await
            .expect_err("Should have failed because of block range root retriever failure");
    }

    #[test]
    fn transactions_hash_filter_to_prove_apply_remove_empty_hashes() {
        let transactions_hashes = vec!["a".repeat(64), "".to_string(), "b".repeat(64)];
        let filter = TransactionsHashFilterToProve::default();

        let valid_hashes = filter.apply(&transactions_hashes);

        assert_equivalent(vec!["a".repeat(64), "b".repeat(64)], valid_hashes);
    }

    #[test]
    fn transactions_hash_filter_to_prove_apply_deduplicate_hashes() {
        let transactions_hashes = vec![
            "a".repeat(64),
            "b".repeat(64),
            "b".repeat(64),
            "c".repeat(64),
            "b".repeat(64),
        ];
        let filter = TransactionsHashFilterToProve::default();

        let valid_hashes = filter.apply(&transactions_hashes);

        assert_equivalent(
            vec!["a".repeat(64), "b".repeat(64), "c".repeat(64)],
            valid_hashes,
        );
    }

    #[test]
    fn transactions_hash_filter_to_prove_apply_keep_only_hash_that_have_a_size_of_64() {
        let transactions_hashes = vec!["a".repeat(64), "b".repeat(65), "c".repeat(64)];
        let filter = TransactionsHashFilterToProve::default();

        let valid_hashes = filter.apply(&transactions_hashes);

        assert_equivalent(vec!["a".repeat(64), "c".repeat(64)], valid_hashes);
    }

    #[test]
    fn transactions_hash_filter_to_prove_apply_keep_only_hexadecimal_characters() {
        let transactions_hashes = vec![
            "a".repeat(63) + "a",
            "a".repeat(63) + "g",
            "a".repeat(63) + "f",
            "a".repeat(63) + "x",
            "a".repeat(63) + ";",
            "a".repeat(63) + " ",
            "a".repeat(63) + "à",
            "a".repeat(63) + "9",
        ];
        let filter = TransactionsHashFilterToProve::default();

        let valid_hashes = filter.apply(&transactions_hashes);

        assert_equivalent(
            vec![
                "a".repeat(63) + "a",
                "a".repeat(63) + "f",
                "a".repeat(63) + "9",
            ],
            valid_hashes,
        );
    }

    #[test]
    fn transactions_hash_filter_to_prove_apply_with_limit_on_transactions_hashes_number() {
        let transactions_hashes = vec!["a".repeat(64), "b".repeat(64), "c".repeat(64)];
        let filter = TransactionsHashFilterToProve::new(2);

        let valid_hashes = filter.apply(&transactions_hashes);

        assert_equivalent(vec!["a".repeat(64), "b".repeat(64)], valid_hashes);
    }

    #[test]
    fn transactions_hash_filter_to_prove_apply_with_limit_on_transactions_hashes_number_only_on_valid_hashes(
    ) {
        let transactions_hashes = vec![
            "zz".to_string(),
            "a".repeat(64),
            "xx".to_string(),
            "b".repeat(64),
            "c".repeat(64),
        ];
        let filter = TransactionsHashFilterToProve::new(2);

        let valid_hashes = filter.apply(&transactions_hashes);

        assert_equivalent(vec!["a".repeat(64), "b".repeat(64)], valid_hashes);
    }
}

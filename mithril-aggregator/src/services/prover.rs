use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::sync::Arc;
use std::time::Duration;

use anyhow::Context;
use async_trait::async_trait;
use rayon::prelude::*;
use slog::{Logger, debug, info};

use mithril_common::{
    StdResult,
    crypto_helper::{MKMap, MKMapNode, MKTree, MKTreeStorer},
    entities::{
        BlockHash, BlockNumber, BlockRange, CardanoBlock, CardanoBlockTransactionMkTreeNode,
        CardanoTransaction, IntoMKTreeNode, MkSetProof, TransactionHash,
    },
    logging::LoggerExtensions,
    signable_builder::BlockRangeRootRetriever,
};
use mithril_resource_pool::{ResourcePool, ResourcePoolItem};

/// Prover service is the cryptographic engine in charge of producing cryptographic proofs for transactions and blocks
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait ProverService: Sync + Send {
    /// Compute the cryptographic proofs for the given blocks
    async fn compute_blocks_proofs(
        &self,
        up_to: BlockNumber,
        hashes: &[BlockHash],
    ) -> StdResult<Option<MkSetProof<CardanoBlock>>>;

    /// Compute the cryptographic proofs for the given transactions
    async fn compute_transactions_proofs(
        &self,
        up_to: BlockNumber,
        hashes: &[TransactionHash],
    ) -> StdResult<Option<MkSetProof<CardanoTransaction>>>;

    /// Compute the cache
    async fn compute_cache(&self, up_to: BlockNumber) -> StdResult<()>;
}

/// Blocks and transactions retriever
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait BlocksTransactionsRetriever: Sync + Send {
    /// Get a list of block by hashes
    async fn get_block_by_hashes(
        &self,
        block_hashes: Vec<BlockHash>,
        up_to: BlockNumber,
    ) -> StdResult<Vec<CardanoBlock>>;

    /// Get a list of transactions by hashes
    async fn get_transactions_by_hashes(
        &self,
        transaction_hashes: Vec<TransactionHash>,
        up_to: BlockNumber,
    ) -> StdResult<Vec<CardanoTransaction>>;

    /// Get all [CardanoBlockTransactionMkTreeNode] in the given block ranges
    async fn get_all_mk_nodes_by_block_ranges(
        &self,
        block_ranges: Vec<BlockRange>,
    ) -> StdResult<Vec<CardanoBlockTransactionMkTreeNode>>;
}

/// Mithril prover
pub struct MithrilProverService<S: MKTreeStorer> {
    blocks_transactions_retriever: Arc<dyn BlocksTransactionsRetriever>,
    block_range_root_retriever: Arc<dyn BlockRangeRootRetriever<S>>,
    mk_map_pool: ResourcePool<MKMap<BlockRange, MKMapNode<BlockRange, S>, S>>,
    logger: Logger,
}

impl<S: MKTreeStorer> MithrilProverService<S> {
    /// Create a new Mithril prover
    pub fn new(
        blocks_transactions_retriever: Arc<dyn BlocksTransactionsRetriever>,
        block_range_root_retriever: Arc<dyn BlockRangeRootRetriever<S>>,
        mk_map_pool_size: usize,
        logger: Logger,
    ) -> Self {
        Self {
            blocks_transactions_retriever,
            block_range_root_retriever,
            mk_map_pool: ResourcePool::new(mk_map_pool_size, vec![]),
            logger: logger.new_with_component_name::<Self>(),
        }
    }

    async fn get_all_nodes_for_block_ranges(
        &self,
        block_ranges: Vec<BlockRange>,
    ) -> StdResult<HashMap<BlockRange, BTreeSet<CardanoBlockTransactionMkTreeNode>>> {
        let mut block_ranges_map = HashMap::with_capacity(block_ranges.len());
        let nodes = self
            .blocks_transactions_retriever
            .get_all_mk_nodes_by_block_ranges(block_ranges)
            .await?;

        for node in nodes {
            let block_range = BlockRange::from_block_number(node.block_number());
            let block_range_entry: &mut BTreeSet<_> =
                block_ranges_map.entry(block_range).or_insert(BTreeSet::new());
            block_range_entry.insert(node);
        }

        Ok(block_ranges_map)
    }

    async fn get_mk_map(
        &self,
        node_to_insert: HashMap<BlockRange, BTreeSet<CardanoBlockTransactionMkTreeNode>>,
    ) -> StdResult<ResourcePoolItem<'_, MKMap<BlockRange, MKMapNode<BlockRange, S>, S>>> {
        // 1 - Compute block ranges sub Merkle trees
        let mk_trees: StdResult<Vec<(BlockRange, MKTree<S>)>> = node_to_insert
            .into_iter()
            .map(|(block_range, node)| {
                let mk_tree = MKTree::new_from_iter(node)?;
                Ok((block_range, mk_tree))
            })
            .collect();
        let mk_trees = BTreeMap::from_iter(mk_trees?);

        // 2 - Compute block range roots Merkle map
        let acquire_timeout = Duration::from_millis(1000);
        let mut mk_map = self.mk_map_pool.acquire_resource(acquire_timeout)?;

        // 3 - Enrich the Merkle map with the block ranges Merkle trees
        for (block_range, mk_tree) in mk_trees {
            mk_map.replace(block_range, mk_tree.into())?;
        }

        Ok(mk_map)
    }

    async fn compute_proof<T>(
        &self,
        items_to_prove: Vec<T>,
        extract_block_number: fn(&T) -> BlockNumber,
    ) -> StdResult<Option<MkSetProof<T>>>
    where
        T: Into<CardanoBlockTransactionMkTreeNode> + IntoMKTreeNode + Clone,
    {
        if items_to_prove.is_empty() {
            return Ok(None);
        }

        // 1 - Compute the set of block ranges with the items to prove
        let nodes_to_prove: Vec<CardanoBlockTransactionMkTreeNode> =
            items_to_prove.iter().cloned().map(Into::into).collect();
        let block_ranges: BTreeSet<_> = items_to_prove
            .iter()
            .map(|t| BlockRange::from_block_number(extract_block_number(t)))
            .collect();

        // 2 - Fetch all nodes contained in the block ranges
        let nodes_per_block_ranges = self
            .get_all_nodes_for_block_ranges(block_ranges.into_iter().collect())
            .await?;

        // 3 - Fetch a cached Merkle map for the block ranges and replace its block ranges leaves with the fetched nodes
        let mk_map = self.get_mk_map(nodes_per_block_ranges).await?;

        // 4 - Compute the proof for all transactions
        let mk_proof = mk_map.compute_proof(&nodes_to_prove).with_context(|| {
            format!("Failed to compute a merkle proof for the items: {nodes_to_prove:?}")
        })?;
        self.mk_map_pool.give_back_resource_pool_item(mk_map)?;

        Ok(Some(MkSetProof::<T>::new(items_to_prove, mk_proof)))
    }
}

#[async_trait]
impl<S: MKTreeStorer> ProverService for MithrilProverService<S> {
    async fn compute_blocks_proofs(
        &self,
        up_to: BlockNumber,
        block_hashes: &[BlockHash],
    ) -> StdResult<Option<MkSetProof<CardanoBlock>>> {
        let blocks = self
            .blocks_transactions_retriever
            .get_block_by_hashes(block_hashes.to_vec(), up_to)
            .await?;

        self.compute_proof(blocks, |b| b.block_number).await
    }

    async fn compute_transactions_proofs(
        &self,
        up_to: BlockNumber,
        transaction_hashes: &[TransactionHash],
    ) -> StdResult<Option<MkSetProof<CardanoTransaction>>> {
        let transactions = self
            .blocks_transactions_retriever
            .get_transactions_by_hashes(transaction_hashes.to_vec(), up_to)
            .await?;

        self.compute_proof(transactions, |t| t.block_number).await
    }

    async fn compute_cache(&self, up_to: BlockNumber) -> StdResult<()> {
        let pool_size = self.mk_map_pool.size();
        info!(
            self.logger, "Starts computing the Merkle map pool resource of size {pool_size}";
            "up_to_block_number" => *up_to,
        );
        let mk_map_cache = self
            .block_range_root_retriever
            .compute_merkle_map_from_block_range_roots(up_to)
            .await?;
        let mk_maps_new = (1..=pool_size)
            .into_par_iter()
            .map(|i| {
                debug!(
                    self.logger,
                    "Computing the Merkle map pool resource {i}/{pool_size}"
                );
                mk_map_cache.clone()
            })
            .collect::<Vec<MKMap<_, _, _>>>();
        debug!(self.logger, "Draining the Merkle map pool");
        let discriminant_new = self.mk_map_pool.discriminant()? + 1;
        self.mk_map_pool.set_discriminant(discriminant_new)?;
        self.mk_map_pool.clear();
        debug!(
            self.logger,
            "Giving back new resources to the Merkle map pool"
        );
        mk_maps_new
            .into_iter()
            .map(|mk_map| self.mk_map_pool.give_back_resource(mk_map, discriminant_new))
            .collect::<StdResult<Vec<_>>>()?;
        info!(
            self.logger,
            "Completed computing the Merkle map pool resource of size {pool_size}"
        );

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;

    use mithril_cardano_node_chain::test::double::InMemoryChainDataStore;
    use mithril_common::crypto_helper::MKTreeStoreInMemory;
    use mithril_common::entities::CardanoBlockWithTransactions;
    use mithril_common::test::builder::CardanoTransactionsBuilder;
    use mithril_common::test::mock_extensions::MockBuilder;

    use crate::test::TestLogger;
    use crate::test::double::mocks::MockBlockRangeRootRetriever;

    use super::*;

    #[async_trait]
    impl BlocksTransactionsRetriever for InMemoryChainDataStore {
        async fn get_block_by_hashes(
            &self,
            block_hashes: Vec<BlockHash>,
            up_to: BlockNumber,
        ) -> StdResult<Vec<CardanoBlock>> {
            let blocks = self.get_blocks_by_hashes(&block_hashes).await;
            Ok(blocks.into_iter().filter(|b| b.block_number <= up_to).collect())
        }

        async fn get_transactions_by_hashes(
            &self,
            transaction_hashes: Vec<TransactionHash>,
            up_to: BlockNumber,
        ) -> StdResult<Vec<CardanoTransaction>> {
            let transactions = self.get_transactions_by_hashes(&transaction_hashes).await;
            Ok(transactions.into_iter().filter(|b| b.block_number <= up_to).collect())
        }

        async fn get_all_mk_nodes_by_block_ranges(
            &self,
            block_ranges: Vec<BlockRange>,
        ) -> StdResult<Vec<CardanoBlockTransactionMkTreeNode>> {
            Ok(self
                .get_blocks_with_transactions_in_block_ranges(&block_ranges)
                .await
                .into_iter()
                .collect())
        }
    }

    async fn setup_prover_for_test<S: MKTreeStorer>(
        stored_blocks: Vec<CardanoBlockWithTransactions>,
        block_ranges_available_up_to: BlockNumber,
    ) -> MithrilProverService<S> {
        let repository = Arc::new(
            InMemoryChainDataStore::builder()
                .with_blocks_and_transactions(&stored_blocks)
                .compute_block_ranges(block_ranges_available_up_to)
                .await
                .build(),
        );
        let mk_map_pool_size = 1;

        MithrilProverService::<S>::new(
            repository.clone(),
            repository.clone(),
            mk_map_pool_size,
            TestLogger::stdout(),
        )
    }

    fn filter_items_for_indices<T: Clone>(indices: &[usize], items: &[T]) -> Vec<T> {
        items
            .iter()
            .enumerate()
            .filter(|(i, _)| indices.contains(i))
            .map(|(_, t)| t.to_owned())
            .collect()
    }

    mod blocks_proof {
        use super::*;

        fn into_blocks(blocks: &[CardanoBlockWithTransactions]) -> Vec<CardanoBlock> {
            blocks.iter().cloned().map(Into::into).collect()
        }

        fn blocks_hashes(blocks: &[CardanoBlock]) -> Vec<BlockHash> {
            blocks.iter().map(|t| t.block_hash.clone()).collect()
        }

        #[tokio::test]
        async fn compute_proof_for_one_set_of_three_certified_blocks() {
            let blocks_with_tx = CardanoTransactionsBuilder::new()
                .max_transactions_per_block(1)
                .blocks_per_block_range(3)
                .build_blocks_for_block_ranges(5);
            let blocks = into_blocks(&blocks_with_tx);
            let blocks_to_prove = filter_items_for_indices(&[1, 2, 4], &blocks);
            let beacon = blocks_with_tx.last().unwrap().block_number;

            let prover = setup_prover_for_test::<MKTreeStoreInMemory>(blocks_with_tx, beacon).await;
            prover.compute_cache(beacon).await.unwrap();

            let blocks_set_proof = prover
                .compute_blocks_proofs(beacon, &blocks_hashes(&blocks_to_prove))
                .await
                .unwrap()
                .unwrap();

            assert_eq!(blocks_set_proof.blocks(), blocks_to_prove);
            blocks_set_proof.verify().unwrap();
        }

        #[tokio::test]
        async fn cant_compute_proof_for_not_yet_certified_block() {
            let blocks_with_txs = CardanoTransactionsBuilder::new()
                .max_transactions_per_block(1)
                .blocks_per_block_range(3)
                .build_blocks_for_block_ranges(5);
            let blocks: Vec<_> = into_blocks(&blocks_with_txs);
            // Only certify blocks for the first three ranges
            let beacon = blocks_with_txs[3 * 3].block_number;
            // try to generate proofs for blocks that in the last two, uncertified, ranges
            let blocks_to_prove = filter_items_for_indices(&[10, 14], &blocks);

            let prover =
                setup_prover_for_test::<MKTreeStoreInMemory>(blocks_with_txs, beacon).await;
            prover.compute_cache(beacon).await.unwrap();

            let blocks_set_proof = prover
                .compute_blocks_proofs(beacon, &blocks_hashes(&blocks_to_prove))
                .await
                .unwrap();

            assert_eq!(None, blocks_set_proof);
        }

        #[tokio::test]
        async fn cant_compute_proof_for_unknown_block() {
            let blocks_with_txs = CardanoTransactionsBuilder::new()
                .max_transactions_per_block(1)
                .blocks_per_block_range(3)
                .build_blocks_for_block_ranges(5);
            let beacon = blocks_with_txs.last().unwrap().block_number;

            let prover =
                setup_prover_for_test::<MKTreeStoreInMemory>(blocks_with_txs, beacon).await;
            prover.compute_cache(beacon).await.unwrap();

            let blocks_set_proof = prover
                .compute_blocks_proofs(beacon, &["block-unknown".to_string()])
                .await
                .unwrap();

            assert_eq!(None, blocks_set_proof);
        }

        #[tokio::test]
        async fn compute_proof_for_one_set_of_three_certified_blocks_and_two_unknowns() {
            let blocks_with_txs = CardanoTransactionsBuilder::new()
                .max_transactions_per_block(1)
                .blocks_per_block_range(3)
                .build_blocks_for_block_ranges(5);
            let blocks = into_blocks(&blocks_with_txs);
            // Only certify blocks for the first three ranges
            let beacon = blocks_with_txs[3 * 3].block_number;
            let blocks_to_prove = filter_items_for_indices(&[1, 4, 8], &blocks);

            let prover =
                setup_prover_for_test::<MKTreeStoreInMemory>(blocks_with_txs, beacon).await;
            prover.compute_cache(beacon).await.unwrap();

            let blocks_set_proof = prover
                .compute_blocks_proofs(
                    beacon,
                    &[
                        blocks_hashes(&blocks_to_prove),
                        vec!["block-unknown-1".to_string(), "block-unknown-2".to_string()],
                    ]
                    .concat(),
                )
                .await
                .unwrap()
                .unwrap();

            assert_eq!(blocks_set_proof.blocks(), blocks_to_prove);
            blocks_set_proof.verify().unwrap();
        }
    }

    mod transactions_proof {
        use super::*;

        fn into_transactions(blocks: &[CardanoBlockWithTransactions]) -> Vec<CardanoTransaction> {
            blocks
                .iter()
                .cloned()
                .flat_map(|block| block.into_transactions())
                .collect()
        }

        fn transactions_hashes(transactions: &[CardanoTransaction]) -> Vec<TransactionHash> {
            transactions.iter().map(|t| t.transaction_hash.clone()).collect()
        }

        #[tokio::test]
        async fn compute_proof_for_one_set_of_three_certified_transactions() {
            let blocks_with_txs = CardanoTransactionsBuilder::new()
                .max_transactions_per_block(1)
                .blocks_per_block_range(3)
                .build_blocks_for_block_ranges(5);
            let transactions: Vec<_> = into_transactions(&blocks_with_txs);
            let transactions_to_prove = filter_items_for_indices(&[1, 2, 4], &transactions);
            let beacon = blocks_with_txs.last().unwrap().block_number;

            let prover =
                setup_prover_for_test::<MKTreeStoreInMemory>(blocks_with_txs, beacon).await;
            prover.compute_cache(beacon).await.unwrap();

            let transactions_set_proof = prover
                .compute_transactions_proofs(beacon, &transactions_hashes(&transactions_to_prove))
                .await
                .unwrap()
                .unwrap();

            assert_eq!(transactions_set_proof.transactions(), transactions_to_prove);
            transactions_set_proof.verify().unwrap();
        }

        #[tokio::test]
        async fn cant_compute_proof_for_not_yet_certified_transaction() {
            let blocks_with_txs = CardanoTransactionsBuilder::new()
                .max_transactions_per_block(1)
                .blocks_per_block_range(3)
                .build_blocks_for_block_ranges(5);
            let transactions: Vec<_> = into_transactions(&blocks_with_txs);
            // Only certify transactions for the first three ranges
            let beacon = blocks_with_txs[3 * 3].block_number;
            // try to generate proofs for transactions that in the last two, uncertified, ranges
            let transactions_to_prove = filter_items_for_indices(&[10, 14], &transactions);

            let prover =
                setup_prover_for_test::<MKTreeStoreInMemory>(blocks_with_txs, beacon).await;
            prover.compute_cache(beacon).await.unwrap();

            let transactions_set_proof = prover
                .compute_transactions_proofs(beacon, &transactions_hashes(&transactions_to_prove))
                .await
                .unwrap();

            assert_eq!(None, transactions_set_proof);
        }

        #[tokio::test]
        async fn cant_compute_proof_for_unknown_transaction() {
            let blocks_with_txs = CardanoTransactionsBuilder::new()
                .max_transactions_per_block(1)
                .blocks_per_block_range(3)
                .build_blocks_for_block_ranges(5);
            let beacon = blocks_with_txs.last().unwrap().block_number;

            let prover =
                setup_prover_for_test::<MKTreeStoreInMemory>(blocks_with_txs, beacon).await;
            prover.compute_cache(beacon).await.unwrap();

            let transactions_set_proof = prover
                .compute_transactions_proofs(beacon, &["tx-unknown".to_string()])
                .await
                .unwrap();

            assert_eq!(None, transactions_set_proof);
        }

        #[tokio::test]
        async fn compute_proof_for_one_set_of_three_certified_transactions_and_two_unknowns() {
            let blocks_with_txs = CardanoTransactionsBuilder::new()
                .max_transactions_per_block(1)
                .blocks_per_block_range(3)
                .build_blocks_for_block_ranges(5);
            let transactions: Vec<_> = into_transactions(&blocks_with_txs);
            // Only certify transactions for the first three ranges
            let beacon = blocks_with_txs[3 * 3].block_number;
            let transactions_to_prove = filter_items_for_indices(&[1, 4, 8], &transactions);

            let prover =
                setup_prover_for_test::<MKTreeStoreInMemory>(blocks_with_txs, beacon).await;
            prover.compute_cache(beacon).await.unwrap();

            let transactions_set_proof = prover
                .compute_transactions_proofs(
                    beacon,
                    &[
                        transactions_hashes(&transactions_to_prove),
                        vec!["tx-unknown-1".to_string(), "tx-unknown-2".to_string()],
                    ]
                    .concat(),
                )
                .await
                .unwrap()
                .unwrap();

            assert_eq!(transactions_set_proof.transactions(), transactions_to_prove);
            transactions_set_proof.verify().unwrap();
        }
    }

    #[tokio::test]
    async fn cant_compute_proof_if_block_retriever_fails() {
        let blocks_with_txs = CardanoTransactionsBuilder::new()
            .max_transactions_per_block(1)
            .blocks_per_block_range(1)
            .build_blocks_for_block_ranges(1);
        let first_block = blocks_with_txs.last().unwrap().clone();
        let beacon = blocks_with_txs.last().unwrap().block_number;

        let prover = MithrilProverService {
            blocks_transactions_retriever:
                MockBuilder::<MockBlocksTransactionsRetriever>::configure(|mock| {
                    mock.expect_get_all_mk_nodes_by_block_ranges()
                        .returning(|_| Err(anyhow!("fail")));
                    mock.expect_get_block_by_hashes()
                        .returning(|_, _| Err(anyhow!("fail")));
                    mock.expect_get_transactions_by_hashes()
                        .returning(|_, _| Err(anyhow!("fail")));
                }),
            ..setup_prover_for_test::<MKTreeStoreInMemory>(blocks_with_txs, beacon).await
        };
        prover.compute_cache(beacon).await.unwrap();

        prover
            .compute_transactions_proofs(beacon, &[first_block.transactions_hashes[0].clone()])
            .await
            .expect_err("Should fail to compute proof since blocks/transactions retriever fails");

        prover
            .compute_blocks_proofs(beacon, std::slice::from_ref(&first_block.block_hash))
            .await
            .expect_err("Should fail to compute proof since blocks/transactions retriever fails");
    }

    #[tokio::test]
    async fn cant_compute_proof_if_block_range_root_retriever_fails() {
        let blocks_with_txs = CardanoTransactionsBuilder::new()
            .max_transactions_per_block(1)
            .blocks_per_block_range(1)
            .build_blocks_for_block_ranges(1);
        let first_block = blocks_with_txs.last().unwrap().clone();
        let beacon = blocks_with_txs.last().unwrap().block_number;

        let prover = MithrilProverService {
            block_range_root_retriever: MockBuilder::<
                MockBlockRangeRootRetriever<MKTreeStoreInMemory>,
            >::configure(|mock| {
                mock.expect_compute_merkle_map_from_block_range_roots()
                    .returning(|_| Err(anyhow!("fail")));
                mock.expect_retrieve_block_range_roots()
                    .returning(|_| Err(anyhow!("fail")));
            }),
            ..setup_prover_for_test::<MKTreeStoreInMemory>(blocks_with_txs, beacon).await
        };
        prover
            .compute_cache(beacon)
            .await
            .expect_err("Should fail to compute cache since block range retriever fails");

        prover
            .compute_transactions_proofs(beacon, &[first_block.transactions_hashes[0].clone()])
            .await
            .expect_err("Should fail to compute proof since blocks range retriever fails");

        prover
            .compute_blocks_proofs(beacon, std::slice::from_ref(&first_block.block_hash))
            .await
            .expect_err("Should fail to compute proof since blocks range retriever fails");
    }
}

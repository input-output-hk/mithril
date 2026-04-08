use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::ops::{Range, RangeToInclusive};
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
    async fn get_all_mk_nodes_by_ranges_of_block_numbers(
        &self,
        ranges_of_block: Vec<Range<BlockNumber>>,
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

    async fn get_all_nodes_in_ranges_and_group_them_by_block_ranges(
        &self,
        ranges_of_block: Vec<Range<BlockNumber>>,
    ) -> StdResult<HashMap<BlockRange, BTreeSet<CardanoBlockTransactionMkTreeNode>>> {
        let mut block_ranges_map = HashMap::with_capacity(ranges_of_block.len());
        let nodes = self
            .blocks_transactions_retriever
            .get_all_mk_nodes_by_ranges_of_block_numbers(ranges_of_block)
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
        nodes_to_insert: HashMap<BlockRange, BTreeSet<CardanoBlockTransactionMkTreeNode>>,
    ) -> StdResult<ResourcePoolItem<'_, MKMap<BlockRange, MKMapNode<BlockRange, S>, S>>> {
        // 1 - Compute block ranges sub Merkle trees
        let mk_trees: StdResult<Vec<(BlockRange, MKTree<S>)>> = nodes_to_insert
            .into_iter()
            .map(|(block_range, node)| {
                let mk_tree = MKTree::new_from_iter(node)?;
                Ok((block_range, mk_tree))
            })
            .collect();
        let mk_trees = BTreeMap::from_iter(mk_trees?);

        // 2 - Acquire global Merkle map kept in cache
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
        up_to: BlockNumber,
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

        // 2 - Fetch all nodes contained in the block ranges
        let ranges_to_retrieve = compute_ranges_of_block_number_to_retrieve(
            &items_to_prove,
            extract_block_number,
            ..=up_to,
        );
        let nodes_per_block_ranges = self
            .get_all_nodes_in_ranges_and_group_them_by_block_ranges(ranges_to_retrieve)
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

        self.compute_proof(up_to, blocks, |b| b.block_number).await
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

        self.compute_proof(up_to, transactions, |t| t.block_number).await
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

fn compute_ranges_of_block_number_to_retrieve<T>(
    items_with_block_number: &[T],
    extract_block_number: fn(&T) -> BlockNumber,
    up_to: RangeToInclusive<BlockNumber>,
) -> Vec<Range<BlockNumber>> {
    // Note: Range<T> does not implement Ord and dedup only remove consecutive duplicates.
    let ordered_block_numbers: BTreeSet<_> =
        items_with_block_number.iter().map(extract_block_number).collect();
    let mut ranges: Vec<_> = ordered_block_numbers
        .into_iter()
        .filter_map(|n| {
            up_to
                .contains(&n)
                .then_some(BlockRange::from_block_number(n))
                .map(|block_range| block_range.start..block_range.end.min(up_to.end + 1))
                .filter(|r| !r.is_empty())
        })
        .collect();
    ranges.dedup();

    ranges
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;

    use mithril_cardano_node_chain::chain_importer::ChainDataStore;
    use mithril_cardano_node_chain::test::double::InMemoryChainDataStore;
    use mithril_common::{
        crypto_helper::MKTreeStoreInMemory,
        entities::{CardanoBlockWithTransactions, SlotNumber},
        test::{
            builder::CardanoTransactionsBuilder, crypto_helper::MKMapTestExtension,
            mock_extensions::MockBuilder,
        },
    };

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

        async fn get_all_mk_nodes_by_ranges_of_block_numbers(
            &self,
            ranges_of_block: Vec<Range<BlockNumber>>,
        ) -> StdResult<Vec<CardanoBlockTransactionMkTreeNode>> {
            Ok(self
                .get_blocks_with_transactions_in_ranges(&ranges_of_block)
                .await
                .into_iter()
                .collect())
        }
    }

    async fn setup_prover_for_test<S: MKTreeStorer>(
        stored_blocks: &[CardanoBlockWithTransactions],
        block_ranges_ending_up_to: BlockNumber,
    ) -> MithrilProverService<S> {
        let repository = Arc::new(
            InMemoryChainDataStore::builder()
                .with_blocks_and_transactions(stored_blocks)
                .compute_block_ranges(block_ranges_ending_up_to)
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

    fn compute_certificate_merkle_root(
        leaves: &[CardanoBlockWithTransactions],
        max_block_to_include: BlockNumber,
    ) -> String {
        let filtered_leaves: Vec<_> = leaves
            .iter()
            .filter(|t| t.block_number <= max_block_to_include)
            .cloned()
            .collect();
        let map =
            MKMap::<_, _, MKTreeStoreInMemory>::from_blocks_with_transactions(&filtered_leaves)
                .unwrap();
        let merkle_root = map.compute_root().unwrap();
        merkle_root.to_hex()
    }

    fn into_blocks(blocks: &[CardanoBlockWithTransactions]) -> Vec<CardanoBlock> {
        blocks.iter().cloned().map(Into::into).collect()
    }

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

    fn blocks_hashes(blocks: &[CardanoBlock]) -> Vec<BlockHash> {
        blocks.iter().map(|t| t.block_hash.clone()).collect()
    }

    fn filter_items_for_indices<T: Clone>(indices: &[usize], items: &[T]) -> Vec<T> {
        items
            .iter()
            .enumerate()
            .filter(|(i, _)| indices.contains(i))
            .map(|(_, t)| t.to_owned())
            .collect()
    }

    mod compute_ranges_of_block_number_to_retrieve {
        use super::*;

        #[test]
        fn for_empty_item_list() {
            assert_eq!(
                compute_ranges_of_block_number_to_retrieve(
                    &[],
                    |&x| BlockNumber(x),
                    ..=BlockNumber(10_000)
                ),
                Vec::<Range<BlockNumber>>::new()
            );
        }

        #[test]
        fn for_unordered_list_entirely_contained_in_the_first_block_range() {
            assert_eq!(
                compute_ranges_of_block_number_to_retrieve(
                    &[1, 5, 2],
                    |&x| BlockNumber(x),
                    ..=BlockNumber(10_000)
                ),
                vec![BlockNumber(0)..BlockNumber(15)]
            );
        }

        #[test]
        fn for_unordered_list_contained_in_two_consecutive_block_ranges() {
            assert_eq!(
                compute_ranges_of_block_number_to_retrieve(
                    &[1, 16, 2, 15, 14, 11, 29],
                    |&x| BlockNumber(x),
                    ..=BlockNumber(10_000)
                ),
                vec![BlockNumber(0)..BlockNumber(15), BlockNumber(15)..BlockNumber(30)]
            );
        }

        #[test]
        fn for_unordered_list_contained_in_two_non_consecutive_block_ranges() {
            assert_eq!(
                compute_ranges_of_block_number_to_retrieve(
                    &[1, 44, 2, 30, 14, 11, 31],
                    |&x| BlockNumber(x),
                    ..=BlockNumber(10_000)
                ),
                vec![BlockNumber(0)..BlockNumber(15), BlockNumber(30)..BlockNumber(45)]
            );
        }

        #[test]
        fn when_max_end_boundary_is_below_all_blocks() {
            assert_eq!(
                compute_ranges_of_block_number_to_retrieve(
                    &[19, 15, 12],
                    |&x| BlockNumber(x),
                    ..=BlockNumber(10)
                ),
                Vec::<Range<BlockNumber>>::new()
            );
        }

        #[test]
        fn when_up_to_included_equal_a_block_range_start() {
            let block_range_start = BlockRange::from_block_number(BlockRange::LENGTH * 2).start;
            assert_eq!(
                compute_ranges_of_block_number_to_retrieve(
                    &[block_range_start],
                    |&x| x,
                    ..=block_range_start
                ),
                [BlockNumber(30)..BlockNumber(31)]
            );
        }

        #[test]
        fn when_up_to_included_equal_a_block_range_end() {
            assert_eq!(
                compute_ranges_of_block_number_to_retrieve(
                    &[15, 31, 29, 28, 30],
                    |&x| BlockNumber(x),
                    ..=BlockRange::from_block_number(BlockRange::LENGTH).end
                ),
                vec![BlockNumber(15)..BlockNumber(30), BlockNumber(30)..BlockNumber(31)]
            );
        }

        #[test]
        fn when_up_to_included_is_not_a_block_range_boundary() {
            assert_eq!(
                compute_ranges_of_block_number_to_retrieve(
                    &[15, 31, 29, 28, 30],
                    |&x| BlockNumber(x),
                    ..=BlockRange::from_block_number(BlockRange::LENGTH).end - 1
                ),
                vec![BlockNumber(15)..BlockNumber(30)]
            );
        }
    }

    mod partial_block_ranges_proof {
        use super::*;

        #[tokio::test]
        async fn compute_proof_when_last_range_is_partial_and_additional_block_range_were_stored_after_cache_computation()
         {
            let initial_blocks_with_txs = vec![
                CardanoBlockWithTransactions::new(
                    "block_hash-30",
                    BlockRange::LENGTH * 2,
                    SlotNumber(100),
                    vec!["tx_hash-1"],
                ),
                CardanoBlockWithTransactions::new(
                    "block_hash-48",
                    BlockRange::LENGTH * 3 + 3,
                    SlotNumber(101),
                    vec!["tx_hash-2"],
                ),
            ];
            let additional_blocks_with_txs = vec![CardanoBlockWithTransactions::new(
                "block_hash-50",
                BlockRange::LENGTH * 3 + 5,
                SlotNumber(102),
                vec!["tx_hash-3"],
            )];
            let transactions: Vec<_> = into_transactions(&initial_blocks_with_txs);
            // Transaction in a complete range
            let transactions_to_prove = filter_items_for_indices(&[1], &transactions);
            let beacon_to_prove = BlockRange::LENGTH * 3 + 3;
            // Partial block ranges are not pre-computed and stored in the database
            let stored_block_ranges_max_end = BlockRange::LENGTH * 3;

            let repository = Arc::new(
                InMemoryChainDataStore::builder()
                    .with_blocks_and_transactions(&initial_blocks_with_txs)
                    .compute_block_ranges(stored_block_ranges_max_end)
                    .await
                    .build(),
            );
            let mk_map_pool_size = 1;
            let prover = MithrilProverService::<MKTreeStoreInMemory>::new(
                repository.clone(),
                repository.clone(),
                mk_map_pool_size,
                TestLogger::stdout(),
            );
            prover.compute_cache(beacon_to_prove).await.unwrap();
            repository
                .store_blocks_and_transactions(additional_blocks_with_txs.clone())
                .await
                .unwrap();

            let transactions_set_proof = prover
                .compute_transactions_proofs(
                    beacon_to_prove,
                    &transactions_hashes(&transactions_to_prove),
                )
                .await
                .unwrap()
                .unwrap();

            assert_eq!(transactions_set_proof.transactions(), transactions_to_prove);
            transactions_set_proof.verify().unwrap();
            let expected_merkle_root = compute_certificate_merkle_root(
                &[initial_blocks_with_txs, additional_blocks_with_txs].concat(),
                beacon_to_prove,
            );
            assert_eq!(transactions_set_proof.merkle_root(), expected_merkle_root)
        }

        // Note: In that case the last block range will be retrieved from the cached merkle map and not substituted
        #[tokio::test]
        async fn compute_proof_when_last_block_range_is_partial_for_transactions_only_in_complete_ranges()
         {
            let blocks_with_txs = vec![
                CardanoBlockWithTransactions::new(
                    "block_hash-30",
                    BlockRange::LENGTH * 2,
                    SlotNumber(100),
                    vec!["tx_hash-1"],
                ),
                CardanoBlockWithTransactions::new(
                    "block_hash-48",
                    BlockRange::LENGTH * 3 + 3,
                    SlotNumber(100),
                    vec!["tx_hash-2"],
                ),
            ];
            let transactions: Vec<_> = into_transactions(&blocks_with_txs);
            let transactions_to_prove = filter_items_for_indices(&[1], &transactions);
            let beacon_to_prove = BlockRange::LENGTH * 3 + 3;
            // Partial block ranges are not pre-computed and stored in the database
            let stored_block_ranges_max_end = BlockRange::LENGTH * 3;

            let prover = setup_prover_for_test::<MKTreeStoreInMemory>(
                &blocks_with_txs,
                stored_block_ranges_max_end,
            )
            .await;
            prover.compute_cache(beacon_to_prove).await.unwrap();

            let transactions_set_proof = prover
                .compute_transactions_proofs(
                    beacon_to_prove,
                    &transactions_hashes(&transactions_to_prove),
                )
                .await
                .unwrap()
                .unwrap();

            assert_eq!(transactions_set_proof.transactions(), transactions_to_prove);
            transactions_set_proof.verify().unwrap();
            let expected_merkle_root =
                compute_certificate_merkle_root(&blocks_with_txs, beacon_to_prove);
            assert_eq!(transactions_set_proof.merkle_root(), expected_merkle_root)
        }

        // Note: in that case the last and partial range will be recomputed and substituted to its value in the cached merkle map
        #[tokio::test]
        async fn compute_proof_when_last_block_range_is_partial_for_at_least_one_transaction_the_last_and_partial_range()
         {
            let blocks_with_txs = vec![
                CardanoBlockWithTransactions::new(
                    "block_hash-30",
                    BlockRange::LENGTH * 2,
                    SlotNumber(100),
                    vec!["tx_hash-1"],
                ),
                CardanoBlockWithTransactions::new(
                    "block_hash-48",
                    BlockRange::LENGTH * 3 + 3,
                    SlotNumber(100),
                    vec!["tx_hash-2"],
                ),
            ];
            let transactions: Vec<_> = into_transactions(&blocks_with_txs);
            let transactions_to_prove = filter_items_for_indices(&[0], &transactions);
            let beacon_to_prove = BlockRange::LENGTH * 3 + 3;
            // Partial block ranges are not pre-computed and stored in the database
            let stored_block_ranges_max_end = BlockRange::LENGTH * 3;

            let prover = setup_prover_for_test::<MKTreeStoreInMemory>(
                &blocks_with_txs,
                stored_block_ranges_max_end,
            )
            .await;
            prover.compute_cache(beacon_to_prove).await.unwrap();

            let transactions_set_proof = prover
                .compute_transactions_proofs(
                    beacon_to_prove,
                    &transactions_hashes(&transactions_to_prove),
                )
                .await
                .unwrap()
                .unwrap();

            assert_eq!(transactions_set_proof.transactions(), transactions_to_prove);
            transactions_set_proof.verify().unwrap();
            let expected_merkle_root =
                compute_certificate_merkle_root(&blocks_with_txs, beacon_to_prove);
            assert_eq!(transactions_set_proof.merkle_root(), expected_merkle_root)
        }
    }

    mod blocks_proof {
        use super::*;

        #[tokio::test]
        async fn compute_proof_for_one_set_of_three_certified_blocks() {
            let blocks_with_txs = CardanoTransactionsBuilder::new()
                .max_transactions_per_block(1)
                .blocks_per_block_range(3)
                .build_blocks_for_block_ranges(5);
            let blocks = into_blocks(&blocks_with_txs);
            let blocks_to_prove = filter_items_for_indices(&[1, 2, 4], &blocks);
            let beacon = blocks_with_txs.last().unwrap().block_number;

            let prover =
                setup_prover_for_test::<MKTreeStoreInMemory>(&blocks_with_txs, beacon).await;
            prover.compute_cache(beacon).await.unwrap();

            let blocks_set_proof = prover
                .compute_blocks_proofs(beacon, &blocks_hashes(&blocks_to_prove))
                .await
                .unwrap()
                .unwrap();

            assert_eq!(blocks_set_proof.blocks(), blocks_to_prove);
            blocks_set_proof.verify().unwrap();
            let expected_merkle_root = compute_certificate_merkle_root(&blocks_with_txs, beacon);
            assert_eq!(blocks_set_proof.merkle_root(), expected_merkle_root)
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
                setup_prover_for_test::<MKTreeStoreInMemory>(&blocks_with_txs, beacon).await;
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
                setup_prover_for_test::<MKTreeStoreInMemory>(&blocks_with_txs, beacon).await;
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
            let beacon = blocks_with_txs[3 * 3].block_number - 1;
            let blocks_to_prove = filter_items_for_indices(&[1, 4, 8], &blocks);

            let prover =
                setup_prover_for_test::<MKTreeStoreInMemory>(&blocks_with_txs, beacon).await;
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
            let expected_merkle_root = compute_certificate_merkle_root(&blocks_with_txs, beacon);
            assert_eq!(blocks_set_proof.merkle_root(), expected_merkle_root)
        }
    }

    mod transactions_proof {
        use super::*;

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
                setup_prover_for_test::<MKTreeStoreInMemory>(&blocks_with_txs, beacon).await;
            prover.compute_cache(beacon).await.unwrap();

            let transactions_set_proof = prover
                .compute_transactions_proofs(beacon, &transactions_hashes(&transactions_to_prove))
                .await
                .unwrap()
                .unwrap();

            assert_eq!(transactions_set_proof.transactions(), transactions_to_prove);
            transactions_set_proof.verify().unwrap();
            let expected_merkle_root = compute_certificate_merkle_root(&blocks_with_txs, beacon);
            assert_eq!(transactions_set_proof.merkle_root(), expected_merkle_root)
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
                setup_prover_for_test::<MKTreeStoreInMemory>(&blocks_with_txs, beacon).await;
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
                setup_prover_for_test::<MKTreeStoreInMemory>(&blocks_with_txs, beacon).await;
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
            let beacon = blocks_with_txs[3 * 3].block_number - 1;
            let transactions_to_prove = filter_items_for_indices(&[1, 4, 8], &transactions);

            let prover =
                setup_prover_for_test::<MKTreeStoreInMemory>(&blocks_with_txs, beacon).await;
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
            let expected_merkle_root = compute_certificate_merkle_root(&blocks_with_txs, beacon);
            assert_eq!(transactions_set_proof.merkle_root(), expected_merkle_root)
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
                    mock.expect_get_all_mk_nodes_by_ranges_of_block_numbers()
                        .returning(|_| Err(anyhow!("fail")));
                    mock.expect_get_block_by_hashes()
                        .returning(|_, _| Err(anyhow!("fail")));
                    mock.expect_get_transactions_by_hashes()
                        .returning(|_, _| Err(anyhow!("fail")));
                }),
            ..setup_prover_for_test::<MKTreeStoreInMemory>(&blocks_with_txs, beacon).await
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
            ..setup_prover_for_test::<MKTreeStoreInMemory>(&blocks_with_txs, beacon).await
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

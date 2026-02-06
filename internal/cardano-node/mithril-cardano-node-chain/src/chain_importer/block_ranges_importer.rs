use std::mem;
use std::sync::Arc;

use slog::{Logger, debug};

use mithril_common::StdResult;
use mithril_common::crypto_helper::{MKTree, MKTreeNode, MKTreeStoreInMemory};
use mithril_common::entities::{BlockNumber, BlockRange};

use crate::chain_importer::ChainDataStore;

/// Internal component responsible for computing and storing block range roots
#[derive(Clone)]
pub(crate) struct BlockRangeImporter {
    transaction_store: Arc<dyn ChainDataStore>,
    logger: Logger,
}

impl BlockRangeImporter {
    /// Constructor
    pub fn new(transaction_store: Arc<dyn ChainDataStore>, logger: Logger) -> Self {
        Self {
            transaction_store,
            logger,
        }
    }

    pub async fn run(&self, until: BlockNumber) -> StdResult<()> {
        let block_ranges = match self.transaction_store.get_highest_legacy_block_range().await?.map(
            |highest_stored_block_range| {
                BlockRange::all_block_ranges_in(
                    BlockRange::start(highest_stored_block_range.end)..=(until),
                )
            },
        ) {
            // No block range root stored yet, start from the beginning
            None => BlockRange::all_block_ranges_in(BlockNumber(0)..=(until)),
            // Not enough block to form at least one block range
            Some(ranges) if ranges.is_empty() => return Ok(()),
            Some(ranges) => ranges,
        };

        debug!(
            self.logger, "Computing Legacy Block Range Roots";
            "start_block" => *block_ranges.start(), "end_block" => *block_ranges.end(),
        );

        let mut block_ranges_with_merkle_root: Vec<(BlockRange, MKTreeNode)> = vec![];
        for block_range in block_ranges {
            let transactions = self
                .transaction_store
                .get_transactions_in_range(block_range.start..block_range.end)
                .await?;

            if transactions.is_empty() {
                continue;
            }

            let merkle_root =
                MKTree::<MKTreeStoreInMemory>::new_from_iter(transactions)?.compute_root()?;
            block_ranges_with_merkle_root.push((block_range, merkle_root));

            if block_ranges_with_merkle_root.len() >= 100 {
                let block_ranges_with_merkle_root_save =
                    mem::take(&mut block_ranges_with_merkle_root);
                self.transaction_store
                    .store_legacy_block_range_roots(block_ranges_with_merkle_root_save)
                    .await?;
            }
        }

        self.transaction_store
            .store_legacy_block_range_roots(block_ranges_with_merkle_root)
            .await
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Range;

    use mithril_common::entities::{BlockRangesSequence, CardanoTransaction, SlotNumber};

    use crate::chain_importer::MockChainDataStore;
    use crate::entities::ScannedBlock;
    use crate::test::TestLogger;
    use crate::test::double::InMemoryChainDataStore;

    use super::*;

    pub fn build_blocks(
        start_block_number: BlockNumber,
        number_of_consecutive_block: BlockNumber,
    ) -> Vec<ScannedBlock> {
        (*start_block_number..*(start_block_number + number_of_consecutive_block))
            .map(|block_number| {
                ScannedBlock::new(
                    format!("block_hash-{block_number}"),
                    BlockNumber(block_number),
                    SlotNumber(block_number * 100),
                    vec![format!("tx_hash-{}", block_number)],
                )
            })
            .collect()
    }

    pub fn merkle_root_for_blocks(block_ranges: &[ScannedBlock]) -> MKTreeNode {
        let tx: Vec<_> = block_ranges
            .iter()
            .flat_map(|br| br.clone().into_transactions())
            .collect();
        MKTree::<MKTreeStoreInMemory>::new(&tx)
            .unwrap()
            .compute_root()
            .unwrap()
    }

    #[tokio::test]
    async fn if_nothing_stored_parse_and_store_all_block_ranges() {
        let up_to_block_number = BlockRange::LENGTH * 5;
        let blocks = build_blocks(BlockNumber(0), up_to_block_number + 1);
        let repository = Arc::new(
            InMemoryChainDataStore::builder()
                .with_blocks_and_transactions(&blocks)
                .build(),
        );

        let importer = BlockRangeImporter::new(repository.clone(), TestLogger::stdout());

        importer.run(up_to_block_number).await.unwrap();

        assert_eq!(
            vec![
                BlockRange::from_block_number(BlockNumber(0)),
                BlockRange::from_block_number(BlockRange::LENGTH),
                BlockRange::from_block_number(BlockRange::LENGTH * 2),
                BlockRange::from_block_number(BlockRange::LENGTH * 3),
                BlockRange::from_block_number(BlockRange::LENGTH * 4),
            ],
            repository.get_all_legacy_block_ranges().await
        );
    }

    #[tokio::test]
    async fn if_theres_gap_between_two_stored_block_ranges_it_can_still_compute_their_root() {
        let up_to_block_number = BlockRange::LENGTH * 4;
        // Two block ranges with a gap
        let blocks: Vec<ScannedBlock> = [
            build_blocks(BlockNumber(0), BlockRange::LENGTH),
            build_blocks(BlockRange::LENGTH * 3, BlockRange::LENGTH),
        ]
        .concat();
        let repository = Arc::new(
            InMemoryChainDataStore::builder()
                .with_blocks_and_transactions(&blocks)
                .build(),
        );

        let importer = BlockRangeImporter::new(repository.clone(), TestLogger::stdout());

        importer.run(up_to_block_number).await.unwrap();

        assert_eq!(
            vec![
                BlockRange::from_block_number(BlockNumber(0)),
                BlockRange::from_block_number(BlockRange::LENGTH * 3),
            ],
            repository.get_all_legacy_block_ranges().await
        );
    }

    #[tokio::test]
    async fn if_all_block_ranges_computed_nothing_computed_and_stored() {
        let repository = Arc::new(InMemoryChainDataStore::default());

        let importer = BlockRangeImporter::new(repository.clone(), TestLogger::stdout());

        importer.run(BlockNumber(10_000)).await.unwrap();

        let block_range_roots = repository.get_all_legacy_block_range_root().await;
        assert!(
            block_range_roots.is_empty(),
            "No block range root should be stored, found: {block_range_roots:?}"
        );
    }

    #[tokio::test]
    async fn if_half_block_ranges_are_stored_the_other_half_is_computed_and_stored() {
        let up_to_block_number = BlockRange::LENGTH * 4;
        let blocks = build_blocks(BlockNumber(0), up_to_block_number + 1);
        let repository = Arc::new(
            InMemoryChainDataStore::builder()
                .with_blocks_and_transactions(&blocks)
                .with_legacy_block_range_roots(&[
                    (
                        BlockRange::from_block_number(BlockNumber(0)),
                        MKTreeNode::from_hex("AAAA").unwrap(),
                    ),
                    (
                        BlockRange::from_block_number(BlockRange::LENGTH),
                        MKTreeNode::from_hex("BBBB").unwrap(),
                    ),
                ])
                .build(),
        );

        let importer = BlockRangeImporter::new(repository.clone(), TestLogger::stdout());

        importer.run(up_to_block_number).await.unwrap();

        assert_eq!(
            vec![
                BlockRange::from_block_number(BlockNumber(0)),
                BlockRange::from_block_number(BlockRange::LENGTH),
                BlockRange::from_block_number(BlockRange::LENGTH * 2),
                BlockRange::from_block_number(BlockRange::LENGTH * 3),
            ],
            repository.get_all_legacy_block_ranges().await
        );
    }

    #[tokio::test]
    async fn can_compute_block_ranges_up_to_the_strict_end_of_a_block_range() {
        // Transactions for all blocks in the (15..=29) interval
        let blocks = build_blocks(BlockRange::LENGTH, BlockRange::LENGTH - 1);
        let repository = Arc::new(
            InMemoryChainDataStore::builder()
                .with_blocks_and_transactions(&blocks)
                .build(),
        );

        let importer = BlockRangeImporter::new(repository.clone(), TestLogger::stdout());

        importer.run(BlockRange::LENGTH * 2 - 1).await.unwrap();

        assert_eq!(
            vec![BlockRange::from_block_number(BlockRange::LENGTH)],
            repository.get_all_legacy_block_ranges().await
        );
    }

    #[tokio::test]
    async fn can_compute_block_ranges_even_if_last_blocks_in_range_dont_have_transactions() {
        // For the block range (15..=29) we only have transactions in the 10 first blocks (15..=24)
        let blocks = build_blocks(BlockRange::LENGTH, BlockNumber(10));
        let repository = Arc::new(
            InMemoryChainDataStore::builder()
                .with_blocks_and_transactions(&blocks)
                .build(),
        );

        let importer = BlockRangeImporter::new(repository.clone(), TestLogger::stdout());

        importer.run(BlockRange::LENGTH * 2).await.unwrap();

        assert_eq!(
            vec![BlockRange::from_block_number(BlockRange::LENGTH)],
            repository.get_all_legacy_block_ranges().await
        );
    }

    #[tokio::test]
    async fn block_range_root_retrieves_only_strictly_required_transactions() {
        fn transactions_for_block(range: Range<BlockNumber>) -> StdResult<Vec<CardanoTransaction>> {
            Ok(build_blocks(range.start, range.end - range.start)
                .into_iter()
                .flat_map(|b| b.into_transactions())
                .collect())
        }
        const HIGHEST_BLOCK_RANGE_START: BlockNumber = BlockRange::LENGTH;
        let up_to_block_number = BlockRange::LENGTH * 5;

        let importer = {
            let mut store_mock = MockChainDataStore::new();
            store_mock
                .expect_get_highest_legacy_block_range()
                .returning(|| {
                    Ok(Some(BlockRange::from_block_number(
                        HIGHEST_BLOCK_RANGE_START,
                    )))
                })
                .once();
            store_mock
                .expect_get_transactions_in_range()
                // Lower bound should be the end block number of the last known block range
                // Upper bound should be the block number provided to `import_block_ranges`
                .withf(move |range| {
                    BlockRangesSequence::new(HIGHEST_BLOCK_RANGE_START..=up_to_block_number)
                        .contains(range)
                })
                .returning(transactions_for_block);
            store_mock
                .expect_store_legacy_block_range_roots()
                .returning(|_| Ok(()));

            BlockRangeImporter::new(Arc::new(store_mock), TestLogger::stdout())
        };

        importer.run(up_to_block_number).await.unwrap();
    }

    #[tokio::test]
    async fn compute_block_range_merkle_root() {
        // 2 block ranges worth of blocks with one more block that should be ignored for merkle root computation
        let up_to_block_number = BlockRange::LENGTH * 2;
        let blocks = build_blocks(BlockNumber(0), up_to_block_number + 1);
        let expected_block_range_roots = vec![
            (
                BlockRange::from_block_number(BlockNumber(0)),
                merkle_root_for_blocks(&blocks[0..(*BlockRange::LENGTH as usize)]),
            ),
            (
                BlockRange::from_block_number(BlockRange::LENGTH),
                merkle_root_for_blocks(
                    &blocks[(*BlockRange::LENGTH as usize)..((*BlockRange::LENGTH * 2) as usize)],
                ),
            ),
        ];

        let repository = Arc::new(
            InMemoryChainDataStore::builder()
                .with_blocks_and_transactions(&blocks)
                .build(),
        );

        let importer = BlockRangeImporter::new(repository.clone(), TestLogger::stdout());

        importer.run(up_to_block_number).await.unwrap();

        let block_range_roots = repository.get_all_legacy_block_range_root().await;
        assert_eq!(
            expected_block_range_roots,
            block_range_roots
                .into_iter()
                .map(|r| (r.range, r.merkle_root))
                .collect::<Vec<_>>()
        );
    }
}

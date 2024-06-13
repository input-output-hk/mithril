use std::mem;
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use async_trait::async_trait;
use slog::{debug, Logger};

use mithril_common::cardano_block_scanner::{BlockScanner, ChainScannedBlocks};
use mithril_common::crypto_helper::{MKTree, MKTreeNode};
use mithril_common::entities::{BlockNumber, BlockRange, CardanoTransaction, ChainPoint};
use mithril_common::signable_builder::TransactionsImporter;
use mithril_common::StdResult;

/// Cardano transactions store
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait TransactionStore: Send + Sync {
    /// Get the highest known transaction beacon
    async fn get_highest_beacon(&self) -> StdResult<Option<ChainPoint>>;

    /// Store list of transactions
    async fn store_transactions(&self, transactions: Vec<CardanoTransaction>) -> StdResult<()>;

    /// Get the interval of blocks whose merkle root has yet to be computed
    async fn get_block_interval_without_block_range_root(
        &self,
    ) -> StdResult<Option<Range<BlockNumber>>>;

    /// Get transactions in an interval of blocks
    async fn get_transactions_in_range(
        &self,
        range: Range<BlockNumber>,
    ) -> StdResult<Vec<CardanoTransaction>>;

    /// Store list of block ranges with their corresponding merkle root
    async fn store_block_range_roots(
        &self,
        block_ranges: Vec<(BlockRange, MKTreeNode)>,
    ) -> StdResult<()>;

    /// Remove transactions and block range roots that are in a rolled-back fork
    ///
    /// * Remove transactions with block number strictly greater than the given block number
    /// * Remove block range roots that have lower bound range strictly above the given block number
    async fn remove_rolled_back_transactions_and_block_range(
        &self,
        block_number: BlockNumber,
    ) -> StdResult<()>;
}

/// Import and store [CardanoTransaction].
pub struct CardanoTransactionsImporter {
    block_scanner: Arc<dyn BlockScanner>,
    transaction_store: Arc<dyn TransactionStore>,
    logger: Logger,
    dirpath: PathBuf,
}

impl CardanoTransactionsImporter {
    /// Constructor
    ///
    /// About `rescan_offset`: if Some(x) the importer will be asked to rescan the previous 'x'
    /// immutables starting after the highest immutable known in the store.
    /// This is useful when one of the last immutable was not full scanned.
    pub fn new(
        block_scanner: Arc<dyn BlockScanner>,
        transaction_store: Arc<dyn TransactionStore>,
        dirpath: &Path,
        logger: Logger,
    ) -> Self {
        Self {
            block_scanner,
            transaction_store,
            logger,
            dirpath: dirpath.to_owned(),
        }
    }

    async fn import_transactions(&self, up_to_beacon: BlockNumber) -> StdResult<()> {
        let from = self.transaction_store.get_highest_beacon().await?;
        self.parse_and_store_transactions_not_imported_yet(from, up_to_beacon)
            .await
    }

    async fn parse_and_store_transactions_not_imported_yet(
        &self,
        from: Option<ChainPoint>,
        until: BlockNumber,
    ) -> StdResult<()> {
        if from.as_ref().is_some_and(|f| f.block_number >= until) {
            debug!(
                self.logger,
                "TransactionsImporter does not need to retrieve Cardano transactions, the database is up to date for block_number '{until}'",
            );
            return Ok(());
        }
        debug!(
            self.logger,
            "TransactionsImporter will retrieve Cardano transactions between block_number '{}' and '{until}'",
            from.as_ref().map(|c|c.block_number).unwrap_or(0)
        );

        let mut streamer = self.block_scanner.scan(&self.dirpath, from, until).await?;

        while let Some(blocks) = streamer.poll_next().await? {
            match blocks {
                ChainScannedBlocks::RollForwards(forward_blocks) => {
                    let parsed_transactions: Vec<CardanoTransaction> = forward_blocks
                        .into_iter()
                        .flat_map(|b| b.into_transactions())
                        .collect();

                    self.transaction_store
                        .store_transactions(parsed_transactions)
                        .await?;
                }
                ChainScannedBlocks::RollBackward(chain_point) => {
                    self.transaction_store
                        .remove_rolled_back_transactions_and_block_range(chain_point.block_number)
                        .await?;
                }
            }
        }

        Ok(())
    }

    async fn import_block_ranges(&self) -> StdResult<()> {
        let block_ranges = match self
            .transaction_store
            .get_block_interval_without_block_range_root()
            .await?
            .map(|range| BlockRange::all_block_ranges_in(BlockRange::start(range.start)..range.end))
        {
            // Everything is already computed
            None => return Ok(()),
            // Not enough block to form at least one block range
            Some(ranges) if ranges.is_empty() => return Ok(()),
            Some(ranges) => ranges,
        };

        debug!(
            self.logger, "TransactionsImporter - computing Block Range Roots";
            "start_block" => block_ranges.start(), "end_block" => block_ranges.end(),
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

            let merkle_root = MKTree::new(&transactions)?.compute_root()?;
            block_ranges_with_merkle_root.push((block_range, merkle_root));

            if block_ranges_with_merkle_root.len() >= 100 {
                let block_ranges_with_merkle_root_save =
                    mem::take(&mut block_ranges_with_merkle_root);
                self.transaction_store
                    .store_block_range_roots(block_ranges_with_merkle_root_save)
                    .await?;
            }
        }

        self.transaction_store
            .store_block_range_roots(block_ranges_with_merkle_root)
            .await
    }
}

#[async_trait]
impl TransactionsImporter for CardanoTransactionsImporter {
    async fn import(&self, up_to_beacon: BlockNumber) -> StdResult<()> {
        self.import_transactions(up_to_beacon).await?;
        self.import_block_ranges().await
    }
}

#[cfg(test)]
mod tests {
    use mockall::mock;

    use mithril_common::cardano_block_scanner::{
        BlockStreamer, DumbBlockScanner, DumbBlockStreamer, ScannedBlock,
    };
    use mithril_common::crypto_helper::MKTree;
    use mithril_common::entities::{BlockNumber, BlockRangesSequence};
    use mithril_persistence::database::repository::CardanoTransactionRepository;

    use crate::database::test_helper::cardano_tx_db_connection;

    use super::*;

    mock! {
        pub BlockScannerImpl { }

        #[async_trait]
        impl BlockScanner for BlockScannerImpl {
            async fn scan(
              &self,
              dirpath: &Path,
              from: Option<ChainPoint>,
              until: BlockNumber,
            ) -> StdResult<Box<dyn BlockStreamer>>;
        }
    }

    impl CardanoTransactionsImporter {
        pub fn new_for_test(
            scanner: Arc<dyn BlockScanner>,
            transaction_store: Arc<dyn TransactionStore>,
        ) -> Self {
            CardanoTransactionsImporter::new(
                scanner,
                transaction_store,
                Path::new(""),
                crate::test_tools::logger_for_tests(),
            )
        }
    }

    fn build_blocks(
        start_block_number: BlockNumber,
        number_of_consecutive_block: BlockNumber,
    ) -> Vec<ScannedBlock> {
        (start_block_number..(start_block_number + number_of_consecutive_block))
            .map(|block_number| {
                ScannedBlock::new(
                    format!("block_hash-{}", block_number),
                    block_number,
                    block_number * 100,
                    block_number * 10,
                    vec![format!("tx_hash-{}", block_number)],
                )
            })
            .collect()
    }

    fn into_transactions(blocks: &[ScannedBlock]) -> Vec<CardanoTransaction> {
        blocks
            .iter()
            .flat_map(|b| b.clone().into_transactions())
            .collect()
    }

    fn merkle_root_for_blocks(block_ranges: &[ScannedBlock]) -> MKTreeNode {
        let tx: Vec<_> = block_ranges
            .iter()
            .flat_map(|br| br.clone().into_transactions())
            .collect();
        MKTree::new(&tx).unwrap().compute_root().unwrap()
    }

    #[tokio::test]
    async fn if_nothing_stored_parse_and_store_all_transactions() {
        let connection = cardano_tx_db_connection().unwrap();
        let repository = Arc::new(CardanoTransactionRepository::new(Arc::new(connection)));

        let blocks = vec![
            ScannedBlock::new("block_hash-1", 10, 15, 11, vec!["tx_hash-1", "tx_hash-2"]),
            ScannedBlock::new("block_hash-2", 20, 25, 12, vec!["tx_hash-3", "tx_hash-4"]),
        ];
        let expected_transactions = into_transactions(&blocks);
        let up_to_block_number = 1000;

        let importer = {
            let mut scanner_mock = MockBlockScannerImpl::new();
            scanner_mock
                .expect_scan()
                .withf(move |_, from, until| from.is_none() && until == &up_to_block_number)
                .return_once(move |_, _, _| {
                    Ok(Box::new(DumbBlockStreamer::new().forwards(vec![blocks])))
                });
            CardanoTransactionsImporter::new_for_test(Arc::new(scanner_mock), repository.clone())
        };

        importer
            .import_transactions(up_to_block_number)
            .await
            .expect("Transactions Importer should succeed");

        let stored_transactions = repository.get_all().await.unwrap();
        assert_eq!(expected_transactions, stored_transactions);
    }

    #[tokio::test]
    async fn if_nothing_stored_parse_and_store_all_block_ranges() {
        let connection = cardano_tx_db_connection().unwrap();
        let repository = Arc::new(CardanoTransactionRepository::new(Arc::new(connection)));

        let blocks = build_blocks(0, BlockRange::LENGTH * 5 + 1);
        let transactions = into_transactions(&blocks);
        repository.store_transactions(transactions).await.unwrap();

        let importer = CardanoTransactionsImporter::new_for_test(
            Arc::new(MockBlockScannerImpl::new()),
            repository.clone(),
        );

        importer
            .import_block_ranges()
            .await
            .expect("Transactions Importer should succeed");

        let block_range_roots = repository.get_all_block_range_root().unwrap();
        assert_eq!(
            vec![
                BlockRange::from_block_number(0),
                BlockRange::from_block_number(BlockRange::LENGTH),
                BlockRange::from_block_number(BlockRange::LENGTH * 2),
                BlockRange::from_block_number(BlockRange::LENGTH * 3),
                BlockRange::from_block_number(BlockRange::LENGTH * 4),
            ],
            block_range_roots
                .into_iter()
                .map(|r| r.range)
                .collect::<Vec<_>>()
        );
    }

    #[tokio::test]
    async fn if_theres_gap_between_two_stored_block_ranges_it_can_still_compute_their_root() {
        let connection = cardano_tx_db_connection().unwrap();
        let repository = Arc::new(CardanoTransactionRepository::new(Arc::new(connection)));

        // Two block ranges with a gap
        let blocks: Vec<ScannedBlock> = [
            build_blocks(0, BlockRange::LENGTH),
            build_blocks(BlockRange::LENGTH * 3, BlockRange::LENGTH),
        ]
        .concat();
        let transactions = into_transactions(&blocks);
        repository.store_transactions(transactions).await.unwrap();

        let importer = CardanoTransactionsImporter::new_for_test(
            Arc::new(MockBlockScannerImpl::new()),
            repository.clone(),
        );

        importer
            .import_block_ranges()
            .await
            .expect("Transactions Importer should succeed");

        let block_range_roots = repository.get_all_block_range_root().unwrap();
        assert_eq!(
            vec![
                BlockRange::from_block_number(0),
                BlockRange::from_block_number(BlockRange::LENGTH * 3),
            ],
            block_range_roots
                .into_iter()
                .map(|r| r.range)
                .collect::<Vec<_>>()
        );
    }

    #[tokio::test]
    async fn if_all_block_ranges_computed_nothing_computed_and_stored() {
        let connection = cardano_tx_db_connection().unwrap();
        let repository = Arc::new(CardanoTransactionRepository::new(Arc::new(connection)));

        let importer = CardanoTransactionsImporter::new_for_test(
            Arc::new(MockBlockScannerImpl::new()),
            repository.clone(),
        );

        importer
            .import_block_ranges()
            .await
            .expect("Transactions Importer should succeed");

        let block_range_roots = repository.get_all_block_range_root().unwrap();
        assert!(
            block_range_roots.is_empty(),
            "No block range root should be stored, found: {block_range_roots:?}"
        );
    }

    #[tokio::test]
    async fn if_all_transactions_stored_nothing_is_parsed_and_stored() {
        let up_to_block_number = 12;
        let connection = cardano_tx_db_connection().unwrap();
        let repository = Arc::new(CardanoTransactionRepository::new(Arc::new(connection)));
        let scanner = DumbBlockScanner::new().forwards(vec![vec![
            ScannedBlock::new("block_hash-1", 10, 15, 10, vec!["tx_hash-1", "tx_hash-2"]),
            ScannedBlock::new("block_hash-2", 20, 25, 11, vec!["tx_hash-3", "tx_hash-4"]),
        ]]);

        let last_tx = CardanoTransaction::new("tx-20", 30, 35, "block_hash-3", up_to_block_number);
        repository
            .store_transactions(vec![last_tx.clone()])
            .await
            .unwrap();

        let importer =
            CardanoTransactionsImporter::new_for_test(Arc::new(scanner), repository.clone());

        importer
            .import_transactions(up_to_block_number)
            .await
            .expect("Transactions Importer should succeed");

        let transactions = repository.get_all().await.unwrap();
        assert_eq!(vec![last_tx], transactions);
    }

    #[tokio::test]
    async fn if_half_transactions_are_already_stored_the_other_half_is_parsed_and_stored() {
        let connection = cardano_tx_db_connection().unwrap();
        let repository = Arc::new(CardanoTransactionRepository::new(Arc::new(connection)));

        let highest_stored_chain_point = ChainPoint::new(134, 10, "block_hash-1");
        let stored_block = ScannedBlock::new(
            highest_stored_chain_point.block_hash.clone(),
            highest_stored_chain_point.block_number,
            highest_stored_chain_point.slot_number,
            5,
            vec!["tx_hash-1", "tx_hash-2"],
        );
        let to_store_block =
            ScannedBlock::new("block_hash-2", 20, 229, 8, vec!["tx_hash-3", "tx_hash-4"]);
        let expected_transactions: Vec<CardanoTransaction> = [
            stored_block.clone().into_transactions(),
            to_store_block.clone().into_transactions(),
        ]
        .concat();
        let up_to_block_number = 22;

        repository
            .store_transactions(stored_block.clone().into_transactions())
            .await
            .unwrap();

        let importer = {
            let scanned_blocks = vec![to_store_block.clone()];
            let mut scanner_mock = MockBlockScannerImpl::new();
            scanner_mock
                .expect_scan()
                .withf(move |_, from, until| {
                    from == &Some(highest_stored_chain_point.clone())
                        && *until == up_to_block_number
                })
                .return_once(move |_, _, _| {
                    Ok(Box::new(
                        DumbBlockStreamer::new().forwards(vec![scanned_blocks]),
                    ))
                })
                .once();
            CardanoTransactionsImporter::new_for_test(Arc::new(scanner_mock), repository.clone())
        };

        let stored_transactions = repository.get_all().await.unwrap();
        assert_eq!(stored_block.into_transactions(), stored_transactions);

        importer
            .import_transactions(up_to_block_number)
            .await
            .expect("Transactions Importer should succeed");

        let stored_transactions = repository.get_all().await.unwrap();
        assert_eq!(expected_transactions, stored_transactions);
    }

    #[tokio::test]
    async fn if_half_block_ranges_are_stored_the_other_half_is_computed_and_stored() {
        let connection = cardano_tx_db_connection().unwrap();
        let repository = Arc::new(CardanoTransactionRepository::new(Arc::new(connection)));

        let blocks = build_blocks(0, BlockRange::LENGTH * 4 + 1);
        let transactions = into_transactions(&blocks);
        repository.store_transactions(transactions).await.unwrap();
        repository
            .store_block_range_roots(
                blocks[0..((BlockRange::LENGTH * 2) as usize)]
                    .iter()
                    .map(|b| {
                        (
                            BlockRange::from_block_number(b.block_number),
                            MKTreeNode::from_hex("AAAA").unwrap(),
                        )
                    })
                    .collect(),
            )
            .await
            .unwrap();

        let importer = CardanoTransactionsImporter::new_for_test(
            Arc::new(MockBlockScannerImpl::new()),
            repository.clone(),
        );

        let block_range_roots = repository.get_all_block_range_root().unwrap();
        assert_eq!(
            vec![
                BlockRange::from_block_number(0),
                BlockRange::from_block_number(BlockRange::LENGTH),
            ],
            block_range_roots
                .into_iter()
                .map(|r| r.range)
                .collect::<Vec<_>>()
        );

        importer
            .import_block_ranges()
            .await
            .expect("Transactions Importer should succeed");

        let block_range_roots = repository.get_all_block_range_root().unwrap();
        assert_eq!(
            vec![
                BlockRange::from_block_number(0),
                BlockRange::from_block_number(BlockRange::LENGTH),
                BlockRange::from_block_number(BlockRange::LENGTH * 2),
                BlockRange::from_block_number(BlockRange::LENGTH * 3),
            ],
            block_range_roots
                .into_iter()
                .map(|r| r.range)
                .collect::<Vec<_>>()
        );
    }

    #[tokio::test]
    async fn block_range_root_retrieves_only_strictly_required_transactions() {
        fn transactions_for_block(range: Range<BlockNumber>) -> StdResult<Vec<CardanoTransaction>> {
            Ok(build_blocks(range.start, range.count() as BlockNumber)
                .iter()
                .flat_map(|b| b.clone().into_transactions())
                .collect())
        }

        let importer = {
            let mut store_mock = MockTransactionStore::new();
            store_mock
                .expect_get_block_interval_without_block_range_root()
                // Specification of the interval without block range root
                // Note: in reality the lower bound will always be a multiple of BlockRange::LENGTH
                // since it's computed from the `block_range_root` table
                .returning(|| Ok(Some((BlockRange::LENGTH + 2)..(BlockRange::LENGTH * 5))))
                .once();
            store_mock
                .expect_get_transactions_in_range()
                // Lower bound should be the block number that start after the last known block range end
                //
                // if it's not a multiple of BlockRange::LENGTH, it should be the start block number
                // of the block range that contains the end of the last known block range.
                //
                // Upper bound should be the block number of the highest transaction in a db that can be
                // included in a block range
                .withf(|range| {
                    BlockRangesSequence::new(BlockRange::LENGTH..(BlockRange::LENGTH * 5))
                        .contains(range)
                })
                .returning(transactions_for_block);
            store_mock
                .expect_store_block_range_roots()
                .returning(|_| Ok(()));

            CardanoTransactionsImporter::new_for_test(
                Arc::new(MockBlockScannerImpl::new()),
                Arc::new(store_mock),
            )
        };

        importer
            .import_block_ranges()
            .await
            .expect("Transactions Importer should succeed");
    }

    #[tokio::test]
    async fn compute_block_range_merkle_root() {
        let connection = cardano_tx_db_connection().unwrap();
        let repository = Arc::new(CardanoTransactionRepository::new(Arc::new(connection)));

        // 2 block ranges worth of blocks with one more block that should be ignored for merkle root computation
        let blocks = build_blocks(0, BlockRange::LENGTH * 2 + 1);
        let transactions = into_transactions(&blocks);
        let expected_block_range_roots = vec![
            (
                BlockRange::from_block_number(0),
                merkle_root_for_blocks(&blocks[0..(BlockRange::LENGTH as usize)]),
            ),
            (
                BlockRange::from_block_number(BlockRange::LENGTH),
                merkle_root_for_blocks(
                    &blocks[(BlockRange::LENGTH as usize)..((BlockRange::LENGTH * 2) as usize)],
                ),
            ),
        ];

        repository.store_transactions(transactions).await.unwrap();

        let importer = CardanoTransactionsImporter::new_for_test(
            Arc::new(MockBlockScannerImpl::new()),
            repository.clone(),
        );

        importer
            .import_block_ranges()
            .await
            .expect("Transactions Importer should succeed");

        let block_range_roots = repository.get_all_block_range_root().unwrap();
        assert_eq!(
            expected_block_range_roots,
            block_range_roots
                .into_iter()
                .map(|br| br.into())
                .collect::<Vec<_>>()
        );
    }

    #[tokio::test]
    async fn importing_twice_starting_with_nothing_in_a_real_db_should_yield_transactions_in_same_order(
    ) {
        let blocks = vec![
            ScannedBlock::new("block_hash-1", 10, 15, 11, vec!["tx_hash-1", "tx_hash-2"]),
            ScannedBlock::new("block_hash-2", 20, 25, 12, vec!["tx_hash-3", "tx_hash-4"]),
        ];
        let up_to_block_number = 1000;
        let transactions = into_transactions(&blocks);

        let (importer, repository) = {
            let connection = Arc::new(cardano_tx_db_connection().unwrap());
            let repository = Arc::new(CardanoTransactionRepository::new(connection.clone()));
            let importer = CardanoTransactionsImporter::new_for_test(
                Arc::new(DumbBlockScanner::new().forwards(vec![blocks.clone()])),
                Arc::new(CardanoTransactionRepository::new(connection.clone())),
            );
            (importer, repository)
        };

        importer
            .import(up_to_block_number)
            .await
            .expect("Transactions Importer should succeed");
        let cold_imported_transactions = repository.get_all().await.unwrap();

        importer
            .import(up_to_block_number)
            .await
            .expect("Transactions Importer should succeed");
        let warm_imported_transactions = repository.get_all().await.unwrap();

        assert_eq!(transactions, cold_imported_transactions);
        assert_eq!(cold_imported_transactions, warm_imported_transactions);
    }

    #[tokio::test]
    async fn when_rollbackward_should_remove_transactions() {
        let connection = cardano_tx_db_connection().unwrap();
        let repository = Arc::new(CardanoTransactionRepository::new(Arc::new(connection)));

        let expected_remaining_transactions =
            ScannedBlock::new("block_hash-130", 130, 5, 1, vec!["tx_hash-6", "tx_hash-7"])
                .into_transactions();
        repository
            .store_transactions(expected_remaining_transactions.clone())
            .await
            .unwrap();
        repository
            .store_transactions(
                ScannedBlock::new(
                    "block_hash-131",
                    131,
                    10,
                    2,
                    vec!["tx_hash-8", "tx_hash-9", "tx_hash-10"],
                )
                .into_transactions(),
            )
            .await
            .unwrap();

        let chain_point = ChainPoint::new(1, 130, "block_hash-131");
        let scanner = DumbBlockScanner::new().backward(chain_point);

        let importer =
            CardanoTransactionsImporter::new_for_test(Arc::new(scanner), repository.clone());

        importer
            .import(3000)
            .await
            .expect("Transactions Importer should succeed");

        let stored_transactions = repository.get_all().await.unwrap();
        assert_eq!(expected_remaining_transactions, stored_transactions);
    }

    #[tokio::test]
    async fn when_rollbackward_should_remove_block_ranges() {
        let connection = cardano_tx_db_connection().unwrap();
        let repository = Arc::new(CardanoTransactionRepository::new(Arc::new(connection)));

        let expected_remaining_block_ranges = vec![
            BlockRange::from_block_number(0),
            BlockRange::from_block_number(BlockRange::LENGTH),
            BlockRange::from_block_number(BlockRange::LENGTH * 2),
        ];

        repository
            .store_block_range_roots(
                expected_remaining_block_ranges
                    .iter()
                    .map(|b| (b.clone(), MKTreeNode::from_hex("AAAA").unwrap()))
                    .collect(),
            )
            .await
            .unwrap();
        repository
            .store_block_range_roots(
                [
                    BlockRange::from_block_number(BlockRange::LENGTH * 3),
                    BlockRange::from_block_number(BlockRange::LENGTH * 4),
                    BlockRange::from_block_number(BlockRange::LENGTH * 5),
                ]
                .iter()
                .map(|b| (b.clone(), MKTreeNode::from_hex("AAAA").unwrap()))
                .collect(),
            )
            .await
            .unwrap();

        let block_range_roots = repository.get_all_block_range_root().unwrap();
        assert_eq!(6, block_range_roots.len());

        let chain_point = ChainPoint::new(1, BlockRange::LENGTH * 3, "block_hash-131");
        let scanner = DumbBlockScanner::new().backward(chain_point);

        let importer =
            CardanoTransactionsImporter::new_for_test(Arc::new(scanner), repository.clone());

        importer
            .import(3000)
            .await
            .expect("Transactions Importer should succeed");

        let block_range_roots = repository.get_all_block_range_root().unwrap();
        assert_eq!(
            expected_remaining_block_ranges,
            block_range_roots
                .into_iter()
                .map(|r| r.range)
                .collect::<Vec<_>>()
        );
    }
}

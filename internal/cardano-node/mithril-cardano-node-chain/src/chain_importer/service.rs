use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;
use slog::{Logger, debug};
use tokio::{runtime::Handle, sync::Mutex, task};

use mithril_common::StdResult;
use mithril_common::entities::{BlockNumber, CardanoBlockWithTransactions, ChainPoint};
use mithril_common::logging::LoggerExtensions;

use crate::chain_importer::block_ranges_importer::BlockRangeImporter;
use crate::chain_importer::{ChainDataImporter, ChainDataStore};
use crate::chain_scanner::{BlockScanner, ChainScannedBlocks};
use crate::entities::RawCardanoPoint;

/// Import and store Cardano chain data (transactions, blocks) and aggregate them into Merkle trees
/// for Mithril certificate generation and proof verification.
#[derive(Clone)]
pub struct CardanoChainDataImporter {
    block_scanner: Arc<dyn BlockScanner>,
    transaction_store: Arc<dyn ChainDataStore>,
    block_ranges_importer: BlockRangeImporter,
    last_polled_point: Arc<Mutex<Option<RawCardanoPoint>>>,
    logger: Logger,
}

impl CardanoChainDataImporter {
    /// Constructor
    pub fn new(
        block_scanner: Arc<dyn BlockScanner>,
        transaction_store: Arc<dyn ChainDataStore>,
        logger: Logger,
    ) -> Self {
        let logger = logger.new_with_component_name::<Self>();
        Self {
            block_scanner,
            transaction_store: transaction_store.clone(),
            block_ranges_importer: BlockRangeImporter::new(transaction_store, logger.clone()),
            last_polled_point: Arc::new(Mutex::new(None)),
            logger,
        }
    }

    async fn start_point(
        &self,
        highest_stored_chain_point: &Option<ChainPoint>,
    ) -> StdResult<Option<RawCardanoPoint>> {
        let last_polled_point = self.last_polled_point.lock().await.clone();
        if last_polled_point.is_none() {
            debug!(
                self.logger,
                "No last polled point available, falling back to the highest stored chain point"
            );
        }

        Ok(last_polled_point.or(highest_stored_chain_point.as_ref().map(RawCardanoPoint::from)))
    }

    async fn import_blocks_and_transactions(&self, up_to_beacon: BlockNumber) -> StdResult<()> {
        let highest_stored_beacon = self.transaction_store.get_highest_beacon().await?;
        let from = self.start_point(&highest_stored_beacon).await?;

        if highest_stored_beacon
            .as_ref()
            .is_some_and(|f| f.block_number >= up_to_beacon)
        {
            debug!(
                self.logger,
                "No need to retrieve Cardano blocks and transactions, the database is up to date for block_number '{up_to_beacon}'",
            );

            Ok(())
        } else {
            debug!(
                self.logger, "Retrieving Cardano blocks and transactions until block numbered '{up_to_beacon}'";
                "starting_slot_number" => ?from.as_ref().map(|c| c.slot_number),
                "highest_stored_block_number" => ?highest_stored_beacon.as_ref().map(|c| c.block_number),
            );

            self.parse_and_store_block_and_transactions_not_imported_yet(from, up_to_beacon)
                .await
        }
    }

    async fn parse_and_store_block_and_transactions_not_imported_yet(
        &self,
        from: Option<RawCardanoPoint>,
        until: BlockNumber,
    ) -> StdResult<()> {
        let mut streamer = self.block_scanner.scan(from, until).await?;

        while let Some(blocks) = streamer.poll_next().await? {
            match blocks {
                ChainScannedBlocks::RollForwards(forward_blocks) => {
                    let parsed_blocks_with_transactions: Vec<CardanoBlockWithTransactions> =
                        forward_blocks.into_iter().map(Into::into).collect();

                    self.transaction_store
                        .store_blocks_and_transactions(parsed_blocks_with_transactions)
                        .await?;
                }
                ChainScannedBlocks::RollBackward(slot_number) => {
                    self.transaction_store
                        .remove_rolled_chain_data_and_block_range(slot_number)
                        .await?;
                }
            }
        }

        if let Some(point) = streamer.last_polled_point() {
            *self.last_polled_point.lock().await = Some(point);
        }

        Ok(())
    }
}

#[async_trait]
impl ChainDataImporter for CardanoChainDataImporter {
    async fn import(&self, up_to_beacon: BlockNumber) -> StdResult<()> {
        let importer = self.clone();
        task::spawn_blocking(move || {
            Handle::current().block_on(async move {
                importer.import_blocks_and_transactions(up_to_beacon).await?;
                importer.block_ranges_importer.run(up_to_beacon).await?;
                Ok(())
            })
        })
        .await
        .with_context(|| "ChainDataImporter - worker thread crashed")?
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Range;
    use std::sync::atomic::AtomicUsize;
    use std::time::Duration;

    use mockall::mock;

    use mithril_common::crypto_helper::MKTreeNode;
    use mithril_common::entities::{BlockRange, CardanoTransaction, SlotNumber};

    use crate::chain_scanner::BlockStreamer;
    use crate::entities::ScannedBlock;
    use crate::test::TestLogger;
    use crate::test::double::{DumbBlockScanner, DumbBlockStreamer, InMemoryChainDataStore};

    use super::*;

    mock! {
        pub BlockScannerImpl { }

        #[async_trait]
        impl BlockScanner for BlockScannerImpl {
            async fn scan(
              &self,
              from: Option<RawCardanoPoint>,
              until: BlockNumber,
            ) -> StdResult<Box<dyn BlockStreamer>>;
        }
    }

    impl CardanoChainDataImporter {
        pub(crate) fn new_for_test(
            scanner: Arc<dyn BlockScanner>,
            transaction_store: Arc<dyn ChainDataStore>,
        ) -> Self {
            Self::new(scanner, transaction_store, TestLogger::stdout())
        }
    }

    fn into_blocks_with_transactions(blocks: &[ScannedBlock]) -> Vec<CardanoBlockWithTransactions> {
        blocks.iter().map(|b| b.clone().into()).collect()
    }

    mod store_blocks_and_transactions {
        use super::*;

        #[tokio::test]
        async fn store_blocks_that_do_not_have_transactions() {
            let repository = Arc::new(InMemoryChainDataStore::default());
            let up_to_block_number = BlockNumber(1000);

            let scanner = DumbBlockScanner::new().forwards(vec![vec![ScannedBlock::new(
                "block_hash-1",
                BlockNumber(10),
                SlotNumber(15),
                Vec::<String>::new(),
            )]]);
            let importer =
                CardanoChainDataImporter::new_for_test(Arc::new(scanner), repository.clone());

            importer
                .import_blocks_and_transactions(up_to_block_number)
                .await
                .unwrap();

            let stored_transactions = repository.get_all_block_with_txs().await;
            assert_eq!(
                vec![CardanoBlockWithTransactions::new(
                    hex::encode("block_hash-1"),
                    BlockNumber(10),
                    SlotNumber(15),
                    Vec::<String>::new()
                )],
                stored_transactions
            );
        }

        #[tokio::test]
        async fn if_nothing_stored_parse_and_store_all_blocks_and_transactions() {
            let repository = Arc::new(InMemoryChainDataStore::default());

            let blocks = vec![
                ScannedBlock::new(
                    "block_hash-1",
                    BlockNumber(10),
                    SlotNumber(15),
                    vec!["tx_hash-1", "tx_hash-2"],
                ),
                ScannedBlock::new(
                    "block_hash-2",
                    BlockNumber(20),
                    SlotNumber(25),
                    vec!["tx_hash-3", "tx_hash-4"],
                ),
            ];
            let expected_blocks_with_transactions = into_blocks_with_transactions(&blocks);
            let up_to_block_number = BlockNumber(1000);

            let importer = {
                let mut scanner_mock = MockBlockScannerImpl::new();
                scanner_mock
                    .expect_scan()
                    .withf(move |from, until| from.is_none() && until == up_to_block_number)
                    .return_once(move |_, _| {
                        Ok(Box::new(DumbBlockStreamer::new().forwards(vec![blocks])))
                    });
                CardanoChainDataImporter::new_for_test(Arc::new(scanner_mock), repository.clone())
            };

            importer
                .import_blocks_and_transactions(up_to_block_number)
                .await
                .unwrap();

            let stored_transactions = repository.get_all_block_with_txs().await;
            assert_eq!(expected_blocks_with_transactions, stored_transactions);
        }

        #[tokio::test]
        async fn if_all_blocks_and_transactions_are_stored_nothing_is_parsed_and_stored() {
            let up_to_block_number = BlockNumber(12);
            let last_block = vec![CardanoBlockWithTransactions::new(
                hex::encode("block_hash-3"),
                BlockNumber(30),
                SlotNumber(35),
                vec!["tx-20"],
            )];
            let repository = Arc::new(
                InMemoryChainDataStore::builder()
                    .with_blocks_and_transactions(&last_block)
                    .build(),
            );
            let scanner = DumbBlockScanner::new().forwards(vec![vec![
                ScannedBlock::new(
                    "block_hash-1",
                    BlockNumber(10),
                    SlotNumber(15),
                    vec!["tx_hash-1", "tx_hash-2"],
                ),
                ScannedBlock::new(
                    "block_hash-2",
                    BlockNumber(20),
                    SlotNumber(25),
                    vec!["tx_hash-3", "tx_hash-4"],
                ),
            ]]);

            let importer =
                CardanoChainDataImporter::new_for_test(Arc::new(scanner), repository.clone());

            importer
                .import_blocks_and_transactions(up_to_block_number)
                .await
                .unwrap();

            let blocks_with_txs = repository.get_all_block_with_txs().await;
            assert_eq!(last_block, blocks_with_txs);
        }

        #[tokio::test]
        async fn if_half_blocks_and_transactions_are_already_stored_the_other_half_is_parsed_and_stored()
         {
            let highest_stored_chain_point = ChainPoint::new(
                SlotNumber(134),
                BlockNumber(10),
                hex::encode("block_hash-1"),
            );
            let stored_block = ScannedBlock::new(
                hex::decode(&highest_stored_chain_point.block_hash).unwrap(),
                highest_stored_chain_point.block_number,
                highest_stored_chain_point.slot_number,
                vec!["tx_hash-1", "tx_hash-2"],
            );
            let to_store_block = ScannedBlock::new(
                "block_hash-2",
                BlockNumber(20),
                SlotNumber(229),
                vec!["tx_hash-3", "tx_hash-4"],
            );
            let existing_blocks = vec![stored_block.clone()];
            let expected_blocks_with_transactions =
                into_blocks_with_transactions(&[stored_block.clone(), to_store_block.clone()]);
            let up_to_block_number = BlockNumber(22);

            let repository = Arc::new(
                InMemoryChainDataStore::builder()
                    .with_blocks_and_transactions(&existing_blocks)
                    .build(),
            );

            let importer = {
                let scanned_blocks = vec![to_store_block.clone()];
                let mut scanner_mock = MockBlockScannerImpl::new();
                scanner_mock
                    .expect_scan()
                    .withf(move |from, until| {
                        from == &Some(highest_stored_chain_point.clone().into())
                            && *until == up_to_block_number
                    })
                    .return_once(move |_, _| {
                        Ok(Box::new(
                            DumbBlockStreamer::new().forwards(vec![scanned_blocks]),
                        ))
                    })
                    .once();
                CardanoChainDataImporter::new_for_test(Arc::new(scanner_mock), repository.clone())
            };

            importer
                .import_blocks_and_transactions(up_to_block_number)
                .await
                .unwrap();

            let stored_blocks_with_transactions = repository.get_all_block_with_txs().await;
            assert_eq!(
                expected_blocks_with_transactions,
                stored_blocks_with_transactions
            );
        }
    }

    mod transactions_import_start_point {
        use super::*;

        async fn importer_with_last_polled_point(
            last_polled_point: Option<RawCardanoPoint>,
        ) -> CardanoChainDataImporter {
            let repository = Arc::new(InMemoryChainDataStore::default());

            CardanoChainDataImporter {
                last_polled_point: Arc::new(Mutex::new(last_polled_point)),
                ..CardanoChainDataImporter::new_for_test(
                    Arc::new(DumbBlockScanner::new()),
                    repository,
                )
            }
        }

        #[tokio::test]
        async fn cloning_keep_last_polled_point() {
            let importer = importer_with_last_polled_point(Some(RawCardanoPoint::new(
                SlotNumber(15),
                "block_hash-1",
            )))
            .await;

            let cloned_importer = importer.clone();
            let start_point = cloned_importer.start_point(&None).await.unwrap();
            assert_eq!(
                Some(RawCardanoPoint::new(SlotNumber(15), "block_hash-1")),
                start_point
            );
        }

        #[tokio::test]
        async fn none_if_nothing_stored_nor_scanned() {
            let importer = importer_with_last_polled_point(None).await;
            let highest_stored_block_number = None;

            let start_point = importer.start_point(&highest_stored_block_number).await.unwrap();
            assert_eq!(None, start_point);
        }

        #[tokio::test]
        async fn start_at_last_stored_point_if_nothing_scanned() {
            let importer = importer_with_last_polled_point(None).await;
            let highest_stored_block_number = Some(ChainPoint::new(
                SlotNumber(25),
                BlockNumber(20),
                hex::encode("block_hash-2"),
            ));

            let start_point = importer.start_point(&highest_stored_block_number).await.unwrap();
            assert_eq!(
                Some(RawCardanoPoint::new(SlotNumber(25), "block_hash-2")),
                start_point
            );
        }

        #[tokio::test]
        async fn start_at_last_scanned_point_when_nothing_stored() {
            let importer = importer_with_last_polled_point(Some(RawCardanoPoint::new(
                SlotNumber(15),
                "block_hash-1",
            )))
            .await;
            let highest_stored_block_number = None;

            let start_point = importer.start_point(&highest_stored_block_number).await.unwrap();
            assert_eq!(
                Some(RawCardanoPoint::new(SlotNumber(15), "block_hash-1")),
                start_point
            );
        }

        #[tokio::test]
        async fn start_at_last_scanned_point_even_if_something_stored() {
            let importer = importer_with_last_polled_point(Some(RawCardanoPoint::new(
                SlotNumber(15),
                "block_hash-1",
            )))
            .await;
            let highest_stored_block_number = Some(ChainPoint::new(
                SlotNumber(25),
                BlockNumber(20),
                hex::encode("block_hash-2"),
            ));

            let start_point = importer.start_point(&highest_stored_block_number).await.unwrap();
            assert_eq!(
                Some(RawCardanoPoint::new(SlotNumber(15), "block_hash-1")),
                start_point
            );
        }

        #[tokio::test]
        async fn importing_transactions_update_start_point_even_if_no_transactions_are_found() {
            let importer = CardanoChainDataImporter {
                last_polled_point: Arc::new(Mutex::new(None)),
                ..CardanoChainDataImporter::new_for_test(
                    Arc::new(
                        DumbBlockScanner::new()
                            .forwards(vec![vec![ScannedBlock::new(
                                "block_hash-1",
                                BlockNumber(10),
                                SlotNumber(15),
                                Vec::<&str>::new(),
                            )]])
                            .last_polled_point(Some(RawCardanoPoint::new(
                                SlotNumber(25),
                                "block_hash-2",
                            ))),
                    ),
                    Arc::new(InMemoryChainDataStore::default()),
                )
            };
            let highest_stored_block_number = None;

            let start_point_before_import =
                importer.start_point(&highest_stored_block_number).await.unwrap();
            assert_eq!(None, start_point_before_import);

            importer
                .import_blocks_and_transactions(BlockNumber(1000))
                .await
                .unwrap();

            let start_point_after_import =
                importer.start_point(&highest_stored_block_number).await.unwrap();
            assert_eq!(
                Some(RawCardanoPoint::new(SlotNumber(25), "block_hash-2")),
                start_point_after_import
            );
        }

        #[tokio::test]
        async fn importing_transactions_dont_update_start_point_if_streamer_did_nothing() {
            let importer = CardanoChainDataImporter {
                last_polled_point: Arc::new(Mutex::new(Some(RawCardanoPoint::new(
                    SlotNumber(15),
                    "block_hash-1",
                )))),
                ..CardanoChainDataImporter::new_for_test(
                    Arc::new(DumbBlockScanner::new()),
                    Arc::new(InMemoryChainDataStore::default()),
                )
            };
            let highest_stored_block_number = None;

            importer
                .import_blocks_and_transactions(BlockNumber(1000))
                .await
                .unwrap();

            let start_point_after_import =
                importer.start_point(&highest_stored_block_number).await.unwrap();
            assert_eq!(
                Some(RawCardanoPoint::new(SlotNumber(15), "block_hash-1")),
                start_point_after_import
            );
        }
    }

    mod chain_data_rollback {
        use super::*;

        #[tokio::test]
        async fn when_rollbackward_should_remove_transactions() {
            let expected_remaining_block_with_transactions =
                vec![CardanoBlockWithTransactions::new(
                    "block_hash-130",
                    BlockNumber(130),
                    SlotNumber(5),
                    vec!["tx_hash-6", "tx_hash-7"],
                )];
            let repository = Arc::new(
                InMemoryChainDataStore::builder()
                    .with_blocks_and_transactions(
                        &[
                            expected_remaining_block_with_transactions.clone(),
                            vec![CardanoBlockWithTransactions::new(
                                hex::encode("block_hash-131"),
                                BlockNumber(131),
                                SlotNumber(10),
                                vec!["tx_hash-8", "tx_hash-9", "tx_hash-10"],
                            )],
                        ]
                        .concat(),
                    )
                    .build(),
            );

            let chain_point = ChainPoint::new(SlotNumber(5), BlockNumber(130), "block_hash-130");
            let scanner = DumbBlockScanner::new().backward(chain_point);

            let importer =
                CardanoChainDataImporter::new_for_test(Arc::new(scanner), repository.clone());

            importer
                .import_blocks_and_transactions(BlockNumber(3000))
                .await
                .unwrap();

            let stored_blocks_with_transactions = repository.get_all_block_with_txs().await;
            assert_eq!(
                expected_remaining_block_with_transactions,
                stored_blocks_with_transactions
            );
        }

        #[tokio::test]
        async fn when_rollbackward_should_remove_block_ranges() {
            let expected_remaining_block_ranges = vec![
                BlockRange::from_block_number(BlockNumber(0)),
                BlockRange::from_block_number(BlockRange::LENGTH),
                BlockRange::from_block_number(BlockRange::LENGTH * 2),
            ];

            let repository = Arc::new(
                InMemoryChainDataStore::builder()
                    .with_block_range_roots(
                        &[
                            expected_remaining_block_ranges.clone(),
                            vec![
                                BlockRange::from_block_number(BlockRange::LENGTH * 3),
                                BlockRange::from_block_number(BlockRange::LENGTH * 4),
                                BlockRange::from_block_number(BlockRange::LENGTH * 5),
                            ],
                        ]
                        .concat()
                        .into_iter()
                        .map(|b| (b, MKTreeNode::from_hex("AAAA").unwrap()))
                        .collect::<Vec<_>>(),
                    )
                    .with_blocks_and_transactions(&[CardanoBlockWithTransactions::new(
                        hex::encode("block_hash-131"),
                        BlockRange::from_block_number(BlockRange::LENGTH * 3).start,
                        SlotNumber(1),
                        vec!["tx_hash-1", "tx_hash-2", "tx_hash-3"],
                    )])
                    .build(),
            );

            let block_range_roots = repository.get_all_block_range_root().await;
            assert_eq!(6, block_range_roots.len());

            let chain_point =
                ChainPoint::new(SlotNumber(1), BlockRange::LENGTH * 3, "block_hash-131");
            let scanner = DumbBlockScanner::new().backward(chain_point);

            let importer =
                CardanoChainDataImporter::new_for_test(Arc::new(scanner), repository.clone());

            importer
                .import_blocks_and_transactions(BlockNumber(3000))
                .await
                .unwrap();

            assert_eq!(
                expected_remaining_block_ranges,
                repository.get_all_block_range().await
            );
        }
    }

    #[tokio::test]
    async fn test_import_is_non_blocking() {
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        static MAX_COUNTER: usize = 25;
        static WAIT_TIME: u64 = 50;

        // Use a local set to ensure the counter-task is not dispatched on a different thread
        let local = task::LocalSet::new();
        local
            .run_until(async {
                let importer = CardanoChainDataImporter::new_for_test(
                    Arc::new(DumbBlockScanner::new()),
                    Arc::new(BlockingRepository {
                        wait_time: Duration::from_millis(WAIT_TIME),
                    }),
                );

                let importer_future = importer.import(BlockNumber(100));
                let counter_task = task::spawn_local(async {
                    while COUNTER.load(std::sync::atomic::Ordering::SeqCst) < MAX_COUNTER {
                        tokio::time::sleep(Duration::from_millis(1)).await;
                        COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                    }
                });
                importer_future.await.unwrap();

                counter_task.abort();
            })
            .await;

        assert_eq!(
            MAX_COUNTER,
            COUNTER.load(std::sync::atomic::Ordering::SeqCst)
        );

        struct BlockingRepository {
            wait_time: Duration,
        }

        impl BlockingRepository {
            fn block_thread(&self) {
                std::thread::sleep(self.wait_time);
            }
        }

        #[async_trait]
        impl ChainDataStore for BlockingRepository {
            async fn get_highest_beacon(&self) -> StdResult<Option<ChainPoint>> {
                self.block_thread();
                Ok(None)
            }

            async fn get_highest_legacy_block_range(&self) -> StdResult<Option<BlockRange>> {
                self.block_thread();
                Ok(None)
            }

            async fn store_blocks_and_transactions(
                &self,
                _: Vec<CardanoBlockWithTransactions>,
            ) -> StdResult<()> {
                self.block_thread();
                Ok(())
            }

            async fn get_transactions_in_range(
                &self,
                _: Range<BlockNumber>,
            ) -> StdResult<Vec<CardanoTransaction>> {
                self.block_thread();
                Ok(vec![])
            }

            async fn store_legacy_block_range_roots(
                &self,
                _: Vec<(BlockRange, MKTreeNode)>,
            ) -> StdResult<()> {
                self.block_thread();
                Ok(())
            }

            async fn remove_rolled_chain_data_and_block_range(
                &self,
                _: SlotNumber,
            ) -> StdResult<()> {
                self.block_thread();
                Ok(())
            }
        }
    }
}

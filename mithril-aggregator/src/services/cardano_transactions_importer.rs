use std::path::{Path, PathBuf};
use std::sync::Arc;

use async_trait::async_trait;
#[cfg(test)]
use mockall::automock;
use slog::{debug, Logger};

use mithril_common::cardano_block_scanner::BlockScanner;
use mithril_common::entities::{CardanoTransaction, ImmutableFileNumber};
use mithril_common::signable_builder::TransactionsImporter;
use mithril_common::StdResult;

/// Cardano transactions store
#[cfg_attr(test, automock)]
#[async_trait]
pub trait TransactionStore: Send + Sync {
    /// Get the highest known transaction beacon
    async fn get_highest_beacon(&self) -> StdResult<Option<ImmutableFileNumber>>;

    /// Get stored transactions up to the given beacon
    async fn get_up_to(
        &self,
        immutable_file_number: ImmutableFileNumber,
    ) -> StdResult<Vec<CardanoTransaction>>;

    /// Store list of transactions
    async fn store_transactions(&self, transactions: Vec<CardanoTransaction>) -> StdResult<()>;
}

/// Import and store [CardanoTransaction].
pub struct CardanoTransactionsImporter {
    block_scanner: Arc<dyn BlockScanner>,
    transaction_store: Arc<dyn TransactionStore>,
    logger: Logger,
    rescan_offset: Option<usize>,
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
        rescan_offset: Option<usize>,
        logger: Logger,
    ) -> Self {
        Self {
            block_scanner,
            transaction_store,
            logger,
            rescan_offset,
            dirpath: dirpath.to_owned(),
        }
    }

    async fn get_starting_beacon(&self) -> StdResult<Option<u64>> {
        let highest = self.transaction_store.get_highest_beacon().await?;
        let rescan_offset = self.rescan_offset.unwrap_or(0);
        let highest = highest.map(|h| (h + 1).saturating_sub(rescan_offset as u64));
        Ok(highest)
    }

    async fn parse_and_store_transactions_not_imported_yet(
        &self,
        from: Option<ImmutableFileNumber>,
        until: ImmutableFileNumber,
    ) -> StdResult<()> {
        if from.is_some_and(|f| f >= until) {
            debug!(
                self.logger,
                "TransactionsImporter does not need to retrieve Cardano transactions, the database is up to date for immutable '{until}'",
            );
            return Ok(());
        }

        // todo: temp algorithm, should be optimized to avoid loading all blocks & transactions
        // at once in memory (probably using iterators)
        let scanned_blocks = self.block_scanner.scan(&self.dirpath, from, until).await?;
        let parsed_transactions: Vec<CardanoTransaction> = scanned_blocks
            .into_iter()
            .flat_map(|b| b.into_transactions())
            .collect();
        debug!(
            self.logger,
            "TransactionsImporter retrieved '{}' Cardano transactions between immutables '{}' and '{until}'",
            parsed_transactions.len(),
            from.unwrap_or(0)
        );

        self.transaction_store
            .store_transactions(parsed_transactions)
            .await?;
        Ok(())
    }
}

#[async_trait]
impl TransactionsImporter for CardanoTransactionsImporter {
    async fn import(
        &self,
        up_to_beacon: ImmutableFileNumber,
    ) -> StdResult<Vec<CardanoTransaction>> {
        let from = self.get_starting_beacon().await?;
        self.parse_and_store_transactions_not_imported_yet(from, up_to_beacon)
            .await?;

        let transactions = self.transaction_store.get_up_to(up_to_beacon).await?;
        Ok(transactions)
    }
}

#[cfg(test)]
mod tests {
    use mockall::mock;
    use mockall::predicate::eq;

    use mithril_common::cardano_block_scanner::ScannedBlock;

    use crate::database::repository::CardanoTransactionRepository;
    use crate::database::test_helper::cardano_tx_db_connection;

    use super::*;

    mock! {
        pub BlockScannerImpl { }

        #[async_trait]
        impl BlockScanner for BlockScannerImpl {
            async fn scan(
              &self,
              dirpath: &Path,
              from_immutable: Option<ImmutableFileNumber>,
              until_immutable: ImmutableFileNumber,
            ) -> StdResult<Vec<ScannedBlock>>;
        }
    }

    fn build_importer<TParser, TStore>(
        scanner_mock_config: TParser,
        store_mock_config: TStore,
    ) -> CardanoTransactionsImporter
    where
        TParser: FnOnce(&mut MockBlockScannerImpl),
        TStore: FnOnce(&mut MockTransactionStore),
    {
        let db_path = Path::new("");
        let mut scanner = MockBlockScannerImpl::new();
        scanner_mock_config(&mut scanner);

        let mut store = MockTransactionStore::new();
        store.expect_get_up_to().returning(|_| Ok(vec![]));
        store_mock_config(&mut store);

        CardanoTransactionsImporter::new(
            Arc::new(scanner),
            Arc::new(store),
            db_path,
            None,
            crate::test_tools::logger_for_tests(),
        )
    }

    #[tokio::test]
    async fn if_nothing_stored_parse_and_store_all_transactions() {
        let blocks = vec![
            ScannedBlock::new("block_hash-1", 10, 15, 11, vec!["tx_hash-1", "tx_hash-2"]),
            ScannedBlock::new("block_hash-2", 20, 25, 12, vec!["tx_hash-3", "tx_hash-4"]),
        ];
        let transactions: Vec<CardanoTransaction> = blocks
            .iter()
            .flat_map(|b| b.clone().into_transactions())
            .collect();
        let up_to_beacon = 12;

        let importer = build_importer(
            |scanner_mock| {
                scanner_mock
                    .expect_scan()
                    .withf(move |_, from, until| from.is_none() && until == &up_to_beacon)
                    .return_once(move |_, _, _| Ok(blocks));
            },
            |store_mock| {
                let expected_stored_transactions = transactions.clone();
                store_mock
                    .expect_get_highest_beacon()
                    .returning(|| Ok(None));
                store_mock
                    .expect_store_transactions()
                    .with(eq(expected_stored_transactions))
                    .returning(|_| Ok(()))
                    .once();
            },
        );

        importer
            .import(up_to_beacon)
            .await
            .expect("Transactions Parser should succeed");
    }

    #[tokio::test]
    async fn if_all_stored_nothing_is_parsed_and_stored() {
        let up_to_beacon = 12;

        let importer = build_importer(
            |scanner_mock| {
                scanner_mock.expect_scan().never();
            },
            |store_mock| {
                store_mock
                    .expect_get_highest_beacon()
                    .returning(|| Ok(Some(12)));
                store_mock.expect_store_transactions().never();
            },
        );

        importer
            .import(up_to_beacon)
            .await
            .expect("Transactions Parser should succeed");
    }

    #[tokio::test]
    async fn if_all_half_are_stored_the_other_half_is_parsed_and_stored() {
        let blocks = [
            ScannedBlock::new("block_hash-1", 10, 15, 11, vec!["tx_hash-1", "tx_hash-2"]),
            ScannedBlock::new("block_hash-2", 20, 25, 12, vec!["tx_hash-3", "tx_hash-4"]),
        ];
        let transactions: Vec<CardanoTransaction> = blocks
            .iter()
            .flat_map(|b| b.clone().into_transactions())
            .collect();
        let up_to_beacon = 14;

        let importer = build_importer(
            |scanner_mock| {
                let scanned_blocks = vec![blocks[1].clone()];
                scanner_mock
                    .expect_scan()
                    .withf(move |_, from, until| from == &Some(13) && until == &up_to_beacon)
                    .return_once(move |_, _, _| Ok(scanned_blocks));
            },
            |store_mock| {
                store_mock
                    .expect_get_highest_beacon()
                    .returning(|| Ok(Some(12)));
                let expected_to_store_transactions = transactions[2..=3].to_vec();
                store_mock
                    .expect_store_transactions()
                    .with(eq(expected_to_store_transactions))
                    .returning(|_| Ok(()))
                    .once();
            },
        );

        importer
            .import(up_to_beacon)
            .await
            .expect("Transactions Parser should succeed");
    }

    #[tokio::test]
    async fn importing_twice_starting_with_nothing_in_a_real_db_should_yield_the_transactions_in_same_order(
    ) {
        let blocks = vec![
            ScannedBlock::new("block_hash-1", 10, 15, 11, vec!["tx_hash-1", "tx_hash-2"]),
            ScannedBlock::new("block_hash-2", 20, 25, 12, vec!["tx_hash-3", "tx_hash-4"]),
        ];
        let transactions: Vec<CardanoTransaction> = blocks
            .iter()
            .flat_map(|b| b.clone().into_transactions())
            .collect();
        let importer = {
            let connection = cardano_tx_db_connection().unwrap();
            let mut scanner = MockBlockScannerImpl::new();
            scanner.expect_scan().return_once(move |_, _, _| Ok(blocks));

            CardanoTransactionsImporter::new(
                Arc::new(scanner),
                Arc::new(CardanoTransactionRepository::new(Arc::new(connection))),
                Path::new(""),
                None,
                crate::test_tools::logger_for_tests(),
            )
        };

        let cold_imported_transactions = importer
            .import(12)
            .await
            .expect("Transactions Parser should succeed");

        let warm_imported_transactions = importer
            .import(12)
            .await
            .expect("Transactions Parser should succeed");

        assert_eq!(transactions, cold_imported_transactions);
        assert_eq!(cold_imported_transactions, warm_imported_transactions);
    }

    #[tokio::test]
    async fn change_parsed_lower_bound_when_rescan_limit_is_set() {
        fn importer_with_offset(
            highest_stored_beacon: ImmutableFileNumber,
            rescan_offset: ImmutableFileNumber,
        ) -> CardanoTransactionsImporter {
            let mut store = MockTransactionStore::new();
            store
                .expect_get_highest_beacon()
                .returning(move || Ok(Some(highest_stored_beacon)));

            CardanoTransactionsImporter::new(
                Arc::new(MockBlockScannerImpl::new()),
                Arc::new(store),
                Path::new(""),
                Some(rescan_offset as usize),
                crate::test_tools::logger_for_tests(),
            )
        }
        let importer = importer_with_offset(8, 3);

        let from = importer.get_starting_beacon().await.unwrap();
        // Expected should be: highest_stored_beacon + 1 - rescan_offset
        assert_eq!(Some(6), from);

        let importer = importer_with_offset(5, 10);

        let from = importer.get_starting_beacon().await.unwrap();
        // If sub overflow it should be 0
        assert_eq!(Some(0), from);
    }
}

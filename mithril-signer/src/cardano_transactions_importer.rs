use async_trait::async_trait;
use mithril_common::cardano_transaction_parser::TransactionParser;
use mithril_common::entities::{CardanoDbBeacon, CardanoTransaction, ImmutableFileNumber};
use mithril_common::signable_builder::TransactionsImporter;
use mithril_common::StdResult;
use slog::{debug, Logger};
use std::path::{Path, PathBuf};
use std::sync::Arc;

#[cfg(test)]
use mockall::automock;

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
    async fn store_transactions(&self, transactions: &[CardanoTransaction]) -> StdResult<()>;
}

// Todo: see if we can add quickly a parameter to rescan the last "x" immutables
/// Import and store [CardanoTransaction].
pub struct CardanoTransactionsImporter {
    transaction_parser: Arc<dyn TransactionParser>,
    transaction_store: Arc<dyn TransactionStore>,
    logger: Logger,
    rescan_offset: Option<usize>,
    dirpath: PathBuf,
}

impl CardanoTransactionsImporter {
    /// Constructor
    ///
    /// About `rescan_offset`: if Some(x) the importer will be asked to rescan the previous 'x'
    /// immutables starting from the highest immutable known in the store.
    /// This is useful when one of the last immutable was not full scanned.
    pub fn new(
        transaction_parser: Arc<dyn TransactionParser>,
        transaction_store: Arc<dyn TransactionStore>,
        dirpath: &Path,
        rescan_offset: Option<usize>,
        logger: Logger,
    ) -> Self {
        Self {
            transaction_parser,
            transaction_store,
            logger,
            rescan_offset,
            dirpath: dirpath.to_owned(),
        }
    }

    async fn parse_and_store_missing_transactions(
        &self,
        from: Option<u64>,
        until: ImmutableFileNumber,
    ) -> StdResult<()> {
        if from.is_some_and(|f| f >= until) {
            // Db is up-to-date - nothing to do
            return Ok(());
        }

        let parsed_transactions = self
            .transaction_parser
            .parse(&self.dirpath, from, until)
            .await?;
        debug!(
            self.logger,
            "Retrieved {} Cardano transactions at between immutables {} and {until}",
            parsed_transactions.len(),
            from.unwrap_or(0)
        );

        let transaction_chunk_size = 100;
        for transactions_in_chunk in parsed_transactions.chunks(transaction_chunk_size) {
            self.transaction_store
                .store_transactions(transactions_in_chunk)
                .await?;
        }
        Ok(())
    }
}

#[async_trait]
impl TransactionsImporter for CardanoTransactionsImporter {
    async fn import(&self, beacon: &CardanoDbBeacon) -> StdResult<Vec<CardanoTransaction>> {
        let highest = self.transaction_store.get_highest_beacon().await?;
        let rescan_offset = self.rescan_offset.unwrap_or(0);
        self.parse_and_store_missing_transactions(
            highest.map(|h| (h + 1).saturating_sub(rescan_offset as u64)),
            beacon.immutable_file_number,
        )
        .await?;

        let transactions = self
            .transaction_store
            .get_up_to(beacon.immutable_file_number)
            .await?;
        Ok(transactions)
    }
}

#[cfg(test)]
mod tests {
    use crate::database::provider::CardanoTransactionRepository;
    use crate::{Configuration, ProductionServiceBuilder};
    use mithril_persistence::sqlite::SqliteConnection;
    use mockall::mock;
    use mockall::predicate::eq;

    use super::*;

    mock! {
        pub TransactionParserImpl { }

        #[async_trait]
        impl TransactionParser for TransactionParserImpl {
            async fn parse(
              &self,
              dirpath: &Path,
              from_immutable: Option<ImmutableFileNumber>,
              until_immutable: ImmutableFileNumber,
            ) -> StdResult<Vec<CardanoTransaction>>;
        }
    }

    async fn get_connection() -> Arc<SqliteConnection> {
        let party_id = "party-id-123".to_string();
        let configuration = Configuration::new_sample(&party_id);
        let production_service_builder = ProductionServiceBuilder::new(&configuration);
        production_service_builder
            .build_sqlite_connection(
                ":memory:",
                crate::database::cardano_transaction_migration::get_migrations(),
            )
            .await
            .unwrap()
    }

    fn build_importer(
        parser_mock_config: &dyn Fn(&mut MockTransactionParserImpl),
        store_mock_config: &dyn Fn(&mut MockTransactionStore),
    ) -> CardanoTransactionsImporter {
        let db_path = Path::new("");
        let mut parser = MockTransactionParserImpl::new();
        parser_mock_config(&mut parser);

        let mut store = MockTransactionStore::new();
        store.expect_get_up_to().returning(|_| Ok(vec![]));
        store_mock_config(&mut store);

        CardanoTransactionsImporter::new(
            Arc::new(parser),
            Arc::new(store),
            db_path,
            None,
            crate::test_tools::logger_for_tests(),
        )
    }

    #[tokio::test]
    async fn if_nothing_stored_parse_and_store_all_transactions() {
        let transactions = vec![
            CardanoTransaction::new("tx_hash-1", 10, 15, "block_hash-1", 11),
            CardanoTransaction::new("tx_hash-2", 10, 20, "block_hash-1", 11),
            CardanoTransaction::new("tx_hash-3", 20, 25, "block_hash-2", 12),
            CardanoTransaction::new("tx_hash-4", 20, 30, "block_hash-2", 12),
        ];
        let beacon = CardanoDbBeacon::new("", 1, 12);

        let importer = build_importer(
            &|parser_mock| {
                let expected_until = beacon.immutable_file_number;
                let parsed_transactions = transactions.clone();
                parser_mock
                    .expect_parse()
                    .withf(move |_, from, until| from.is_none() && until == &expected_until)
                    .return_once(move |_, _, _| Ok(parsed_transactions));
            },
            &|store_mock| {
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
            .import(&beacon)
            .await
            .expect("Transactions Parser should succeed");
    }

    #[tokio::test]
    async fn if_all_stored_nothing_is_parsed_and_stored() {
        let beacon = CardanoDbBeacon::new("", 1, 12);

        let importer = build_importer(
            &|parser_mock| {
                parser_mock.expect_parse().never();
            },
            &|store_mock| {
                store_mock
                    .expect_get_highest_beacon()
                    .returning(|| Ok(Some(12)));
                store_mock.expect_store_transactions().never();
            },
        );

        importer
            .import(&beacon)
            .await
            .expect("Transactions Parser should succeed");
    }

    #[tokio::test]
    async fn if_all_half_are_stored_the_other_half_is_parsed_and_stored() {
        let transactions = vec![
            CardanoTransaction::new("tx_hash-1", 10, 15, "block_hash-10", 11),
            CardanoTransaction::new("tx_hash-2", 20, 20, "block_hash-20", 12),
            CardanoTransaction::new("tx_hash-3", 30, 25, "block_hash-30", 13),
            CardanoTransaction::new("tx_hash-4", 40, 30, "block_hash-40", 14),
        ];
        let beacon = CardanoDbBeacon::new("", 1, 14);

        let importer = build_importer(
            &|parser_mock| {
                let expected_until = beacon.immutable_file_number;
                let parsed_transactions = transactions[2..=3].to_vec();
                parser_mock
                    .expect_parse()
                    .withf(move |_, from, until| from == &Some(13) && until == &expected_until)
                    .return_once(move |_, _, _| Ok(parsed_transactions));
            },
            &|store_mock| {
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
            .import(&beacon)
            .await
            .expect("Transactions Parser should succeed");
    }

    #[tokio::test]
    async fn importing_twice_starting_with_nothing_in_a_real_db_should_yield_the_transactions_in_same_order(
    ) {
        let transactions = vec![
            CardanoTransaction::new("tx_hash-1", 10, 15, "block_hash-1", 11),
            CardanoTransaction::new("tx_hash-2", 10, 20, "block_hash-1", 11),
            CardanoTransaction::new("tx_hash-3", 20, 25, "block_hash-2", 12),
            CardanoTransaction::new("tx_hash-4", 20, 30, "block_hash-2", 12),
        ];
        let beacon = CardanoDbBeacon::new("", 1, 12);
        let importer = {
            let connection = get_connection().await;
            let parsed_transactions = transactions.clone();
            let mut parser = MockTransactionParserImpl::new();
            parser
                .expect_parse()
                .return_once(move |_, _, _| Ok(parsed_transactions));

            CardanoTransactionsImporter::new(
                Arc::new(parser),
                Arc::new(CardanoTransactionRepository::new(connection.clone())),
                Path::new(""),
                None,
                crate::test_tools::logger_for_tests(),
            )
        };

        let cold_imported_transactions = importer
            .import(&beacon)
            .await
            .expect("Transactions Parser should succeed");

        let warm_imported_transactions = importer
            .import(&beacon)
            .await
            .expect("Transactions Parser should succeed");

        assert_eq!(transactions, cold_imported_transactions);
        assert_eq!(cold_imported_transactions, warm_imported_transactions);
    }

    #[tokio::test]
    async fn change_parsed_lower_bound_when_rescan_limit_is_set() {
        let beacon = CardanoDbBeacon::new("", 1, 12);
        let highest_stored_beacon = 8;
        let rescan_offset = 3;
        let expected_parsed_lower_bound = highest_stored_beacon + 1 - rescan_offset;

        let importer = {
            let mut parser = MockTransactionParserImpl::new();
            parser
                .expect_parse()
                .withf(move |_, from, _| from.is_some_and(|f| f == expected_parsed_lower_bound))
                .return_once(move |_, _, _| Ok(vec![]));

            let mut store = MockTransactionStore::new();
            store
                .expect_get_highest_beacon()
                .returning(move || Ok(Some(highest_stored_beacon)));
            store.expect_get_up_to().returning(|_| Ok(vec![]));

            CardanoTransactionsImporter::new(
                Arc::new(parser),
                Arc::new(store),
                Path::new(""),
                Some(rescan_offset as usize),
                crate::test_tools::logger_for_tests(),
            )
        };

        importer
            .import(&beacon)
            .await
            .expect("Transactions Parser should succeed");
    }
}

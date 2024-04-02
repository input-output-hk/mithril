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
    /// Get stored transactions at most up to the given beacon
    ///
    /// Alongside the transactions it will return the highest immutable file number
    async fn get_at_most_to(
        &self,
        beacon: &CardanoDbBeacon,
    ) -> StdResult<Option<(ImmutableFileNumber, Vec<CardanoTransaction>)>>;

    /// Store list of transactions
    async fn store_transactions(&self, transactions: &[CardanoTransaction]) -> StdResult<()>;
}

/// Import and store [CardanoTransaction].
pub struct CardanoTransactionsImporter {
    transaction_parser: Arc<dyn TransactionParser>,
    transaction_store: Arc<dyn TransactionStore>,
    logger: Logger,
    dirpath: PathBuf,
}

impl CardanoTransactionsImporter {
    /// Constructor
    pub fn new(
        transaction_parser: Arc<dyn TransactionParser>,
        transaction_store: Arc<dyn TransactionStore>,
        dirpath: &Path,
        logger: Logger,
    ) -> Self {
        Self {
            transaction_parser,
            transaction_store,
            logger,
            dirpath: dirpath.to_owned(),
        }
    }
}

#[async_trait]
impl TransactionsImporter for CardanoTransactionsImporter {
    async fn import(&self, beacon: &CardanoDbBeacon) -> StdResult<Vec<CardanoTransaction>> {
        if let Some((_highest_immutable, stored_transactions)) =
            self.transaction_store.get_at_most_to(beacon).await?
        {
            Ok(stored_transactions)
        } else {
            let transactions = self.transaction_parser.parse(&self.dirpath, beacon).await?;
            debug!(
                self.logger,
                "Retrieved {} Cardano transactions at beacon: {beacon}",
                transactions.len()
            );

            let transaction_chunk_size = 100;
            for transactions_in_chunk in transactions.chunks(transaction_chunk_size) {
                self.transaction_store
                    .store_transactions(transactions_in_chunk)
                    .await?;
            }

            Ok(transactions)
        }
    }
}

#[cfg(test)]
mod tests {
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
              beacon: &CardanoDbBeacon,
            ) -> StdResult<Vec<CardanoTransaction>>;
        }
    }

    fn build_importer(
        parser_mock_config: &dyn Fn(&mut MockTransactionParserImpl),
        store_mock_config: &dyn Fn(&mut MockTransactionStore),
    ) -> CardanoTransactionsImporter {
        let db_path = Path::new("");
        let mut parser = MockTransactionParserImpl::new();
        parser_mock_config(&mut parser);

        let mut store = MockTransactionStore::new();
        store_mock_config(&mut store);

        CardanoTransactionsImporter::new(
            Arc::new(parser),
            Arc::new(store),
            db_path,
            crate::test_tools::logger_for_tests(),
        )
    }

    #[tokio::test]
    async fn if_nothing_stored_parse_and_store_all_transactions() {
        let transactions = vec![
            CardanoTransaction::new("tx_hash-1", 10, 15, "block_hash-1", 1),
            CardanoTransaction::new("tx_hash-2", 10, 20, "block_hash-1", 1),
            CardanoTransaction::new("tx_hash-3", 20, 25, "block_hash-2", 2),
            CardanoTransaction::new("tx_hash-4", 20, 30, "block_hash-2", 2),
        ];
        let beacon = CardanoDbBeacon::new("", 1, 3);

        let importer = build_importer(
            &|parser_mock| {
                let expected_beacon = beacon.clone();
                let parsed_transactions = transactions.clone();
                parser_mock
                    .expect_parse()
                    .withf(move |_, beacon_arg| beacon_arg == &expected_beacon)
                    .return_once(move |_, _| Ok(parsed_transactions));
            },
            &|store_mock| {
                let expected_stored_transactions = transactions.clone();
                store_mock.expect_get_at_most_to().returning(|_| Ok(None));
                store_mock
                    .expect_store_transactions()
                    .with(eq(expected_stored_transactions))
                    .returning(|_| Ok(()))
                    .once();
            },
        );
        let imported_transactions = importer
            .import(&beacon)
            .await
            .expect("Transactions Parser should succeed");

        assert_eq!(transactions, imported_transactions);
    }

    #[tokio::test]
    async fn if_all_stored_nothing_is_parsed_and_stored() {
        let transactions = vec![
            CardanoTransaction::new("tx_hash-1", 10, 15, "block_hash-1", 1),
            CardanoTransaction::new("tx_hash-2", 10, 20, "block_hash-1", 1),
            CardanoTransaction::new("tx_hash-3", 20, 25, "block_hash-2", 2),
            CardanoTransaction::new("tx_hash-4", 20, 30, "block_hash-2", 2),
        ];
        let beacon = CardanoDbBeacon::new("", 1, 3);

        let importer = build_importer(
            &|parser_mock| {
                parser_mock.expect_parse().never();
            },
            &|store_mock| {
                let stored_transactions = transactions.clone();
                store_mock
                    .expect_get_at_most_to()
                    .return_once(|_| Ok(Some((2, stored_transactions))));
                store_mock.expect_store_transactions().never();
            },
        );
        let imported_transactions = importer
            .import(&beacon)
            .await
            .expect("Transactions Parser should succeed");

        assert_eq!(transactions, imported_transactions);
    }
}

use std::sync::Arc;

use async_trait::async_trait;
use slog::{debug, Logger};

use mithril_common::entities::BlockNumber;
use mithril_common::signable_builder::TransactionsImporter;
use mithril_common::StdResult;

/// Trait to get the highest transaction block number
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait HighestTransactionBlockNumberGetter: Send + Sync {
    /// Get the highest known transaction block number
    async fn get(&self) -> StdResult<Option<BlockNumber>>;
}

/// A decorator of [TransactionsImporter] that does the import by chunks
pub struct TransactionsImporterByChunk {
    highest_transaction_block_number_getter: Arc<dyn HighestTransactionBlockNumberGetter>,
    wrapped_importer: Arc<dyn TransactionsImporter>,
    chunk_size: BlockNumber,
    logger: Logger,
}

impl TransactionsImporterByChunk {
    /// Create a new instance of `TransactionsImporterByChunk`.
    pub fn new(
        highest_transaction_block_number_getter: Arc<dyn HighestTransactionBlockNumberGetter>,
        wrapped_importer: Arc<dyn TransactionsImporter>,
        chunk_size: BlockNumber,
        logger: Logger,
    ) -> Self {
        Self {
            highest_transaction_block_number_getter,
            wrapped_importer,
            chunk_size,
            logger,
        }
    }
}

#[async_trait]
impl TransactionsImporter for TransactionsImporterByChunk {
    async fn import(&self, up_to_beacon: BlockNumber) -> StdResult<()> {
        let mut intermediate_up_to = self
            .highest_transaction_block_number_getter
            .get()
            .await?
            .unwrap_or(0);

        while intermediate_up_to < up_to_beacon {
            let next_up_to = (intermediate_up_to + self.chunk_size).min(up_to_beacon);
            debug!(
                self.logger,
                "Running Transactions importer between block '{intermediate_up_to}' and '{next_up_to}'";
            );
            self.wrapped_importer.import(next_up_to).await?;
            intermediate_up_to = next_up_to;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use mockall::predicate::eq;
    use mockall::{mock, Sequence};

    use super::*;

    mock! {
        pub TransactionImporterImpl {}

        #[async_trait]
        impl TransactionsImporter for TransactionImporterImpl {
            async fn import(&self, up_to_beacon: BlockNumber) -> StdResult<()>;
        }
    }

    fn create_highest_transaction_block_number_getter_mock(
        highest_block_number: BlockNumber,
    ) -> Arc<dyn HighestTransactionBlockNumberGetter> {
        Arc::new({
            let mut mock = MockHighestTransactionBlockNumberGetter::new();
            mock.expect_get()
                .returning(move || Ok(Some(highest_block_number)));
            mock
        })
    }

    fn create_transaction_importer_mock(expected_values: Vec<u64>) -> MockTransactionImporterImpl {
        let mut seq = Sequence::new();
        let mut wrapped_importer = MockTransactionImporterImpl::new();
        for expected_value in expected_values {
            wrapped_importer
                .expect_import()
                .once()
                .in_sequence(&mut seq)
                .with(eq(expected_value))
                .returning(|_| Ok(()));
        }
        wrapped_importer
    }

    #[tokio::test]
    async fn test_import_nothing_to_do_when_highest_block_number_lower_or_equal_up_to_beacon() {
        let highest_block_number = 10;
        let chunk_size = 5;

        let highest_transaction_block_number_getter =
            create_highest_transaction_block_number_getter_mock(highest_block_number);
        let mut wrapped_importer = MockTransactionImporterImpl::new();
        wrapped_importer.expect_import().never();

        let importer = TransactionsImporterByChunk::new(
            highest_transaction_block_number_getter,
            Arc::new(wrapped_importer),
            chunk_size,
            crate::test_tools::logger_for_tests(),
        );

        let up_to_beacon = highest_block_number;
        importer.import(up_to_beacon).await.unwrap();

        let up_to_beacon = highest_block_number - 1;
        importer.import(up_to_beacon).await.unwrap();
    }

    #[tokio::test]
    async fn test_import_even_when_highest_block_number_is_none() {
        let highest_block_number = None;
        let chunk_size = 5;
        let up_to_beacon = chunk_size - 1;

        let highest_transaction_block_number_getter = Arc::new({
            let mut mock = MockHighestTransactionBlockNumberGetter::new();
            mock.expect_get()
                .returning(move || Ok(highest_block_number));
            mock
        });
        let wrapped_importer = create_transaction_importer_mock(vec![up_to_beacon]);

        let importer = TransactionsImporterByChunk::new(
            highest_transaction_block_number_getter,
            Arc::new(wrapped_importer),
            chunk_size,
            crate::test_tools::logger_for_tests(),
        );

        importer.import(up_to_beacon).await.unwrap();
    }

    #[tokio::test]
    async fn test_import_only_once_when_block_delta_less_than_chunk_size() {
        let highest_block_number = 10;
        let chunk_size = 5;
        let up_to_beacon = highest_block_number + chunk_size - 1;

        let highest_transaction_block_number_getter =
            create_highest_transaction_block_number_getter_mock(highest_block_number);
        let wrapped_importer = create_transaction_importer_mock(vec![up_to_beacon]);

        let importer = TransactionsImporterByChunk::new(
            highest_transaction_block_number_getter,
            Arc::new(wrapped_importer),
            chunk_size,
            crate::test_tools::logger_for_tests(),
        );

        importer.import(up_to_beacon).await.unwrap();
    }

    #[tokio::test]
    async fn test_import_multiple_times_when_block_delta_is_not_a_multiple_of_chunk_size() {
        let highest_block_number = 10;
        let chunk_size = 5;
        let up_to_beacon = highest_block_number + chunk_size * 2 + 1;

        let highest_transaction_block_number_getter =
            create_highest_transaction_block_number_getter_mock(highest_block_number);
        let wrapped_importer = create_transaction_importer_mock(vec![
            highest_block_number + chunk_size,
            highest_block_number + chunk_size * 2,
            up_to_beacon,
        ]);

        let importer = TransactionsImporterByChunk::new(
            highest_transaction_block_number_getter,
            Arc::new(wrapped_importer),
            chunk_size,
            crate::test_tools::logger_for_tests(),
        );

        importer.import(up_to_beacon).await.unwrap();
    }

    #[tokio::test]
    async fn test_import_multiple_times_when_block_delta_is_a_multiple_of_chunk_size() {
        let highest_block_number = 10;
        let chunk_size = 5;
        let up_to_beacon = highest_block_number + chunk_size * 2;

        let highest_transaction_block_number_getter =
            create_highest_transaction_block_number_getter_mock(highest_block_number);
        let wrapped_importer = create_transaction_importer_mock(vec![
            highest_block_number + chunk_size,
            highest_block_number + chunk_size * 2,
        ]);

        let importer = TransactionsImporterByChunk::new(
            highest_transaction_block_number_getter,
            Arc::new(wrapped_importer),
            chunk_size,
            crate::test_tools::logger_for_tests(),
        );

        importer.import(up_to_beacon).await.unwrap();
    }
}

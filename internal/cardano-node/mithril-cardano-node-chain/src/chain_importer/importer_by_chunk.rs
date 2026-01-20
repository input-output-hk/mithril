use std::sync::Arc;

use async_trait::async_trait;
use slog::{Logger, debug};

use mithril_common::StdResult;
use mithril_common::entities::BlockNumber;
use mithril_common::logging::LoggerExtensions;

use crate::chain_importer::ChainDataImporter;

/// Trait to get the highest stored chain data block number
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait HighestStoredBlockNumberGetter: Send + Sync {
    /// Get the highest known transaction block number
    async fn get(&self) -> StdResult<Option<BlockNumber>>;
}

/// A decorator of [ChainDataImporter] that does the import by chunks
pub struct ChainDataImporterByChunk {
    highest_stored_block_number_getter: Arc<dyn HighestStoredBlockNumberGetter>,
    wrapped_importer: Arc<dyn ChainDataImporter>,
    chunk_size: BlockNumber,
    logger: Logger,
}

impl ChainDataImporterByChunk {
    /// Create a new instance of `TransactionsImporterByChunk`.
    pub fn new(
        highest_transaction_block_number_getter: Arc<dyn HighestStoredBlockNumberGetter>,
        wrapped_importer: Arc<dyn ChainDataImporter>,
        chunk_size: BlockNumber,
        logger: Logger,
    ) -> Self {
        Self {
            highest_stored_block_number_getter: highest_transaction_block_number_getter,
            wrapped_importer,
            chunk_size,
            logger: logger.new_with_component_name::<Self>(),
        }
    }
}

#[async_trait]
impl ChainDataImporter for ChainDataImporterByChunk {
    async fn import(&self, up_to_beacon: BlockNumber) -> StdResult<()> {
        let mut intermediate_up_to = self
            .highest_stored_block_number_getter
            .get()
            .await?
            .unwrap_or(BlockNumber(0));

        while intermediate_up_to < up_to_beacon {
            let next_up_to = (intermediate_up_to + self.chunk_size).min(up_to_beacon);
            debug!(
                self.logger,
                "Running Transactions import between block '{intermediate_up_to}' and '{next_up_to}'";
            );
            self.wrapped_importer.import(next_up_to).await?;
            intermediate_up_to = next_up_to;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use mockall::Sequence;
    use mockall::predicate::eq;

    use crate::chain_importer::MockChainDataImporter;
    use crate::test::TestLogger;

    use super::*;

    fn create_highest_stored_block_number_getter_mock(
        highest_block_number: BlockNumber,
    ) -> Arc<dyn HighestStoredBlockNumberGetter> {
        Arc::new({
            let mut mock = MockHighestStoredBlockNumberGetter::new();
            mock.expect_get().returning(move || Ok(Some(highest_block_number)));
            mock
        })
    }

    fn create_chain_data_importer_mock(expected_values: Vec<BlockNumber>) -> MockChainDataImporter {
        let mut seq = Sequence::new();
        let mut wrapped_importer = MockChainDataImporter::new();
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
        let highest_block_number = BlockNumber(10);
        let chunk_size = BlockNumber(5);

        let highest_transaction_block_number_getter =
            create_highest_stored_block_number_getter_mock(highest_block_number);
        let mut wrapped_importer = MockChainDataImporter::new();
        wrapped_importer.expect_import().never();

        let importer = ChainDataImporterByChunk::new(
            highest_transaction_block_number_getter,
            Arc::new(wrapped_importer),
            chunk_size,
            TestLogger::stdout(),
        );

        let up_to_beacon = highest_block_number;
        importer.import(up_to_beacon).await.unwrap();

        let up_to_beacon = highest_block_number - 1;
        importer.import(up_to_beacon).await.unwrap();
    }

    #[tokio::test]
    async fn test_import_even_when_highest_block_number_is_none() {
        let highest_block_number = None;
        let chunk_size = BlockNumber(5);
        let up_to_beacon = chunk_size - 1;

        let highest_transaction_block_number_getter = Arc::new({
            let mut mock = MockHighestStoredBlockNumberGetter::new();
            mock.expect_get().returning(move || Ok(highest_block_number));
            mock
        });
        let wrapped_importer = create_chain_data_importer_mock(vec![up_to_beacon]);

        let importer = ChainDataImporterByChunk::new(
            highest_transaction_block_number_getter,
            Arc::new(wrapped_importer),
            chunk_size,
            TestLogger::stdout(),
        );

        importer.import(up_to_beacon).await.unwrap();
    }

    #[tokio::test]
    async fn test_import_only_once_when_block_delta_less_than_chunk_size() {
        let highest_block_number = BlockNumber(10);
        let chunk_size = BlockNumber(5);
        let up_to_beacon = highest_block_number + chunk_size - 1;

        let highest_transaction_block_number_getter =
            create_highest_stored_block_number_getter_mock(highest_block_number);
        let wrapped_importer = create_chain_data_importer_mock(vec![up_to_beacon]);

        let importer = ChainDataImporterByChunk::new(
            highest_transaction_block_number_getter,
            Arc::new(wrapped_importer),
            chunk_size,
            TestLogger::stdout(),
        );

        importer.import(up_to_beacon).await.unwrap();
    }

    #[tokio::test]
    async fn test_import_multiple_times_when_block_delta_is_not_a_multiple_of_chunk_size() {
        let highest_block_number = BlockNumber(10);
        let chunk_size = BlockNumber(5);
        let up_to_beacon = highest_block_number + chunk_size * 2 + 1;

        let highest_transaction_block_number_getter =
            create_highest_stored_block_number_getter_mock(highest_block_number);
        let wrapped_importer = create_chain_data_importer_mock(vec![
            highest_block_number + chunk_size,
            highest_block_number + chunk_size * 2,
            up_to_beacon,
        ]);

        let importer = ChainDataImporterByChunk::new(
            highest_transaction_block_number_getter,
            Arc::new(wrapped_importer),
            chunk_size,
            TestLogger::stdout(),
        );

        importer.import(up_to_beacon).await.unwrap();
    }

    #[tokio::test]
    async fn test_import_multiple_times_when_block_delta_is_a_multiple_of_chunk_size() {
        let highest_block_number = BlockNumber(10);
        let chunk_size = BlockNumber(5);
        let up_to_beacon = highest_block_number + chunk_size * 2;

        let highest_transaction_block_number_getter =
            create_highest_stored_block_number_getter_mock(highest_block_number);
        let wrapped_importer = create_chain_data_importer_mock(vec![
            highest_block_number + chunk_size,
            highest_block_number + chunk_size * 2,
        ]);

        let importer = ChainDataImporterByChunk::new(
            highest_transaction_block_number_getter,
            Arc::new(wrapped_importer),
            chunk_size,
            TestLogger::stdout(),
        );

        importer.import(up_to_beacon).await.unwrap();
    }
}

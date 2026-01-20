use std::sync::Arc;

use async_trait::async_trait;
use slog::{Logger, debug};

use mithril_common::StdResult;
use mithril_common::entities::BlockNumber;
use mithril_common::logging::LoggerExtensions;

use crate::chain_importer::ChainDataImporter;

/// Cardano stored chain data pruner
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait ChainDataPruner: Send + Sync {
    /// Prune the chain data older than the given number of blocks.
    async fn prune(&self, number_of_blocks_to_keep: BlockNumber) -> StdResult<()>;
}

/// A decorator of [ChainDataImporter] that prunes the chain data older than a given number of
/// blocks after running the import.
///
/// If the number of blocks to keep is not provided, no pruning is performed.
pub struct ChainDataImporterWithPruner {
    number_of_blocks_to_keep: Option<BlockNumber>,
    chain_data_pruner: Arc<dyn ChainDataPruner>,
    wrapped_importer: Arc<dyn ChainDataImporter>,
    logger: Logger,
}

impl ChainDataImporterWithPruner {
    /// Create a new instance of [ChainDataImporterWithPruner].
    pub fn new(
        number_of_blocks_to_keep: Option<BlockNumber>,
        chain_data_pruner: Arc<dyn ChainDataPruner>,
        wrapped_importer: Arc<dyn ChainDataImporter>,
        logger: Logger,
    ) -> Self {
        Self {
            number_of_blocks_to_keep,
            chain_data_pruner,
            wrapped_importer,
            logger: logger.new_with_component_name::<Self>(),
        }
    }
}

#[async_trait]
impl ChainDataImporter for ChainDataImporterWithPruner {
    async fn import(&self, up_to_beacon: BlockNumber) -> StdResult<()> {
        self.wrapped_importer.import(up_to_beacon).await?;

        if let Some(number_of_blocks_to_keep) = self.number_of_blocks_to_keep {
            debug!(
                self.logger,
                "Chain data Import finished - Pruning data included in block range roots";
                "number_of_blocks_to_keep" => *number_of_blocks_to_keep,
            );
            self.chain_data_pruner.prune(number_of_blocks_to_keep).await?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use mockall::predicate::eq;

    use crate::chain_importer::MockChainDataImporter;
    use crate::test::TestLogger;

    use super::*;

    impl ChainDataImporterWithPruner {
        pub(crate) fn new_with_mock<P, I>(
            number_of_blocks_to_keep: Option<BlockNumber>,
            pruner_mock_config: P,
            importer_mock_config: I,
        ) -> Self
        where
            P: FnOnce(&mut MockChainDataPruner),
            I: FnOnce(&mut MockChainDataImporter),
        {
            let mut chain_data_pruner = MockChainDataPruner::new();
            pruner_mock_config(&mut chain_data_pruner);
            let mut chain_data_importer = MockChainDataImporter::new();
            importer_mock_config(&mut chain_data_importer);

            Self::new(
                number_of_blocks_to_keep,
                Arc::new(chain_data_pruner),
                Arc::new(chain_data_importer),
                TestLogger::stdout(),
            )
        }
    }

    #[tokio::test]
    async fn test_does_not_prune_if_none_is_configured() {
        let importer = ChainDataImporterWithPruner::new_with_mock(
            None,
            |mock| {
                mock.expect_prune().never();
            },
            |mock| {
                mock.expect_import().once().returning(|_| Ok(()));
            },
        );

        importer
            .import(BlockNumber(100))
            .await
            .expect("Import should not fail");
    }

    #[tokio::test]
    async fn test_does_prune_if_a_block_number_is_configured() {
        let expected_block_number = BlockNumber(5);
        let importer = ChainDataImporterWithPruner::new_with_mock(
            Some(expected_block_number),
            |mock| {
                mock.expect_prune()
                    .with(eq(expected_block_number))
                    .once()
                    .returning(|_| Ok(()));
            },
            |mock| {
                mock.expect_import().once().returning(|_| Ok(()));
            },
        );

        importer
            .import(BlockNumber(100))
            .await
            .expect("Import should not fail");
    }
}

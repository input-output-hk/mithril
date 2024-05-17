use std::sync::Arc;

use async_trait::async_trait;

use mithril_common::entities::{BlockNumber, ImmutableFileNumber};
use mithril_common::signable_builder::TransactionsImporter;
use mithril_common::StdResult;

/// Cardano transactions pruner
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait TransactionPruner: Send + Sync {
    /// Prune the transactions older than the given number of blocks.
    async fn prune(&self, number_of_blocks_to_keep: BlockNumber) -> StdResult<()>;
}

/// A decorator of [TransactionsImporter] that prunes the transactions older than a given number of
/// blocks after running the import.
///
/// If the number of blocks to keep is not provided, no pruning is performed.
pub struct TransactionsImporterWithPruner {
    number_of_blocks_to_keep: Option<BlockNumber>,
    transaction_pruner: Arc<dyn TransactionPruner>,
    wrapped_importer: Arc<dyn TransactionsImporter>,
}

impl TransactionsImporterWithPruner {
    /// Create a new instance of [TransactionsImporterWithPruner].
    pub fn new(
        number_of_blocks_to_keep: Option<BlockNumber>,
        transaction_pruner: Arc<dyn TransactionPruner>,
        wrapped_importer: Arc<dyn TransactionsImporter>,
    ) -> Self {
        Self {
            number_of_blocks_to_keep,
            transaction_pruner,
            wrapped_importer,
        }
    }
}

#[async_trait]
impl TransactionsImporter for TransactionsImporterWithPruner {
    async fn import(&self, up_to_beacon: ImmutableFileNumber) -> StdResult<()> {
        self.wrapped_importer.import(up_to_beacon).await?;

        if let Some(number_of_blocks_to_keep) = self.number_of_blocks_to_keep {
            self.transaction_pruner
                .prune(number_of_blocks_to_keep)
                .await?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use mockall::mock;
    use mockall::predicate::eq;

    use super::*;

    mock! {
        pub TransactionImporterImpl {}

        #[async_trait]
        impl TransactionsImporter for TransactionImporterImpl {
            async fn import(&self, up_to_beacon: ImmutableFileNumber) -> StdResult<()>;
        }
    }

    impl TransactionsImporterWithPruner {
        pub fn new_with_mock<P, I>(
            number_of_blocks_to_keep: Option<BlockNumber>,
            transaction_pruner_mock_config: P,
            importer_mock_config: I,
        ) -> Self
        where
            P: FnOnce(&mut MockTransactionPruner),
            I: FnOnce(&mut MockTransactionImporterImpl),
        {
            let mut transaction_pruner = MockTransactionPruner::new();
            transaction_pruner_mock_config(&mut transaction_pruner);
            let mut transaction_importer = MockTransactionImporterImpl::new();
            importer_mock_config(&mut transaction_importer);

            Self::new(
                number_of_blocks_to_keep,
                Arc::new(transaction_pruner),
                Arc::new(transaction_importer),
            )
        }
    }

    #[tokio::test]
    async fn test_does_not_prune_if_none_is_configured() {
        let importer = TransactionsImporterWithPruner::new_with_mock(
            None,
            |mock| {
                mock.expect_prune().never();
            },
            |mock| {
                mock.expect_import().once().returning(|_| Ok(()));
            },
        );

        importer.import(10).await.expect("Import should not fail");
    }

    #[tokio::test]
    async fn test_does_prune_if_a_block_number_is_configured() {
        let expected_block_number: BlockNumber = 5;
        let importer = TransactionsImporterWithPruner::new_with_mock(
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

        importer.import(10).await.expect("Import should not fail");
    }
}

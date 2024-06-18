//! # Cardano Transaction Preloader
//!
//! This module provides a preload mechanism for Cardano Transaction signed entity, allowing
//! to compute in advance the Transactions & Block Range Root to be signed.

use std::sync::Arc;

use anyhow::Context;
use slog::{debug, info, Logger};

use crate::chain_observer::ChainObserver;
use crate::entities::{BlockNumber, SignedEntityTypeDiscriminants};
use crate::signable_builder::TransactionsImporter;
use crate::signed_entity_type_lock::SignedEntityTypeLock;
use crate::StdResult;

/// Preload mechanism for Cardano Transaction signed entity, allowing
/// to compute in advance the Transactions & Block Range Root to be signed.
pub struct CardanoTransactionsPreloader {
    signed_entity_type_lock: Arc<SignedEntityTypeLock>,
    importer: Arc<dyn TransactionsImporter>,
    security_parameter: BlockNumber,
    chain_observer: Arc<dyn ChainObserver>,
    logger: Logger,
}

impl CardanoTransactionsPreloader {
    /// Create a new instance of `CardanoTransactionPreloader`.
    pub fn new(
        signed_entity_type_lock: Arc<SignedEntityTypeLock>,
        importer: Arc<dyn TransactionsImporter>,
        security_parameter: BlockNumber,
        chain_observer: Arc<dyn ChainObserver>,
        logger: Logger,
    ) -> Self {
        Self {
            signed_entity_type_lock,
            importer,
            security_parameter,
            chain_observer,
            logger,
        }
    }

    /// Preload the Cardano Transactions by running the importer up to the current chain block number.
    pub async fn preload(&self) -> StdResult<()> {
        info!(self.logger, "🔥 Preload Cardano Transactions - Started");
        debug!(self.logger, "🔥 Locking signed entity type"; "entity_type" => "CardanoTransactions");
        self.signed_entity_type_lock
            .lock(SignedEntityTypeDiscriminants::CardanoTransactions)
            .await;

        let preload_result = self.do_preload().await;

        debug!(self.logger, "🔥 Releasing signed entity type"; "entity_type" => "CardanoTransactions");
        self.signed_entity_type_lock
            .release(SignedEntityTypeDiscriminants::CardanoTransactions)
            .await;
        info!(self.logger, "🔥 Preload Cardano Transactions - Finished");

        preload_result
    }

    async fn do_preload(&self) -> StdResult<()> {
        let chain_point = self
            .chain_observer
            .get_current_chain_point()
            .await?
            .with_context(|| {
                "No chain point yielded by the chain observer, is your cardano node ready?"
            })?;
        let up_to_block_number = chain_point
            .block_number
            .saturating_sub(self.security_parameter);
        self.importer.import(up_to_block_number).await?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use async_trait::async_trait;
    use mockall::predicate::eq;

    use crate::chain_observer::FakeObserver;
    use crate::entities::{BlockNumber, ChainPoint, TimePoint};
    use crate::signable_builder::MockTransactionsImporter;
    use crate::test_utils::TestLogger;

    use super::*;

    struct ImporterWithSignedEntityTypeLockCheck {
        signed_entity_type_lock: Arc<SignedEntityTypeLock>,
    }
    #[async_trait]
    impl TransactionsImporter for ImporterWithSignedEntityTypeLockCheck {
        async fn import(&self, _up_to_beacon: BlockNumber) -> StdResult<()> {
            assert!(
                self.signed_entity_type_lock
                    .is_locked(SignedEntityTypeDiscriminants::CardanoTransactions)
                    .await
            );
            Ok(())
        }
    }

    #[tokio::test]
    async fn call_its_inner_importer() {
        let chain_block_number = 5000;
        let security_parameter = 542;
        let chain_observer = FakeObserver::new(Some(TimePoint {
            chain_point: ChainPoint {
                block_number: chain_block_number,
                ..ChainPoint::dummy()
            },
            ..TimePoint::dummy()
        }));
        let expected_parsed_block_number = chain_block_number - security_parameter;

        let mut importer = MockTransactionsImporter::new();
        importer
            .expect_import()
            .times(1)
            .with(eq(expected_parsed_block_number))
            .returning(|_| Ok(()));

        let preloader = CardanoTransactionsPreloader::new(
            Arc::new(SignedEntityTypeLock::default()),
            Arc::new(importer),
            security_parameter,
            Arc::new(chain_observer),
            TestLogger::stdout(),
        );

        preloader.preload().await.unwrap();
    }

    #[tokio::test]
    async fn fail_if_chain_point_is_not_available() {
        let chain_observer = FakeObserver::new(None);
        let mut importer = MockTransactionsImporter::new();
        importer.expect_import().never();

        let preloader = CardanoTransactionsPreloader::new(
            Arc::new(SignedEntityTypeLock::default()),
            Arc::new(importer),
            0,
            Arc::new(chain_observer),
            TestLogger::stdout(),
        );

        preloader
            .preload()
            .await
            .expect_err("should raise an error when chain point is not available");
    }

    #[tokio::test]
    async fn should_lock_entity_type_while_preloading() {
        let signed_entity_type_lock = Arc::new(SignedEntityTypeLock::default());

        let preloader = CardanoTransactionsPreloader::new(
            signed_entity_type_lock.clone(),
            Arc::new(ImporterWithSignedEntityTypeLockCheck {
                signed_entity_type_lock: signed_entity_type_lock.clone(),
            }),
            0,
            Arc::new(FakeObserver::new(Some(TimePoint::dummy()))),
            TestLogger::stdout(),
        );

        assert!(
            !signed_entity_type_lock
                .is_locked(SignedEntityTypeDiscriminants::CardanoTransactions)
                .await
        );

        preloader.preload().await.unwrap();

        assert!(
            !signed_entity_type_lock
                .is_locked(SignedEntityTypeDiscriminants::CardanoTransactions)
                .await
        );
    }

    #[tokio::test]
    async fn should_release_locked_entity_type_when_preloading_fail() {
        let signed_entity_type_lock = Arc::new(SignedEntityTypeLock::default());
        let chain_observer = FakeObserver::new(None);

        let preloader = CardanoTransactionsPreloader::new(
            signed_entity_type_lock.clone(),
            Arc::new(ImporterWithSignedEntityTypeLockCheck {
                signed_entity_type_lock: signed_entity_type_lock.clone(),
            }),
            0,
            Arc::new(chain_observer),
            TestLogger::stdout(),
        );

        preloader.preload().await.unwrap_err();

        assert!(
            !signed_entity_type_lock
                .is_locked(SignedEntityTypeDiscriminants::CardanoTransactions)
                .await
        );
    }
}

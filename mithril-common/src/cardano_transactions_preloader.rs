//! # Cardano Transaction Preloader
//!
//! This module provides a preload mechanism for Cardano Transaction signed entity, allowing
//! to compute in advance the Transactions & Block Range Root to be signed.

use std::sync::Arc;

use anyhow::Context;
use slog::{debug, trace, Logger};

use crate::chain_observer::ChainObserver;
use crate::entities::{CardanoTransactionsSigningConfig, SignedEntityTypeDiscriminants};
use crate::signable_builder::TransactionsImporter;
use crate::signed_entity_type_lock::SignedEntityTypeLock;
use crate::StdResult;

/// Preload mechanism for Cardano Transaction signed entity, allowing
/// to compute in advance the Transactions & Block Range Root to be signed.
pub struct CardanoTransactionsPreloader {
    signed_entity_type_lock: Arc<SignedEntityTypeLock>,
    importer: Arc<dyn TransactionsImporter>,
    cardano_transactions_signing_config: CardanoTransactionsSigningConfig,
    chain_observer: Arc<dyn ChainObserver>,
    logger: Logger,
}

impl CardanoTransactionsPreloader {
    /// Create a new instance of `CardanoTransactionPreloader`.
    pub fn new(
        signed_entity_type_lock: Arc<SignedEntityTypeLock>,
        importer: Arc<dyn TransactionsImporter>,
        cardano_transactions_signing_config: CardanoTransactionsSigningConfig,
        chain_observer: Arc<dyn ChainObserver>,
        logger: Logger,
    ) -> Self {
        Self {
            signed_entity_type_lock,
            importer,
            cardano_transactions_signing_config,
            chain_observer,
            logger,
        }
    }

    /// Preload the Cardano Transactions by running the importer up to the current chain block number.
    pub async fn preload(&self) -> StdResult<()> {
        debug!(self.logger, "🔥 Preload Cardano Transactions - Started");
        trace!(self.logger, "🔥 Locking signed entity type"; "entity_type" => "CardanoTransactions");
        self.signed_entity_type_lock
            .lock(SignedEntityTypeDiscriminants::CardanoTransactions)
            .await;

        let preload_result = self.do_preload().await;

        trace!(self.logger, "🔥 Releasing signed entity type"; "entity_type" => "CardanoTransactions");
        self.signed_entity_type_lock
            .release(SignedEntityTypeDiscriminants::CardanoTransactions)
            .await;
        debug!(self.logger, "🔥 Preload Cardano Transactions - Finished");

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
        let up_to_block_number = self
            .cardano_transactions_signing_config
            .compute_block_number_to_be_signed(chain_point.block_number);
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
        let cardano_transactions_signing_config = CardanoTransactionsSigningConfig::dummy();
        let chain_observer = FakeObserver::new(Some(TimePoint {
            chain_point: ChainPoint {
                block_number: chain_block_number,
                ..ChainPoint::dummy()
            },
            ..TimePoint::dummy()
        }));
        let expected_parsed_block_number = cardano_transactions_signing_config
            .compute_block_number_to_be_signed(chain_block_number);

        let mut importer = MockTransactionsImporter::new();
        importer
            .expect_import()
            .times(1)
            .with(eq(expected_parsed_block_number))
            .returning(|_| Ok(()));

        let preloader = CardanoTransactionsPreloader::new(
            Arc::new(SignedEntityTypeLock::default()),
            Arc::new(importer),
            cardano_transactions_signing_config,
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
            CardanoTransactionsSigningConfig::dummy(),
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
            CardanoTransactionsSigningConfig::dummy(),
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
            CardanoTransactionsSigningConfig::dummy(),
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

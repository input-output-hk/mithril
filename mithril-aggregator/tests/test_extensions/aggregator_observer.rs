use anyhow::{anyhow, Context};
use mithril_aggregator::{
    dependency_injection::{DependenciesBuilder, EpochServiceWrapper},
    entities::OpenMessage,
    services::{CertifierService, SignedEntityService},
};
use mithril_common::entities::{CardanoTransactionsSnapshot, Certificate, SignedEntity};
use mithril_common::{
    entities::{Epoch, SignedEntityType, SignedEntityTypeDiscriminants, TimePoint},
    CardanoNetwork, StdResult, TickerService,
};
use std::sync::Arc;

// An observer that allow to inspect currently available open messages.
pub struct AggregatorObserver {
    network: CardanoNetwork,
    certifier_service: Arc<dyn CertifierService>,
    signed_entity_service: Arc<dyn SignedEntityService>,
    ticker_service: Arc<dyn TickerService>,
    epoch_service: EpochServiceWrapper,
}

impl AggregatorObserver {
    // [AggregatorObserver] factory
    pub async fn new(deps_builder: &mut DependenciesBuilder) -> Self {
        Self {
            network: deps_builder.configuration.get_network().unwrap(),
            certifier_service: deps_builder.get_certifier_service().await.unwrap(),
            signed_entity_service: deps_builder.get_signed_entity_service().await.unwrap(),
            ticker_service: deps_builder.get_ticker_service().await.unwrap(),
            epoch_service: deps_builder.get_epoch_service().await.unwrap(),
        }
    }

    /// Get the current [Epoch] known to the aggregator
    pub async fn current_epoch(&self) -> Epoch {
        self.ticker_service.get_current_epoch().await.unwrap()
    }

    /// Get the current [TimePoint] known to the aggregator
    pub async fn current_time_point(&self) -> TimePoint {
        self.ticker_service.get_current_time_point().await.unwrap()
    }

    /// Get the current [open message][OpenMessage] for the given message type
    pub async fn get_current_open_message(
        &self,
        discriminant: SignedEntityTypeDiscriminants,
    ) -> StdResult<Option<OpenMessage>> {
        let signed_entity_type = self.build_current_signed_entity_type(discriminant).await?;

        self.certifier_service
            .get_open_message(&signed_entity_type)
            .await
            .with_context(|| {
                format!(
                    "Requesting current open message of type '{discriminant}' should be not fail"
                )
            })
    }

    /// Compute the full [SignedEntityType] for the given discriminant based on the current
    /// [TimePoint].
    pub async fn build_current_signed_entity_type(
        &self,
        discriminant: SignedEntityTypeDiscriminants,
    ) -> StdResult<SignedEntityType> {
        let time_point = self
            .ticker_service
            .get_current_time_point()
            .await
            .with_context(|| "Querying the current beacon should not fail")?;

        self.epoch_service
            .read()
            .await
            .signed_entity_config()?
            .time_point_to_signed_entity(discriminant, &time_point)
    }

    /// Get the last certificate produced by the aggregator
    pub async fn get_last_certificate(&self) -> StdResult<Certificate> {
        let certificate = self
            .certifier_service
            .get_latest_certificates(1)
            .await
            .with_context(|| "Querying last certificate should not fail")?
            .pop()
            .ok_or(anyhow!(
                "No certificate have been produced by the aggregator"
            ))?;
        Ok(certificate)
    }

    /// Get the last cardano transactions snapshot produced by the aggregator
    pub async fn get_last_cardano_transactions_snapshot(
        &self,
    ) -> StdResult<SignedEntity<CardanoTransactionsSnapshot>> {
        let last_tx_snapshot = self
            .signed_entity_service
            .get_last_cardano_transaction_snapshot()
            .await
            .with_context(|| "Querying last cardano transactions snapshot should not fail")?
            .ok_or(anyhow!(
                "No cardano transactions snapshot have been produced by the aggregator"
            ))?;
        Ok(last_tx_snapshot)
    }

    pub async fn is_last_signed_entity(
        &self,
        signed_entity_type_expected: &SignedEntityType,
    ) -> StdResult<bool> {
        match signed_entity_type_expected {
            SignedEntityType::CardanoImmutableFilesFull(_) => Ok(Some(signed_entity_type_expected)
                == self
                    .signed_entity_service
                    .get_last_signed_snapshots(1)
                    .await?
                    .first()
                    .map(|s| &s.signed_entity_type)),
            SignedEntityType::MithrilStakeDistribution(_) => Ok(Some(signed_entity_type_expected)
                == self
                    .signed_entity_service
                    .get_last_signed_mithril_stake_distributions(1)
                    .await?
                    .first()
                    .map(|s| &s.signed_entity_type)),
            SignedEntityType::CardanoTransactions(_, _) => Ok(Some(signed_entity_type_expected)
                == self
                    .signed_entity_service
                    .get_last_cardano_transaction_snapshot()
                    .await?
                    .map(|s| s.signed_entity_type)
                    .as_ref()),
            SignedEntityType::CardanoStakeDistribution(_) => Ok(Some(signed_entity_type_expected)
                == self
                    .signed_entity_service
                    .get_last_signed_cardano_stake_distributions(1)
                    .await?
                    .first()
                    .map(|s| &s.signed_entity_type)),
        }
    }
}

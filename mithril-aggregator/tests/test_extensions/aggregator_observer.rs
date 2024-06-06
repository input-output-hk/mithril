use anyhow::{anyhow, Context};
use mithril_aggregator::{
    dependency_injection::DependenciesBuilder, entities::OpenMessage, services::CertifierService,
};
use mithril_common::{
    entities::{
        Epoch, SignedEntityConfig, SignedEntityType, SignedEntityTypeDiscriminants, TimePoint,
    },
    CardanoNetwork, StdResult, TickerService,
};
use std::sync::Arc;

// An observer that allow to inspect currently available open messages.
pub struct AggregatorObserver {
    network: CardanoNetwork,
    certifier_service: Arc<dyn CertifierService>,
    ticker_service: Arc<dyn TickerService>,
    signed_entity_config: SignedEntityConfig,
}

impl AggregatorObserver {
    // [AggregatorObserver] factory
    pub async fn new(deps_builder: &mut DependenciesBuilder) -> Self {
        Self {
            network: deps_builder.configuration.get_network().unwrap(),
            certifier_service: deps_builder.get_certifier_service().await.unwrap(),
            ticker_service: deps_builder.get_ticker_service().await.unwrap(),
            signed_entity_config: deps_builder.get_signed_entity_config().unwrap(),
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

    /// Get the current [open message][OpenMessageWithSingleSignatures] for the given message type
    pub async fn get_current_open_message(
        &self,
        discriminant: SignedEntityTypeDiscriminants,
    ) -> StdResult<Option<OpenMessage>> {
        let signed_entity_type = self.build_current_signed_entity_type(discriminant).await?;

        self.certifier_service
            .get_open_message(&signed_entity_type)
            .await
            .with_context(|| "Requesting current open message of type CardanoImmutableFilesFull should be not fail")
    }

    // Get the [entity type][SignedEntityType::CardanoImmutableFilesFull] of the current current open message
    pub async fn get_current_signed_entity_type(
        &self,
        discriminant: SignedEntityTypeDiscriminants,
    ) -> StdResult<SignedEntityType> {
        match self.get_current_open_message(discriminant).await? {
            None => Err(anyhow!(
                "An open message should be available for cardano immutables"
            )),
            Some(message) => Ok(message.signed_entity_type),
        }
    }

    async fn build_current_signed_entity_type(
        &self,
        discriminant: SignedEntityTypeDiscriminants,
    ) -> StdResult<SignedEntityType> {
        let time_point = self
            .ticker_service
            .get_current_time_point()
            .await
            .with_context(|| "Querying the current beacon should not fail")?;

        Ok(self
            .signed_entity_config
            .time_point_to_signed_entity(discriminant, &time_point))
    }
}

use mithril_aggregator::{
    certifier_service::CertifierService, dependency_injection::DependenciesBuilder,
    entities::OpenMessage, ticker_service::TickerService,
};
use mithril_common::{
    entities::{Beacon, Epoch, SignedEntityType, SignedEntityTypeDiscriminants},
    BeaconProvider,
};
use std::sync::Arc;

// An observer that allow to inspect currently available open messages.
pub struct AggregatorObserver {
    beacon_provider: Arc<dyn BeaconProvider>,
    certifier_service: Arc<dyn CertifierService>,
    ticker_service: Arc<dyn TickerService>,
}

impl AggregatorObserver {
    // [OpenMessageObserver] factory
    pub async fn new(deps_builder: &mut DependenciesBuilder) -> Self {
        Self {
            beacon_provider: deps_builder.get_beacon_provider().await.unwrap(),
            certifier_service: deps_builder.get_certifier_service().await.unwrap(),
            ticker_service: deps_builder.get_ticker_service().await.unwrap(),
        }
    }

    /// Get the current [Epoch] known to the aggregator
    pub async fn current_epoch(&self) -> Epoch {
        self.ticker_service.get_current_epoch().await.unwrap()
    }

    /// Get the current [Beacon] known to the aggregator
    pub async fn current_beacon(&self) -> Beacon {
        self.ticker_service
            .get_current_immutable_beacon()
            .await
            .unwrap()
    }

    /// Get the current [open message][OpenMessageWithSingleSignatures] for the given message type
    pub async fn get_current_open_message(
        &self,
        discriminant: SignedEntityTypeDiscriminants,
    ) -> Result<Option<OpenMessage>, String> {
        let signed_entity_type = self.build_current_signed_entity_type(discriminant).await?;

        self.certifier_service
            .get_open_message(&signed_entity_type)
            .await
            .map_err(|e| {
                format!(
                    "Requesting current open message of type CardanoImmutableFilesFull should be not fail: {e:?}"
                )
            })
    }

    // Get the [entity type][SignedEntityType::CardanoImmutableFilesFull] of the current current open message
    pub async fn get_current_signed_entity_type(
        &self,
        discriminant: SignedEntityTypeDiscriminants,
    ) -> Result<SignedEntityType, String> {
        match self.get_current_open_message(discriminant).await? {
            None => Err("An open message should be available for cardano immutables".to_string()),
            Some(message) => Ok(message.signed_entity_type),
        }
    }

    async fn build_current_signed_entity_type(
        &self,
        discriminant: SignedEntityTypeDiscriminants,
    ) -> Result<SignedEntityType, String> {
        let beacon = self
            .beacon_provider
            .get_current_beacon()
            .await
            .map_err(|e| format!("Querying the current beacon should not fail: {e:?}"))?;

        match discriminant {
            SignedEntityTypeDiscriminants::MithrilStakeDistribution => {
                Ok(SignedEntityType::MithrilStakeDistribution(beacon.epoch))
            }
            SignedEntityTypeDiscriminants::CardanoStakeDistribution => {
                Ok(SignedEntityType::CardanoStakeDistribution(beacon.epoch))
            }
            SignedEntityTypeDiscriminants::CardanoImmutableFilesFull => {
                Ok(SignedEntityType::CardanoImmutableFilesFull(beacon))
            }
        }
    }
}

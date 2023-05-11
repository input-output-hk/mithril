use mithril_aggregator::dependency_injection::DependenciesBuilder;
use mithril_aggregator::{certifier_service::CertifierService, entities::OpenMessage};
use mithril_common::{entities::SignedEntityType, BeaconProvider};
use std::sync::Arc;

// An observer that allow to inspect currently available open messages.
pub struct AggregatorObserver {
    beacon_provider: Arc<dyn BeaconProvider>,
    certifier_service: Arc<dyn CertifierService>,
}

#[derive(Debug, Clone, Copy)]
pub enum SignedEntityTypeDiscriminants {
    MithrilStakeDistribution,
    CardanoStakeDistribution,
    CardanoImmutableFilesFull,
}

impl AggregatorObserver {
    // [OpenMessageObserver] factory
    pub async fn new(deps_builder: &mut DependenciesBuilder) -> Self {
        Self {
            beacon_provider: deps_builder.get_beacon_provider().await.unwrap(),
            certifier_service: deps_builder.get_certifier_service().await.unwrap(),
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
}

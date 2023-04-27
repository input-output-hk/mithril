use mithril_aggregator::{
    certifier_service::CertifierService, database::provider::OpenMessageWithSingleSignatures,
};
use mithril_common::{entities::SignedEntityType, BeaconProvider};
use std::sync::Arc;

// An observer that allow to inspect currently available open messages.
pub struct OpenMessageObserver {
    pub beacon_provider: Arc<dyn BeaconProvider>,
    pub certifier_service: Arc<dyn CertifierService>,
}

impl OpenMessageObserver {
    // [OpenMessageObserver] factory
    pub fn new(
        beacon_provider: Arc<dyn BeaconProvider>,
        certifier_service: Arc<dyn CertifierService>,
    ) -> Self {
        Self {
            beacon_provider,
            certifier_service,
        }
    }

    // Get the current [open message][OpenMessageWithSingleSignatures] for [cardano immutables][SignedEntityType::CardanoImmutableFilesFull]
    pub async fn get_current_immutable_message(
        &self,
    ) -> Result<Option<OpenMessageWithSingleSignatures>, String> {
        let immutable_signer_entity_type = SignedEntityType::CardanoImmutableFilesFull(
            self.beacon_provider
                .get_current_beacon()
                .await
                .map_err(|e| format!("Querying the current beacon should not fail: {e:?}"))?,
        );

        self.certifier_service
            .get_open_message(&immutable_signer_entity_type)
            .await
            .map_err(|e| {
                format!(
                    "Requesting current open message of type CardanoImmutableFilesFull should be not fail: {e:?}"
                )
            })
    }

    // Get the [entity type][SignedEntityType::CardanoImmutableFilesFull] of the current current open message
    pub async fn get_current_immutable_entity_type(&self) -> Result<SignedEntityType, String> {
        match self.get_current_immutable_message().await? {
            None => Err("An open message should be available for cardano immutables".to_string()),
            Some(message) => Ok(message.signed_entity_type),
        }
    }
}

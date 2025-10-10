use std::{collections::BTreeSet, sync::Arc};

use crate::{
    ConfigurationSource,
    dependency_injection::{DependenciesBuilder, Result},
    entities::AggregatorEpochSettings,
    get_dependency,
};
use async_trait::async_trait;
use mithril_common::{
    StdResult,
    entities::{Epoch, SignedEntityTypeDiscriminants},
};
use mithril_protocol_config::{
    interface::MithrilNetworkConfigurationProvider,
    model::{MithrilNetworkConfiguration, SignedEntityTypeConfiguration},
};

pub struct LocalMithrilNetworkConfigurationProvider {
    epoch_settings: AggregatorEpochSettings,
    allowed_discriminants: BTreeSet<SignedEntityTypeDiscriminants>,
}

impl LocalMithrilNetworkConfigurationProvider {
    pub fn new(configuration: Arc<dyn ConfigurationSource>) -> Self {
        let epoch_settings = configuration.get_epoch_settings_configuration();
        let allowed_discriminants = configuration
            .compute_allowed_signed_entity_types_discriminants()
            .expect("Failed to compute allowed signed entity types discriminants");

        Self {
            epoch_settings,
            allowed_discriminants,
        }
    }
}

#[async_trait]
impl MithrilNetworkConfigurationProvider for LocalMithrilNetworkConfigurationProvider {
    async fn get_network_configuration(&self) -> StdResult<MithrilNetworkConfiguration> {
        let epoch = Epoch(42); // TODO implement proper epoch retrieval
        let signer_registration_protocol_parameters =
            self.epoch_settings.protocol_parameters.clone();
        let available_signed_entity_types = self.allowed_discriminants.clone();
        let signed_entity_types_config = SignedEntityTypeConfiguration {
            cardano_transactions: Some(
                self.epoch_settings.cardano_transactions_signing_config.clone(),
            ),
        };

        let config = MithrilNetworkConfiguration {
            epoch, //TODO implement
            signer_registration_protocol_parameters,
            available_signed_entity_types,
            signed_entity_types_config,
        };
        Ok(config)
    }
}

impl DependenciesBuilder {
    async fn build_mithril_network_configuration_provider(
        &mut self,
    ) -> Result<Arc<dyn MithrilNetworkConfigurationProvider>> {
        let network_configuration_provider = Arc::new(
            LocalMithrilNetworkConfigurationProvider::new(self.configuration.clone()),
        );

        Ok(network_configuration_provider)
    }

    /// [MithrilNetworkConfigurationProvider][mithril_protocol_config::interface::MithrilNetworkConfigurationProvider] service
    pub async fn get_mithril_network_configuration_provider(
        &mut self,
    ) -> Result<Arc<dyn MithrilNetworkConfigurationProvider>> {
        get_dependency!(self.mithril_network_configuration_provider)
    }
}

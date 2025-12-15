use std::sync::Arc;

use mithril_protocol_config::{
    http::HttpMithrilNetworkConfigurationProvider, interface::MithrilNetworkConfigurationProvider,
};
use tokio::sync::RwLock;

use crate::dependency_injection::{DependenciesBuilder, EpochServiceWrapper, Result};
use crate::get_dependency;
use crate::services::{
    EpochServiceDependencies, LocalMithrilNetworkConfigurationProvider, MithrilEpochService,
};

impl DependenciesBuilder {
    async fn build_epoch_service(&mut self) -> Result<EpochServiceWrapper> {
        let verification_key_store = self.get_verification_key_store().await?;
        let epoch_settings_storer = self.get_epoch_settings_store().await?;
        let chain_observer = self.get_chain_observer().await?;
        let era_checker = self.get_era_checker().await?;
        let stake_store = self.get_stake_store().await?;
        let allowed_discriminants = self
            .configuration
            .compute_allowed_signed_entity_types_discriminants()?;

        let epoch_service = Arc::new(RwLock::new(MithrilEpochService::new(
            EpochServiceDependencies::new(
                self.get_mithril_network_configuration_provider().await?,
                epoch_settings_storer,
                verification_key_store,
                chain_observer,
                era_checker,
                stake_store,
            ),
            allowed_discriminants,
            self.root_logger(),
        )));

        Ok(epoch_service)
    }

    /// [EpochService][crate::services::EpochService] service
    pub async fn get_epoch_service(&mut self) -> Result<EpochServiceWrapper> {
        get_dependency!(self.epoch_service)
    }

    async fn build_mithril_network_configuration_provider(
        &mut self,
    ) -> Result<Arc<dyn MithrilNetworkConfigurationProvider>> {
        let network_configuration_provider: Arc<dyn MithrilNetworkConfigurationProvider> =
            if self.configuration.is_follower_aggregator() {
                Arc::new(HttpMithrilNetworkConfigurationProvider::new(
                    self.get_leader_aggregator_client().await?,
                ))
            } else {
                Arc::new(LocalMithrilNetworkConfigurationProvider::new(
                    self.configuration
                        .get_leader_aggregator_epoch_settings_configuration()?,
                    self.configuration
                        .compute_allowed_signed_entity_types_discriminants()?,
                    self.get_epoch_settings_store().await?,
                ))
            };

        Ok(network_configuration_provider)
    }

    /// [MithrilNetworkConfigurationProvider][mithril_protocol_config::interface::MithrilNetworkConfigurationProvider] service
    pub async fn get_mithril_network_configuration_provider(
        &mut self,
    ) -> Result<Arc<dyn MithrilNetworkConfigurationProvider>> {
        get_dependency!(self.mithril_network_configuration_provider)
    }
}

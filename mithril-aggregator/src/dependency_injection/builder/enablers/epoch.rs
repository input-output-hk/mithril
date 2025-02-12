use std::sync::Arc;
use tokio::sync::RwLock;

use crate::dependency_injection::{DependenciesBuilder, EpochServiceWrapper, Result};
use crate::services::{EpochServiceDependencies, MithrilEpochService};

impl DependenciesBuilder {
    async fn build_epoch_service(&mut self) -> Result<EpochServiceWrapper> {
        let verification_key_store = self.get_verification_key_store().await?;
        let epoch_settings_storer = self.get_epoch_settings_store().await?;
        let chain_observer = self.get_chain_observer().await?;
        let era_checker = self.get_era_checker().await?;
        let stake_store = self.get_stake_store().await?;
        let epoch_settings = self.configuration.get_epoch_settings_configuration();
        let allowed_discriminants = self
            .configuration
            .compute_allowed_signed_entity_types_discriminants()?;

        let epoch_service = Arc::new(RwLock::new(MithrilEpochService::new(
            epoch_settings,
            EpochServiceDependencies::new(
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
        if self.epoch_service.is_none() {
            self.epoch_service = Some(self.build_epoch_service().await?);
        }

        Ok(self.epoch_service.as_ref().cloned().unwrap())
    }
}

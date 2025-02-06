//! Service for pacing the aggregation process, such as handling epoch boundaries and getting
//! current epoch number and stake distribution.

use std::sync::Arc;
use tokio::sync::RwLock;

use mithril_common::{MithrilTickerService, TickerService};

use crate::dependency_injection::{DependenciesBuilder, EpochServiceWrapper};
use crate::services::{
    EpochServiceDependencies, MithrilEpochService, MithrilStakeDistributionService,
    StakeDistributionService,
};

impl DependenciesBuilder {
    async fn build_epoch_service(
        &mut self,
    ) -> crate::dependency_injection::Result<EpochServiceWrapper> {
        let verification_key_store = self.get_verification_key_store().await?;
        let epoch_settings_storer = self.get_epoch_settings_store().await?;
        let chain_observer = self.get_chain_observer().await?;
        let era_checker = self.get_era_checker().await?;
        let stake_distribution_service = self.get_stake_distribution_service().await?;
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
                stake_distribution_service,
            ),
            allowed_discriminants,
            self.root_logger(),
        )));

        Ok(epoch_service)
    }

    /// [EpochService][crate::services::EpochService] service
    pub async fn get_epoch_service(
        &mut self,
    ) -> crate::dependency_injection::Result<EpochServiceWrapper> {
        if self.epoch_service.is_none() {
            self.epoch_service = Some(self.build_epoch_service().await?);
        }

        Ok(self.epoch_service.as_ref().cloned().unwrap())
    }

    async fn build_stake_distribution_service(
        &mut self,
    ) -> crate::dependency_injection::Result<Arc<dyn StakeDistributionService>> {
        let stake_distribution_service = Arc::new(MithrilStakeDistributionService::new(
            self.get_stake_store().await?,
            self.get_chain_observer().await?,
        ));

        Ok(stake_distribution_service)
    }

    /// [StakeDistributionService] service
    pub async fn get_stake_distribution_service(
        &mut self,
    ) -> crate::dependency_injection::Result<Arc<dyn StakeDistributionService>> {
        if self.stake_distribution_service.is_none() {
            self.stake_distribution_service = Some(self.build_stake_distribution_service().await?);
        }

        Ok(self.stake_distribution_service.as_ref().cloned().unwrap())
    }

    /// Create [TickerService] instance.
    pub async fn build_ticker_service(
        &mut self,
    ) -> crate::dependency_injection::Result<Arc<dyn TickerService>> {
        let chain_observer = self.get_chain_observer().await?;
        let immutable_observer = self.get_immutable_file_observer().await?;

        Ok(Arc::new(MithrilTickerService::new(
            chain_observer,
            immutable_observer,
        )))
    }

    /// [StakeDistributionService] service
    pub async fn get_ticker_service(
        &mut self,
    ) -> crate::dependency_injection::Result<Arc<dyn TickerService>> {
        if self.ticker_service.is_none() {
            self.ticker_service = Some(self.build_ticker_service().await?);
        }

        Ok(self.ticker_service.as_ref().cloned().unwrap())
    }
}

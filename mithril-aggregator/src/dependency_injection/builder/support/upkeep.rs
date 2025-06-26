use std::sync::Arc;

use crate::database::repository::SignerRegistrationStore;
use crate::dependency_injection::{DependenciesBuilder, Result};
use crate::get_dependency;
use crate::services::{AggregatorUpkeepService, EpochPruningTask, UpkeepService};

impl DependenciesBuilder {
    /// Return a [EpochPruningTask] instance
    pub async fn get_signer_registration_pruning_task(
        &mut self,
    ) -> Result<Arc<dyn EpochPruningTask>> {
        Ok(Arc::new(SignerRegistrationStore::new(
            self.get_sqlite_connection().await?,
            self.configuration.safe_epoch_retention_limit(),
        )))
    }

    async fn build_upkeep_service(&mut self) -> Result<Arc<dyn UpkeepService>> {
        let stake_pool_pruning_task = self.get_stake_store().await?;
        let epoch_settings_pruning_task = self.get_epoch_settings_store().await?;
        let signer_registration_pruning_task = self.get_signer_registration_pruning_task().await?;

        let upkeep_service = Arc::new(AggregatorUpkeepService::new(
            self.get_sqlite_connection().await?,
            self.get_sqlite_connection_cardano_transaction_pool().await?,
            self.get_event_store_sqlite_connection().await?,
            self.get_signed_entity_type_lock().await?,
            vec![
                stake_pool_pruning_task,
                epoch_settings_pruning_task,
                signer_registration_pruning_task,
            ],
            self.root_logger(),
        ));

        Ok(upkeep_service)
    }

    /// Get the [UpkeepService] instance
    pub async fn get_upkeep_service(&mut self) -> Result<Arc<dyn UpkeepService>> {
        get_dependency!(self.upkeep_service)
    }
}

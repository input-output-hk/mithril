use std::sync::Arc;

use crate::dependency_injection::{DependenciesBuilder, Result};
use crate::services::{AggregatorUpkeepService, EpochPruningTask, UpkeepService};

impl DependenciesBuilder {
    /// Return a [EpochPruningTask] instance
    pub async fn get_signer_registration_pruning_task(
        &mut self,
    ) -> Result<Arc<dyn EpochPruningTask>> {
        if self.signer_registration_pruning_task.is_none() {
            self.signer_registration_pruning_task =
                Some(if self.configuration.is_slave_aggregator() {
                    self.get_mithril_signer_registration_slave().await?
                } else {
                    self.get_mithril_signer_registration_master().await?
                });
        }

        Ok(self
            .signer_registration_pruning_task
            .as_ref()
            .cloned()
            .unwrap())
    }

    async fn build_upkeep_service(&mut self) -> Result<Arc<dyn UpkeepService>> {
        let stake_pool_pruning_task = self.get_stake_store().await?;
        let epoch_settings_pruning_task = self.get_epoch_settings_store().await?;
        let signer_registration_pruning_task = self.get_signer_registration_pruning_task().await?;

        let upkeep_service = Arc::new(AggregatorUpkeepService::new(
            self.get_sqlite_connection().await?,
            self.get_sqlite_connection_cardano_transaction_pool()
                .await?,
            self.get_event_store_sqlite_connection().await?,
            self.get_signed_entity_lock().await?,
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
        if self.upkeep_service.is_none() {
            self.upkeep_service = Some(self.build_upkeep_service().await?);
        }

        Ok(self.upkeep_service.as_ref().cloned().unwrap())
    }
}

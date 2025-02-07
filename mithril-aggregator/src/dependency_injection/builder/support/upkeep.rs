use std::sync::Arc;

use crate::dependency_injection::{DependenciesBuilder, Result};
use crate::services::{AggregatorUpkeepService, UpkeepService};

impl DependenciesBuilder {
    async fn build_upkeep_service(&mut self) -> Result<Arc<dyn UpkeepService>> {
        let stake_pool_pruning_task = self.get_stake_store().await?;
        let epoch_settings_pruning_task = self.get_epoch_settings_store().await?;
        let mithril_registerer_pruning_task = self.get_mithril_registerer().await?;

        let upkeep_service = Arc::new(AggregatorUpkeepService::new(
            self.get_sqlite_connection().await?,
            self.get_sqlite_connection_cardano_transaction_pool()
                .await?,
            self.get_event_store_sqlite_connection().await?,
            self.get_signed_entity_lock().await?,
            vec![
                stake_pool_pruning_task,
                epoch_settings_pruning_task,
                mithril_registerer_pruning_task,
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

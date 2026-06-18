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

    /// Build the epoch pruning tasks ordered so that they respect foreign key constraints.
    ///
    /// `signer_registration` references `epoch_setting`, so it must be pruned before
    /// `epoch_setting`, otherwise deleting a still referenced `epoch_setting` row fails the
    /// constraint.
    async fn build_epoch_pruning_tasks(&mut self) -> Result<Vec<Arc<dyn EpochPruningTask>>> {
        let stake_pool_pruning_task = self.get_stake_store().await?;
        let epoch_settings_pruning_task = self.get_epoch_settings_store().await?;
        let signer_registration_pruning_task = self.get_signer_registration_pruning_task().await?;

        let pruning_tasks: Vec<Arc<dyn EpochPruningTask>> = vec![
            stake_pool_pruning_task,
            signer_registration_pruning_task,
            epoch_settings_pruning_task,
        ];

        Ok(pruning_tasks)
    }

    async fn build_upkeep_service(&mut self) -> Result<Arc<dyn UpkeepService>> {
        let upkeep_service = Arc::new(AggregatorUpkeepService::new(
            self.get_sqlite_connection().await?,
            self.get_sqlite_connection_cardano_transaction_pool().await?,
            self.get_event_store_sqlite_connection().await?,
            self.get_signed_entity_type_lock().await?,
            self.build_epoch_pruning_tasks().await?,
            self.root_logger(),
        ));

        Ok(upkeep_service)
    }

    /// Get the [UpkeepService] instance
    pub async fn get_upkeep_service(&mut self) -> Result<Arc<dyn UpkeepService>> {
        get_dependency!(self.upkeep_service)
    }
}

#[cfg(test)]
mod tests {
    use chrono::Utc;

    use mithril_common::entities::Epoch;
    use mithril_common::temp_dir_create;
    use mithril_common::test::double::fake_data;

    use crate::database::record::SignerRecord;
    use crate::database::test_helper::{
        insert_epoch_settings, insert_signer_registrations, insert_signers,
    };
    use crate::{ExecutionEnvironment, ServeCommandConfiguration};

    use super::*;

    #[tokio::test]
    async fn epoch_pruning_tasks_are_ordered_to_respect_foreign_keys() {
        let configuration = ServeCommandConfiguration {
            environment: ExecutionEnvironment::Test,
            store_retention_limit: Some(3),
            ..ServeCommandConfiguration::new_sample(temp_dir_create!())
        };
        let mut builder = DependenciesBuilder::new_with_stdout_logger(Arc::new(configuration));

        let connection = builder.get_sqlite_connection().await.unwrap();
        let signers = fake_data::signers_with_stakes(2);
        insert_signers(
            &connection,
            signers
                .iter()
                .map(|signer| SignerRecord {
                    signer_id: signer.party_id.clone(),
                    pool_ticker: None,
                    created_at: Utc::now(),
                    updated_at: Utc::now(),
                    last_registered_at: None,
                })
                .collect(),
        )
        .unwrap();
        insert_epoch_settings(&connection, &[1, 2, 3, 4, 5, 6]).unwrap();
        let signer_registrations = (3..=6).map(|epoch| (Epoch(epoch), signers.clone())).collect();
        insert_signer_registrations(&connection, signer_registrations).unwrap();

        let pruning_tasks = builder.build_epoch_pruning_tasks().await.unwrap();

        for task in &pruning_tasks {
            task.prune(Epoch(17)).await.unwrap_or_else(|error| {
                panic!(
                    "Pruning '{}' should not fail with a foreign key constraint error: {error:?}",
                    task.pruned_data()
                )
            });
        }
    }
}

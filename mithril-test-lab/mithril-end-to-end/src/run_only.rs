use std::{path::Path, sync::Arc};

use tokio::{fs, task::JoinSet};

use mithril_common::{StdResult, StmAggrSigType};

use crate::{assertions, Aggregator, MithrilInfrastructure};

pub struct RunOnly {
    pub infrastructure: Arc<MithrilInfrastructure>,
}

impl RunOnly {
    pub fn new(infrastructure: Arc<MithrilInfrastructure>) -> Self {
        Self { infrastructure }
    }

    pub async fn run(self) -> StdResult<()> {
        let run_only = Arc::new(self);
        let mut join_set = JoinSet::new();

        for index in 0..run_only.infrastructure.aggregators().len() {
            let run_only_clone = run_only.clone();
            join_set.spawn(async move {
                let infrastructure = &run_only_clone.infrastructure;

                run_only_clone
                    .bootstrap_aggregator(infrastructure.aggregator(index), infrastructure)
                    .await
            });
        }

        while let Some(res) = join_set.join_next().await {
            res??;
        }

        Ok(())
    }

    pub async fn bootstrap_aggregator(
        &self,
        aggregator: &Aggregator,
        infrastructure: &MithrilInfrastructure,
    ) -> StdResult<()> {
        assertions::wait_for_enough_immutable(aggregator).await?;
        let chain_observer = aggregator.chain_observer();
        let start_epoch = chain_observer
            .get_current_epoch()
            .await?
            .unwrap_or_default();

        // Wait 3 epochs after start epoch for the aggregator to be able to bootstrap a genesis certificate
        let target_epoch = start_epoch + 3;
        assertions::wait_for_aggregator_at_target_epoch(
            aggregator,
            target_epoch,
            "minimal epoch for the aggregator to be able to bootstrap genesis certificate"
                .to_string(),
        )
        .await?;
        assertions::bootstrap_genesis_certificate(aggregator).await?;
        assertions::wait_for_epoch_settings(aggregator).await?;

        if aggregator.is_first() {
            // Transfer some funds on the devnet to have some Cardano transactions to sign
            assertions::transfer_funds(infrastructure.devnet()).await?;
        }

        if aggregator.is_first() {
            loop {
                let current_epoch = chain_observer
                    .get_current_epoch()
                    .await?
                    .unwrap_or_default();
                let target_epoch = current_epoch + 1;
                assertions::wait_for_aggregator_at_target_epoch(
                    aggregator,
                    target_epoch,
                    "update the aggregation type on leader aggregator".to_string(),
                )
                .await?;
                let trigger_restart_file = "./restart-aggregator.txt";
                if Path::new(trigger_restart_file).exists() {
                    fs::remove_file(trigger_restart_file).await?;
                    aggregator
                        .set_aggregation_type(StmAggrSigType::StmAggrSigCentralizedTelescopeAlba)
                        .await;
                    aggregator.stop().await?;
                    aggregator.serve().await?;
                }
            }
        }

        Ok(())
    }
}

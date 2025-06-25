use std::sync::Arc;

use tokio::task::JoinSet;

use mithril_common::StdResult;

use crate::{Aggregator, MithrilInfrastructure, assertions};

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

        Ok(())
    }
}

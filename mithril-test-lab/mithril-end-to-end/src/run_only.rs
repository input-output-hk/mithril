use std::sync::Arc;

use mithril_common::StdResult;
use slog_scope::info;

use crate::{MithrilInfrastructure, assertions};

pub struct RunOnly {
    pub infrastructure: Arc<MithrilInfrastructure>,
}

impl RunOnly {
    pub fn new(infrastructure: Arc<MithrilInfrastructure>) -> Self {
        Self { infrastructure }
    }

    pub async fn run(self) -> StdResult<()> {
        let run_only = Arc::new(self);
        info!("Bootstrapping leader aggregator");
        run_only.bootstrap_leader_aggregator(&run_only.infrastructure).await?;

        info!("Starting followers");
        for follower_aggregator in run_only.infrastructure.follower_aggregators() {
            follower_aggregator.serve().await?;
        }

        Ok(())
    }

    pub async fn bootstrap_leader_aggregator(
        &self,
        infrastructure: &MithrilInfrastructure,
    ) -> StdResult<()> {
        let leader_aggregator = infrastructure.leader_aggregator();

        assertions::wait_for_enough_immutable(leader_aggregator).await?;
        let chain_observer = leader_aggregator.chain_observer();
        let start_epoch = chain_observer.get_current_epoch().await?.unwrap_or_default();

        // Wait 3 epochs after start epoch for the aggregator to be able to bootstrap a genesis certificate
        let target_epoch = start_epoch + 3;
        assertions::wait_for_aggregator_at_target_epoch(
            leader_aggregator,
            target_epoch,
            "minimal epoch for the aggregator to be able to bootstrap genesis certificate"
                .to_string(),
        )
        .await?;
        assertions::bootstrap_genesis_certificate(leader_aggregator).await?;
        assertions::wait_for_epoch_settings(leader_aggregator).await?;

        // Transfer some funds on the devnet to have some Cardano transactions to sign
        assertions::transfer_funds(infrastructure.devnet()).await?;

        Ok(())
    }
}

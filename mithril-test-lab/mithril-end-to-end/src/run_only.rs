use std::sync::Arc;

use anyhow::anyhow;
use mithril_common::chain_observer::ChainObserver;
use tokio::sync::RwLock;
use tokio::task::JoinSet;

use mithril_common::StdResult;

use crate::{assertions, Aggregator, MithrilInfrastructure};

pub struct RunOnly {
    pub infrastructure: Arc<RwLock<Option<MithrilInfrastructure>>>,
}

impl RunOnly {
    pub fn new(infrastructure: Arc<RwLock<Option<MithrilInfrastructure>>>) -> Self {
        Self { infrastructure }
    }

    pub async fn run(self) -> StdResult<()> {
        let run_only = Arc::new(self);
        let mut join_set = JoinSet::new();

        let run_only_clone = run_only.clone();
        join_set.spawn(async move { run_only_clone.start_master().await });

        let infrastructure_guard = run_only.infrastructure.read().await;
        let slave_aggregators = infrastructure_guard
            .as_ref()
            .ok_or(anyhow!("No infrastructure found"))?
            .slave_aggregators();
        for index in 0..slave_aggregators.len() {
            let run_only_clone = run_only.clone();
            join_set.spawn(async move { run_only_clone.start_slave(index).await });
        }

        while let Some(res) = join_set.join_next().await {
            res??;
        }

        Ok(())
    }

    pub async fn start_master(&self) -> StdResult<()> {
        let infrastructure_guard = self.infrastructure.read().await;
        let infrastructure = infrastructure_guard
            .as_ref()
            .ok_or(anyhow!("No infrastructure found"))?;

        self.start_aggregator(
            infrastructure.master_aggregator(),
            infrastructure.master_chain_observer(),
            infrastructure,
        )
        .await
    }

    pub async fn start_slave(&self, slave_index: usize) -> StdResult<()> {
        let infrastructure_guard = self.infrastructure.read().await;
        let infrastructure = infrastructure_guard
            .as_ref()
            .ok_or(anyhow!("No infrastructure found"))?;

        self.start_aggregator(
            infrastructure.slave_aggregator(slave_index),
            infrastructure.slave_chain_observer(slave_index),
            infrastructure,
        )
        .await
    }

    pub async fn start_aggregator(
        &self,
        aggregator: &Aggregator,
        chain_observer: Arc<dyn ChainObserver>,
        infrastructure: &MithrilInfrastructure,
    ) -> StdResult<()> {
        assertions::wait_for_enough_immutable(aggregator.db_directory()).await?;
        let start_epoch = chain_observer
            .get_current_epoch()
            .await?
            .unwrap_or_default();

        // Wait 3 epochs after start epoch for the aggregator to be able to bootstrap a genesis certificate
        let target_epoch = start_epoch + 3;
        assertions::wait_for_target_epoch(
            chain_observer,
            target_epoch,
            "minimal epoch for the aggregator to be able to bootstrap genesis certificate"
                .to_string(),
        )
        .await?;
        assertions::bootstrap_genesis_certificate(aggregator).await?;
        assertions::wait_for_epoch_settings(&aggregator.endpoint()).await?;

        if aggregator.index() == 0 {
            // Transfer some funds on the devnet to have some Cardano transactions to sign
            assertions::transfer_funds(infrastructure.devnet()).await?;
        }

        Ok(())
    }
}

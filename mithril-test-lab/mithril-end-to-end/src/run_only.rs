use crate::assertions;
use crate::MithrilInfrastructure;
use mithril_common::chain_observer::ChainObserver;
use mithril_common::StdResult;

pub struct RunOnly<'a> {
    pub infrastructure: &'a mut MithrilInfrastructure,
}

impl<'a> RunOnly<'a> {
    pub fn new(infrastructure: &'a mut MithrilInfrastructure) -> Self {
        Self { infrastructure }
    }

    pub async fn start(&mut self) -> StdResult<()> {
        let aggregator_endpoint = self.infrastructure.aggregator().endpoint();
        assertions::wait_for_enough_immutable(self.infrastructure.aggregator().db_directory())
            .await?;
        let start_epoch = self
            .infrastructure
            .chain_observer()
            .get_current_epoch()
            .await?
            .unwrap_or_default();

        // Wait 3 epochs after start epoch for the aggregator to be able to bootstrap a genesis certificate
        let target_epoch = start_epoch + 3;
        assertions::wait_for_target_epoch(
            self.infrastructure.chain_observer(),
            target_epoch,
            "minimal epoch for the aggregator to be able to bootstrap genesis certificate"
                .to_string(),
        )
        .await?;
        assertions::bootstrap_genesis_certificate(self.infrastructure.aggregator_mut()).await?;
        assertions::wait_for_epoch_settings(&aggregator_endpoint).await?;

        Ok(())
    }
}

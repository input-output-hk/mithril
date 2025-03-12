use crate::assertions;
use crate::MithrilInfrastructure;
use mithril_common::StdResult;

pub struct RunOnly<'a> {
    pub infrastructure: &'a MithrilInfrastructure,
}

impl<'a> RunOnly<'a> {
    pub fn new(infrastructure: &'a MithrilInfrastructure) -> Self {
        Self { infrastructure }
    }

    pub async fn start(&self) -> StdResult<()> {
        let aggregator_endpoint = self.infrastructure.master_aggregator().endpoint();
        assertions::wait_for_enough_immutable(
            self.infrastructure.master_aggregator().db_directory(),
        )
        .await?;
        let start_epoch = self
            .infrastructure
            .master_chain_observer()
            .get_current_epoch()
            .await?
            .unwrap_or_default();

        // Wait 3 epochs after start epoch for the aggregator to be able to bootstrap a genesis certificate
        let target_epoch = start_epoch + 3;
        assertions::wait_for_target_epoch(
            self.infrastructure.master_chain_observer(),
            target_epoch,
            "minimal epoch for the aggregator to be able to bootstrap genesis certificate"
                .to_string(),
        )
        .await?;
        assertions::bootstrap_genesis_certificate(self.infrastructure.master_aggregator()).await?;
        assertions::wait_for_epoch_settings(&aggregator_endpoint).await?;

        // Transfer some funds on the devnet to have some Cardano transactions to sign
        assertions::transfer_funds(self.infrastructure.devnet()).await?;

        Ok(())
    }
}

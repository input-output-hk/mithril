use crate::assertions;
use crate::MithrilInfrastructure;
use mithril_common::chain_observer::ChainObserver;
use slog_scope::info;
use std::{error::Error, thread::sleep, time::Duration};

pub struct RunOnly {
    infrastructure: MithrilInfrastructure,
}

impl RunOnly {
    pub fn new(infrastructure: MithrilInfrastructure) -> Self {
        Self { infrastructure }
    }

    pub async fn run(&mut self) -> Result<(), Box<dyn Error>> {
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
        let mut target_epoch = start_epoch + 3;
        assertions::wait_for_target_epoch(
            self.infrastructure.chain_observer(),
            target_epoch,
            "minimal epoch for the aggregator to be able to bootstrap genesis certificate"
                .to_string(),
        )
        .await?;
        assertions::bootstrap_genesis_certificate(self.infrastructure.aggregator_mut()).await?;
        assertions::wait_for_epoch_settings(&aggregator_endpoint).await?;

        // Wait 2 epochs before changing stake distribution, so that we use at least one original stake distribution
        target_epoch += 2;
        assertions::wait_for_target_epoch(
            self.infrastructure.chain_observer(),
            target_epoch,
            "epoch after which the stake distribution will change".to_string(),
        )
        .await?;
        assertions::delegate_stakes_to_pools(self.infrastructure.devnet()).await?;

        // Wait 2 epochs before changing protocol parameters
        target_epoch += 2;
        assertions::wait_for_target_epoch(
            self.infrastructure.chain_observer(),
            target_epoch,
            "epoch after which the protocol parameters will change".to_string(),
        )
        .await?;
        assertions::update_protocol_parameters(self.infrastructure.aggregator_mut()).await?;

        // Wait 5 epochs after protocol parameters update, so that we make sure that we use new protocol parameters as well as new stake distribution a few times
        target_epoch += 5;
        assertions::wait_for_target_epoch(
            self.infrastructure.chain_observer(),
            target_epoch,
            "epoch after which the certificate chain will be long enough to catch most common troubles with stake distribution and protocol parameters".to_string(),
        )
        .await?;

        loop {
            info!("Mithril end to end is running and will remain active until manually stopped...");
            sleep(Duration::from_secs(5));
        }

        Ok(())
    }

    pub async fn tail_logs(&self, number_of_line: u64) -> Result<(), String> {
        self.infrastructure
            .aggregator()
            .tail_logs(number_of_line)
            .await?;
        for signer in self.infrastructure.signers() {
            signer.tail_logs(number_of_line).await?;
        }

        Ok(())
    }
}

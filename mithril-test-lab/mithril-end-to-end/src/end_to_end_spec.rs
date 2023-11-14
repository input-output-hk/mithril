use crate::assertions;
use crate::MithrilInfrastructure;
use mithril_common::chain_observer::ChainObserver;
use mithril_common::StdResult;

pub struct Spec<'a> {
    pub infrastructure: &'a mut MithrilInfrastructure,
}

impl<'a> Spec<'a> {
    pub fn new(infrastructure: &'a mut MithrilInfrastructure) -> Self {
        Self { infrastructure }
    }

    pub async fn run(&mut self) -> StdResult<()> {
        let aggregator_endpoint = self.infrastructure.aggregator().endpoint();
        assertions::wait_for_enough_immutable(self.infrastructure.aggregator().db_directory())
            .await?;
        let start_epoch = self
            .infrastructure
            .chain_observer()
            .get_current_epoch()
            .await?
            .unwrap_or_default();

        // Wait 4 epochs after start epoch for the aggregator to be able to bootstrap a genesis certificate
        let mut target_epoch = start_epoch + 4;
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

        // Wait 6 epochs after protocol parameters update, so that we make sure that we use new protocol parameters as well as new stake distribution a few times
        target_epoch += 6;
        assertions::wait_for_target_epoch(
            self.infrastructure.chain_observer(),
            target_epoch,
            "epoch after which the certificate chain will be long enough to catch most common troubles with stake distribution and protocol parameters".to_string(),
        )
        .await?;

        // Verify that mithril stake distribution artifacts are produced and signed correctly
        {
            let hash =
                assertions::assert_node_producing_mithril_stake_distribution(&aggregator_endpoint)
                    .await?;
            let certificate_hash = assertions::assert_signer_is_signing_mithril_stake_distribution(
                &aggregator_endpoint,
                &hash,
                target_epoch - 3,
            )
            .await?;
            assertions::assert_is_creating_certificate_with_enough_signers(
                &aggregator_endpoint,
                &certificate_hash,
                self.infrastructure.signers().len(),
            )
            .await?;
            let mut client = self.infrastructure.build_client()?;
            assertions::assert_client_can_verify_mithril_stake_distribution(&mut client, &hash)
                .await?;
        }

        // Verify that snapshot artifacts are produced and signed correctly
        {
            let digest = assertions::assert_node_producing_snapshot(&aggregator_endpoint).await?;
            let certificate_hash = assertions::assert_signer_is_signing_snapshot(
                &aggregator_endpoint,
                &digest,
                target_epoch - 3,
            )
            .await?;

            assertions::assert_is_creating_certificate_with_enough_signers(
                &aggregator_endpoint,
                &certificate_hash,
                self.infrastructure.signers().len(),
            )
            .await?;

            let mut client = self.infrastructure.build_client()?;
            assertions::assert_client_can_verify_snapshot(&mut client, &digest).await?;
        }

        Ok(())
    }
}

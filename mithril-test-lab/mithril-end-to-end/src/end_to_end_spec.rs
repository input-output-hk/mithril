use std::sync::Arc;

use slog_scope::info;
use tokio::task::JoinSet;

use mithril_common::{
    StdResult,
    entities::{Epoch, SignedEntityTypeDiscriminants},
};

use crate::{Aggregator, MithrilInfrastructure, assertions};

pub struct Spec {
    pub infrastructure: Arc<MithrilInfrastructure>,
    is_signing_cardano_transactions: bool,
    is_signing_cardano_stake_distribution: bool,
    is_signing_cardano_database: bool,
    next_era: Option<String>,
    regenesis_on_era_switch: bool,
}

impl Spec {
    pub fn new(
        infrastructure: Arc<MithrilInfrastructure>,
        signed_entity_types: Vec<String>,
        next_era: Option<String>,
        regenesis_on_era_switch: bool,
    ) -> Self {
        Self {
            infrastructure,
            is_signing_cardano_transactions: signed_entity_types.contains(
                &SignedEntityTypeDiscriminants::CardanoTransactions
                    .as_ref()
                    .to_string(),
            ),
            is_signing_cardano_stake_distribution: signed_entity_types.contains(
                &SignedEntityTypeDiscriminants::CardanoStakeDistribution
                    .as_ref()
                    .to_string(),
            ),
            is_signing_cardano_database: signed_entity_types
                .contains(&SignedEntityTypeDiscriminants::CardanoDatabase.as_ref().to_string()),
            next_era,
            regenesis_on_era_switch,
        }
    }

    pub async fn run(self) -> StdResult<()> {
        let mut join_set = JoinSet::new();
        let spec = Arc::new(self);

        // Transfer some funds on the devnet to have some Cardano transactions to sign.
        // This step needs to be executed early in the process so that the transactions are available
        // for signing in the penultimate immutable chunk before the end of the test.
        // As we get closer to the tip of the chain when signing, we'll be able to relax this constraint.
        assertions::transfer_funds(spec.infrastructure.devnet()).await?;

        info!("bootstrapping leader aggregator");
        spec.bootstrap_leader_aggregator(&spec.infrastructure).await?;

        info!("Starting followers");
        for follower_aggregator in spec.infrastructure.follower_aggregators() {
            follower_aggregator.serve().await?;
        }

        info!("Running scenarios");
        for index in 0..spec.infrastructure.aggregators().len() {
            let spec_clone = spec.clone();
            join_set.spawn(async move {
                let infrastructure = &spec_clone.infrastructure;

                spec_clone
                    .run_scenario(infrastructure.aggregator(index), infrastructure)
                    .await
            });
        }

        while let Some(res) = join_set.join_next().await {
            res??;
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

        // Wait 4 epochs after start epoch for the aggregator to be able to bootstrap a genesis certificate
        let mut target_epoch = start_epoch + 4;
        assertions::wait_for_aggregator_at_target_epoch(
            leader_aggregator,
            target_epoch,
            "minimal epoch for the aggregator to be able to bootstrap genesis certificate"
                .to_string(),
        )
        .await?;
        assertions::bootstrap_genesis_certificate(leader_aggregator).await?;
        assertions::wait_for_epoch_settings(leader_aggregator).await?;

        // Wait 2 epochs before changing stake distribution, so that we use at least one original stake distribution
        target_epoch += 2;
        assertions::wait_for_aggregator_at_target_epoch(
            leader_aggregator,
            target_epoch,
            "epoch after which the stake distribution will change".to_string(),
        )
        .await?;

        // Delegate some stakes to pools
        let delegation_round = 1;
        assertions::delegate_stakes_to_pools(infrastructure.devnet(), delegation_round).await?;

        Ok(())
    }

    pub async fn run_scenario(
        &self,
        aggregator: &Aggregator,
        infrastructure: &MithrilInfrastructure,
    ) -> StdResult<()> {
        let chain_observer = aggregator.chain_observer();
        let start_epoch = chain_observer.get_current_epoch().await?.unwrap_or_default();

        // Wait 2 epochs before changing protocol parameters
        let mut target_epoch = start_epoch + 2;
        assertions::wait_for_aggregator_at_target_epoch(
            aggregator,
            target_epoch,
            "epoch after which the protocol parameters will change".to_string(),
        )
        .await?;
        assertions::update_protocol_parameters(aggregator).await?;

        // Wait 6 epochs after protocol parameters update, so that we make sure that we use new protocol parameters as well as new stake distribution a few times
        target_epoch += 6;
        assertions::wait_for_aggregator_at_target_epoch(
            aggregator,
            target_epoch,
            "epoch after which the certificate chain will be long enough to catch most common troubles with stake distribution and protocol parameters".to_string(),
        )
        .await?;

        // Verify that artifacts are produced and signed correctly
        let mut target_epoch = self
            .verify_artifacts_production(target_epoch, aggregator, infrastructure)
            .await?;

        // Verify that artifacts are produced and signed correctly after era switch
        if let Some(next_era) = &self.next_era {
            // Switch to next era
            if aggregator.is_first() {
                infrastructure.register_switch_to_next_era(next_era).await?;
            }
            target_epoch += 5;
            assertions::wait_for_aggregator_at_target_epoch(
                aggregator,
                target_epoch,
                "epoch after which the era switch will have triggered".to_string(),
            )
            .await?;

            // Proceed to a re-genesis of the certificate chain
            if self.regenesis_on_era_switch {
                assertions::bootstrap_genesis_certificate(aggregator).await?;
                target_epoch += 5;
                assertions::wait_for_aggregator_at_target_epoch(
                    aggregator,
                    target_epoch,
                    "epoch after which the re-genesis on era switch will be completed".to_string(),
                )
                .await?;
            }

            // Verify that artifacts are produced and signed correctly
            self.verify_artifacts_production(target_epoch, aggregator, infrastructure)
                .await?;
        }

        Ok(())
    }

    async fn verify_artifacts_production(
        &self,
        target_epoch: Epoch,
        aggregator: &Aggregator,
        infrastructure: &MithrilInfrastructure,
    ) -> StdResult<Epoch> {
        let expected_epoch_min = target_epoch - 3;
        // Verify that mithril stake distribution artifacts are produced and signed correctly
        {
            let hash =
                assertions::assert_node_producing_mithril_stake_distribution(aggregator).await?;
            let certificate_hash = assertions::assert_signer_is_signing_mithril_stake_distribution(
                aggregator,
                &hash,
                expected_epoch_min,
            )
            .await?;
            assertions::assert_is_creating_certificate_with_enough_signers(
                aggregator,
                &certificate_hash,
                infrastructure.signers().len(),
            )
            .await?;
            let mut client = infrastructure.build_client(aggregator).await?;
            assertions::assert_client_can_verify_mithril_stake_distribution(&mut client, &hash)
                .await?;
        }

        // Verify that snapshot artifacts are produced and signed correctly
        {
            let digest = assertions::assert_node_producing_snapshot(aggregator).await?;
            let certificate_hash = assertions::assert_signer_is_signing_snapshot(
                aggregator,
                &digest,
                expected_epoch_min,
            )
            .await?;

            assertions::assert_is_creating_certificate_with_enough_signers(
                aggregator,
                &certificate_hash,
                infrastructure.signers().len(),
            )
            .await?;

            let mut client = infrastructure.build_client(aggregator).await?;
            assertions::assert_client_can_verify_snapshot(&mut client, &digest).await?;
        }

        // Verify that Cardano database snapshot artifacts are produced and signed correctly
        if self.is_signing_cardano_database {
            let hash =
                assertions::assert_node_producing_cardano_database_snapshot(aggregator).await?;
            let certificate_hash = assertions::assert_signer_is_signing_cardano_database_snapshot(
                aggregator,
                &hash,
                expected_epoch_min,
            )
            .await?;

            assertions::assert_is_creating_certificate_with_enough_signers(
                aggregator,
                &certificate_hash,
                infrastructure.signers().len(),
            )
            .await?;

            assertions::assert_node_producing_cardano_database_digests_map(aggregator).await?;

            let mut client = infrastructure.build_client(aggregator).await?;
            assertions::assert_client_can_verify_cardano_database(&mut client, &hash).await?;
        }

        // Verify that Cardano transactions artifacts are produced and signed correctly
        if self.is_signing_cardano_transactions {
            let hash = assertions::assert_node_producing_cardano_transactions(aggregator).await?;
            let certificate_hash = assertions::assert_signer_is_signing_cardano_transactions(
                aggregator,
                &hash,
                expected_epoch_min,
            )
            .await?;

            assertions::assert_is_creating_certificate_with_enough_signers(
                aggregator,
                &certificate_hash,
                infrastructure.signers().len(),
            )
            .await?;

            let transaction_hashes =
                infrastructure.devnet().mithril_payments_transaction_hashes()?;
            let mut client = infrastructure.build_client(aggregator).await?;
            assertions::assert_client_can_verify_transactions(&mut client, transaction_hashes)
                .await?;
        }

        // Verify that Cardano stake distribution artifacts are produced and signed correctly
        if self.is_signing_cardano_stake_distribution {
            {
                let (hash, epoch) =
                    assertions::assert_node_producing_cardano_stake_distribution(aggregator)
                        .await?;
                let certificate_hash =
                    assertions::assert_signer_is_signing_cardano_stake_distribution(
                        aggregator,
                        &hash,
                        expected_epoch_min,
                    )
                    .await?;
                assertions::assert_is_creating_certificate_with_enough_signers(
                    aggregator,
                    &certificate_hash,
                    infrastructure.signers().len(),
                )
                .await?;

                let mut client = infrastructure.build_client(aggregator).await?;
                assertions::assert_client_can_verify_cardano_stake_distribution(
                    &mut client,
                    &hash,
                    epoch,
                )
                .await?;
            }
        }

        Ok(target_epoch)
    }
}

use slog_scope::info;

use mithril_common::{
    StdResult, entities::Epoch, messages::CardanoStakeDistributionListItemMessage,
};

use crate::{
    Aggregator, CardanoStakeDistributionCommand, Client, ClientCommand,
    toolkit::{CheckCertificateToolkit, ScenarioToolkitContext},
};

use super::utils;

#[derive(Debug, Clone)]
pub struct CheckCardanoStakeDistributionToolkit {
    context: ScenarioToolkitContext,
}

impl CheckCardanoStakeDistributionToolkit {
    pub fn new(context: ScenarioToolkitContext) -> Self {
        Self { context }
    }

    pub async fn is_certified_and_verified(
        &self,
        aggregator: &Aggregator,
        client: &mut Client,
        expected_epoch_min: Epoch,
        total_signers_expected: usize,
    ) -> StdResult<()> {
        let certificate_toolkit = CheckCertificateToolkit::new(self.context.clone());

        let artifact = self.wait_for_artifact(aggregator, expected_epoch_min).await?;
        certificate_toolkit
            .is_creating_certificate_with_enough_signers(
                aggregator,
                &artifact.certificate_hash,
                total_signers_expected,
            )
            .await?;
        self.verify_with_client(client, &artifact.hash, artifact.epoch)
            .await?;

        Ok(())
    }

    pub async fn wait_for_artifact(
        &self,
        aggregator: &Aggregator,
        expected_epoch_min: Epoch,
    ) -> StdResult<CardanoStakeDistributionListItemMessage> {
        utils::wait_for_latest_artifact_with_condition(
            "Cardano stake distribution",
            "/artifact/cardano-stake-distributions",
            |a: &CardanoStakeDistributionListItemMessage| a.hash.clone(),
            &self.context,
            aggregator,
            |a| a.epoch >= expected_epoch_min,
            |last_invalid_artifact| {
                format!(
                    "no Cardano stake distribution found with epoch >= {expected_epoch_min}\nlast artifact: {last_invalid_artifact:?}"
                )
            },
        )
        .await
    }

    pub async fn verify_with_client(
        &self,
        client: &mut Client,
        hash: &str,
        epoch: Epoch,
    ) -> StdResult<()> {
        client
            .run(ClientCommand::CardanoStakeDistribution(
                CardanoStakeDistributionCommand::Download {
                    unique_identifier: epoch.to_string(),
                },
            ))
            .await?;
        info!("Client downloaded the Cardano stake distribution by epoch"; "epoch" => epoch.to_string());

        client
            .run(ClientCommand::CardanoStakeDistribution(
                CardanoStakeDistributionCommand::Download {
                    unique_identifier: hash.to_string(),
                },
            ))
            .await?;
        info!("Client downloaded the Cardano stake distribution by hash"; "hash" => hash.to_string());

        Ok(())
    }
}

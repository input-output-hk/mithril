use slog_scope::info;

use mithril_common::{
    StdResult, entities::Epoch, messages::MithrilStakeDistributionListItemMessage,
};

use crate::{
    Aggregator, Client, ClientCommand, MithrilStakeDistributionCommand,
    toolkit::{CheckCertificateToolkit, ScenarioToolkitContext},
};

use super::utils;

#[derive(Debug, Clone)]
pub struct CheckMithrilStakeDistributionToolkit {
    context: ScenarioToolkitContext,
}

impl CheckMithrilStakeDistributionToolkit {
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
        self.verify_with_client(client, &artifact.hash).await?;

        Ok(())
    }

    pub async fn wait_for_artifact(
        &self,
        aggregator: &Aggregator,
        expected_epoch_min: Epoch,
    ) -> StdResult<MithrilStakeDistributionListItemMessage> {
        utils::wait_for_latest_artifact_with_condition(
            "Mithril stake distribution",
            "/artifact/mithril-stake-distributions",
            |a: &MithrilStakeDistributionListItemMessage| a.hash.clone(),
            &self.context,
            aggregator,
            |a| a.epoch >= expected_epoch_min,
            |last_invalid_artifact| {
                format!(
                    "no Mithril stake distribution found with epoch >= {expected_epoch_min}\nlast artifact: {last_invalid_artifact:?}"
                )
            },
        )
        .await
    }

    pub async fn verify_with_client(&self, client: &mut Client, hash: &str) -> StdResult<()> {
        client
            .run(ClientCommand::MithrilStakeDistribution(
                MithrilStakeDistributionCommand::Download {
                    hash: hash.to_owned(),
                },
            ))
            .await?;
        info!("Client downloaded the Mithril stake distribution"; "hash" => &hash);

        Ok(())
    }
}

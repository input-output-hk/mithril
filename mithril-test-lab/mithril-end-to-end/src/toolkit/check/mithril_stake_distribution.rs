use slog_scope::info;

use mithril_common::{
    StdResult, entities::Epoch, messages::MithrilStakeDistributionListItemMessage,
};

use crate::{
    Aggregator, Client, ClientCommand, MithrilStakeDistributionCommand,
    toolkit::{CheckCertificateToolkit, ScenarioToolkitContext},
};

use super::utils;

#[derive(Debug, Clone, Default)]
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

        let artifact = self.wait_for_artifact(aggregator).await?;
        self.check_artifact(&artifact, expected_epoch_min)?;
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
    ) -> StdResult<MithrilStakeDistributionListItemMessage> {
        utils::wait_for_latest_artifact::<MithrilStakeDistributionListItemMessage>(
            "Mithril stake distribution",
            "/artifact/mithril-stake-distributions",
            |a| a.hash.clone(),
            &self.context,
            aggregator,
        )
        .await
    }

    pub fn check_artifact(
        &self,
        artifact: &MithrilStakeDistributionListItemMessage,
        expected_epoch_min: Epoch,
    ) -> StdResult<()> {
        utils::assert_minimal_epoch(artifact, |a| a.epoch, expected_epoch_min)
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

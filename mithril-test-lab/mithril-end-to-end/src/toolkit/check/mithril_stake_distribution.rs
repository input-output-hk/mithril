use anyhow::{Context, anyhow};
use slog_scope::info;
use std::time::Duration;

use mithril_common::{
    StdResult,
    entities::Epoch,
    messages::{MithrilStakeDistributionListMessage, MithrilStakeDistributionMessage},
};

use crate::{
    Aggregator, Client, ClientCommand, MithrilStakeDistributionCommand, attempt,
    toolkit::{ScenarioToolkitContext, check::get_json_response},
    utils::AttemptResult,
};

#[derive(Debug, Clone, Default)]
pub struct CheckMithrilStakeDistributionToolkit {
    context: ScenarioToolkitContext,
}

impl CheckMithrilStakeDistributionToolkit {
    pub fn new(context: ScenarioToolkitContext) -> Self {
        Self { context }
    }

    pub async fn node_producing_mithril_stake_distribution(
        &self,
        aggregator: &Aggregator,
    ) -> StdResult<String> {
        let url = format!(
            "{}/artifact/mithril-stake-distributions",
            aggregator.endpoint()
        );
        info!("Waiting for the aggregator to produce a mithril stake distribution"; "aggregator" => &aggregator.name());

        async fn fetch_last_mithril_stake_distribution_hash(
            url: String,
        ) -> StdResult<Option<String>> {
            match get_json_response::<MithrilStakeDistributionListMessage>(url)
                .await?
                .as_deref()
            {
                Ok([stake_distribution, ..]) => Ok(Some(stake_distribution.hash.clone())),
                Ok(&[]) => Ok(None),
                Err(err) => Err(anyhow!("Invalid mithril stake distribution body: {err}",)),
            }
        }

        match attempt!(30, Duration::from_secs(3), {
        fetch_last_mithril_stake_distribution_hash(url.clone()).await
    }) {
            AttemptResult::Ok(hash) => {
                info!("Aggregator produced a mithril stake distribution"; "hash" => &hash, "aggregator" => &aggregator.name());
                Ok(hash)
            }
            AttemptResult::Err(error) => Err(error),
            AttemptResult::Timeout() => Err(anyhow!(
            "Timeout exhausted assert_node_producing_mithril_stake_distribution, no response from `{url}`"
        )),
        }.with_context(|| {
            format!(
                "Requesting aggregator `{}`",
                aggregator.name()
            )
        })
    }

    pub async fn signer_is_signing_mithril_stake_distribution(
        &self,
        aggregator: &Aggregator,
        hash: &str,
        expected_epoch_min: Epoch,
    ) -> StdResult<String> {
        let url = format!(
            "{}/artifact/mithril-stake-distribution/{hash}",
            aggregator.endpoint()
        );
        info!(
            "Asserting the aggregator is signing the mithril stake distribution message `{}` with an expected min epoch of `{}`",
            hash,
            expected_epoch_min;
            "aggregator" => &aggregator.name()
        );

        async fn fetch_mithril_stake_distribution_message(
            url: String,
            expected_epoch_min: Epoch,
        ) -> StdResult<Option<MithrilStakeDistributionMessage>> {
            match get_json_response::<MithrilStakeDistributionMessage>(url.clone()).await? {
                Ok(stake_distribution) => match stake_distribution.epoch {
                    epoch if epoch >= expected_epoch_min => Ok(Some(stake_distribution)),
                    epoch => Err(anyhow!(
                        "Minimum expected mithril stake distribution epoch not reached: {epoch} < {expected_epoch_min}"
                    )),
                },
                Err(err) => Err(anyhow!("Invalid mithril stake distribution body: {err}",)),
            }
        }

        match attempt!(10, Duration::from_millis(1000), {
        fetch_mithril_stake_distribution_message(url.clone(), expected_epoch_min).await
    }) {
            AttemptResult::Ok(stake_distribution) => {
                info!("Signer signed a mithril stake distribution"; "certificate_hash" => &stake_distribution.certificate_hash, "aggregator" => &aggregator.name());
                Ok(stake_distribution.certificate_hash)
            }
            AttemptResult::Err(error) => Err(error),
            AttemptResult::Timeout() => Err(anyhow!(
            "Timeout exhausted assert_signer_is_signing_mithril_stake_distribution, no response from `{url}`"
        )),
        }.with_context(|| {
            format!(
                "Requesting aggregator `{}`",
                aggregator.name()
            )
        })
    }

    pub async fn client_can_verify_mithril_stake_distribution(
        &self,
        client: &mut Client,
        hash: &str,
    ) -> StdResult<()> {
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

use anyhow::{Context, anyhow};
use slog_scope::info;
use std::time::Duration;

use mithril_common::{
    StdResult,
    entities::Epoch,
    messages::{CardanoStakeDistributionListMessage, CardanoStakeDistributionMessage},
};

use crate::{
    Aggregator, CardanoStakeDistributionCommand, Client, ClientCommand, attempt,
    toolkit::{ScenarioToolkitContext, check::get_json_response},
    utils::AttemptResult,
};

#[derive(Debug, Clone, Default)]
pub struct CheckCardanoStakeDistributionToolkit {
    context: ScenarioToolkitContext,
}

impl CheckCardanoStakeDistributionToolkit {
    pub fn new(context: ScenarioToolkitContext) -> Self {
        Self { context }
    }

    pub async fn node_producing_cardano_stake_distribution(
        &self,
        aggregator: &Aggregator,
    ) -> StdResult<(String, Epoch)> {
        let url = format!(
            "{}/artifact/cardano-stake-distributions",
            aggregator.endpoint()
        );
        info!("Waiting for the aggregator to produce a Cardano stake distribution"; "aggregator" => &aggregator.name());

        async fn fetch_last_cardano_stake_distribution_message(
            url: String,
        ) -> StdResult<Option<(String, Epoch)>> {
            match get_json_response::<CardanoStakeDistributionListMessage>(url)
                .await?
                .as_deref()
            {
                Ok([stake_distribution, ..]) => Ok(Some((
                    stake_distribution.hash.clone(),
                    stake_distribution.epoch,
                ))),
                Ok(&[]) => Ok(None),
                Err(err) => Err(anyhow!("Invalid Cardano stake distribution body: {err}",)),
            }
        }

        match attempt!(30, Duration::from_millis(2000), {
        fetch_last_cardano_stake_distribution_message(url.clone()).await
    }) {
            AttemptResult::Ok((hash, epoch)) => {
                info!("Aggregator produced a Cardano stake distribution"; "hash" => &hash, "epoch" => #?epoch, "aggregator" => &aggregator.name());
                Ok((hash, epoch))
            }
            AttemptResult::Err(error) => Err(error),
            AttemptResult::Timeout() => Err(anyhow!(
            "Timeout exhausted assert_node_producing_cardano_stake_distribution, no response from `{url}`"
        )),
        }.with_context(|| {
            format!(
                "Requesting aggregator `{}`",
                aggregator.name()
            )
        })
    }

    pub async fn signer_is_signing_cardano_stake_distribution(
        &self,
        aggregator: &Aggregator,
        hash: &str,
        expected_epoch_min: Epoch,
    ) -> StdResult<String> {
        let url = format!(
            "{}/artifact/cardano-stake-distribution/{hash}",
            aggregator.endpoint()
        );
        info!(
            "Asserting the aggregator is signing the Cardano stake distribution message `{}` with an expected min epoch of `{}`",
            hash,
            expected_epoch_min;
            "aggregator" => &aggregator.name()
        );

        async fn fetch_cardano_stake_distribution_message(
            url: String,
            expected_epoch_min: Epoch,
        ) -> StdResult<Option<CardanoStakeDistributionMessage>> {
            match get_json_response::<CardanoStakeDistributionMessage>(url).await? {
                Ok(stake_distribution) => match stake_distribution.epoch {
                    epoch if epoch >= expected_epoch_min => Ok(Some(stake_distribution)),
                    epoch => Err(anyhow!(
                        "Minimum expected Cardano stake distribution epoch not reached: {epoch} < {expected_epoch_min}"
                    )),
                },
                Err(err) => Err(anyhow!(err).context("Invalid Cardano stake distribution body")),
            }
        }

        match attempt!(10, Duration::from_millis(1000), {
        fetch_cardano_stake_distribution_message(url.clone(), expected_epoch_min).await
    }) {
            AttemptResult::Ok(cardano_stake_distribution) => {
                info!("Signer signed a Cardano stake distribution"; "certificate_hash" => &cardano_stake_distribution.certificate_hash, "aggregator" => &aggregator.name());
                Ok(cardano_stake_distribution.certificate_hash)
            }
            AttemptResult::Err(error) => Err(error),
            AttemptResult::Timeout() => Err(anyhow!(
            "Timeout exhausted assert_signer_is_signing_cardano_stake_distribution, no response from `{url}`"
        )),
        }.with_context(|| {
            format!(
                "Requesting aggregator `{}`",
                aggregator.name()
            )
        })
    }

    pub async fn client_can_verify_cardano_stake_distribution(
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

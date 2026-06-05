use anyhow::{Context, anyhow};
use slog_scope::{info, warn};
use std::time::Duration;

use mithril_common::{
    StdResult,
    entities::{Epoch, EpochSpecifier},
    messages::{
        CardanoDatabaseDigestListMessage, CardanoDatabaseSnapshotListMessage,
        CardanoDatabaseSnapshotMessage,
    },
};

use crate::{
    Aggregator, CardanoDbV2Command, Client, ClientCommand, attempt,
    toolkit::{ScenarioToolkitContext, check::get_json_response},
    utils::AttemptResult,
};

#[derive(Debug, Clone, Default)]
pub struct CheckCardanoDatabaseToolkit {
    context: ScenarioToolkitContext,
}

impl CheckCardanoDatabaseToolkit {
    pub fn new(context: ScenarioToolkitContext) -> Self {
        Self { context }
    }

    pub async fn node_producing_cardano_database_snapshot(
        &self,
        aggregator: &Aggregator,
    ) -> StdResult<String> {
        let url = format!("{}/artifact/cardano-database", aggregator.endpoint());
        info!("Waiting for the aggregator to produce a Cardano database snapshot"; "aggregator" => &aggregator.name());

        async fn fetch_last_cardano_database_snapshot_hash(
            url: String,
        ) -> StdResult<Option<String>> {
            match get_json_response::<CardanoDatabaseSnapshotListMessage>(url)
                .await?
                .as_deref()
            {
                Ok([cardano_database_snapshot, ..]) => {
                    Ok(Some(cardano_database_snapshot.hash.clone()))
                }
                Ok(&[]) => Ok(None),
                Err(err) => Err(anyhow!("Invalid Cardano database snapshot body: {err}",)),
            }
        }

        match attempt!(30, Duration::from_millis(2000), {
        fetch_last_cardano_database_snapshot_hash(url.clone()).await
    }) {
            AttemptResult::Ok(hash) => {
                info!("Aggregator produced a Cardano database snapshot"; "hash" => &hash, "aggregator" => &aggregator.name());
                Ok(hash)
            }
            AttemptResult::Err(error) => Err(error),
            AttemptResult::Timeout() => Err(anyhow!(
            "Timeout exhausted assert_node_producing_snapshot, no response from `{url}`"
        )),
        }
            .with_context(|| format!("Requesting aggregator `{}`", aggregator.name()))
    }

    pub async fn signer_is_signing_cardano_database_snapshot(
        &self,
        aggregator: &Aggregator,
        hash: &str,
        expected_epoch_min: Epoch,
    ) -> StdResult<String> {
        let url = format!("{}/artifact/cardano-database/{hash}", aggregator.endpoint());
        info!(
            "Asserting the aggregator is signing the Cardano database snapshot message `{}` with an expected min epoch of `{}`",
            hash,
            expected_epoch_min;
            "aggregator" => &aggregator.name()
        );

        async fn fetch_cardano_database_snapshot_message(
            url: String,
            expected_epoch_min: Epoch,
        ) -> StdResult<Option<CardanoDatabaseSnapshotMessage>> {
            match get_json_response::<CardanoDatabaseSnapshotMessage>(url).await? {
                Ok(cardano_database_snapshot) => match cardano_database_snapshot.beacon.epoch {
                    epoch if epoch >= expected_epoch_min => Ok(Some(cardano_database_snapshot)),
                    epoch => Err(anyhow!(
                        "Minimum expected Cardano database snapshot epoch not reached: {epoch} < {expected_epoch_min}"
                    )),
                },
                Err(err) => Err(anyhow!(err).context("Invalid Cardano database snapshot body")),
            }
        }

        match attempt!(10, Duration::from_millis(1000), {
        fetch_cardano_database_snapshot_message(url.clone(), expected_epoch_min).await
    }) {
            AttemptResult::Ok(snapshot) => {
                info!("Signer signed a snapshot"; "certificate_hash" => &snapshot.certificate_hash, "aggregator" => &aggregator.name());
                Ok(snapshot.certificate_hash)
            }
            AttemptResult::Err(error) => Err(error),
            AttemptResult::Timeout() => Err(anyhow!(
            "Timeout exhausted assert_signer_is_signing_snapshot, no response from `{url}`"
        )),
        }
            .with_context(|| format!("Requesting aggregator `{}`", aggregator.name()))
    }

    pub async fn node_producing_cardano_database_digests_map(
        &self,
        aggregator: &Aggregator,
    ) -> StdResult<Vec<(String, String)>> {
        let url = format!(
            "{}/artifact/cardano-database/digests",
            aggregator.endpoint()
        );
        info!("Waiting for the aggregator to produce a Cardano database digests map"; "aggregator" => &aggregator.name());

        async fn fetch_cardano_database_digests_map(
            url: String,
        ) -> StdResult<Option<Vec<(String, String)>>> {
            match get_json_response::<CardanoDatabaseDigestListMessage>(url)
                .await?
                .as_deref()
            {
                Ok(&[]) => Ok(None),
                Ok(cardano_database_digests_map) => Ok(Some(
                    cardano_database_digests_map
                        .iter()
                        .map(|item| (item.immutable_file_name.clone(), item.digest.clone()))
                        .collect(),
                )),
                Err(err) => Err(anyhow!("Invalid Cardano database digests map body: {err}",)),
            }
        }

        match attempt!(30, Duration::from_millis(2000), {
            fetch_cardano_database_digests_map(url.clone()).await
        }) {
            AttemptResult::Ok(cardano_database_digests_map) => {
                info!("Aggregator produced a Cardano database digests map"; "total_digests" => &cardano_database_digests_map.len(), "aggregator" => &aggregator.name());
                Ok(cardano_database_digests_map)
            }
            AttemptResult::Err(error) => Err(error),
            AttemptResult::Timeout() => Err(anyhow!(
            "Timeout exhausted assert_node_producing_cardano_database_digests_map, no response from `{url}`"
        )),
        }.with_context(|| {
            format!(
                "Requesting aggregator `{}`",
                aggregator.name()
            )
        })
    }

    pub async fn client_can_verify_cardano_database(
        &self,
        client: &mut Client,
        hash: &str,
    ) -> StdResult<()> {
        client
            .run(ClientCommand::CardanoDbV2(CardanoDbV2Command::List))
            .await?;

        if client.version().is_above_or_equal("0.12.34") {
            client
                .run(ClientCommand::CardanoDbV2(
                    CardanoDbV2Command::ListPerEpoch {
                        epoch_specifier: EpochSpecifier::LatestMinusOffset(5),
                    },
                ))
                .await?;
        } else {
            warn!(
                "Client version is below 0.12.34, skipping `cardano-db snapshot list --epoch latest-5` check"
            );
        }

        client
            .run(ClientCommand::CardanoDbV2(CardanoDbV2Command::Show {
                hash: hash.to_string(),
            }))
            .await?;
        info!("Client list & show the cardano database snapshot"; "hash" => &hash);

        client
            .run(ClientCommand::CardanoDbV2(CardanoDbV2Command::Download {
                hash: hash.to_string(),
            }))
            .await?;
        info!("Client downloaded & restored the cardano database snapshot"; "hash" => &hash);

        Ok(())
    }
}

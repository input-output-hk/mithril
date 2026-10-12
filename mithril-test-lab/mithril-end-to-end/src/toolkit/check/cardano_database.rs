use anyhow::{Context, anyhow};
use reqwest::Url;
use slog_scope::{info, warn};

use mithril_common::{
    StdResult,
    entities::{Epoch, EpochSpecifier, ImmutableFileNumber},
    messages::{CardanoDatabaseDigestListMessage, CardanoDatabaseSnapshotListItemMessage},
};

use crate::{
    Aggregator, CardanoDbV2Command, Client, ClientCommand, KuboNode,
    mithril::IPNS_KEY_NAME_FOR_SNAPSHOT,
    poll_until,
    toolkit::{CheckCertificateToolkit, ScenarioToolkitContext},
    utils::AttemptResult,
};

use super::utils;

#[derive(Debug, Clone)]
pub struct CheckCardanoDatabaseToolkit {
    context: ScenarioToolkitContext,
}

impl CheckCardanoDatabaseToolkit {
    pub fn new(context: ScenarioToolkitContext) -> Self {
        Self { context }
    }

    pub async fn is_certified_and_verified(
        &self,
        aggregator: &Aggregator,
        client: &mut Client,
        client_ipfs_node: Option<&KuboNode>,
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
        self.node_producing_cardano_database_digests_map(aggregator).await?;
        self.verify_with_client(
            client,
            client_ipfs_node.map(|n| n.rpc_url.clone()),
            &artifact.hash,
        )
        .await?;

        if let Some(client_kubo_node) = client_ipfs_node {
            let aggregator_kubo_node = aggregator.ipfs_kubo_node().with_context(|| {
                format!(
                    "IPFS devnet is enabled but {} has no Kubo node",
                    aggregator.name()
                )
            })?;
            self.check_immutable_published_to_ipns(
                aggregator,
                aggregator_kubo_node,
                client_kubo_node,
                artifact.beacon.immutable_file_number,
            )
            .await?;
        }

        Ok(())
    }

    pub async fn wait_for_artifact(
        &self,
        aggregator: &Aggregator,
        expected_epoch_min: Epoch,
    ) -> StdResult<CardanoDatabaseSnapshotListItemMessage> {
        utils::wait_for_latest_artifact_with_condition(
            "Cardano database snapshot",
            "/artifact/cardano-database",
            |a: &CardanoDatabaseSnapshotListItemMessage| a.hash.clone(),
            &self.context,
            aggregator,
            |a| a.beacon.epoch >= expected_epoch_min,
            |last_invalid_artifact| {
                format!(
                    "no Cardano database snapshot found with epoch >= {expected_epoch_min}\nlast artifact: {last_invalid_artifact:?}"
                )
            },
        )
        .await
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
            match utils::get_json_response::<CardanoDatabaseDigestListMessage>(url)
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

        match poll_until!(self.context.artifact_production_timeout(), self.context.poll_backoff(), {
            fetch_cardano_database_digests_map(url.clone()).await
        }) {
            AttemptResult::Ok(cardano_database_digests_map) => {
                info!("Aggregator produced a Cardano database digests map"; "total_digests" => &cardano_database_digests_map.len(), "aggregator" => &aggregator.name());
                Ok(cardano_database_digests_map)
            }
            AttemptResult::Err(error) => Err(error),
            AttemptResult::Timeout(..) => Err(anyhow!(
            "Timeout exhausted assert_node_producing_cardano_database_digests_map, no response from `{url}`"
        )),
        }.with_context(|| {
            format!(
                "Requesting aggregator `{}`",
                aggregator.name()
            )
        })
    }

    pub async fn verify_with_client(
        &self,
        client: &mut Client,
        ipfs_rpc_url: Option<Url>,
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
                ipfs_rpc_url: None,
            }))
            .await?;
        info!("Client downloaded & restored the cardano database snapshot"; "hash" => &hash, "storage_source" => "local");

        if ipfs_rpc_url.is_some() {
            client
                .run(ClientCommand::CardanoDbV2(CardanoDbV2Command::Download {
                    hash: hash.to_string(),
                    ipfs_rpc_url,
                }))
                .await?;
            info!("Client downloaded & restored the cardano database snapshot"; "hash" => &hash, "storage_source" => "ipfs");
        }

        Ok(())
    }

    pub async fn check_immutable_published_to_ipns(
        &self,
        aggregator: &Aggregator,
        aggregator_kubo_node: &KuboNode,
        client_kubo_node: &KuboNode,
        immutable_file_number: ImmutableFileNumber,
    ) -> StdResult<()> {
        info!(
            "Checking IPNS publication of immutable file #{immutable_file_number}";
            "aggregator" => &aggregator.name(),
        );

        let ipns_key_id = aggregator_kubo_node
            .get_ipns_key_id(IPNS_KEY_NAME_FOR_SNAPSHOT)
            .await?
            .with_context(|| {
                format!(
                    "Failed to get IPNS key ID for key '{IPNS_KEY_NAME_FOR_SNAPSHOT}' and {}",
                    aggregator.name()
                )
            })?;
        info!("IPNS key ID for key '{IPNS_KEY_NAME_FOR_SNAPSHOT}': '{ipns_key_id}'"; "aggregator" => &aggregator.name());

        // Note: we could use `ipfs resolve` directly with the ipns key id, but this would make the
        // query prone to cache issues since this request does not allow ignoring the cache (contrary
        // to `ipfs name resolve` which has a `--nocache` option).
        let ipfs_dir_path = client_kubo_node
            .ipns_name_resolve(&format!("/ipns/{ipns_key_id}"))
            .await?;
        info!("'/ipns/{ipns_key_id}' resolves to: '{ipfs_dir_path}'"; "aggregator" => &aggregator.name());

        let ipfs_path_for_immutable =
            format!("{ipfs_dir_path}/{immutable_file_number:0>5}.tar.zst");
        client_kubo_node.ipfs_resolve(&ipfs_path_for_immutable).await?;
        info!(
            "Success, '/ipns/{ipns_key_id}' contains immutable {immutable_file_number:0>5}";
            "aggregator" => &aggregator.name()
        );

        Ok(())
    }
}

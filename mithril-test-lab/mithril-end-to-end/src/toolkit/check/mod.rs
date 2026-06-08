mod cardano_blocks_transactions;
mod cardano_database;
mod cardano_stake_distribution;
mod cardano_transactions;
mod certificate;
mod mithril_stake_distribution;

pub use cardano_blocks_transactions::*;
pub use cardano_database::*;
pub use cardano_stake_distribution::*;
pub use cardano_transactions::*;
pub use certificate::*;
pub use mithril_stake_distribution::*;

use std::{path::PathBuf, time::Duration};

use anyhow::{Context, anyhow};
use reqwest::StatusCode;
use serde::de::DeserializeOwned;
use slog_scope::{info, warn};

use mithril_common::{StdResult, entities::Epoch};

use crate::{
    Aggregator, Client, ClientCommand, FullNode, NodeVersion, ToolsCommand, UtxoHdCommand, attempt,
    toolkit::ScenarioToolkitContext,
    utils::{AttemptResult, file_utils::copy_dir_all},
};

async fn get_json_response<T: DeserializeOwned>(url: String) -> StdResult<reqwest::Result<T>> {
    match reqwest::get(url.clone()).await {
        Ok(response) => {
            let r = response.status();
            match r {
                StatusCode::OK => Ok(response.json::<T>().await),
                s => Err(anyhow!("Unexpected status code from Aggregator: {s}")),
            }
        }
        Err(err) => Err(anyhow!(err).context(format!("Request to `{url}` failed"))),
    }
}

/// Wait until the aggregator produces an artifact, returning the latest one
///
/// Note: the `artifact_list_url` must start with a `/`
async fn wait_for_artifact<T: DeserializeOwned>(
    artifact_name: &str,
    artifact_list_url: &str,
    hash_extractor: fn(&T) -> String,
    _context: &ScenarioToolkitContext,
    aggregator: &Aggregator,
) -> StdResult<T> {
    let url = format!("{}{artifact_list_url}", aggregator.endpoint());
    info!("Waiting for the aggregator to produce a {artifact_name} artifact"; "aggregator" => &aggregator.name());

    async fn fetch_last_artifact<T: DeserializeOwned>(
        artifact_name: &str,
        url: String,
    ) -> StdResult<Option<T>> {
        match get_json_response::<Vec<T>>(url).await? {
            // Artifact lists are sorted from newest to oldest, so the first item is the latest
            Ok(list) => Ok(list.into_iter().next()),
            Err(err) => Err(anyhow!("Invalid {artifact_name} artifact body: {err}",)),
        }
    }

    match attempt!(30, Duration::from_millis(2000), {
        fetch_last_artifact(artifact_name, url.clone()).await
    }) {
        AttemptResult::Ok(last_artifact) => {
            info!("Aggregator produced a {artifact_name} artifact"; "hash" => hash_extractor(&last_artifact), "aggregator" => &aggregator.name());
            Ok(last_artifact)
        }
        AttemptResult::Err(error) => Err(error),
        AttemptResult::Timeout() => Err(anyhow!(
            "Timeout exhausted waiting for {artifact_name}, no response from `{url}`"
        )),
    }
        .with_context(|| format!("Requesting aggregator `{}`", aggregator.name()))
}

fn assert_minimal_epoch<T>(
    artifact: &T,
    epoch_extractor: fn(&T) -> Epoch,
    expected_epoch_min: Epoch,
) -> StdResult<()> {
    match epoch_extractor(artifact) {
        epoch if epoch >= expected_epoch_min => Ok(()),
        epoch => Err(anyhow!(
            "Minimum expected artifact epoch not reached: {epoch} < {expected_epoch_min}"
        )),
    }
}

#[derive(Debug, Clone, Default)]
pub struct CheckToolkit {
    pub cardano_blocks_transactions: CheckCardanoBlocksTransactionsToolkit,
    pub cardano_database: CheckCardanoDatabaseToolkit,
    pub cardano_stake_distribution: CheckCardanoStakeDistributionToolkit,
    pub cardano_transactions: CheckCardanoTransactionsToolkit,
    pub certificate: CheckCertificateToolkit,
    pub mithril_stake_distribution: CheckMithrilStakeDistributionToolkit,
}

impl CheckToolkit {
    pub fn new(context: ScenarioToolkitContext) -> Self {
        Self {
            cardano_blocks_transactions: CheckCardanoBlocksTransactionsToolkit::new(
                context.clone(),
            ),
            cardano_database: CheckCardanoDatabaseToolkit::new(context.clone()),
            cardano_stake_distribution: CheckCardanoStakeDistributionToolkit::new(context.clone()),
            cardano_transactions: CheckCardanoTransactionsToolkit::new(context.clone()),
            certificate: CheckCertificateToolkit::new(context.clone()),
            mithril_stake_distribution: CheckMithrilStakeDistributionToolkit::new(context.clone()),
        }
    }

    pub async fn client_can_convert_the_ledger_snapshot(
        &self,
        client: &mut Client,
        full_node: &FullNode,
        artifacts_dir: PathBuf,
        cardano_node_version: NodeVersion,
    ) -> StdResult<()> {
        if client.version().is_below("0.13.10") {
            warn!("Client version is below 0.13.10, skipping snapshot conversion check");
            return Ok(());
        }

        let utxo_hd_flavor = if cardano_node_version.is_below("10.7.0") {
            "LMDB"
        } else {
            "LSM"
        };

        let binary_path = artifacts_dir.join("bin").join("snapshot-converter");

        //copy the db to another temporary location to avoid any risk of modifying the original one during the conversion process
        let db_to_convert = artifacts_dir.join("db_to_convert");
        copy_dir_all(&full_node.db_path, &db_to_convert).with_context(|| {
            format!(
                "Failed to copy the ledger state database from `{}` to `{}` for the snapshot conversion process",
                full_node.db_path.display(),
                db_to_convert.display()
            )
        })?;

        client
            .run(ClientCommand::Tools(ToolsCommand::UtxoHd(
                UtxoHdCommand::SnapshotConverter {
                    db_directory: db_to_convert.to_string_lossy().to_string(),
                    cardano_node_version: cardano_node_version.to_string(),
                    binary_path: binary_path.to_string_lossy().to_string(),
                    config_path: full_node
                        .snapshot_converter_config_path
                        .to_string_lossy()
                        .to_string(),
                    utxo_hd_flavor: utxo_hd_flavor.to_string(),
                    commit: true,
                },
            )))
            .await?;
        info!("Client converted the ledger state into {utxo_hd_flavor} format");

        Ok(())
    }
}

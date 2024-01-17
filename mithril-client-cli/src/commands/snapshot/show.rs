use anyhow::{anyhow, Context};
use clap::Parser;
use cli_table::{print_stdout, Cell, Table};
use config::{builder::DefaultState, ConfigBuilder};
use slog_scope::logger;
use std::{collections::HashMap, sync::Arc};

use crate::{configuration::ConfigParameters, utils::ExpanderUtils};
use mithril_client::ClientBuilder;
use mithril_common::{test_utils::fake_keys, StdResult};

/// Clap command to show a given snapshot
#[derive(Parser, Debug, Clone)]
pub struct SnapshotShowCommand {
    /// Enable JSON output.
    #[clap(long)]
    json: bool,

    /// Snapshot digest.
    ///
    /// If `latest` is specified as digest, the command will return the latest snapshot.
    digest: String,
}

impl SnapshotShowCommand {
    /// Snapshot Show command
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
        let config = config_builder.build()?;
        let params = Arc::new(ConfigParameters::new(
            config.try_deserialize::<HashMap<String, String>>()?,
        ));
        // TODO: This should not be done this way.
        // Now that mithril-client-cli uses the mithril-client library, the genesis verification key is required for all commands
        let fallback_genesis_verification_key =
            fake_keys::genesis_verification_key()[0].to_string();
        let client = ClientBuilder::aggregator(
            &params.require("aggregator_endpoint")?,
            &params.get_or(
                "genesis_verification_key",
                &fallback_genesis_verification_key,
            ),
        )
        .with_logger(logger())
        .build()?;

        let get_list_of_artifact_ids = || async {
            let snapshots = client.snapshot().list().await.with_context(|| {
                "Can not get the list of artifacts while retrieving the latest snapshot digest"
            })?;

            Ok(snapshots
                .iter()
                .map(|snapshot| snapshot.digest.to_owned())
                .collect::<Vec<String>>())
        };

        let snapshot_message = client
            .snapshot()
            .get(
                &ExpanderUtils::expand_eventual_id_alias(&self.digest, get_list_of_artifact_ids())
                    .await?,
            )
            .await?
            .ok_or_else(|| anyhow!("Snapshot not found for digest: '{}'", &self.digest))?;

        if self.json {
            println!("{}", serde_json::to_string(&snapshot_message)?);
        } else {
            let snapshot_table = vec![
                vec![
                    "Epoch".cell(),
                    format!("{}", &snapshot_message.beacon.epoch).cell(),
                ],
                vec![
                    "Immutable File Number".cell(),
                    format!("{}", &snapshot_message.beacon.immutable_file_number).cell(),
                ],
                vec!["Network".cell(), snapshot_message.beacon.network.cell()],
                vec!["Digest".cell(), snapshot_message.digest.cell()],
                vec!["Size".cell(), format!("{}", &snapshot_message.size).cell()],
                vec![
                    "Cardano node version".cell(),
                    snapshot_message
                        .cardano_node_version
                        .as_ref()
                        .unwrap_or(&"NA".to_string())
                        .to_string()
                        .cell(),
                ],
                vec![
                    "Location".cell(),
                    snapshot_message.locations.join(",").cell(),
                ],
                vec![
                    "Created".cell(),
                    snapshot_message.created_at.to_string().cell(),
                ],
                vec![
                    "Compression Algorithm".cell(),
                    format!(
                        "{}",
                        &snapshot_message.compression_algorithm.unwrap_or_default()
                    )
                    .cell(),
                ],
            ]
            .table();

            print_stdout(snapshot_table)?
        }

        Ok(())
    }
}

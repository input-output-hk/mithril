use anyhow::{anyhow, Context};
use clap::Parser;
use cli_table::{print_stdout, Cell, Table};
use config::{builder::DefaultState, ConfigBuilder};
use std::collections::HashMap;

use crate::{
    commands::client_builder_with_fallback_genesis_key, configuration::ConfigParameters,
    utils::ExpanderUtils,
};
use mithril_client::MithrilResult;

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
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> MithrilResult<()> {
        let config = config_builder.build()?;
        let params = ConfigParameters::new(config.try_deserialize::<HashMap<String, String>>()?);
        let client = client_builder_with_fallback_genesis_key(&params)?.build()?;

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

use anyhow::Context;
use clap::Parser;
use cli_table::{print_stdout, Cell, Table};
use config::{builder::DefaultState, ConfigBuilder};
use std::{collections::HashMap, sync::Arc};

use mithril_common::StdResult;

use mithril_client::dependencies::{ConfigParameters, DependenciesBuilder};

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
        let params: Arc<ConfigParameters> = Arc::new(ConfigParameters::new(
            config.try_deserialize::<HashMap<String, String>>()?,
        ));
        let mut dependencies_builder = DependenciesBuilder::new(params);
        let snapshot_service = dependencies_builder
            .get_snapshot_service()
            .await
            .with_context(|| "Dependencies Builder can not get Snapshot Service")?;
        let snapshot_message = snapshot_service.show(&self.digest).await.with_context(|| {
            format!(
                "Snapshot Service can not show the snapshot for digest: '{}'",
                self.digest
            )
        })?;

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

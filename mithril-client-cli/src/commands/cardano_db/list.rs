use clap::Parser;
use cli_table::{Cell, Table, format::Justify, print_stdout};
use mithril_client::common::{EpochSpecifier, SignedEntityTypeDiscriminants};
use mithril_client::{Client, MithrilResult, RequiredAggregatorCapabilities};

use crate::{
    CommandContext,
    commands::{
        cardano_db::{
            CardanoDbCommandsBackend, warn_deprecated_v1_backend,
            warn_unused_parameter_with_v1_backend,
        },
        client_builder_with_fallback_genesis_key,
    },
    utils::CardanoDbUtils,
};

/// Clap command to list existing Cardano dbs
#[derive(Parser, Debug, Clone)]
pub struct CardanoDbListCommand {
    ///Backend to use, either: `v1` (default, full database restoration only) or `v2` (full or partial database restoration)
    #[arg(short, long, value_enum, default_value_t)]
    backend: CardanoDbCommandsBackend,

    /// [backend `v2` only] Epoch of the Cardano db snapshots to list, or `latest` for the latest artifact, or `latest-X` for the artifact of the latest epoch minus X.
    #[clap(long)]
    epoch: Option<String>,
}

impl CardanoDbListCommand {
    /// Main command execution
    pub async fn execute(&self, context: CommandContext) -> MithrilResult<()> {
        match self.backend {
            CardanoDbCommandsBackend::V1 => {
                let client = client_builder_with_fallback_genesis_key(context.config_parameters())?
                    .with_capabilities(RequiredAggregatorCapabilities::And(vec![
                        RequiredAggregatorCapabilities::SignedEntityType(
                            SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
                        ),
                    ]))
                    .with_logger(context.logger().clone())
                    .build()?;
                self.print_v1(client, context).await?;
            }
            CardanoDbCommandsBackend::V2 => {
                let client = client_builder_with_fallback_genesis_key(context.config_parameters())?
                    .with_capabilities(RequiredAggregatorCapabilities::And(vec![
                        RequiredAggregatorCapabilities::SignedEntityType(
                            SignedEntityTypeDiscriminants::CardanoDatabase,
                        ),
                    ]))
                    .with_logger(context.logger().clone())
                    .build()?;
                self.print_v2(client, context).await?;
            }
        }

        Ok(())
    }

    #[allow(deprecated)]
    async fn print_v1(&self, client: Client, context: CommandContext) -> MithrilResult<()> {
        warn_deprecated_v1_backend(&context);
        if self.epoch.is_some() {
            warn_unused_parameter_with_v1_backend(&context, ["--epoch"]);
        }

        let items = client.cardano_database().list().await?;

        if context.is_json_output_enabled() {
            println!("{}", serde_json::to_string(&items)?);
        } else {
            let items = items
                .into_iter()
                .map(|item| {
                    vec![
                        format!("{}", item.beacon.epoch).cell(),
                        format!("{}", item.beacon.immutable_file_number).cell(),
                        item.network.cell(),
                        item.digest.cell(),
                        CardanoDbUtils::format_bytes_to_gigabytes(item.size).cell(),
                        format!("{}", item.locations.len()).cell(),
                        item.created_at.to_string().cell(),
                    ]
                })
                .collect::<Vec<_>>()
                .table()
                .title(vec![
                    "Epoch".cell(),
                    "Immutable".cell(),
                    "Network".cell(),
                    "Digest".cell(),
                    "Size".cell().justify(Justify::Right),
                    "Locations".cell().justify(Justify::Right),
                    "Created".cell().justify(Justify::Right),
                ]);
            print_stdout(items)?;
        }
        Ok(())
    }

    async fn print_v2(&self, client: Client, context: CommandContext) -> MithrilResult<()> {
        let cdb_v2_client = client.cardano_database_v2();
        let items = match &self.epoch {
            None => cdb_v2_client.list().await?,
            Some(epoch_str) => match EpochSpecifier::parse(epoch_str)? {
                EpochSpecifier::Number(epoch) => cdb_v2_client.list_by_epoch(epoch).await?,
                EpochSpecifier::Latest => cdb_v2_client.list_for_latest_epoch().await?,
                EpochSpecifier::LatestMinusOffset(offset) => {
                    cdb_v2_client.list_for_latest_epoch_with_offset(offset).await?
                }
            },
        };

        if context.is_json_output_enabled() {
            println!("{}", serde_json::to_string(&items)?);
        } else {
            let items = items
                .into_iter()
                .map(|item| {
                    vec![
                        format!("{}", item.beacon.epoch).cell(),
                        format!("{}", item.beacon.immutable_file_number).cell(),
                        item.hash.cell(),
                        item.merkle_root.cell(),
                        CardanoDbUtils::format_bytes_to_gigabytes(item.total_db_size_uncompressed)
                            .cell(),
                        item.cardano_node_version.cell(),
                        item.created_at.to_string().cell(),
                    ]
                })
                .collect::<Vec<_>>()
                .table()
                .title(vec![
                    "Epoch".cell(),
                    "Immutable".cell(),
                    "Hash".cell(),
                    "Merkle root".cell(),
                    "Database size".cell().justify(Justify::Right),
                    "Cardano node".cell(),
                    "Created".cell().justify(Justify::Right),
                ]);
            print_stdout(items)?;
        }

        Ok(())
    }
}

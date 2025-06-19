use clap::Parser;
use cli_table::{format::Justify, print_stdout, Cell, Table};

use crate::{
    commands::{
        cardano_db::CardanoDbCommandsBackend, client_builder_with_fallback_genesis_key, SharedArgs,
    },
    utils::CardanoDbUtils,
    CommandContext,
};
use mithril_client::{Client, MithrilResult};

/// Clap command to list existing Cardano dbs
#[derive(Parser, Debug, Clone)]
pub struct CardanoDbListCommand {
    #[arg(short, long, value_enum, default_value_t)]
    backend: CardanoDbCommandsBackend,

    #[clap(flatten)]
    shared_args: SharedArgs,
}

impl CardanoDbListCommand {
    /// Is JSON output enabled
    pub fn is_json_output_enabled(&self) -> bool {
        self.shared_args.json
    }

    /// Main command execution
    pub async fn execute(&self, context: CommandContext) -> MithrilResult<()> {
        if self.backend.is_v2() {
            context.require_unstable("cardano-db snapshot list v2", None)?;
        }
        let params = context.config_parameters()?;
        let client = client_builder_with_fallback_genesis_key(&params)?
            .with_logger(context.logger().clone())
            .build()?;

        match self.backend {
            CardanoDbCommandsBackend::V1 => self.print_v1(client).await?,
            CardanoDbCommandsBackend::V2 => self.print_v2(client).await?,
        }

        Ok(())
    }

    async fn print_v1(&self, client: Client) -> MithrilResult<()> {
        let items = client.cardano_database().list().await?;

        if self.is_json_output_enabled() {
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

    async fn print_v2(&self, client: Client) -> MithrilResult<()> {
        let items = client.cardano_database_v2().list().await?;

        if self.is_json_output_enabled() {
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

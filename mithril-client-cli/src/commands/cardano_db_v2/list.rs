use clap::Parser;
use cli_table::{format::Justify, print_stdout, Cell, Table};

use mithril_client::MithrilResult;

use crate::{
    commands::{client_builder_with_fallback_genesis_key, SharedArgs},
    CommandContext,
};

/// Clap command to list existing cardano db snapshots
#[derive(Parser, Debug, Clone)]
pub struct CardanoDbListCommand {
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
        let params = context.config_parameters()?;
        let client = client_builder_with_fallback_genesis_key(&params)?
            .with_logger(context.logger().clone())
            .build()?;
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
                        item.hash.cell(),
                        item.merkle_root.cell(),
                        item.total_db_size_uncompressed.cell(),
                        format!("{}", item.compression_algorithm).cell(),
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
                    "Compression".cell(),
                    "Cardano node".cell(),
                    "Created".cell().justify(Justify::Right),
                ]);
            print_stdout(items)?;
        }

        Ok(())
    }
}

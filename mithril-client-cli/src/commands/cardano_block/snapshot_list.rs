use clap::Parser;
use cli_table::format::Justify;
use cli_table::{Cell, Table, print_stdout};
use mithril_client::common::SignedEntityTypeDiscriminants;

use crate::CommandContext;
use crate::commands::build_client;
use mithril_client::{CardanoBlocksTransactionsSnapshotListItem, MithrilResult};

/// Cardano block snapshot list command
#[derive(Parser, Debug, Clone)]
pub struct CardanoBlockSnapshotListCommand {}

impl CardanoBlockSnapshotListCommand {
    /// Main command execution
    pub async fn execute(&self, context: CommandContext) -> MithrilResult<()> {
        //require unstable
        context.require_unstable("cardano-block snapshot list", None)?;

        let client = build_client(
            &context,
            SignedEntityTypeDiscriminants::CardanoBlocksTransactions,
        )?;

        let lines = client.cardano_block().list_snapshots().await?;
        Self::print_lines(context.is_json_output_enabled(), lines)?;

        Ok(())
    }

    fn print_lines(
        is_json_output_enabled: bool,
        lines: Vec<CardanoBlocksTransactionsSnapshotListItem>,
    ) -> Result<(), anyhow::Error> {
        if is_json_output_enabled {
            println!("{}", serde_json::to_string(&lines)?);
        } else {
            let lines = lines
                .into_iter()
                .map(|item| {
                    vec![
                        format!("{}", item.epoch).cell(),
                        format!("{}", item.block_number_signed).cell(),
                        format!("{}", item.block_number_tip).cell(),
                        item.hash.cell(),
                        item.certificate_hash.cell(),
                        item.created_at.to_string().cell(),
                    ]
                })
                .collect::<Vec<_>>()
                .table()
                .title(vec![
                    "Epoch".cell(),
                    "Block Number Signed".cell(),
                    "Block Number Tip".cell(),
                    "Hash".cell(),
                    "Certificate Hash".cell(),
                    "Created".cell().justify(Justify::Right),
                ]);
            print_stdout(lines)?;
        }
        Ok(())
    }
}

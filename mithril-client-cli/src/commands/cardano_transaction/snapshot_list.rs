use clap::Parser;
use cli_table::format::Justify;
use cli_table::{Cell, Table, print_stdout};
use mithril_client::common::SignedEntityTypeDiscriminants;

use crate::CommandContext;
use crate::commands::cardano_transaction::CardanoTransactionCommandsBackend;
use mithril_client::{
    CardanoBlocksTransactionsSnapshotListItem, CardanoTransactionSnapshotListItem, MithrilResult,
};

/// Cardano transaction snapshot list command
#[derive(Parser, Debug, Clone)]
pub struct CardanoTransactionSnapshotListCommand {
    ///Backend to use, either: `v1` (default) or `v2`
    #[arg(short, long, value_enum, default_value_t)]
    backend: CardanoTransactionCommandsBackend,
}

impl CardanoTransactionSnapshotListCommand {
    /// Main command execution
    pub async fn execute(&self, context: CommandContext) -> MithrilResult<()> {
        match self.backend {
            CardanoTransactionCommandsBackend::V1 => {
                let client = context
                    .setup_mithril_client_builder_with_fallback_genesis_key()?
                    .with_capabilities(SignedEntityTypeDiscriminants::CardanoTransactions.into())
                    .build()?;
                let lines = client.cardano_transaction().list_snapshots().await?;
                Self::print_lines_v1(context.is_json_output_enabled(), lines)?;
            }
            CardanoTransactionCommandsBackend::V2 => {
                let client = context
                    .setup_mithril_client_builder_with_fallback_genesis_key()?
                    .with_capabilities(
                        SignedEntityTypeDiscriminants::CardanoBlocksTransactions.into(),
                    )
                    .build()?;
                context.require_unstable("cardano-transaction snapshot list --backend v2", None)?;
                let lines = client.cardano_transaction_v2().list_snapshots().await?;
                Self::print_lines_v2(context.is_json_output_enabled(), lines)?;
            }
        }

        Ok(())
    }

    fn print_lines_v1(
        is_json_output_enabled: bool,
        lines: Vec<CardanoTransactionSnapshotListItem>,
    ) -> Result<(), anyhow::Error> {
        if is_json_output_enabled {
            println!("{}", serde_json::to_string(&lines)?);
        } else {
            let lines = lines
                .into_iter()
                .map(|item| {
                    vec![
                        format!("{}", item.epoch).cell(),
                        format!("{}", item.block_number).cell(),
                        item.hash.cell(),
                        item.certificate_hash.cell(),
                        item.created_at.to_string().cell(),
                    ]
                })
                .collect::<Vec<_>>()
                .table()
                .title(vec![
                    "Epoch".cell(),
                    "Block Number".cell(),
                    "Hash".cell(),
                    "Certificate Hash".cell(),
                    "Created".cell().justify(Justify::Right),
                ]);
            print_stdout(lines)?;
        }
        Ok(())
    }

    fn print_lines_v2(
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

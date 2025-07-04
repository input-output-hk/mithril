use anyhow::{Context, anyhow};
use clap::Parser;
use cli_table::{Cell, Table, print_stdout};

use crate::{
    CommandContext, commands::client_builder_with_fallback_genesis_key, utils::ExpanderUtils,
};
use mithril_client::MithrilResult;

/// Clap command to show a given Cardano transaction snapshot
#[derive(Parser, Debug, Clone)]
pub struct CardanoTransactionsSnapshotShowCommand {
    /// Hash of the Cardano transaction snapshot to show or `latest` for the latest artifact
    hash: String,
}

impl CardanoTransactionsSnapshotShowCommand {
    /// Cardano transaction snapshot Show command
    pub async fn execute(&self, context: CommandContext) -> MithrilResult<()> {
        let client = client_builder_with_fallback_genesis_key(context.config_parameters())?
            .with_logger(context.logger().clone())
            .build()?;

        let get_list_of_artifact_ids = || async {
            let transactions_sets = client.cardano_transaction().list_snapshots().await.with_context(|| {
                "Can not get the list of artifacts while retrieving the latest Cardano transaction snapshot hash"
            })?;

            Ok(transactions_sets
                .iter()
                .map(|tx_sets| tx_sets.hash.to_owned())
                .collect::<Vec<String>>())
        };

        let tx_sets = client
            .cardano_transaction()
            .get_snapshot(
                &ExpanderUtils::expand_eventual_id_alias(&self.hash, get_list_of_artifact_ids())
                    .await?,
            )
            .await?
            .ok_or_else(|| {
                anyhow!(
                    "Cardano transaction snapshot not found for hash: '{}'",
                    &self.hash
                )
            })?;

        if context.is_json_output_enabled() {
            println!("{}", serde_json::to_string(&tx_sets)?);
        } else {
            let transaction_sets_table = vec![
                vec!["Epoch".cell(), format!("{}", &tx_sets.epoch).cell()],
                vec!["Block Number".cell(), format!("{}", &tx_sets.block_number).cell()],
                vec!["Merkle Root".cell(), tx_sets.merkle_root.to_string().cell()],
                vec!["Certificate Hash".cell(), tx_sets.certificate_hash.to_string().cell()],
                vec!["Hash".cell(), tx_sets.hash.cell()],
                vec!["Created".cell(), tx_sets.created_at.to_string().cell()],
            ]
            .table();

            print_stdout(transaction_sets_table)?
        }

        Ok(())
    }
}

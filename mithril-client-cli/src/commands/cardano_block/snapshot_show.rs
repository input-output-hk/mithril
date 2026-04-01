use anyhow::Context;
use clap::Parser;
use cli_table::{Cell, Table, print_stdout};

use crate::{CommandContext, utils::ExpanderUtils};
use mithril_client::{
    CardanoBlocksTransactionsSnapshot, Client, MithrilResult, common::SignedEntityTypeDiscriminants,
};

/// Clap command to show a given Cardano blocks transactions snapshot
#[derive(Parser, Debug, Clone)]
pub struct CardanoBlocksSnapshotShowCommand {
    /// Hash of the Cardano blocks transactions snapshot to show or `latest` for the latest artifact
    hash: String,
}

impl CardanoBlocksSnapshotShowCommand {
    /// Cardano blocks transactions snapshot Show command
    pub async fn execute(&self, context: CommandContext) -> MithrilResult<()> {
        context.require_unstable("cardano-block snapshot show", None)?;
        let client = context
            .setup_mithril_client_builder_with_fallback_genesis_key()?
            .with_capabilities(SignedEntityTypeDiscriminants::CardanoBlocksTransactions.into())
            .build()?;
        let get_list_of_artifact_ids = || async { Self::get_list_of_artifact_ids(&client).await };
        let snapshot =
            Self::retrieve_block_snapshot(&client, &self.hash, get_list_of_artifact_ids()).await?;
        Self::print_blocks_transactions_snapshot(context.is_json_output_enabled(), &snapshot)?;

        Ok(())
    }

    async fn get_list_of_artifact_ids(client: &Client) -> MithrilResult<Vec<String>> {
        let snapshots = client.cardano_block().list_snapshots().await.with_context(|| {
            "Can not get the list of artifacts while retrieving the latest Cardano blocks transactions snapshot hash"
        })?;

        Ok(snapshots.iter().map(|snapshot| snapshot.hash.to_owned()).collect())
    }

    async fn retrieve_block_snapshot(
        client: &Client,
        hash: &str,
        get_list_of_artifact_ids: impl Future<Output = MithrilResult<Vec<String>>>,
    ) -> MithrilResult<CardanoBlocksTransactionsSnapshot> {
        let snapshot = client
            .cardano_block()
            .get_snapshot(
                &ExpanderUtils::expand_eventual_id_alias(hash, get_list_of_artifact_ids).await?,
            )
            .await?
            .with_context(|| {
                format!(
                    "Cardano blocks transactions snapshot not found for hash: '{}'",
                    hash
                )
            })?;

        Ok(snapshot)
    }

    fn print_blocks_transactions_snapshot(
        is_json_output_enabled: bool,
        snapshot: &CardanoBlocksTransactionsSnapshot,
    ) -> Result<(), anyhow::Error> {
        if is_json_output_enabled {
            println!("{}", serde_json::to_string(&snapshot)?);
        } else {
            let snapshot_sets_table = vec![
                vec!["Epoch".cell(), format!("{}", &snapshot.epoch).cell()],
                vec![
                    "Block Number Signed".cell(),
                    format!("{}", &snapshot.block_number_signed).cell(),
                ],
                vec![
                    "Block Number Tip".cell(),
                    format!("{}", &snapshot.block_number_tip).cell(),
                ],
                vec!["Merkle Root".cell(), snapshot.merkle_root.to_string().cell()],
                vec![
                    "Certificate Hash".cell(),
                    snapshot.certificate_hash.to_string().cell(),
                ],
                vec!["Hash".cell(), snapshot.hash.clone().cell()],
                vec!["Created".cell(), snapshot.created_at.to_string().cell()],
            ]
            .table();

            print_stdout(snapshot_sets_table)?
        }
        Ok(())
    }
}

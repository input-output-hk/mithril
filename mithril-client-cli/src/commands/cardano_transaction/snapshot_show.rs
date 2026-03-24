use anyhow::Context;
use clap::Parser;
use cli_table::{Cell, Table, print_stdout};

use crate::{
    CommandContext,
    commands::{build_client, cardano_transaction::CardanoTransactionCommandsBackend},
    utils::ExpanderUtils,
};
use mithril_client::{
    CardanoBlocksTransactionsSnapshot, CardanoTransactionSnapshot, Client, MithrilResult,
    common::SignedEntityTypeDiscriminants,
};

/// Clap command to show a given Cardano transaction snapshot
#[derive(Parser, Debug, Clone)]
pub struct CardanoTransactionsSnapshotShowCommand {
    ///Backend to use, either: `v1` (default) or `v2`
    #[arg(short, long, value_enum, default_value_t)]
    backend: CardanoTransactionCommandsBackend,

    /// Hash of the Cardano transaction snapshot to show or `latest` for the latest artifact
    hash: String,
}

impl CardanoTransactionsSnapshotShowCommand {
    /// Cardano transaction snapshot Show command
    pub async fn execute(&self, context: CommandContext) -> MithrilResult<()> {
        match self.backend {
            CardanoTransactionCommandsBackend::V1 => {
                let client =
                    build_client(&context, SignedEntityTypeDiscriminants::CardanoTransactions)?;
                let get_list_of_artifact_ids =
                    || async { Self::get_list_of_artifact_ids_v1(&client).await };
                let snapshot = Self::retrieve_transaction_snapshot_v1(
                    &client,
                    &self.hash,
                    get_list_of_artifact_ids(),
                )
                .await?;
                Self::print_transaction_snapshot_v1(context.is_json_output_enabled(), &snapshot)?;
            }
            CardanoTransactionCommandsBackend::V2 => {
                context.require_unstable("cardano-transaction snapshot show --backend v2", None)?;
                let client = build_client(
                    &context,
                    SignedEntityTypeDiscriminants::CardanoBlocksTransactions,
                )?;
                let get_list_of_artifact_ids =
                    || async { Self::get_list_of_artifact_ids_v2(&client).await };
                let snapshot = Self::retrieve_transaction_snapshot_v2(
                    &client,
                    &self.hash,
                    get_list_of_artifact_ids(),
                )
                .await?;
                Self::print_transaction_snapshot_v2(context.is_json_output_enabled(), &snapshot)?;
            }
        }

        Ok(())
    }

    async fn get_list_of_artifact_ids_v1(client: &Client) -> MithrilResult<Vec<String>> {
        let transactions_sets = client.cardano_transaction().list_snapshots().await.with_context(|| {
            "Can not get the list of artifacts while retrieving the latest Cardano transaction snapshot hash"
        })?;

        Ok(transactions_sets
            .iter()
            .map(|tx_sets| tx_sets.hash.to_owned())
            .collect())
    }

    async fn retrieve_transaction_snapshot_v1(
        client: &Client,
        hash: &str,
        get_list_of_artifact_ids: impl Future<Output = MithrilResult<Vec<String>>>,
    ) -> MithrilResult<CardanoTransactionSnapshot> {
        let snapshot = client
            .cardano_transaction()
            .get_snapshot(
                &ExpanderUtils::expand_eventual_id_alias(hash, get_list_of_artifact_ids).await?,
            )
            .await?
            .with_context(|| {
                format!(
                    "Cardano transaction snapshot not found for hash: '{}'",
                    hash
                )
            })?;

        Ok(snapshot)
    }

    fn print_transaction_snapshot_v1(
        is_json_output_enabled: bool,
        snapshot: &CardanoTransactionSnapshot,
    ) -> Result<(), anyhow::Error> {
        if is_json_output_enabled {
            println!("{}", serde_json::to_string(&snapshot)?);
        } else {
            let transaction_sets_table = vec![
                vec!["Epoch".cell(), format!("{}", &snapshot.epoch).cell()],
                vec!["Block Number".cell(), format!("{}", &snapshot.block_number).cell()],
                vec!["Merkle Root".cell(), snapshot.merkle_root.to_string().cell()],
                vec![
                    "Certificate Hash".cell(),
                    snapshot.certificate_hash.to_string().cell(),
                ],
                vec!["Hash".cell(), snapshot.hash.clone().cell()],
                vec!["Created".cell(), snapshot.created_at.to_string().cell()],
            ]
            .table();

            print_stdout(transaction_sets_table)?
        }
        Ok(())
    }

    async fn get_list_of_artifact_ids_v2(client: &Client) -> MithrilResult<Vec<String>> {
        let transactions_sets = client.cardano_transaction_v2().list_snapshots().await.with_context(|| {
            "Can not get the list of artifacts while retrieving the latest Cardano blocks transactions snapshot hash"
        })?;

        Ok(transactions_sets
            .iter()
            .map(|tx_sets| tx_sets.hash.to_owned())
            .collect())
    }

    async fn retrieve_transaction_snapshot_v2(
        client: &Client,
        hash: &str,
        get_list_of_artifact_ids: impl Future<Output = MithrilResult<Vec<String>>>,
    ) -> MithrilResult<CardanoBlocksTransactionsSnapshot> {
        let snapshot = client
            .cardano_transaction_v2()
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

    fn print_transaction_snapshot_v2(
        is_json_output_enabled: bool,
        snapshot: &CardanoBlocksTransactionsSnapshot,
    ) -> Result<(), anyhow::Error> {
        if is_json_output_enabled {
            println!("{}", serde_json::to_string(&snapshot)?);
        } else {
            let transaction_sets_table = vec![
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

            print_stdout(transaction_sets_table)?
        }
        Ok(())
    }
}

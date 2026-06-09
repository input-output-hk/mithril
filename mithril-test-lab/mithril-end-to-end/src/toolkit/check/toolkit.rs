use std::path::PathBuf;

use anyhow::Context;
use mithril_common::StdResult;
use slog_scope::{info, warn};

use crate::toolkit::{
    CheckCardanoBlocksTransactionsToolkit, CheckCardanoDatabaseToolkit,
    CheckCardanoStakeDistributionToolkit, CheckCardanoTransactionsToolkit, CheckCertificateToolkit,
    CheckMithrilStakeDistributionToolkit, ScenarioToolkitContext,
};
use crate::utils::file_utils::copy_dir_all;
use crate::{Client, ClientCommand, FullNode, NodeVersion, ToolsCommand, UtxoHdCommand};

#[derive(Debug, Clone)]
pub struct CheckToolkit {
    pub cardano_blocks_transactions: CheckCardanoBlocksTransactionsToolkit,
    pub cardano_database: CheckCardanoDatabaseToolkit,
    pub cardano_stake_distribution: CheckCardanoStakeDistributionToolkit,
    pub cardano_transactions: CheckCardanoTransactionsToolkit,
    pub certificate: CheckCertificateToolkit,
    pub mithril_stake_distribution: CheckMithrilStakeDistributionToolkit,
}

impl CheckToolkit {
    pub fn new(context: ScenarioToolkitContext) -> Self {
        Self {
            cardano_blocks_transactions: CheckCardanoBlocksTransactionsToolkit::new(
                context.clone(),
            ),
            cardano_database: CheckCardanoDatabaseToolkit::new(context.clone()),
            cardano_stake_distribution: CheckCardanoStakeDistributionToolkit::new(context.clone()),
            cardano_transactions: CheckCardanoTransactionsToolkit::new(context.clone()),
            certificate: CheckCertificateToolkit::new(context.clone()),
            mithril_stake_distribution: CheckMithrilStakeDistributionToolkit::new(context.clone()),
        }
    }

    pub async fn client_can_convert_the_ledger_snapshot(
        &self,
        client: &mut Client,
        full_node: &FullNode,
        artifacts_dir: PathBuf,
        cardano_node_version: NodeVersion,
    ) -> StdResult<()> {
        if client.version().is_below("0.13.10") {
            warn!("Client version is below 0.13.10, skipping snapshot conversion check");
            return Ok(());
        }

        let utxo_hd_flavor = if cardano_node_version.is_below("10.7.0") {
            "LMDB"
        } else {
            "LSM"
        };

        let binary_path = artifacts_dir.join("bin").join("snapshot-converter");

        // copy the db to another temporary location to avoid any risk of modifying the original one during the conversion process
        let db_to_convert = artifacts_dir.join("db_to_convert");
        copy_dir_all(&full_node.db_path, &db_to_convert).with_context(|| {
            format!(
                "Failed to copy the ledger state database from `{}` to `{}` for the snapshot conversion process",
                full_node.db_path.display(),
                db_to_convert.display()
            )
        })?;

        client
            .run(ClientCommand::Tools(ToolsCommand::UtxoHd(
                UtxoHdCommand::SnapshotConverter {
                    db_directory: db_to_convert.to_string_lossy().to_string(),
                    cardano_node_version: cardano_node_version.to_string(),
                    binary_path: binary_path.to_string_lossy().to_string(),
                    config_path: full_node
                        .snapshot_converter_config_path
                        .to_string_lossy()
                        .to_string(),
                    utxo_hd_flavor: utxo_hd_flavor.to_string(),
                    commit: true,
                },
            )))
            .await?;
        info!("Client converted the ledger state into {utxo_hd_flavor} format");

        Ok(())
    }
}

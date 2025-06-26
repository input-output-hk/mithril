use crate::devnet::PoolNode;
use crate::utils::MithrilCommand;
use crate::{DEVNET_MAGIC_ID, ERA_MARKERS_VERIFICATION_KEY};
use anyhow::Context;
use mithril_common::entities::PartyId;
use mithril_common::StdResult;
use slog_scope::info;
use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;
use tokio::process::Child;
use tokio::sync::RwLock;

#[derive(Debug)]
pub struct SignerConfig<'a> {
    pub signer_number: usize,
    pub aggregator_endpoint: String,
    pub pool_node: &'a PoolNode,
    pub cardano_cli_path: &'a Path,
    pub work_dir: &'a Path,
    pub store_dir: &'a Path,
    pub bin_dir: &'a Path,
    pub mithril_run_interval: u32,
    pub mithril_era: &'a str,
    pub mithril_era_reader_adapter: &'a str,
    pub mithril_era_marker_address: &'a str,
    pub enable_certification: bool,
}

#[derive(Debug)]
pub struct Signer {
    name: String,
    party_id: PartyId,
    command: Arc<RwLock<MithrilCommand>>,
    process: RwLock<Option<Child>>,
}

impl Signer {
    pub fn new(signer_config: &SignerConfig) -> StdResult<Self> {
        let party_id = signer_config.pool_node.party_id()?;
        let magic_id = DEVNET_MAGIC_ID.to_string();
        let era_reader_adapter_params =
            if signer_config.mithril_era_reader_adapter == "cardano-chain" {
                format!(
                    r#"{{"address": "{}", "verification_key": "{}"}}"#,
                    signer_config.mithril_era_marker_address, ERA_MARKERS_VERIFICATION_KEY
                )
            } else {
                format!(
                    r#"{{"markers": [{{"name": "{}", "epoch": 0}}]}}"#,
                    signer_config.mithril_era
                )
            };
        let mithril_run_interval = format!("{}", signer_config.mithril_run_interval);
        let mut env = HashMap::from([
            ("NETWORK", "devnet"),
            ("RUN_INTERVAL", &mithril_run_interval),
            ("AGGREGATOR_ENDPOINT", &signer_config.aggregator_endpoint),
            (
                "DB_DIRECTORY",
                signer_config.pool_node.db_path.to_str().unwrap(),
            ),
            (
                "DATA_STORES_DIRECTORY",
                signer_config.store_dir.to_str().unwrap(),
            ),
            ("STORE_RETENTION_LIMIT", "10"),
            ("NETWORK_MAGIC", &magic_id),
            (
                "CARDANO_NODE_SOCKET_PATH",
                signer_config.pool_node.socket_path.to_str().unwrap(),
            ),
            (
                "CARDANO_CLI_PATH",
                signer_config.cardano_cli_path.to_str().unwrap(),
            ),
            (
                "ERA_READER_ADAPTER_TYPE",
                signer_config.mithril_era_reader_adapter,
            ),
            ("ERA_READER_ADAPTER_PARAMS", &era_reader_adapter_params),
            ("TRANSACTIONS_IMPORT_BLOCK_CHUNK_SIZE", "150"),
            ("PRELOADING_REFRESH_INTERVAL_IN_SECONDS", "10"),
            ("SIGNATURE_PUBLISHER_RETRY_DELAY_MS", "1"),
            ("SIGNATURE_PUBLISHER_DELAYER_DELAY_MS", "1"),
        ]);
        if signer_config.enable_certification {
            env.insert(
                "KES_SECRET_KEY_PATH",
                signer_config.pool_node.kes_secret_key_path.to_str().unwrap(),
            );
            env.insert(
                "OPERATIONAL_CERTIFICATE_PATH",
                signer_config.pool_node.operational_certificate_path.to_str().unwrap(),
            );
        } else {
            env.insert("PARTY_ID", &party_id);
        }
        let args = vec!["-vvv"];

        let mut command = MithrilCommand::new(
            "mithril-signer",
            signer_config.work_dir,
            signer_config.bin_dir,
            env,
            &args,
        )?;
        let name = format!("mithril-signer-{}-{party_id}", signer_config.signer_number);
        command.set_log_name(&name);

        Ok(Self {
            name,
            party_id,
            command: Arc::new(RwLock::new(command)),
            process: RwLock::new(None),
        })
    }

    pub async fn start(&self) -> StdResult<()> {
        let mut command = self.command.write().await;
        let mut process = self.process.write().await;
        *process = Some(command.start(&[])?);
        Ok(())
    }

    pub async fn stop(&self) -> StdResult<()> {
        let mut process_option = self.process.write().await;
        if let Some(process) = process_option.as_mut() {
            let name = self.name.as_str();
            info!("Stopping {name}");
            process.kill().await.with_context(|| "Could not kill signer")?;
            *process_option = None;
        }
        Ok(())
    }
    pub async fn tail_logs(&self, number_of_line: u64) -> StdResult<()> {
        self.command
            .read()
            .await
            .tail_logs(
                Some(format!("mithril-signer-{}", self.party_id).as_str()),
                number_of_line,
            )
            .await
    }
}

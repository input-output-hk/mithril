use std::cmp;
use std::collections::HashMap;
use std::fmt::Debug;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use anyhow::{anyhow, Context};
use slog_scope::info;
use tokio::process::Child;
use tokio::sync::RwLock;

use mithril_cardano_node_chain::chain_observer::{ChainObserver, PallasChainObserver};
use mithril_common::{entities, CardanoNetwork, StdResult};

use crate::utils::MithrilCommand;
use crate::{
    FullNode, RetryableDevnetError, ANCILLARY_MANIFEST_SECRET_KEY, DEVNET_MAGIC_ID,
    ERA_MARKERS_SECRET_KEY, ERA_MARKERS_VERIFICATION_KEY, GENESIS_SECRET_KEY,
    GENESIS_VERIFICATION_KEY,
};

#[derive(Debug)]
pub struct AggregatorConfig<'a> {
    pub index: usize,
    pub name: &'a str,
    pub server_port: u64,
    pub full_node: &'a FullNode,
    pub cardano_cli_path: &'a Path,
    pub work_dir: &'a Path,
    pub store_dir: &'a Path,
    pub artifacts_dir: &'a Path,
    pub bin_dir: &'a Path,
    pub cardano_node_version: &'a str,
    pub mithril_run_interval: u32,
    pub mithril_era: &'a str,
    pub mithril_era_reader_adapter: &'a str,
    pub mithril_era_marker_address: &'a str,
    pub signed_entity_types: &'a [String],
    pub chain_observer_type: &'a str,
    pub leader_aggregator_endpoint: &'a Option<String>,
}

pub struct Aggregator {
    index: usize,
    name_suffix: String,
    server_port: u64,
    db_directory: PathBuf,
    mithril_run_interval: u32,
    command: Arc<RwLock<MithrilCommand>>,
    process: RwLock<Option<Child>>,
    chain_observer: Arc<dyn ChainObserver>,
}

impl Aggregator {
    pub fn new(aggregator_config: &AggregatorConfig) -> StdResult<Self> {
        let magic_id = DEVNET_MAGIC_ID.to_string();
        let server_port_parameter = aggregator_config.server_port.to_string();
        let era_reader_adapter_params =
            if aggregator_config.mithril_era_reader_adapter == "cardano-chain" {
                format!(
                    r#"{{"address": "{}", "verification_key": "{}"}}"#,
                    aggregator_config.mithril_era_marker_address, ERA_MARKERS_VERIFICATION_KEY
                )
            } else {
                format!(
                    r#"{{"markers": [{{"name": "{}", "epoch": 0}}]}}"#,
                    aggregator_config.mithril_era
                )
            };
        let ancillary_files_signer_config =
            format!(r#"{{"type": "secret-key", "secret_key": "{ANCILLARY_MANIFEST_SECRET_KEY}"}}"#);
        let signed_entity_types = aggregator_config.signed_entity_types.join(",");
        let mithril_run_interval = format!("{}", aggregator_config.mithril_run_interval);
        let public_server_url = format!("http://localhost:{server_port_parameter}/aggregator");
        let mut env = HashMap::from([
            ("NETWORK", "devnet"),
            ("RUN_INTERVAL", &mithril_run_interval),
            ("SERVER_IP", "0.0.0.0"),
            ("SERVER_PORT", &server_port_parameter),
            ("PUBLIC_SERVER_URL", &public_server_url),
            ("SNAPSHOT_READER_TYPE", "local"),
            ("SNAPSHOT_UPLOADER_TYPE", "local"),
            (
                "SNAPSHOT_DIRECTORY",
                aggregator_config.artifacts_dir.to_str().unwrap(),
            ),
            ("NETWORK_MAGIC", &magic_id),
            (
                "DATA_STORES_DIRECTORY",
                aggregator_config.store_dir.to_str().unwrap(),
            ),
            (
                "CARDANO_NODE_SOCKET_PATH",
                aggregator_config.full_node.socket_path.to_str().unwrap(),
            ),
            ("STORE_RETENTION_LIMIT", "10"),
            (
                "CARDANO_CLI_PATH",
                aggregator_config.cardano_cli_path.to_str().unwrap(),
            ),
            ("GENESIS_VERIFICATION_KEY", GENESIS_VERIFICATION_KEY),
            ("GENESIS_SECRET_KEY", GENESIS_SECRET_KEY),
            (
                "ERA_READER_ADAPTER_TYPE",
                aggregator_config.mithril_era_reader_adapter,
            ),
            (
                "ANCILLARY_FILES_SIGNER_CONFIG",
                &ancillary_files_signer_config,
            ),
            ("ERA_READER_ADAPTER_PARAMS", &era_reader_adapter_params),
            ("SIGNED_ENTITY_TYPES", &signed_entity_types),
            (
                "CARDANO_NODE_VERSION",
                aggregator_config.cardano_node_version,
            ),
            ("CHAIN_OBSERVER_TYPE", aggregator_config.chain_observer_type),
            ("CARDANO_TRANSACTIONS_PROVER_CACHE_POOL_SIZE", "5"),
            ("CARDANO_TRANSACTIONS_DATABASE_CONNECTION_POOL_SIZE", "5"),
            (
                "CARDANO_TRANSACTIONS_SIGNING_CONFIG__SECURITY_PARAMETER",
                "1",
            ),
            ("CARDANO_TRANSACTIONS_SIGNING_CONFIG__STEP", "15"),
            ("PERSIST_USAGE_REPORT_INTERVAL_IN_SECONDS", "3"),
            ("CUSTOM_ORIGIN_TAG_WHITE_LIST", "E2E"),
        ]);
        if let Some(leader_aggregator_endpoint) = aggregator_config.leader_aggregator_endpoint {
            env.insert("LEADER_AGGREGATOR_ENDPOINT", leader_aggregator_endpoint);
        }
        let args = vec![
            "--db-directory",
            aggregator_config.full_node.db_path.to_str().unwrap(),
            "-vvv",
        ];

        let command = MithrilCommand::new(
            "mithril-aggregator",
            aggregator_config.work_dir,
            aggregator_config.bin_dir,
            env,
            &args,
        )?;
        let chain_observer = Arc::new(PallasChainObserver::new(
            &aggregator_config.full_node.socket_path,
            CardanoNetwork::DevNet(DEVNET_MAGIC_ID),
        ));

        Ok(Self {
            index: aggregator_config.index,
            name_suffix: aggregator_config.name.to_string(),
            server_port: aggregator_config.server_port,
            db_directory: aggregator_config.full_node.db_path.clone(),
            mithril_run_interval: aggregator_config.mithril_run_interval,
            command: Arc::new(RwLock::new(command)),
            process: RwLock::new(None),
            chain_observer,
        })
    }

    pub fn name_suffix(index: usize) -> String {
        format!("{}", index + 1)
    }

    pub fn copy_configuration(other: &Aggregator) -> Self {
        Self {
            index: other.index,
            name_suffix: other.name_suffix.clone(),
            server_port: other.server_port,
            db_directory: other.db_directory.clone(),
            mithril_run_interval: other.mithril_run_interval,
            command: other.command.clone(),
            process: RwLock::new(None),
            chain_observer: other.chain_observer.clone(),
        }
    }

    pub fn is_first(&self) -> bool {
        self.index == 0
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn name(&self) -> String {
        format!("mithril-aggregator-{}", self.name_suffix)
    }

    pub fn endpoint(&self) -> String {
        format!("http://localhost:{}/aggregator", &self.server_port)
    }

    pub fn db_directory(&self) -> &Path {
        &self.db_directory
    }

    pub fn mithril_run_interval(&self) -> u32 {
        self.mithril_run_interval
    }

    pub fn chain_observer(&self) -> Arc<dyn ChainObserver> {
        self.chain_observer.clone()
    }

    pub async fn serve(&self) -> StdResult<()> {
        let mut command = self.command.write().await;
        command.set_log_name(&format!("mithril-aggregator-{}", self.name_suffix));
        let mut process = self.process.write().await;
        *process = Some(command.start(&["serve".to_string()])?);
        Ok(())
    }

    pub async fn bootstrap_genesis(&self) -> StdResult<()> {
        // Clone the command so we can alter it without affecting the original
        let mut command = self.command.write().await;
        let command_name = &format!("mithril-aggregator-genesis-bootstrap-{}", self.name_suffix,);
        command.set_log_name(command_name);

        let exit_status = command
            .start(&["genesis".to_string(), "bootstrap".to_string()])?
            .wait()
            .await
            .with_context(|| "`mithril-aggregator genesis bootstrap` crashed")?;

        if exit_status.success() {
            Ok(())
        } else {
            command.tail_logs(Some(command_name), 40).await?;

            Err(match exit_status.code() {
                Some(c) => {
                    anyhow!("`mithril-aggregator genesis bootstrap` exited with code: {c}")
                }
                None => {
                    anyhow!("`mithril-aggregator genesis bootstrap` was terminated with a signal")
                }
            })
            .map_err(|e| anyhow!(RetryableDevnetError(e.to_string())))
        }
    }

    pub async fn stop(&self) -> StdResult<()> {
        let mut process = self.process.write().await;
        if let Some(mut process_running) = process.take() {
            info!("Stopping {}", self.name());
            process_running
                .kill()
                .await
                .with_context(|| "Could not kill aggregator")?;
            *process = None;
        }
        Ok(())
    }

    pub async fn era_generate_tx_datum(
        &self,
        target_path: &Path,
        mithril_era: &str,
        next_era_activation_epoch: entities::Epoch,
    ) -> StdResult<()> {
        let is_not_first_era = entities::SupportedEra::eras().first().map(|e| e.to_string())
            != Some(mithril_era.to_string());
        let current_era_epoch = if is_not_first_era {
            entities::Epoch(0)
        } else {
            next_era_activation_epoch
        };
        let next_era_epoch = entities::Epoch(cmp::max(*next_era_activation_epoch, 1));

        let mut args = vec![
            "era".to_string(),
            "generate-tx-datum".to_string(),
            "--current-era-epoch".to_string(),
            (*current_era_epoch).to_string(),
            "--era-markers-secret-key".to_string(),
            ERA_MARKERS_SECRET_KEY.to_string(),
            "--target-path".to_string(),
            target_path.to_str().unwrap().to_string(),
        ];

        // If only the first available era is targeted we have no "next-era" to activate
        if is_not_first_era {
            args.push("--next-era-epoch".to_string());
            args.push(next_era_epoch.to_string());
        }

        let mut command = self.command.write().await;
        let exit_status = command
            .start(&args)?
            .wait()
            .await
            .with_context(|| "`mithril-aggregator era generate-tx-datum` crashed")?;

        if exit_status.success() {
            Ok(())
        } else {
            Err(match exit_status.code() {
                Some(c) => {
                    anyhow!("`mithril-aggregator era generate-tx-datum` exited with code: {c}")
                }
                None => {
                    anyhow!(
                        "`mithril-aggregator era generate-tx-datum` was terminated with a signal"
                    )
                }
            })
        }
    }

    pub async fn set_protocol_parameters(
        &self,
        protocol_parameters: &entities::ProtocolParameters,
    ) {
        let mut command = self.command.write().await;
        command.set_env_var(
            "PROTOCOL_PARAMETERS__K",
            &format!("{}", protocol_parameters.k),
        );
        command.set_env_var(
            "PROTOCOL_PARAMETERS__M",
            &format!("{}", protocol_parameters.m),
        );
        command.set_env_var(
            "PROTOCOL_PARAMETERS__PHI_F",
            &format!("{}", protocol_parameters.phi_f),
        );
    }

    pub async fn set_mock_cardano_cli_file_path(
        &self,
        stake_distribution_file: &Path,
        epoch_file_path: &Path,
    ) {
        let mut command = self.command.write().await;
        command.set_env_var(
            "MOCK_STAKE_DISTRIBUTION_FILE",
            stake_distribution_file.to_str().unwrap(),
        );
        command.set_env_var("MOCK_EPOCH_FILE", epoch_file_path.to_str().unwrap());
    }

    /// Change the run interval of the aggregator state machine (default: 400ms)
    pub async fn change_run_interval(&self, interval: Duration) {
        let mut command = self.command.write().await;
        command.set_env_var("RUN_INTERVAL", &format!("{}", interval.as_millis()))
    }

    pub async fn tail_logs(&self, number_of_line: u64) -> StdResult<()> {
        let command = self.command.write().await;
        command.tail_logs(Some(&self.name()), number_of_line).await
    }

    pub async fn last_error_in_logs(&self, number_of_error: u64) -> StdResult<()> {
        let command = self.command.write().await;
        command.last_error_in_logs(Some(&self.name()), number_of_error).await
    }
}

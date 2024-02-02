use crate::devnet::BftNode;
use crate::utils::MithrilCommand;
use crate::{
    DEVNET_MAGIC_ID, ERA_MARKERS_SECRET_KEY, ERA_MARKERS_VERIFICATION_KEY, GENESIS_SECRET_KEY,
    GENESIS_VERIFICATION_KEY,
};
use anyhow::{anyhow, Context};
use mithril_common::era::SupportedEra;
use mithril_common::{entities, StdResult};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::time::Duration;
use tokio::process::Child;

#[derive(Debug)]
pub struct AggregatorConfig<'a> {
    pub server_port: u64,
    pub bft_node: &'a BftNode,
    pub cardano_cli_path: &'a Path,
    pub work_dir: &'a Path,
    pub bin_dir: &'a Path,
    pub cardano_node_version: &'a str,
    pub mithril_run_interval: u32,
    pub mithril_era: &'a str,
    pub mithril_era_reader_adapter: &'a str,
    pub mithril_era_marker_address: &'a str,
    pub signed_entity_types: &'a [String],
    pub chain_observer_type: &'a str,
}

#[derive(Debug)]
pub struct Aggregator {
    server_port: u64,
    db_directory: PathBuf,
    command: MithrilCommand,
    process: Option<Child>,
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
        let signed_entity_types = aggregator_config.signed_entity_types.join(",");
        let mithril_run_interval = format!("{}", aggregator_config.mithril_run_interval);
        let env = HashMap::from([
            ("NETWORK", "devnet"),
            ("RUN_INTERVAL", &mithril_run_interval),
            ("SERVER_IP", "0.0.0.0"),
            ("SERVER_PORT", &server_port_parameter),
            ("URL_SNAPSHOT_MANIFEST", ""),
            ("SNAPSHOT_STORE_TYPE", "local"),
            ("SNAPSHOT_UPLOADER_TYPE", "local"),
            ("NETWORK_MAGIC", &magic_id),
            ("DATA_STORES_DIRECTORY", "./stores/aggregator"),
            (
                "CARDANO_NODE_SOCKET_PATH",
                aggregator_config.bft_node.socket_path.to_str().unwrap(),
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
            ("ERA_READER_ADAPTER_PARAMS", &era_reader_adapter_params),
            ("SIGNED_ENTITY_TYPES", &signed_entity_types),
            (
                "CARDANO_NODE_VERSION",
                aggregator_config.cardano_node_version,
            ),
            ("CHAIN_OBSERVER_TYPE", aggregator_config.chain_observer_type),
        ]);
        let args = vec![
            "--db-directory",
            aggregator_config.bft_node.db_path.to_str().unwrap(),
            "-vvv",
        ];

        let command = MithrilCommand::new(
            "mithril-aggregator",
            aggregator_config.work_dir,
            aggregator_config.bin_dir,
            env,
            &args,
        )?;

        Ok(Self {
            server_port: aggregator_config.server_port,
            db_directory: aggregator_config.bft_node.db_path.clone(),
            command,
            process: None,
        })
    }

    pub fn copy_configuration(other: &Aggregator) -> Self {
        Self {
            server_port: other.server_port,
            db_directory: other.db_directory.clone(),
            command: other.command.clone(),
            process: None,
        }
    }

    pub fn endpoint(&self) -> String {
        format!("http://localhost:{}/aggregator", &self.server_port)
    }

    pub fn db_directory(&self) -> &Path {
        &self.db_directory
    }

    pub fn serve(&mut self) -> StdResult<()> {
        self.process = Some(self.command.start(&["serve".to_string()])?);
        Ok(())
    }

    pub async fn bootstrap_genesis(&mut self) -> StdResult<()> {
        let exit_status = self
            .command
            .start(&["genesis".to_string(), "bootstrap".to_string()])?
            .wait()
            .await
            .with_context(|| "`mithril-aggregator genesis bootstrap` crashed")?;

        if exit_status.success() {
            Ok(())
        } else {
            Err(match exit_status.code() {
                Some(c) => {
                    anyhow!("`mithril-aggregator genesis bootstrap` exited with code: {c}")
                }
                None => {
                    anyhow!("`mithril-aggregator genesis bootstrap` was terminated with a signal")
                }
            })
        }
    }

    pub async fn stop(&mut self) -> StdResult<()> {
        if let Some(process) = self.process.as_mut() {
            process
                .kill()
                .await
                .with_context(|| "Could not kill aggregator")?;
        }
        Ok(())
    }

    pub async fn era_generate_tx_datum(
        &mut self,
        target_path: &Path,
        mithril_era: &str,
    ) -> StdResult<()> {
        let is_not_first_era =
            SupportedEra::eras().first().map(|e| e.to_string()) != Some(mithril_era.to_string());

        let mut args = vec![
            "era".to_string(),
            "generate-tx-datum".to_string(),
            "--current-era-epoch".to_string(),
            "0".to_string(),
            "--era-markers-secret-key".to_string(),
            ERA_MARKERS_SECRET_KEY.to_string(),
            "--target-path".to_string(),
            target_path.to_str().unwrap().to_string(),
        ];

        // If only the first available era is targeted we have no "next-era" to activate
        if is_not_first_era {
            args.push("--next-era-epoch".to_string());
            args.push("1".to_string());
        }

        let exit_status = self
            .command
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

    pub fn set_protocol_parameters(&mut self, protocol_parameters: &entities::ProtocolParameters) {
        self.command.set_env_var(
            "PROTOCOL_PARAMETERS__K",
            &format!("{}", protocol_parameters.k),
        );
        self.command.set_env_var(
            "PROTOCOL_PARAMETERS__M",
            &format!("{}", protocol_parameters.m),
        );
        self.command.set_env_var(
            "PROTOCOL_PARAMETERS__PHI_F",
            &format!("{}", protocol_parameters.phi_f),
        );
    }

    pub fn set_mock_cardano_cli_file_path(
        &mut self,
        stake_distribution_file: &Path,
        epoch_file_path: &Path,
    ) {
        self.command.set_env_var(
            "MOCK_STAKE_DISTRIBUTION_FILE",
            stake_distribution_file.to_str().unwrap(),
        );
        self.command
            .set_env_var("MOCK_EPOCH_FILE", epoch_file_path.to_str().unwrap());
    }

    /// Change the run interval of the aggregator state machine (default: 400ms)
    pub fn change_run_interval(&mut self, interval: Duration) {
        self.command
            .set_env_var("RUN_INTERVAL", &format!("{}", interval.as_millis()))
    }

    pub async fn tail_logs(&self, number_of_line: u64) -> StdResult<()> {
        self.command.tail_logs(None, number_of_line).await
    }
}

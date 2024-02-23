use crate::devnet::PoolNode;
use crate::utils::MithrilCommand;
use crate::{DEVNET_MAGIC_ID, ERA_MARKERS_VERIFICATION_KEY};
use mithril_common::entities::PartyId;
use mithril_common::StdResult;
use std::collections::HashMap;
use std::path::Path;
use tokio::process::Child;

#[derive(Debug)]
pub struct SignerConfig<'a> {
    pub aggregator_endpoint: String,
    pub pool_node: &'a PoolNode,
    pub cardano_cli_path: &'a Path,
    pub work_dir: &'a Path,
    pub bin_dir: &'a Path,
    pub mithril_run_interval: u32,
    pub mithril_era: &'a str,
    pub mithril_era_reader_adapter: &'a str,
    pub mithril_era_marker_address: &'a str,
    pub enable_certification: bool,
}

#[derive(Debug)]
pub struct Signer {
    party_id: PartyId,
    command: MithrilCommand,
    process: Option<Child>,
}

impl Signer {
    pub fn new(signer_config: &SignerConfig) -> StdResult<Self> {
        let party_id = signer_config.pool_node.party_id()?;
        let magic_id = DEVNET_MAGIC_ID.to_string();
        let data_stores_path = format!("./stores/signer-{party_id}");
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
            ("DATA_STORES_DIRECTORY", &data_stores_path),
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
        ]);
        if signer_config.enable_certification {
            env.insert(
                "KES_SECRET_KEY_PATH",
                signer_config
                    .pool_node
                    .kes_secret_key_path
                    .to_str()
                    .unwrap(),
            );
            env.insert(
                "OPERATIONAL_CERTIFICATE_PATH",
                signer_config
                    .pool_node
                    .operational_certificate_path
                    .to_str()
                    .unwrap(),
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
        command.set_log_name(format!("mithril-signer-{party_id}").as_str());

        Ok(Self {
            party_id,
            command,
            process: None,
        })
    }

    pub fn start(&mut self) -> StdResult<()> {
        self.process = Some(self.command.start(&[])?);
        Ok(())
    }

    pub async fn tail_logs(&self, number_of_line: u64) -> StdResult<()> {
        self.command
            .tail_logs(
                Some(format!("mithril-signer-{}", self.party_id).as_str()),
                number_of_line,
            )
            .await
    }
}

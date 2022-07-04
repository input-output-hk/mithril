use crate::devnet::PoolNode;
use crate::utils::MithrilCommand;
use mithril_common::entities::PartyId;
use std::collections::HashMap;
use std::path::Path;
use tokio::process::Child;

#[derive(Debug)]
pub struct Signer {
    party_id: PartyId,
    command: MithrilCommand,
    process: Option<Child>,
}

impl Signer {
    pub fn new(
        aggregator_endpoint: String,
        pool_node: &PoolNode,
        cardano_cli_path: &Path,
        work_dir: &Path,
        bin_dir: &Path,
    ) -> Result<Self, String> {
        let party_id = pool_node.party_id.clone();
        let stake_store_path = format!("./store/signer-{}/stakes", party_id);
        let env = HashMap::from([
            ("NETWORK", "devnet"),
            ("PARTY_ID", &party_id),
            ("RUN_INTERVAL", "2000"),
            ("AGGREGATOR_ENDPOINT", &aggregator_endpoint),
            ("DB_DIRECTORY", pool_node.db_path.to_str().unwrap()),
            ("STAKE_STORE_DIRECTORY", &stake_store_path),
            ("NETWORK_MAGIC", "42"),
            (
                "CARDANO_NODE_SOCKET_PATH",
                pool_node.socket_path.to_str().unwrap(),
            ),
            ("CARDANO_CLI_PATH", cardano_cli_path.to_str().unwrap()),
        ]);
        let args = vec!["-vvv"];

        let mut command = MithrilCommand::new("mithril-signer", work_dir, bin_dir, env, &args)?;
        command.set_log_name(format!("mithril-signer-{}", party_id).as_str());

        Ok(Self {
            party_id,
            command,
            process: None,
        })
    }

    pub fn start(&mut self) -> Result<(), String> {
        self.process = Some(self.command.start(&[])?);
        Ok(())
    }

    pub async fn tail_logs(&self, number_of_line: u64) -> Result<(), String> {
        self.command
            .tail_logs(
                Some(format!("mithril-signer-{}", self.party_id).as_str()),
                number_of_line,
            )
            .await
    }
}

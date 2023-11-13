use crate::devnet::PoolNode;
use crate::utils::MithrilCommand;
use mithril_common::entities::PartyId;
use mithril_common::StdResult;
use std::collections::HashMap;
use std::path::Path;
use tokio::process::Child;

#[derive(Debug)]
pub struct RelaySigner {
    server_port: u64,
    party_id: PartyId,
    command: MithrilCommand,
    process: Option<Child>,
}

impl RelaySigner {
    pub fn new(
        server_port: u64,
        dial_to: String,
        aggregator_endpoint: String,
        pool_node: &PoolNode,
        work_dir: &Path,
        bin_dir: &Path,
    ) -> StdResult<Self> {
        let server_port_str = format!("{server_port}");
        let party_id = pool_node.party_id()?;
        let env = HashMap::from([
            ("NODE_TYPE", "signer"),
            ("SERVER_PORT", &server_port_str),
            ("AGGREGATOR_ENDPOINT", &aggregator_endpoint),
            ("DIAL_TO", &dial_to),
        ]);
        let args = vec!["-vvv"];

        let mut command = MithrilCommand::new("mithril-relay", work_dir, bin_dir, env, &args)?;
        command.set_log_name(format!("mithril-relay-signer-{party_id}").as_str());

        Ok(Self {
            server_port,
            party_id,
            command,
            process: None,
        })
    }

    pub fn endpoint(&self) -> String {
        format!("http://localhost:{}", &self.server_port)
    }

    pub fn start(&mut self) -> StdResult<()> {
        self.process = Some(self.command.start(&[])?);
        Ok(())
    }

    pub async fn tail_logs(&self, number_of_line: u64) -> StdResult<()> {
        self.command
            .tail_logs(
                Some(format!("mithril-relay-signer-{}", self.party_id).as_str()),
                number_of_line,
            )
            .await
    }
}

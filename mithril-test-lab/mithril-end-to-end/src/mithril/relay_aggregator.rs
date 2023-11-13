use crate::utils::MithrilCommand;
use mithril_common::StdResult;
use std::collections::HashMap;
use std::path::Path;
use tokio::process::Child;

#[derive(Debug)]
pub struct RelayAggregator {
    listen_port: u64,
    command: MithrilCommand,
    process: Option<Child>,
}

impl RelayAggregator {
    pub fn new(
        listen_port: u64,
        aggregator_endpoint: String,
        work_dir: &Path,
        bin_dir: &Path,
    ) -> StdResult<Self> {
        let listen_port_str = format!("{listen_port}");
        let env = HashMap::from([
            ("NODE_TYPE", "aggregator"),
            ("LISTEN_PORT", &listen_port_str),
            ("AGGREGATOR_ENDPOINT", &aggregator_endpoint),
        ]);
        let args = vec!["-vvv"];

        let mut command = MithrilCommand::new("mithril-relay", work_dir, bin_dir, env, &args)?;
        command.set_log_name("mithril-relay-aggregator");

        Ok(Self {
            listen_port,
            command,
            process: None,
        })
    }

    pub fn peer_addr(&self) -> String {
        format!("/ip4/127.0.0.1/tcp/{}", self.listen_port)
    }

    pub fn start(&mut self) -> StdResult<()> {
        self.process = Some(self.command.start(&[])?);
        Ok(())
    }

    pub async fn tail_logs(&self, number_of_line: u64) -> StdResult<()> {
        self.command
            .tail_logs(Some("mithril-relay-aggregator"), number_of_line)
            .await
    }
}

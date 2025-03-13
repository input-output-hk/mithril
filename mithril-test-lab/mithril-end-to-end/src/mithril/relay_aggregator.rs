use crate::utils::MithrilCommand;
use mithril_common::StdResult;
use std::collections::HashMap;
use std::path::Path;
use tokio::process::Child;

#[derive(Debug)]
pub struct RelayAggregator {
    index: usize,
    listen_port: u64,
    command: MithrilCommand,
    process: Option<Child>,
}

impl RelayAggregator {
    pub fn new(
        index: usize,
        listen_port: u64,
        dial_to: Option<String>,
        aggregator_endpoint: &str,
        work_dir: &Path,
        bin_dir: &Path,
    ) -> StdResult<Self> {
        let listen_port_str = format!("{listen_port}");
        let mut env = HashMap::from([
            ("LISTEN_PORT", listen_port_str.as_str()),
            ("AGGREGATOR_ENDPOINT", aggregator_endpoint),
        ]);
        if let Some(dial_to) = &dial_to {
            env.insert("DIAL_TO", dial_to);
        }
        let args = vec!["-vvv", "aggregator"];

        let mut command = MithrilCommand::new("mithril-relay", work_dir, bin_dir, env, &args)?;
        command.set_log_name(&Self::command_name(index));

        Ok(Self {
            index,
            listen_port,
            command,
            process: None,
        })
    }

    pub fn peer_addr(&self) -> String {
        format!("/ip4/127.0.0.1/tcp/{}", self.listen_port)
    }

    fn command_name(index: usize) -> String {
        if index == 0 {
            "mithril-relay-aggregator".to_string()
        } else {
            format!("mithril-relay-aggregator-slave-{}", index)
        }
    }

    pub fn start(&mut self) -> StdResult<()> {
        self.process = Some(self.command.start(&[])?);
        Ok(())
    }

    pub async fn tail_logs(&self, number_of_line: u64) -> StdResult<()> {
        self.command
            .tail_logs(Some(&Self::command_name(self.index)), number_of_line)
            .await
    }
}

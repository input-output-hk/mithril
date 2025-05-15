use crate::utils::MithrilCommand;
use mithril_common::StdResult;
use std::collections::HashMap;
use std::path::Path;
use tokio::process::Child;

#[derive(Debug)]
pub struct RelayAggregator {
    name_suffix: String,
    listen_port: u64,
    command: MithrilCommand,
    process: Option<Child>,
}

impl RelayAggregator {
    pub fn new(
        name: String,
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
        command.set_log_name(&format!("mithril-relay-aggregator-{name}",));

        Ok(Self {
            name_suffix: name,
            listen_port,
            command,
            process: None,
        })
    }

    pub fn peer_addr(&self) -> String {
        format!("/ip4/127.0.0.1/tcp/{}", self.listen_port)
    }

    pub fn name_suffix(&self) -> String {
        self.name_suffix.clone()
    }

    pub fn start(&mut self) -> StdResult<()> {
        self.process = Some(self.command.start(&[])?);
        Ok(())
    }

    pub async fn tail_logs(&self, number_of_line: u64) -> StdResult<()> {
        self.command
            .tail_logs(
                Some(&format!("mithril-relay-aggregator-{}", self.name_suffix())),
                number_of_line,
            )
            .await
    }
}

use crate::utils::MithrilCommand;
use crate::{Aggregator, DEVNET_MAGIC_ID};
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
        index: usize,
        listen_port: u64,
        dial_to: Option<String>,
        aggregator_endpoint: &str,
        work_dir: &Path,
        bin_dir: &Path,
        use_dmq: bool,
    ) -> StdResult<Self> {
        let name = Aggregator::name_suffix(index);
        let listen_port_str = format!("{listen_port}");
        let magic_id = DEVNET_MAGIC_ID.to_string();
        let mut env = HashMap::from([
            ("LISTEN_PORT", listen_port_str.as_str()),
            ("NETWORK", "devnet"),
            ("NETWORK_MAGIC", &magic_id),
            ("AGGREGATOR_ENDPOINT", aggregator_endpoint),
        ]);
        if let Some(dial_to) = &dial_to {
            env.insert("DIAL_TO", dial_to);
        }
        let dmq_node_socket_path = work_dir.join(format!("dmq-aggregator-{index}.socket"));
        if use_dmq {
            env.insert(
                "DMQ_NODE_SOCKET_PATH",
                dmq_node_socket_path.to_str().unwrap(),
            );
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

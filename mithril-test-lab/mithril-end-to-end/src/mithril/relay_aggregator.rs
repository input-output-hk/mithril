use std::collections::HashMap;
use std::path::Path;
use tokio::process::Child;

use mithril_common::StdResult;

use crate::utils::{MithrilCommand, NodeVersion};
use crate::{Aggregator, DEVNET_DMQ_MAGIC_ID};

#[derive(Debug)]
pub struct RelayAggregator {
    name_suffix: String,
    listen_port: u64,
    command: MithrilCommand,
    process: Option<Child>,
    version: NodeVersion,
}

impl RelayAggregator {
    pub const BIN_NAME: &'static str = "mithril-relay";

    pub fn new(
        index: usize,
        listen_port: u64,
        dial_to: Option<String>,
        aggregator_endpoint: &str,
        work_dir: &Path,
        bin_dir: &Path,
        use_dmq: bool,
    ) -> StdResult<Self> {
        let version = NodeVersion::fetch(Self::BIN_NAME, bin_dir)?;
        let name = Aggregator::name_suffix(index);
        let listen_port_str = format!("{listen_port}");
        let dmq_magic_id = DEVNET_DMQ_MAGIC_ID.to_string();
        let mut env = HashMap::from([
            ("LISTEN_PORT", listen_port_str.as_str()),
            ("NETWORK", "devnet"),
            ("DMQ_NETWORK_MAGIC", &dmq_magic_id),
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

        let mut command = MithrilCommand::new(Self::BIN_NAME, work_dir, bin_dir, env, &args)?;
        command.set_log_name(&format!("mithril-relay-aggregator-{name}",));

        Ok(Self {
            name_suffix: name,
            listen_port,
            command,
            process: None,
            version,
        })
    }

    pub fn peer_addr(&self) -> String {
        format!("/ip4/127.0.0.1/tcp/{}", self.listen_port)
    }

    pub fn name_suffix(&self) -> String {
        self.name_suffix.clone()
    }

    /// Get the version of the mithril-relay binary.
    pub fn version(&self) -> &NodeVersion {
        &self.version
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

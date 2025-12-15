use std::collections::HashMap;
use std::path::Path;

use mithril_common::StdResult;
use tokio::process::Child;

use crate::utils::{MithrilCommand, NodeVersion};

#[derive(Debug)]
pub struct RelayPassive {
    listen_port: u64,
    relay_id: String,
    command: MithrilCommand,
    process: Option<Child>,
    version: NodeVersion,
}

impl RelayPassive {
    pub const BIN_NAME: &'static str = "mithril-relay";

    pub fn new(
        listen_port: u64,
        dial_to: Option<String>,
        relay_id: String,
        work_dir: &Path,
        bin_dir: &Path,
    ) -> StdResult<Self> {
        let version = NodeVersion::fetch(Self::BIN_NAME, bin_dir)?;

        let listen_port_str = format!("{listen_port}");
        let mut env = HashMap::from([("LISTEN_PORT", listen_port_str.as_str())]);
        if let Some(dial_to) = &dial_to {
            env.insert("DIAL_TO", dial_to);
        }
        let args = vec!["-vvv", "passive"];

        let mut command = MithrilCommand::new("mithril-relay", work_dir, bin_dir, env, &args)?;
        command.set_log_name(&format!("mithril-relay-passive-{relay_id}"));

        Ok(Self {
            listen_port,
            relay_id,
            command,
            process: None,
            version,
        })
    }

    pub fn peer_addr(&self) -> String {
        format!("/ip4/127.0.0.1/tcp/{}", self.listen_port)
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
                Some(&format!("mithril-relay-passive-{}", self.relay_id)),
                number_of_line,
            )
            .await
    }
}

use crate::utils::MithrilCommand;
use mithril_common::StdResult;
use std::collections::HashMap;
use std::path::Path;
use tokio::process::Child;

#[derive(Debug)]
pub struct RelayPassive {
    listen_port: u64,
    relay_id: String,
    command: MithrilCommand,
    process: Option<Child>,
}

impl RelayPassive {
    pub fn new(
        listen_port: u64,
        dial_to: String,
        relay_id: String,
        work_dir: &Path,
        bin_dir: &Path,
    ) -> StdResult<Self> {
        let listen_port_str = format!("{listen_port}");
        let env = HashMap::from([
            ("LISTEN_PORT", listen_port_str.as_str()),
            ("DIAL_TO", &dial_to),
        ]);
        let args = vec!["-vvv", "passive"];

        let mut command = MithrilCommand::new("mithril-relay", work_dir, bin_dir, env, &args)?;
        command.set_log_name(&format!("mithril-relay-passive-{}", relay_id));

        Ok(Self {
            listen_port,
            relay_id,
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
            .tail_logs(
                Some(&format!("mithril-relay-passive-{}", self.relay_id)),
                number_of_line,
            )
            .await
    }
}

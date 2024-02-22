use crate::utils::MithrilCommand;
use mithril_common::entities::PartyId;
use mithril_common::StdResult;
use std::collections::HashMap;
use std::path::Path;
use tokio::process::Child;

#[derive(Debug)]
pub struct RelaySigner {
    listen_port: u64,
    server_port: u64,
    party_id: PartyId,
    command: MithrilCommand,
    process: Option<Child>,
}

impl RelaySigner {
    pub fn new(
        listen_port: u64,
        server_port: u64,
        dial_to: String,
        aggregator_endpoint: &str,
        party_id: PartyId,
        work_dir: &Path,
        bin_dir: &Path,
    ) -> StdResult<Self> {
        let listen_port_str = format!("{listen_port}");
        let server_port_str = format!("{server_port}");
        let env = HashMap::from([
            ("LISTEN_PORT", listen_port_str.as_str()),
            ("SERVER_PORT", server_port_str.as_str()),
            ("AGGREGATOR_ENDPOINT", aggregator_endpoint),
            ("DIAL_TO", &dial_to),
        ]);
        let args = vec!["-vvv", "signer"];

        let mut command = MithrilCommand::new("mithril-relay", work_dir, bin_dir, env, &args)?;
        command.set_log_name(format!("mithril-relay-signer-{party_id}").as_str());

        Ok(Self {
            listen_port,
            server_port,
            party_id,
            command,
            process: None,
        })
    }

    pub fn peer_addr(&self) -> String {
        format!("/ip4/127.0.0.1/tcp/{}", self.listen_port)
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

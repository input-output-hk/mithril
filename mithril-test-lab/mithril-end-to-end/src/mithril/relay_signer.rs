use crate::DEVNET_DMQ_MAGIC_ID;
use crate::utils::MithrilCommand;
use mithril_common::StdResult;
use mithril_common::entities::PartyId;
use std::collections::HashMap;
use std::path::Path;
use tokio::process::Child;

pub struct RelaySignerConfiguration<'a> {
    pub signer_number: usize,
    pub listen_port: u64,
    pub server_port: u64,
    pub dial_to: Option<String>,
    pub relay_signer_registration_mode: String,
    pub relay_signature_registration_mode: String,
    pub aggregator_endpoint: &'a str,
    pub party_id: PartyId,
    pub work_dir: &'a Path,
    pub bin_dir: &'a Path,
    pub use_dmq: bool,
}

#[derive(Debug)]
pub struct RelaySigner {
    listen_port: u64,
    server_port: u64,
    party_id: PartyId,
    command: MithrilCommand,
    process: Option<Child>,
}

impl RelaySigner {
    pub fn new(configuration: &RelaySignerConfiguration) -> StdResult<Self> {
        let party_id = configuration.party_id.to_owned();
        let listen_port_str = format!("{}", configuration.listen_port);
        let server_port_str = format!("{}", configuration.server_port);

        let dmq_magic_id = DEVNET_DMQ_MAGIC_ID.to_string();
        let relay_signer_registration_mode =
            configuration.relay_signer_registration_mode.to_string();
        let relay_signature_registration_mode =
            configuration.relay_signature_registration_mode.to_string();
        let mut env = HashMap::from([
            ("LISTEN_PORT", listen_port_str.as_str()),
            ("SERVER_PORT", server_port_str.as_str()),
            ("NETWORK", "devnet"),
            ("DMQ_NETWORK_MAGIC", &dmq_magic_id),
            ("AGGREGATOR_ENDPOINT", configuration.aggregator_endpoint),
            ("SIGNER_REPEATER_DELAY", "100"),
            (
                "SIGNER_REGISTRATION_MODE",
                relay_signer_registration_mode.as_str(),
            ),
            (
                "SIGNATURE_REGISTRATION_MODE",
                relay_signature_registration_mode.as_str(),
            ),
        ]);
        if let Some(dial_to) = &configuration.dial_to {
            env.insert("DIAL_TO", dial_to);
        }
        let dmq_node_socket_path = configuration
            .work_dir
            .join(format!("dmq-signer-{}.socket", configuration.signer_number));
        if configuration.use_dmq {
            env.insert(
                "DMQ_NODE_SOCKET_PATH",
                dmq_node_socket_path.to_str().unwrap(),
            );
        }
        let args = vec!["-vvv", "signer"];

        let mut command = MithrilCommand::new(
            "mithril-relay",
            configuration.work_dir,
            configuration.bin_dir,
            env,
            &args,
        )?;
        command.set_log_name(format!("mithril-relay-signer-{}", configuration.party_id).as_str());

        Ok(Self {
            listen_port: configuration.listen_port,
            server_port: configuration.server_port,
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

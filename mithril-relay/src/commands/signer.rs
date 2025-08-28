#[cfg(feature = "future_dmq")]
use std::path::PathBuf;
use std::time::Duration;

use clap::Parser;
use libp2p::Multiaddr;
use slog::error;

#[cfg(feature = "future_dmq")]
use mithril_common::CardanoNetwork;
use mithril_common::StdResult;

use crate::{SignerRelay, SignerRelayMode};

use super::CommandContext;

#[derive(Parser, Debug, Clone)]
pub struct SignerCommand {
    /// HTTP Server listening port
    #[clap(long, env = "SERVER_PORT", default_value_t = 3132)]
    server_port: u16,

    /// Peer listening port
    #[clap(long, env = "LISTEN_PORT", default_value_t = 0)]
    listen_port: u16,

    /// Dial to peer multi-address (e.g. /ip4/0.0.0.0/tcp/1234)
    #[clap(long, env = "DIAL_TO")]
    dial_to: Option<Multiaddr>,

    /// Path to the DMQ socket file
    #[cfg(feature = "future_dmq")]
    #[clap(
        long,
        env = "DMQ_NODE_SOCKET_PATH",
        value_name = "PATH",
        default_value = "./dmq.socket"
    )]
    dmq_node_socket_path: PathBuf,

    /// Cardano network
    #[cfg(feature = "future_dmq")]
    #[clap(long, env = "NETWORK")]
    pub network: String,

    /// Cardano Network Magic number
    /// useful for TestNet & DevNet
    #[cfg(feature = "future_dmq")]
    #[clap(long, env = "NETWORK_MAGIC")]
    pub network_magic: Option<u64>,

    /// Aggregator endpoint URL.
    #[clap(long, env = "AGGREGATOR_ENDPOINT")]
    aggregator_endpoint: String,

    /// Signer registration relay mode
    #[clap(value_enum, env = "SIGNER_REGISTRATION_MODE", default_value_t = SignerRelayMode::Passthrough)]
    signer_registration_mode: SignerRelayMode,

    /// Signature registration relay mode
    #[clap(value_enum, env = "SIGNATURE_REGISTRATION_MODE", default_value_t = SignerRelayMode::P2P)]
    signature_registration_mode: SignerRelayMode,

    /// Interval at which a signer registration should be repeated in milliseconds (defaults to 1 hour)
    #[clap(long, env = "SIGNER_REPEATER_DELAY", default_value_t = 3_600 * 1_000)]
    signer_repeater_delay: u64,
}

impl SignerCommand {
    /// Main command execution
    pub async fn execute(&self, context: CommandContext) -> StdResult<()> {
        let logger = context.logger();
        let server_port = self.server_port.to_owned();
        let dial_to = self.dial_to.to_owned();
        let addr: Multiaddr = format!("/ip4/0.0.0.0/tcp/{}", self.listen_port).parse()?;
        let signer_registration_mode = &self.signer_registration_mode;
        let signature_registration_mode = &self.signature_registration_mode;
        let aggregator_endpoint = self.aggregator_endpoint.to_owned();
        let signer_repeater_delay = Duration::from_millis(self.signer_repeater_delay);
        #[cfg(feature = "future_dmq")]
        let cardano_network =
            CardanoNetwork::from_code(self.network.to_owned(), self.network_magic)?;

        let mut relay = SignerRelay::start(
            &addr,
            &server_port,
            #[cfg(feature = "future_dmq")]
            &self.dmq_node_socket_path,
            #[cfg(feature = "future_dmq")]
            &cardano_network,
            signer_registration_mode,
            signature_registration_mode,
            &aggregator_endpoint,
            &signer_repeater_delay,
            logger,
        )
        .await?;
        if let Some(dial_to_address) = dial_to {
            relay.dial_peer(dial_to_address.clone())?;
        }

        loop {
            if let Err(err) = relay.tick().await {
                error!(logger, "RelaySigner: tick error"; "error" => ?err);
            }
        }
    }
}

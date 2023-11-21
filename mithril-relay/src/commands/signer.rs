use clap::Parser;
use config::{builder::DefaultState, ConfigBuilder};
use libp2p::Multiaddr;
use mithril_common::StdResult;
use slog_scope::error;

use crate::SignerRelay;

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

    /// Aggregator endpoint URL.
    #[clap(long, env = "AGGREGATOR_ENDPOINT")]
    aggregator_endpoint: String,
}

impl SignerCommand {
    /// Main command execution
    pub async fn execute(&self, _config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
        let server_port = self.server_port.to_owned();
        let dial_to = self.dial_to.to_owned();
        let addr: Multiaddr = format!("/ip4/0.0.0.0/tcp/{}", self.listen_port).parse()?;
        let aggregator_endpoint = self.aggregator_endpoint.to_owned();

        let mut relay = SignerRelay::start(&addr, &server_port, &aggregator_endpoint).await?;
        if let Some(dial_to_address) = dial_to {
            relay.dial_peer(dial_to_address.clone())?;
        }

        loop {
            if let Err(err) = relay.tick().await {
                error!("RelaySigner: tick error"; "error" => format!("{err:#?}"));
            }
        }
    }
}

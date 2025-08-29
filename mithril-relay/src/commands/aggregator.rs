#[cfg(feature = "future_dmq")]
use std::path::PathBuf;

use clap::Parser;
use libp2p::Multiaddr;
use slog::error;

#[cfg(feature = "future_dmq")]
use mithril_common::CardanoNetwork;
use mithril_common::StdResult;

use crate::AggregatorRelay;

use super::CommandContext;

#[derive(Parser, Debug, Clone)]
pub struct AggregatorCommand {
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
}

impl AggregatorCommand {
    /// Main command execution
    pub async fn execute(&self, context: CommandContext) -> StdResult<()> {
        let logger = context.logger();
        let dial_to = self.dial_to.to_owned();
        let addr: Multiaddr = format!("/ip4/0.0.0.0/tcp/{}", self.listen_port).parse()?;
        let aggregator_endpoint = self.aggregator_endpoint.to_owned();
        #[cfg(feature = "future_dmq")]
        let cardano_network =
            CardanoNetwork::from_code(self.network.to_owned(), self.network_magic)?;

        let mut relay = AggregatorRelay::start(
            &addr,
            #[cfg(feature = "future_dmq")]
            &self.dmq_node_socket_path,
            #[cfg(feature = "future_dmq")]
            &cardano_network,
            &aggregator_endpoint,
            logger,
        )
        .await?;
        if let Some(dial_to_address) = dial_to {
            relay.dial_peer(dial_to_address.clone())?;
        }

        loop {
            if let Err(err) = relay.tick().await {
                error!(logger, "RelayAggregator: tick error"; "error" => ?err);
            }
        }
    }
}

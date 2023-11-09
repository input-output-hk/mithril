use std::sync::Arc;

use clap::{Parser, ValueEnum};
use libp2p::Multiaddr;
use mithril_common::{StdResult, MITHRIL_API_VERSION_HEADER};
use mithril_relay::{client::P2PClient, relay::SignerRelay};
use reqwest::StatusCode;
use slog::{Drain, Level, Logger};
use slog_scope::{error, info};

#[derive(Parser, Debug, PartialEq, Clone)]
pub struct Config {
    /// Node type (relay or client)
    #[clap(long)]
    #[arg(value_enum)]
    node_type: NodeType,

    /// Pubsub topic name
    #[clap(long, default_value = "mithril/signatures")]
    topic_name: String,

    /// Peer listen port
    #[clap(long)]
    listen_port: Option<u16>,

    /// Dial to peer multi-address
    #[clap(long)]
    dial_to: Option<Multiaddr>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum NodeType {
    Signer,
    Aggregator,
    Peer,
}

#[tokio::main]
async fn main() -> StdResult<()> {
    let config = Config::parse();

    let log_level = Level::Debug;
    let _guard = slog_scope::set_global_logger(build_logger(log_level));

    let topic_name = config.topic_name;
    let node_type = config.node_type;
    let dial_to = config.dial_to;
    let addr: Multiaddr =
        format!("/ip4/0.0.0.0/tcp/{}", config.listen_port.unwrap_or(0)).parse()?;

    match node_type {
        NodeType::Signer => {
            let mut relay = SignerRelay::start(&topic_name, &addr).await?;
            if let Some(dial_to_address) = dial_to {
                relay.peer.dial(dial_to_address.clone())?;
            }
            loop {
                if let Err(err) = relay.tick().await {
                    error!("Relay signer: tick error"; "error" => format!("{err:#?}"));
                }
            }
        }
        NodeType::Aggregator => {
            let mut p2p_client = P2PClient::new(&topic_name, &addr)
                .start()
                .await
                .expect("relay aggregator start failed");
            if let Some(dial_to_address) = dial_to {
                p2p_client.peer.dial(dial_to_address.clone())?;
            }
            loop {
                match p2p_client.tick().await {
                    Ok(Some(p2p_client_event)) => {
                        if let Ok(Some(signature_message_received)) =
                            p2p_client.convert_event(p2p_client_event)
                        {
                            info!("Relay aggregator: received signature from p2p network"; "message" => format!("{signature_message_received:#?}"));

                            let response = reqwest::Client::new()
                                .post("http://localhost:8080/aggregator/register-signatures")
                                .json(&signature_message_received)
                                .header(MITHRIL_API_VERSION_HEADER, "0.1.13") // TODO: retrieve current version
                                .send()
                                .await;
                            match response {
                                Ok(response) => match response.status() {
                                    StatusCode::CREATED => {
                                        info!("Relay aggregator: sent successfully signature message to aggregator");
                                    }
                                    status => {
                                        error!("Relay aggregator: Post `/register-signatures` should have returned a 201 status code, got: {status}")
                                    }
                                },
                                Err(err) => error!(
                                    "Relay aggregator: Post `/register-signatures` failed: {err:?}"
                                ),
                            }
                        }
                    }
                    Ok(None) => {}
                    Err(err) => {
                        error!("Relay aggregator: tick error"; "error" => format!("{err:#?}"));
                    }
                }
            }
        }
        NodeType::Peer => {
            let mut p2p_client = P2PClient::new(&topic_name, &addr)
                .start()
                .await
                .expect("relay peer start failed");
            if let Some(dial_to_address) = dial_to {
                p2p_client.peer.dial(dial_to_address.clone())?;
            }
            loop {
                match p2p_client.tick().await {
                    Ok(_) => {}
                    Err(err) => {
                        error!("P2P Client: tick error"; "error" => format!("{err:#?}"));
                    }
                }
            }
        }
    }
}

fn build_logger(log_level: Level) -> Logger {
    let decorator = slog_term::TermDecorator::new().build();
    let drain = slog_term::CompactFormat::new(decorator).build().fuse();
    let drain = slog::LevelFilter::new(drain, log_level).fuse();
    let drain = slog_async::Async::new(drain).build().fuse();

    Logger::root(Arc::new(drain), slog::o!())
}

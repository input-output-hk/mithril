use clap::Parser;
use libp2p::Multiaddr;
use mithril_common::StdResult;
use mithril_relay::{client::P2PClient, relay::Relay};

#[derive(Parser, Debug, PartialEq, Clone)]
pub struct Config {
    /// Node type (relay or client)
    #[clap(long)]
    node_type: String,

    /// Pubsub topic name
    #[clap(long, default_value = "mithril/signatures")]
    topic_name: String,

    /// Quorum parameter
    #[clap(long)]
    dial_to: Option<Multiaddr>,
}

#[tokio::main]
async fn main() -> StdResult<()> {
    let config = Config::parse();
    let topic_name = config.topic_name;
    let node_type = config.node_type;
    let dial_to = config.dial_to;
    match node_type.as_ref() {
        "relay" => {
            println!("Start relay");
            let mut relay = Relay::start(&topic_name).await?;
            let relay_address = relay.address();
            println!(">> Relay_address is '{relay_address:?}'");
            if let Some(dial_to_address) = dial_to {
                relay.peer.dial(dial_to_address.clone())?;
            }
            loop {
                if let Err(err) = relay.tick().await {
                    println!("Tick error: {err:?}");
                }
            }
        }
        "client" => {
            println!("Start client");
            let mut p2p_client = P2PClient::new(&topic_name)
                .start()
                .await
                .expect("P2P client start failed");
            if let Some(dial_to_address) = dial_to {
                p2p_client.peer.dial(dial_to_address.clone())?;
            }
            loop {
                match p2p_client.tick().await {
                    Ok(Some(p2p_client_event)) => {
                        if let Ok(Some(signature_message_received)) =
                            p2p_client.convert_event(p2p_client_event)
                        {
                            println!(" ");
                            println!("************************************************");
                            println!(
                                "P2P Client received signature: {signature_message_received:#?}"
                            );
                            println!("************************************************");
                            println!(" ");
                        }
                    }
                    Ok(None) => {}
                    Err(err) => {
                        println!("Tick error: {err:?}");
                    }
                }
            }
        }
        node => panic!("node not handled {node}"),
    }
}

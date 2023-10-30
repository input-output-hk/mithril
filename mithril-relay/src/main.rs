use libp2p::Multiaddr;
use mithril_relay::{client::P2PClient, relay::Relay};

#[tokio::main]
async fn main() {
    let args: Vec<_> = std::env::args().collect();
    let topic_name = "mithril/signatures";

    let node_type = &args[1];
    match node_type.as_ref() {
        "relay" => {
            println!("Start relay");
            let mut relay = Relay::start(topic_name).await.expect("Relay start failed");
            let relay_address = relay.address();
            println!(">> Relay_address is '{relay_address:?}'");
            loop {
                relay.tick_peer().await.unwrap();
            }
        }
        "client" => {
            let dial_to_address: Multiaddr = args[2].parse().unwrap();
            println!("Start client");
            let mut p2p_client = P2PClient::new(topic_name)
                .start()
                .await
                .expect("P2P client start failed");
            p2p_client
                .peer
                .dial(dial_to_address.clone())
                .expect("P2P client dial to the relay should not fail");
            loop {
                if let Some(p2p_client_event) = p2p_client.tick_peer().await.unwrap() {
                    if let Ok(Some(signature_message_received)) =
                        p2p_client.consume(p2p_client_event)
                    {
                        println!(" ");
                        println!("************************************************");
                        println!("P2P Client received signature: {signature_message_received:#?}");
                        println!("************************************************");
                        println!(" ");
                    }
                }
            }
        }
        node => panic!("node not handled {node}"),
    }
}

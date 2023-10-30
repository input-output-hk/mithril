use libp2p::gossipsub;
use mithril_common::messages::RegisterSignatureMessage;
use mithril_relay::{
    client::P2PClient,
    peer::{PeerBehaviourEvent, PeerEvent},
    relay::Relay,
};
use reqwest::StatusCode;

// Launch a relay that connects to P2P network. The relay is a peer in the P2P
// network. The relay sends some signatures that must be received by other
// relays.

#[tokio::test]
async fn should_receive_signatures_from_signers_when_subscribed_to_pubsub() {
    let topic_name = "mithril/signatures";
    let mut relay = Relay::start(topic_name).await.expect("Relay start failed");
    let relay_address = relay.address();
    let relay_peer_address = relay.peer_address().unwrap();
    println!(">> Relay_address is '{relay_address:?}'");
    let mut p2p_client = P2PClient::new(topic_name)
        .start()
        .await
        .expect("P2P client start failed");
    p2p_client
        .peer
        .dial(relay_peer_address)
        .expect("the p2pclient dial to the relay should be successful");

    let mut signature_message_sent = RegisterSignatureMessage::dummy();
    signature_message_sent.party_id = format!("{}-new", signature_message_sent.party_id);

    let total_peers = 2;
    let mut total_connected_peers = 0;
    loop {
        if let Some(PeerEvent::Behaviour {
            event: PeerBehaviourEvent::Gossipsub(gossipsub::Event::Subscribed { .. }),
        }) = relay.tick_peer().await.unwrap()
        {
            println!("Relay event has subscribed to gossipsub topic");
            total_connected_peers += 1;
        }
        if let Some(PeerEvent::Behaviour {
            event: PeerBehaviourEvent::Gossipsub(gossipsub::Event::Subscribed { .. }),
        }) = p2p_client.tick_peer().await.unwrap()
        {
            println!("P2P Client event has subscribed to gossipsub topic");
            total_connected_peers += 1;
        }
        if total_connected_peers == total_peers {
            println!("All peers are connected to the gossipsub topic");
            break;
        }
    }

    let response = reqwest::Client::new()
        .post(format!("http://{}/register-signatures", relay_address))
        .json(&signature_message_sent)
        .send()
        .await;
    match response {
        Ok(response) => match response.status() {
            StatusCode::CREATED => {}
            status => {
                panic!("Post `/register-signatures` should have returned a 201 status code, got: {status}")
            }
        },
        Err(err) => panic!("Post `/register-signatures` failed: {err:?}"),
    }
    println!("Sent a signature to the relay via HTTP gateway");

    loop {
        let _ = relay.tick_peer().await.unwrap();
        if let Some(p2p_client_event) = p2p_client.tick_peer().await.unwrap() {
            if let Ok(Some(signature_message_received)) = p2p_client.consume(p2p_client_event) {
                println!("P2P Client consumed signature: {signature_message_received:#?}");
                assert_eq!(signature_message_sent, signature_message_received);
                break;
            }
        }
    }
}

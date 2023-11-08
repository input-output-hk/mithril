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
    let total_p2p_client = 2;
    let total_peers = 1 + total_p2p_client;
    let topic_name = "mithril/signatures";
    let mut relay = Relay::start(topic_name).await.expect("Relay start failed");
    let relay_address = relay.address();
    let relay_peer_address = relay.peer_address().unwrap();
    println!(">> Relay_address is '{relay_address:?}'");

    let mut p2p_client1 = P2PClient::new(topic_name)
        .start()
        .await
        .expect("P2P client start failed");
    p2p_client1
        .peer
        .dial(relay_peer_address.clone())
        .expect("P2P client dial to the relay should not fail");

    let mut p2p_client2 = P2PClient::new(topic_name)
        .start()
        .await
        .expect("P2P client start failed");
    p2p_client2
        .peer
        .dial(relay_peer_address.clone())
        .expect("P2P client dial to the relay should not fail");

    println!("Wait for Relay and P2P clients to subscribe to the pubsub topic");
    let mut total_peers_connected = 0;
    loop {
        println!(">> Subscribed peers: {total_peers_connected}/{total_peers}");
        tokio::select! {
            event =  relay.tick() => {
                if let Ok(Some(PeerEvent::Behaviour {
                    event: PeerBehaviourEvent::Gossipsub(gossipsub::Event::Subscribed { .. }),
                })) = event
                {
                    println!("Relay has subscribed to gossipsub topic");
                    total_peers_connected += 1;
                }
            },
            event =  p2p_client1.tick() => {
                if let Ok(Some(PeerEvent::Behaviour {
                    event: PeerBehaviourEvent::Gossipsub(gossipsub::Event::Subscribed { .. }),
                })) = event
                {
                    println!("P2P Client1 has subscribed to gossipsub topic");
                    total_peers_connected += 1;
                }
            }
            event =  p2p_client2.tick() => {
                if let Ok(Some(PeerEvent::Behaviour {
                    event: PeerBehaviourEvent::Gossipsub(gossipsub::Event::Subscribed { .. }),
                })) = event
                {
                    println!("P2P Client2 has subscribed to gossipsub topic");
                    total_peers_connected += 1;
                }
            }
        }
        if total_peers_connected == total_peers {
            println!("All peers are connected to the gossipsub topic");
            break;
        }
    }

    println!("Send a signature to the relay via HTTP gateway");
    let mut signature_message_sent = RegisterSignatureMessage::dummy();
    signature_message_sent.party_id = format!("{}-new", signature_message_sent.party_id);
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

    println!("Wait for P2P clients to receive the signature");
    let mut total_peers_has_received_message = 0;
    loop {
        tokio::select! {
            _event =  relay.tick() => {

            },
            event =  p2p_client1.tick() => {
                if let Ok(Some(signature_message_received)) = p2p_client1.convert_event(event.unwrap().unwrap())
                {
                    println!("P2P Client consumed signature: {signature_message_received:#?}");
                    assert_eq!(signature_message_sent, signature_message_received);
                    total_peers_has_received_message += 1
                }
            }
            event =  p2p_client2.tick() => {
                if let Ok(Some(signature_message_received)) = p2p_client2.convert_event(event.unwrap().unwrap())
                {
                    println!("P2P Client consumed signature: {signature_message_received:#?}");
                    assert_eq!(signature_message_sent, signature_message_received);
                    total_peers_has_received_message += 1
                }
            }
        }
        let _ = relay.tick().await.unwrap();
        if total_peers_has_received_message == total_p2p_client {
            println!("All P2P clients have consumed the signature");
            break;
        }
    }
}

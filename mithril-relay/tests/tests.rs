use mithril_common::messages::RegisterSignatureMessage;
use mithril_relay::{peer::P2PClient, relay::Relay};
use reqwest::StatusCode;

// Launch a relay that connects to P2P network. The relay is a peer in the P2P
// network. The relay sends some signatures that must be received by other
// relays.

#[tokio::test]
async fn should_receive_signatures_from_signers_when_subscribed_to_pubsub() {
    let topic_name = "mithril/signatures";
    let relay = Relay::start(topic_name).await.expect("Relay start failed");
    let relay_address = relay.address();
    let relay_peer_address = relay.peer_address().unwrap();
    println!("relay_address={relay_address:?}");
    let mut p2p_client = P2PClient::new(topic_name)
        .start()
        .await
        .expect("P2P client start failed");
    p2p_client
        .peer
        .dial(relay_peer_address)
        .expect("the p2pclient dial to the relay should be successful");

    let signature_message_sent = RegisterSignatureMessage::dummy();
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

    loop {
        if let Ok(Some(signature_message_received)) = p2p_client.consume().await {
            assert_eq!(signature_message_sent, signature_message_received);
        }
    }
}

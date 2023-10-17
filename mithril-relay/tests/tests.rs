use mithril_common::messages::RegisterSignatureMessage;
use mithril_relay::{peer::P2PClient, relay::Relay};
use reqwest::StatusCode;

// Launch a relay that connects to P2P network. The relay is a peer in the P2P
// network. The relay sends some signatures that must be received by other
// relays.

#[tokio::test]
async fn should_receive_signatures_from_signers_when_subscribed_to_pubsub() {
    let topic_name = "mithril/signatures";
    let mut relay = Relay::new(topic_name);
    let address = relay.start().await.expect("Relay start failed");
    let p2p_client = P2PClient::new(topic_name);

    let signature_message_sent = RegisterSignatureMessage::dummy();

    let response = reqwest::Client::new()
        .post(format!("{}/register-signatures", address))
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

    let signature_message_received = p2p_client
        .consume()
        .await
        .expect("should have received a single signature");
    assert_eq!(signature_message_sent, signature_message_received);
}

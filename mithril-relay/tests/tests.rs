use mithril_common::{messages::RegisterSignatureMessage, StdResult};
use reqwest::StatusCode;

pub struct Relay {}

impl Relay {
    pub fn new(_port: u16, _topic_name: &str) -> Self {
        todo!()
    }
}

pub struct P2PClient {}

impl P2PClient {
    pub fn new(_topic_name: &str) -> Self {
        todo!()
    }

    pub async fn consume(&self) -> StdResult<RegisterSignatureMessage> {
        todo!()
    }
}

// Launch a relay that connects to P2P network. The relay is a peer in the P2P
// network. The relay sends some signatures that must be received by other
// relays.

#[tokio::test]
async fn should_receive_signatures_from_signers_when_subscribed_to_pubsub() {
    let topic_name = "mithril/signatures";
    let _relay = Relay::new(8080, topic_name);
    let p2p_client = P2PClient::new(topic_name);

    let signature_message_sent = RegisterSignatureMessage::dummy();

    let response = reqwest::Client::new()
        .post("locahost:8080/register-signatures")
        .json(&signature_message_sent)
        .send()
        .await;

    match response {
        Ok(response) => match response.status() {
            StatusCode::CREATED => {}
            _ => {
                panic!("Should have returned a 201 status code")
            }
        },
        Err(err) => panic!("{err:?}"),
    }

    let signature_message_received = p2p_client
        .consume()
        .await
        .expect("should have received a single signature");
    assert_eq!(signature_message_sent, signature_message_received);
}

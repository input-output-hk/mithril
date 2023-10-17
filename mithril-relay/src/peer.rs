use mithril_common::{messages::RegisterSignatureMessage, StdResult};

pub struct P2PClient {}

impl P2PClient {
    pub fn new(_topic_name: &str) -> Self {
        todo!("P2PClient::new")
    }

    pub async fn consume(&self) -> StdResult<RegisterSignatureMessage> {
        todo!("P2PClient::consume")
    }
}

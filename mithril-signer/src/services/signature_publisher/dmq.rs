use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;

use mithril_common::{
    entities::{ProtocolMessage, SignedEntityType, SingleSignature},
    messages::RegisterSignatureMessageDmq,
    StdResult,
};
use mithril_dmq_node::DmqPublisher;

use super::SignaturePublisher;

/// DMQ implementation of the [SignaturePublisher] trait.
pub struct SignaturePublisherDmq {
    dmq_publisher: Arc<dyn DmqPublisher<RegisterSignatureMessageDmq>>,
}

impl SignaturePublisherDmq {
    /// Creates a new instance of [SignaturePublisherDmq].
    pub fn new(dmq_publisher: Arc<dyn DmqPublisher<RegisterSignatureMessageDmq>>) -> Self {
        Self { dmq_publisher }
    }
}

#[async_trait]
impl SignaturePublisher for SignaturePublisherDmq {
    async fn publish(
        &self,
        signed_entity_type: &SignedEntityType,
        signature: &SingleSignature,
        _protocol_message: &ProtocolMessage,
    ) -> StdResult<()> {
        let message = RegisterSignatureMessageDmq {
            signature: signature.signature.to_owned(),
            signed_entity_type: signed_entity_type.to_owned(),
        };

        self.dmq_publisher
            .publish_message(message)
            .await
            .with_context(|| "Failed to publish DMQ message")
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::test_utils::fake_data;
    use mithril_dmq_node::test::double::DmqPublisherFake;

    use super::*;

    #[tokio::test]
    async fn publish_signature_success() {
        let signed_entity_type = SignedEntityType::dummy();
        let signature = fake_data::single_signature(vec![1, 2, 3]);
        let protocol_message = ProtocolMessage::default();
        let dmq_publisher = Arc::new(DmqPublisherFake::new(vec![Ok(())]));
        let publisher = SignaturePublisherDmq::new(dmq_publisher);

        publisher
            .publish(&signed_entity_type, &signature, &protocol_message)
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn publish_signature_failure() {
        let signed_entity_type = SignedEntityType::dummy();
        let signature = fake_data::single_signature(vec![1, 2, 3]);
        let protocol_message = ProtocolMessage::default();
        let dmq_publisher = Arc::new(DmqPublisherFake::new(vec![Err(anyhow::anyhow!(
            "Test error"
        ))]));
        let publisher = SignaturePublisherDmq::new(dmq_publisher);

        publisher
            .publish(&signed_entity_type, &signature, &protocol_message)
            .await
            .expect_err("SignaturePublisherDmq should return an error");
    }
}

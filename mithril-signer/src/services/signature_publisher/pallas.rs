use anyhow::Context;
use async_trait::async_trait;

use mithril_common::{
    entities::{ProtocolMessage, SignedEntityType, SingleSignature},
    messages::RegisterSignatureMessageDmq,
    StdResult,
};
use mithril_dmq_node::{DmqPublisher, DmqPublisherPallas};

use super::SignaturePublisher;

#[async_trait]
impl SignaturePublisher for DmqPublisherPallas<RegisterSignatureMessageDmq> {
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
        self.publish_message(message)
            .await
            .with_context(|| "Failed to publish DMQ message")
    }
}

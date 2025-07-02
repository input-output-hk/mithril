use async_trait::async_trait;
use mithril_common::StdResult;
use mithril_common::entities::{ProtocolMessage, SignedEntityType, SingleSignature};

/// Publishes computed single signature to a third party.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait SignaturePublisher: Send + Sync {
    /// Publish computed single signature.
    async fn publish(
        &self,
        signed_entity_type: &SignedEntityType,
        signature: &SingleSignature,
        protocol_message: &ProtocolMessage,
    ) -> StdResult<()>;
}

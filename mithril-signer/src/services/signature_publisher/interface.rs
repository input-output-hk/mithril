use async_trait::async_trait;
use mithril_common::entities::{ProtocolMessage, SignedEntityType, SingleSignatures};
use mithril_common::StdResult;

/// Publishes computed single signature to a third party.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait SignaturePublisher: Send + Sync {
    /// Publish computed single signature.
    async fn publish(
        &self,
        signed_entity_type: &SignedEntityType,
        signature: &SingleSignatures,
        protocol_message: &ProtocolMessage,
    ) -> StdResult<()>;
}

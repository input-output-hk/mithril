use async_trait::async_trait;
use mithril_common::entities::{ProtocolMessage, SignedEntityType, SingleSignatures};
use mithril_common::StdResult;

/// Publishes computed single signatures to a third party.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait SignaturePublisher: Send + Sync {
    /// Publish computed single signatures.
    async fn publish(
        &self,
        signed_entity_type: &SignedEntityType,
        signatures: &SingleSignatures,
        protocol_message: &ProtocolMessage,
    ) -> StdResult<()>;
}

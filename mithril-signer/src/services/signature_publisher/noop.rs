use mithril_common::{
    StdResult,
    entities::{ProtocolMessage, SignedEntityType, SingleSignature},
};

use super::SignaturePublisher;

/// A no-op implementation of the [SignaturePublisher] trait.
/// This implementation performs no action when a signature is published.
pub struct SignaturePublisherNoop;

#[async_trait::async_trait]
impl SignaturePublisher for SignaturePublisherNoop {
    async fn publish(
        &self,
        _signed_entity_type: &SignedEntityType,
        _signature: &SingleSignature,
        _protocol_message: &ProtocolMessage,
    ) -> StdResult<()> {
        Ok(())
    }
}

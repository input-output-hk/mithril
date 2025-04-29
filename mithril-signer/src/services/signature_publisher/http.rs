use async_trait::async_trait;
use mithril_common::entities::{ProtocolMessage, SignedEntityType, SingleSignature};
use mithril_common::StdResult;

use crate::services::AggregatorClient;

use super::SignaturePublisher;

#[async_trait]
impl<T: AggregatorClient> SignaturePublisher for T {
    async fn publish(
        &self,
        signed_entity_type: &SignedEntityType,
        signature: &SingleSignature,
        protocol_message: &ProtocolMessage,
    ) -> StdResult<()> {
        self.register_signature(signed_entity_type, signature, protocol_message)
            .await?;
        Ok(())
    }
}

use async_trait::async_trait;
use mithril_common::entities::{ProtocolMessage, SignedEntityType, SingleSignatures};
use mithril_common::StdResult;

use crate::services::{AggregatorClient, SignaturePublisher};

#[async_trait]
impl<T: AggregatorClient> SignaturePublisher for T {
    async fn publish(
        &self,
        signed_entity_type: &SignedEntityType,
        signatures: &SingleSignatures,
        protocol_message: &ProtocolMessage,
    ) -> StdResult<()> {
        self.register_signatures(signed_entity_type, signatures, protocol_message)
            .await?;
        Ok(())
    }
}

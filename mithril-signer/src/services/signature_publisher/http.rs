use anyhow::Context;
use async_trait::async_trait;

use mithril_aggregator_client::{AggregatorHttpClient, query::PostRegisterSignatureQuery};
use mithril_common::{
    StdResult,
    entities::{ProtocolMessage, SignedEntityType, SingleSignature},
    messages::TryToMessageAdapter,
};

use crate::message_adapters::ToRegisterSignatureMessageAdapter;

use super::SignaturePublisher;

#[async_trait]
impl SignaturePublisher for AggregatorHttpClient {
    async fn publish(
        &self,
        signed_entity_type: &SignedEntityType,
        signature: &SingleSignature,
        protocol_message: &ProtocolMessage,
    ) -> StdResult<()> {
        let register_single_signature_message = ToRegisterSignatureMessageAdapter::try_adapt((
            signed_entity_type.to_owned(),
            signature.to_owned(),
            protocol_message,
        ))
        .with_context(|| "Failed to adapt message to register single signature message")?;

        self.send(PostRegisterSignatureQuery::new(
            register_single_signature_message,
        ))
        .await?;

        Ok(())
    }
}

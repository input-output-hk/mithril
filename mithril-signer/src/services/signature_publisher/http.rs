use async_trait::async_trait;
use mithril_common::entities::{ProtocolMessage, SignedEntityType, SingleSignatures};
use mithril_common::StdResult;
use std::sync::Arc;

use crate::services::{AggregatorClient, SignaturePublisher};

/// Publishes computed single signatures to an Aggregator http api.
pub struct AggregatorHttpSignaturePublisher {
    client: Arc<dyn AggregatorClient>,
}

impl AggregatorHttpSignaturePublisher {
    /// Creates a new instance of the `AggregatorHttpSignaturePublisher`.
    pub fn new(aggregator_client: Arc<dyn AggregatorClient>) -> Self {
        Self {
            client: aggregator_client,
        }
    }
}

#[async_trait]
impl SignaturePublisher for AggregatorHttpSignaturePublisher {
    async fn publish(
        &self,
        signed_entity_type: &SignedEntityType,
        signatures: &SingleSignatures,
        protocol_message: &ProtocolMessage,
    ) -> StdResult<()> {
        self.client
            .register_signatures(signed_entity_type, signatures, protocol_message)
            .await?;

        Ok(())
    }
}

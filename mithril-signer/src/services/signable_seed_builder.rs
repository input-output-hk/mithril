//! ## SignableSeedBuilderService
//!
//! This service is responsible for computing the seed protocol message
//! that is used by the [SignableBuilder] to compute the final protocol message.
//!
use anyhow::anyhow;
use async_trait::async_trait;
use std::sync::Arc;
use tokio::sync::RwLock;

use mithril_common::{
    entities::{ProtocolMessage, ProtocolMessagePartKey},
    signable_builder::SignableSeedBuilder,
    StdResult,
};

use crate::{
    services::{EpochService, SingleSigner},
    store::ProtocolInitializerStorer,
};

/// SignableSeedBuilder service
pub struct SignableSeedBuilderService {
    epoch_service: Arc<RwLock<dyn EpochService>>,
    single_signer: Arc<dyn SingleSigner>,
    protocol_initializer_store: Arc<dyn ProtocolInitializerStorer>,
}

impl SignableSeedBuilderService {
    /// SignableSeedBuilderService factory
    pub fn new(
        epoch_service: Arc<RwLock<dyn EpochService>>,
        single_signer: Arc<dyn SingleSigner>,
        protocol_initializer_store: Arc<dyn ProtocolInitializerStorer>,
    ) -> Self {
        Self {
            epoch_service,
            single_signer,
            protocol_initializer_store,
        }
    }
}

#[async_trait]
impl SignableSeedBuilder for SignableSeedBuilderService {
    /// Compute seed protocol message
    async fn compute_seed_protocol_message(&self) -> StdResult<ProtocolMessage> {
        let mut protocol_message = ProtocolMessage::new();
        let epoch_service = self.epoch_service.read().await;
        let epoch = (*epoch_service).epoch_of_current_data()?;
        let next_signer_retrieval_epoch = epoch.offset_to_next_signer_retrieval_epoch();
        let next_protocol_initializer = self
            .protocol_initializer_store
            .get_protocol_initializer(next_signer_retrieval_epoch)
            .await?
            .ok_or_else(|| {
                anyhow!("protocol_initializer at epoch {next_signer_retrieval_epoch}")
            })?;

        let next_signers_with_stake = epoch_service.next_signers_with_stake().await?;
        let avk = self
            .single_signer
            .compute_aggregate_verification_key(
                &next_signers_with_stake,
                &next_protocol_initializer,
            )?
            .ok_or_else(|| anyhow!("next_signers avk".to_string()))?;
        protocol_message
            .set_message_part(ProtocolMessagePartKey::NextAggregateVerificationKey, avk);

        Ok(protocol_message)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_compute_seed_protocol_message() {
        //TODO: implement test
    }
}

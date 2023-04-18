use std::sync::Arc;

use async_trait::async_trait;
use tokio::sync::RwLock;

use mithril_common::{
    entities::{Epoch, ProtocolMessage, ProtocolMessagePartKey},
    signable_builder::SignableBuilder,
    StdResult,
};

use crate::MultiSigner;

/// A [MithrilStakeDistributionSignable] builder
pub struct MithrilStakeDistributionSignableBuilder {
    multi_signer: Arc<RwLock<dyn MultiSigner>>,
}

impl MithrilStakeDistributionSignableBuilder {
    /// MithrilStakeDistribution signable builder factory
    pub fn new(multi_signer: Arc<RwLock<dyn MultiSigner>>) -> Self {
        Self { multi_signer }
    }
}

#[async_trait]
impl SignableBuilder<Epoch, ProtocolMessage> for MithrilStakeDistributionSignableBuilder {
    async fn compute_signable(&self, _beacon: Epoch) -> StdResult<ProtocolMessage> {
        let mut protocol_message = ProtocolMessage::new();
        let multi_signer = self.multi_signer.read().await;
        protocol_message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            multi_signer
                .compute_next_stake_distribution_aggregate_verification_key()
                .await?
                .unwrap_or_default(),
        );

        Ok(protocol_message)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::multi_signer::MockMultiSigner;

    #[tokio::test]
    async fn test_compute_signable() {
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_compute_next_stake_distribution_aggregate_verification_key()
            .return_once(|| Ok(Some("avk-123".to_string())));
        let mithril_stake_distribution_signable_builder =
            MithrilStakeDistributionSignableBuilder::new(Arc::new(RwLock::new(mock_multi_signer)));
        let signable = mithril_stake_distribution_signable_builder
            .compute_signable(Epoch(1))
            .await
            .unwrap();
        let mut signable_expected = ProtocolMessage::new();
        signable_expected.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            "avk-123".to_string(),
        );
        assert_eq!(signable_expected, signable);
    }
}

use async_trait::async_trait;

use crate::{
    entities::{Epoch, ProtocolMessage},
    signable_builder::SignableBuilder,
    StdResult,
};

/// A [MithrilStakeDistributionSignableBuilder] builder
#[derive(Default)]
pub struct MithrilStakeDistributionSignableBuilder {}

#[async_trait]
impl SignableBuilder<Epoch> for MithrilStakeDistributionSignableBuilder {
    async fn compute_protocol_message(
        &self,
        _beacon: Epoch,
        seed_protocol_message: ProtocolMessage,
    ) -> StdResult<ProtocolMessage> {
        Ok(seed_protocol_message)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_compute_signable() {
        let seed_protocol_message = ProtocolMessage::new();
        let mithril_stake_distribution_signable_builder =
            MithrilStakeDistributionSignableBuilder::default();
        let signable = mithril_stake_distribution_signable_builder
            .compute_protocol_message(Epoch(1), seed_protocol_message)
            .await
            .unwrap();
        let signable_expected = ProtocolMessage::new();
        assert_eq!(signable_expected, signable);
    }
}

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
impl SignableBuilder<Epoch, ProtocolMessage> for MithrilStakeDistributionSignableBuilder {
    // We just need to return an empty protocol message as the next AVK will be appended by the signing engine automatically
    async fn compute_signable(&self, _beacon: Epoch) -> StdResult<ProtocolMessage> {
        Ok(ProtocolMessage::new())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_compute_signable() {
        let mithril_stake_distribution_signable_builder =
            MithrilStakeDistributionSignableBuilder::default();
        let signable = mithril_stake_distribution_signable_builder
            .compute_signable(Epoch(1))
            .await
            .unwrap();
        let signable_expected = ProtocolMessage::new();
        assert_eq!(signable_expected, signable);
    }
}

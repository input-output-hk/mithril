use async_trait::async_trait;

use mithril_common::{
    entities::{Epoch, ProtocolMessage},
    signable_builder::SignableBuilder,
    StdResult,
};

/// A [MithrilStakeDistributionSignable] builder
pub struct MithrilStakeDistributionSignableBuilder {}

impl MithrilStakeDistributionSignableBuilder {
    /// MithrilStakeDistribution signable builder factory
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for MithrilStakeDistributionSignableBuilder {
    fn default() -> Self {
        Self::new()
    }
}

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
            MithrilStakeDistributionSignableBuilder::new();
        let signable = mithril_stake_distribution_signable_builder
            .compute_signable(Epoch(1))
            .await
            .unwrap();
        let signable_expected = ProtocolMessage::new();
        assert_eq!(signable_expected, signable);
    }
}

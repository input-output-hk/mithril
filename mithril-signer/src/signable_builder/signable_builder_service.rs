use std::sync::Arc;

use mithril_common::{
    entities::SignedEntityType,
    signable_builder::{
        CardanoImmutableFilesFullSignableBuilder, MithrilStakeDistributionSignableBuilder,
        Signable, SignableBuilder,
    },
    StdResult,
};

/// SignableBuilder Service
pub struct SignableBuilderService {
    immutable_signable_builder: CardanoImmutableFilesFullSignableBuilder,
    mithril_stake_distribution_builder: MithrilStakeDistributionSignableBuilder,
}

impl SignableBuilderService {
    /// SignableBuilderService factory
    pub fn new(
        immutable_signable_builder: CardanoImmutableFilesFullSignableBuilder,
        mithril_stake_distribution_builder: MithrilStakeDistributionSignableBuilder,
    ) -> Self {
        Self {
            immutable_signable_builder,
            mithril_stake_distribution_builder,
        }
    }
}

impl SignableBuilderService {
    #[allow(dead_code)]
    async fn compute_signable(
        &self,
        signed_entity_type: SignedEntityType,
    ) -> StdResult<Arc<dyn Signable>> {
        let signable: Arc<dyn Signable> = match signed_entity_type {
            SignedEntityType::CardanoImmutableFilesFull(beacon) => Arc::new(
                self.immutable_signable_builder
                    .compute_signable(beacon)
                    .await?,
            ),
            SignedEntityType::MithrilStakeDistribution(epoch) => Arc::new(
                self.mithril_stake_distribution_builder
                    .compute_signable(epoch)
                    .await?,
            ),
            _ => todo!(),
        };

        Ok(signable)
    }
}

#[cfg(test)]
mod tests {}

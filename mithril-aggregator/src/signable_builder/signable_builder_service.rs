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
    mithril_stake_distribution_builder: MithrilStakeDistributionSignableBuilder,
    immutable_signable_builder: CardanoImmutableFilesFullSignableBuilder,
}

impl SignableBuilderService {
    /// SignableBuilderService factory
    pub fn new(
        mithril_stake_distribution_builder: MithrilStakeDistributionSignableBuilder,
        immutable_signable_builder: CardanoImmutableFilesFullSignableBuilder,
    ) -> Self {
        Self {
            mithril_stake_distribution_builder,
            immutable_signable_builder,
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
            SignedEntityType::MithrilStakeDistribution(e) => Arc::new(
                self.mithril_stake_distribution_builder
                    .compute_signable(e)
                    .await?,
            ),
            SignedEntityType::CardanoImmutableFilesFull(beacon) => Arc::new(
                self.immutable_signable_builder
                    .compute_signable(beacon)
                    .await?,
            ),
            _ => todo!(),
        };

        Ok(signable)
    }
}

#[cfg(test)]
mod tests {}

use std::sync::Arc;

use mithril_common::{
    entities::SignedEntityType,
    signable_builder::{Signable, SignableBuilder},
    StdResult,
};

use super::MithrilStakeDistributionSignableBuilder;

/// SignableBuilder Service
pub struct SignableBuilderService {
    mithril_stake_distribution_builder: MithrilStakeDistributionSignableBuilder,
}

impl SignableBuilderService {
    /// SignableBuilderService factory
    pub fn new(
        mithril_stake_distribution_builder: MithrilStakeDistributionSignableBuilder,
    ) -> Self {
        Self {
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
            SignedEntityType::MithrilStakeDistribution(e) => Arc::new(
                self.mithril_stake_distribution_builder
                    .compute_signable(e)
                    .await?,
            ),
            _ => todo!(),
        };

        Ok(signable)
    }
}

#[cfg(test)]
mod tests {}

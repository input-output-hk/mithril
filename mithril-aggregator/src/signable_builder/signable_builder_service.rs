use std::sync::Arc;

use mithril_common::{
    entities::SignedEntityType,
    signable_builder::{DummyBeacon, DummySignableBuilder, Signable, SignableBuilder},
    StdResult,
};

/// SignableBuilder Service
// TODO: temporary implementation
pub struct SignableBuilderService {
    dummy_signable_builder: DummySignableBuilder,
}

impl SignableBuilderService {
    /// SignableBuilderService factory
    pub fn new(dummy_signable_builder: DummySignableBuilder) -> Self {
        Self {
            dummy_signable_builder,
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
                self.dummy_signable_builder
                    .compute_signable(DummyBeacon { epoch: e })
                    .await?,
            ),
            _ => todo!(),
        };

        Ok(signable)
    }
}

#[cfg(test)]
mod tests {}

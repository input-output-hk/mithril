use std::sync::Arc;

use mithril_common::{
    entities::SignedEntityType,
    signable_builder::{CardanoImmutableFilesFullSignableBuilder, Signable, SignableBuilder},
    StdResult,
};

/// SignableBuilder Service
// TODO: temporary implementation
pub struct SignableBuilderService {
    immutable_signable_builder: CardanoImmutableFilesFullSignableBuilder,
}

impl SignableBuilderService {
    /// SignableBuilderService factory
    pub fn new(immutable_signable_builder: CardanoImmutableFilesFullSignableBuilder) -> Self {
        Self {
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

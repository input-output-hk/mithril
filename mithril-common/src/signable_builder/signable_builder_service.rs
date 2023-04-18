use std::sync::Arc;

use crate::{
    entities::SignedEntityType,
    signable_builder::{DummyBeacon, DummySignableBuilder, Signable},
    StdResult,
};

use super::SignableBuilder;

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
            SignedEntityType::CardanoStakeDistribution(e) => Arc::new(
                self.dummy_signable_builder
                    .compute_signable(DummyBeacon { epoch: e })
                    .await?,
            ),
            SignedEntityType::CardanoImmutableFilesFull(b) => Arc::new(
                self.dummy_signable_builder
                    .compute_signable(DummyBeacon { epoch: b.epoch })
                    .await?,
            ),
        };

        Ok(signable)
    }
}

#[cfg(test)]
mod tests {
    use crate::entities::{Beacon, Epoch};

    use super::*;

    // TODO: temporary test
    #[tokio::test]
    async fn test_signable_builder_service() {
        let dummy_signable_builder = DummySignableBuilder::default();
        let signable_builder_service = SignableBuilderService::new(dummy_signable_builder);

        let signed_entity_type_1 = SignedEntityType::MithrilStakeDistribution(Epoch(1));
        let signable_1 = signable_builder_service
            .compute_signable(signed_entity_type_1)
            .await
            .unwrap();
        let protocol_message_1 = signable_1.compute_protocol_message().unwrap();

        let signed_entity_type_2 = SignedEntityType::CardanoStakeDistribution(Epoch(0));
        let signable_2 = signable_builder_service
            .compute_signable(signed_entity_type_2)
            .await
            .unwrap();
        let protocol_message_2 = signable_2.compute_protocol_message().unwrap();

        let signed_entity_type_3 = SignedEntityType::CardanoImmutableFilesFull(Beacon::default());
        let signable_3 = signable_builder_service
            .compute_signable(signed_entity_type_3)
            .await
            .unwrap();
        let protocol_message_3 = signable_3.compute_protocol_message().unwrap();

        assert_ne!(protocol_message_1, protocol_message_2);
        assert_ne!(protocol_message_1, protocol_message_3);
        assert_eq!(protocol_message_2, protocol_message_3);
    }
}

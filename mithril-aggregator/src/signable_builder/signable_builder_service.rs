use async_trait::async_trait;
use std::sync::Arc;

use mithril_common::{
    entities::{Beacon, Epoch, ProtocolMessage, SignedEntityType},
    signable_builder::{Signable, SignableBuilder},
    StdResult,
};

#[cfg(test)]
use mockall::automock;

/// ArtifactBuilder Service trait
#[cfg_attr(test, automock)]
#[async_trait]
pub trait SignableBuilderService: Send + Sync {
    /// Compute signable from signed entity type
    async fn compute_signable(
        &self,
        signed_entity_type: SignedEntityType,
    ) -> StdResult<Arc<dyn Signable>>;
}

/// Mithril Signable Builder Service
pub struct MithrilSignableBuilderService {
    mithril_stake_distribution_builder: Arc<dyn SignableBuilder<Epoch, ProtocolMessage>>,
    immutable_signable_builder: Arc<dyn SignableBuilder<Beacon, ProtocolMessage>>,
}

impl MithrilSignableBuilderService {
    /// MithrilSignableBuilderService factory
    pub fn new(
        mithril_stake_distribution_builder: Arc<dyn SignableBuilder<Epoch, ProtocolMessage>>,
        immutable_signable_builder: Arc<dyn SignableBuilder<Beacon, ProtocolMessage>>,
    ) -> Self {
        Self {
            mithril_stake_distribution_builder,
            immutable_signable_builder,
        }
    }
}

#[async_trait]
impl SignableBuilderService for MithrilSignableBuilderService {
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
mod tests {
    use super::*;

    use mithril_common::{
        entities::{Epoch, ProtocolMessage},
        signable_builder::{Beacon as Beaconnable, Signable, SignableBuilder},
        StdResult,
    };

    use async_trait::async_trait;
    use mockall::mock;

    mock! {
        SignableBuilderImpl<U, V> { }

        #[async_trait]
        impl<U, V> SignableBuilder<U, V> for SignableBuilderImpl<U, V> where
        U: Beaconnable,
        V: Signable,{

            async fn compute_signable(&self, beacon: U) -> StdResult<V>;
        }
    }

    #[tokio::test]
    async fn build_mithril_stake_distribution_signable_when_given_mithril_stake_distribution_entity_type(
    ) {
        let protocol_message = ProtocolMessage::new();
        let protocol_message_clone = protocol_message.clone();
        let mut mock_mithril_stake_distribution_signable_builder =
            MockSignableBuilderImpl::<Epoch, ProtocolMessage>::new();
        mock_mithril_stake_distribution_signable_builder
            .expect_compute_signable()
            .once()
            .return_once(move |_| Ok(protocol_message_clone));

        let mock_cardano_immutable_files_full_signable_builder =
            MockSignableBuilderImpl::<Beacon, ProtocolMessage>::new();

        let signable_builder_service = MithrilSignableBuilderService::new(
            Arc::new(mock_mithril_stake_distribution_signable_builder),
            Arc::new(mock_cardano_immutable_files_full_signable_builder),
        );

        let signed_entity_type = SignedEntityType::MithrilStakeDistribution(Epoch(1));
        signable_builder_service
            .compute_signable(signed_entity_type)
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn build_snapshot_signable_when_given_cardano_immutable_files_full_entity_type() {
        let protocol_message = ProtocolMessage::new();
        let protocol_message_clone = protocol_message.clone();
        let mock_mithril_stake_distribution_signable_builder =
            MockSignableBuilderImpl::<Epoch, ProtocolMessage>::new();

        let mut mock_cardano_immutable_files_full_signable_builder =
            MockSignableBuilderImpl::<Beacon, ProtocolMessage>::new();
        mock_cardano_immutable_files_full_signable_builder
            .expect_compute_signable()
            .once()
            .return_once(move |_| Ok(protocol_message_clone));

        let signable_builder_service = MithrilSignableBuilderService::new(
            Arc::new(mock_mithril_stake_distribution_signable_builder),
            Arc::new(mock_cardano_immutable_files_full_signable_builder),
        );

        let signed_entity_type = SignedEntityType::CardanoImmutableFilesFull(Beacon::default());
        signable_builder_service
            .compute_signable(signed_entity_type)
            .await
            .unwrap();
    }
}

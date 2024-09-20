use anyhow::Context;
use async_trait::async_trait;
use std::sync::Arc;

use crate::{
    entities::{BlockNumber, CardanoDbBeacon, Epoch, ProtocolMessage, SignedEntityType},
    signable_builder::{SignableBuilder, SignableSeedBuilder},
    StdResult,
};

#[cfg(test)]
use mockall::automock;

/// ArtifactBuilder Service trait
#[cfg_attr(test, automock)]
#[async_trait]
pub trait SignableBuilderService: Send + Sync {
    /// Compute signable from signed entity type
    async fn compute_protocol_message(
        &self,
        signed_entity_type: SignedEntityType,
    ) -> StdResult<ProtocolMessage>;
}

/// Mithril Signable Builder Service
pub struct MithrilSignableBuilderService {
    seed_signable_builder: Arc<dyn SignableSeedBuilder>,
    mithril_stake_distribution_builder: Arc<dyn SignableBuilder<Epoch>>,
    immutable_signable_builder: Arc<dyn SignableBuilder<CardanoDbBeacon>>,
    cardano_transactions_signable_builder: Arc<dyn SignableBuilder<BlockNumber>>,
    cardano_stake_distribution_builder: Arc<dyn SignableBuilder<Epoch>>,
}

impl MithrilSignableBuilderService {
    /// MithrilSignableBuilderService factory
    pub fn new(
        seed_signable_builder: Arc<dyn SignableSeedBuilder>,
        mithril_stake_distribution_builder: Arc<dyn SignableBuilder<Epoch>>,
        immutable_signable_builder: Arc<dyn SignableBuilder<CardanoDbBeacon>>,
        cardano_transactions_signable_builder: Arc<dyn SignableBuilder<BlockNumber>>,
        cardano_stake_distribution_builder: Arc<dyn SignableBuilder<Epoch>>,
    ) -> Self {
        Self {
            seed_signable_builder,
            mithril_stake_distribution_builder,
            immutable_signable_builder,
            cardano_transactions_signable_builder,
            cardano_stake_distribution_builder,
        }
    }
}

#[async_trait]
impl SignableBuilderService for MithrilSignableBuilderService {
    async fn compute_protocol_message(
        &self,
        signed_entity_type: SignedEntityType,
    ) -> StdResult<ProtocolMessage> {
        let seed_protocol_message = self
            .seed_signable_builder
            .compute_seed_protocol_message()
            .await?;
        let protocol_message = match signed_entity_type {
            SignedEntityType::MithrilStakeDistribution(e) => self
                .mithril_stake_distribution_builder
                .compute_protocol_message(e, seed_protocol_message)
                .await
                .with_context(|| format!(
                    "Signable builder service can not compute protocol message with epoch: '{e}'"
                ))?,
            SignedEntityType::CardanoImmutableFilesFull(beacon) => self
                .immutable_signable_builder
                .compute_protocol_message(beacon.clone(), seed_protocol_message)
                .await
                .with_context(|| format!(
                    "Signable builder service can not compute protocol message with beacon: '{beacon}'"
                ))?,
            SignedEntityType::CardanoStakeDistribution(e) => self
                .cardano_stake_distribution_builder
                .compute_protocol_message(e, seed_protocol_message)
                .await
                .with_context(|| "Signable builder service can not compute protocol message for Cardano stake distribution with epoch: '{e}")?,
            SignedEntityType::CardanoTransactions(_, block_number) => self
                .cardano_transactions_signable_builder
                .compute_protocol_message(block_number, seed_protocol_message)
                .await
                .with_context(|| format!(
                    "Signable builder service can not compute protocol message with block_number: '{block_number}'"
                ))?,
        };

        Ok(protocol_message)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::{
        entities::{BlockNumber, Epoch, ProtocolMessage},
        signable_builder::{Beacon as Beaconnable, SignableBuilder},
        StdResult,
    };

    use async_trait::async_trait;
    use mockall::mock;

    mock! {
        SignableBuilderImpl<U> { }

        #[async_trait]
        impl<U> SignableBuilder<U> for SignableBuilderImpl<U> where U: Beaconnable,
        {
            async fn compute_protocol_message(&self, beacon: U, seed_protocol_message: ProtocolMessage) -> StdResult<ProtocolMessage>;
        }
    }

    mock! {
        SignableSeedBuilderImpl { }

        #[async_trait]
        impl SignableSeedBuilder for SignableSeedBuilderImpl {
            async fn compute_seed_protocol_message(&self) -> StdResult<ProtocolMessage>;
        }
    }

    #[tokio::test]
    async fn build_mithril_stake_distribution_signable_when_given_mithril_stake_distribution_entity_type(
    ) {
        let protocol_message = ProtocolMessage::new();
        let protocol_message_clone = protocol_message.clone();
        let mut mock_signable_seed_builder = MockSignableSeedBuilderImpl::new();
        mock_signable_seed_builder
            .expect_compute_seed_protocol_message()
            .once()
            .return_once(move || Ok(ProtocolMessage::new()));
        let mut mock_mithril_stake_distribution_signable_builder =
            MockSignableBuilderImpl::<Epoch>::new();
        mock_mithril_stake_distribution_signable_builder
            .expect_compute_protocol_message()
            .once()
            .return_once(move |_, _| Ok(protocol_message_clone));
        let mock_cardano_immutable_files_full_signable_builder =
            MockSignableBuilderImpl::<CardanoDbBeacon>::new();
        let mock_cardano_transactions_signable_builder =
            MockSignableBuilderImpl::<BlockNumber>::new();
        let mock_cardano_stake_distribution_signable_builder =
            MockSignableBuilderImpl::<Epoch>::new();

        // TODO: refactor mock builders
        let signable_builder_service = MithrilSignableBuilderService::new(
            Arc::new(mock_signable_seed_builder),
            Arc::new(mock_mithril_stake_distribution_signable_builder),
            Arc::new(mock_cardano_immutable_files_full_signable_builder),
            Arc::new(mock_cardano_transactions_signable_builder),
            Arc::new(mock_cardano_stake_distribution_signable_builder),
        );

        let signed_entity_type = SignedEntityType::MithrilStakeDistribution(Epoch(1));
        signable_builder_service
            .compute_protocol_message(signed_entity_type)
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn build_snapshot_signable_when_given_cardano_immutable_files_full_entity_type() {
        let protocol_message = ProtocolMessage::new();
        let protocol_message_clone = protocol_message.clone();
        let mut mock_signable_seed_builder = MockSignableSeedBuilderImpl::new();
        mock_signable_seed_builder
            .expect_compute_seed_protocol_message()
            .once()
            .return_once(move || Ok(ProtocolMessage::new()));
        let mock_mithril_stake_distribution_signable_builder =
            MockSignableBuilderImpl::<Epoch>::new();
        let mut mock_cardano_immutable_files_full_signable_builder =
            MockSignableBuilderImpl::<CardanoDbBeacon>::new();
        mock_cardano_immutable_files_full_signable_builder
            .expect_compute_protocol_message()
            .once()
            .return_once(move |_, _| Ok(protocol_message_clone));
        let mock_cardano_transactions_signable_builder =
            MockSignableBuilderImpl::<BlockNumber>::new();
        let mock_cardano_stake_distribution_signable_builder =
            MockSignableBuilderImpl::<Epoch>::new();

        let signable_builder_service = MithrilSignableBuilderService::new(
            Arc::new(mock_signable_seed_builder),
            Arc::new(mock_mithril_stake_distribution_signable_builder),
            Arc::new(mock_cardano_immutable_files_full_signable_builder),
            Arc::new(mock_cardano_transactions_signable_builder),
            Arc::new(mock_cardano_stake_distribution_signable_builder),
        );

        let signed_entity_type =
            SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::default());
        signable_builder_service
            .compute_protocol_message(signed_entity_type)
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn build_transactions_signable_when_given_cardano_transactions_entity_type() {
        let protocol_message = ProtocolMessage::new();
        let protocol_message_clone = protocol_message.clone();
        let mut mock_signable_seed_builder = MockSignableSeedBuilderImpl::new();
        mock_signable_seed_builder
            .expect_compute_seed_protocol_message()
            .once()
            .return_once(move || Ok(ProtocolMessage::new()));
        let mock_mithril_stake_distribution_signable_builder =
            MockSignableBuilderImpl::<Epoch>::new();
        let mock_cardano_immutable_files_full_signable_builder =
            MockSignableBuilderImpl::<CardanoDbBeacon>::new();
        let mut mock_cardano_transactions_signable_builder =
            MockSignableBuilderImpl::<BlockNumber>::new();
        mock_cardano_transactions_signable_builder
            .expect_compute_protocol_message()
            .once()
            .return_once(move |_, _| Ok(protocol_message_clone));
        let mock_cardano_stake_distribution_signable_builder =
            MockSignableBuilderImpl::<Epoch>::new();

        let signable_builder_service = MithrilSignableBuilderService::new(
            Arc::new(mock_signable_seed_builder),
            Arc::new(mock_mithril_stake_distribution_signable_builder),
            Arc::new(mock_cardano_immutable_files_full_signable_builder),
            Arc::new(mock_cardano_transactions_signable_builder),
            Arc::new(mock_cardano_stake_distribution_signable_builder),
        );

        let signed_entity_type = SignedEntityType::CardanoTransactions(Epoch(5), BlockNumber(1000));
        signable_builder_service
            .compute_protocol_message(signed_entity_type)
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn build_cardano_stake_distribution_signable_when_given_cardano_stake_distribution_entity_type(
    ) {
        let protocol_message = ProtocolMessage::new();
        let protocol_message_clone = protocol_message.clone();
        let mut mock_signable_seed_builder = MockSignableSeedBuilderImpl::new();
        mock_signable_seed_builder
            .expect_compute_seed_protocol_message()
            .once()
            .return_once(move || Ok(ProtocolMessage::new()));
        let mock_mithril_stake_distribution_signable_builder =
            MockSignableBuilderImpl::<Epoch>::new();
        let mock_cardano_immutable_files_full_signable_builder =
            MockSignableBuilderImpl::<CardanoDbBeacon>::new();
        let mock_cardano_transactions_signable_builder =
            MockSignableBuilderImpl::<BlockNumber>::new();
        let mut mock_cardano_stake_distribution_signable_builder =
            MockSignableBuilderImpl::<Epoch>::new();
        mock_cardano_stake_distribution_signable_builder
            .expect_compute_protocol_message()
            .once()
            .return_once(move |_, _| Ok(protocol_message_clone));

        let signable_builder_service = MithrilSignableBuilderService::new(
            Arc::new(mock_signable_seed_builder),
            Arc::new(mock_mithril_stake_distribution_signable_builder),
            Arc::new(mock_cardano_immutable_files_full_signable_builder),
            Arc::new(mock_cardano_transactions_signable_builder),
            Arc::new(mock_cardano_stake_distribution_signable_builder),
        );

        let signed_entity_type = SignedEntityType::CardanoStakeDistribution(Epoch(5));
        signable_builder_service
            .compute_protocol_message(signed_entity_type)
            .await
            .unwrap();
    }
}

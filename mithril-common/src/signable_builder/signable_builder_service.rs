use anyhow::Context;
use async_trait::async_trait;
use std::sync::Arc;

use crate::{
    entities::{BlockNumber, CardanoDbBeacon, Epoch, ProtocolMessage, SignedEntityType},
    signable_builder::SignableBuilder,
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
    mithril_stake_distribution_builder: Arc<dyn SignableBuilder<Epoch>>,
    immutable_signable_builder: Arc<dyn SignableBuilder<CardanoDbBeacon>>,
    cardano_transactions_signable_builder: Arc<dyn SignableBuilder<BlockNumber>>,
}

impl MithrilSignableBuilderService {
    /// MithrilSignableBuilderService factory
    pub fn new(
        mithril_stake_distribution_builder: Arc<dyn SignableBuilder<Epoch>>,
        immutable_signable_builder: Arc<dyn SignableBuilder<CardanoDbBeacon>>,
        cardano_transactions_signable_builder: Arc<dyn SignableBuilder<BlockNumber>>,
    ) -> Self {
        Self {
            mithril_stake_distribution_builder,
            immutable_signable_builder,
            cardano_transactions_signable_builder,
        }
    }
}

#[async_trait]
impl SignableBuilderService for MithrilSignableBuilderService {
    async fn compute_protocol_message(
        &self,
        signed_entity_type: SignedEntityType,
    ) -> StdResult<ProtocolMessage> {
        let protocol_message = match signed_entity_type {
            SignedEntityType::MithrilStakeDistribution(e) => self
                .mithril_stake_distribution_builder
                .compute_protocol_message(e)
                .await
                .with_context(|| format!(
                    "Signable builder service can not compute protocol message with epoch: '{e}'"
                ))?,
            SignedEntityType::CardanoImmutableFilesFull(beacon) => self
                .immutable_signable_builder
                .compute_protocol_message(beacon.clone())
                .await
                .with_context(|| format!(
                    "Signable builder service can not compute protocol message with beacon: '{beacon}'"
                ))?,
            SignedEntityType::CardanoStakeDistribution(_) => todo!(),
            SignedEntityType::CardanoTransactions(_, block_number) => self
                .cardano_transactions_signable_builder
                .compute_protocol_message(block_number)
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

            async fn compute_protocol_message(&self, beacon: U) -> StdResult<ProtocolMessage>;
        }
    }

    #[tokio::test]
    async fn build_mithril_stake_distribution_signable_when_given_mithril_stake_distribution_entity_type(
    ) {
        let protocol_message = ProtocolMessage::new();
        let protocol_message_clone = protocol_message.clone();
        let mut mock_mithril_stake_distribution_signable_builder =
            MockSignableBuilderImpl::<Epoch>::new();
        mock_mithril_stake_distribution_signable_builder
            .expect_compute_protocol_message()
            .once()
            .return_once(move |_| Ok(protocol_message_clone));

        let mock_cardano_immutable_files_full_signable_builder =
            MockSignableBuilderImpl::<CardanoDbBeacon>::new();
        let mock_cardano_transactions_signable_builder =
            MockSignableBuilderImpl::<BlockNumber>::new();

        let signable_builder_service = MithrilSignableBuilderService::new(
            Arc::new(mock_mithril_stake_distribution_signable_builder),
            Arc::new(mock_cardano_immutable_files_full_signable_builder),
            Arc::new(mock_cardano_transactions_signable_builder),
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
        let mock_mithril_stake_distribution_signable_builder =
            MockSignableBuilderImpl::<Epoch>::new();

        let mut mock_cardano_immutable_files_full_signable_builder =
            MockSignableBuilderImpl::<CardanoDbBeacon>::new();
        mock_cardano_immutable_files_full_signable_builder
            .expect_compute_protocol_message()
            .once()
            .return_once(move |_| Ok(protocol_message_clone));
        let mock_cardano_transactions_signable_builder =
            MockSignableBuilderImpl::<BlockNumber>::new();

        let signable_builder_service = MithrilSignableBuilderService::new(
            Arc::new(mock_mithril_stake_distribution_signable_builder),
            Arc::new(mock_cardano_immutable_files_full_signable_builder),
            Arc::new(mock_cardano_transactions_signable_builder),
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
        let mock_mithril_stake_distribution_signable_builder =
            MockSignableBuilderImpl::<Epoch>::new();

        let mock_cardano_immutable_files_full_signable_builder =
            MockSignableBuilderImpl::<CardanoDbBeacon>::new();
        let mut mock_cardano_transactions_signable_builder =
            MockSignableBuilderImpl::<BlockNumber>::new();
        mock_cardano_transactions_signable_builder
            .expect_compute_protocol_message()
            .once()
            .return_once(move |_| Ok(protocol_message_clone));

        let signable_builder_service = MithrilSignableBuilderService::new(
            Arc::new(mock_mithril_stake_distribution_signable_builder),
            Arc::new(mock_cardano_immutable_files_full_signable_builder),
            Arc::new(mock_cardano_transactions_signable_builder),
        );

        let signed_entity_type = SignedEntityType::CardanoTransactions(Epoch(5), 1000);
        signable_builder_service
            .compute_protocol_message(signed_entity_type)
            .await
            .unwrap();
    }
}

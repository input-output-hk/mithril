//! ## SignableSeedBuilderService
//!
//! This service is responsible for computing the seed protocol message
//! that is used by the [SignableBuilder] to compute the final protocol message.
//!
use anyhow::anyhow;
use async_trait::async_trait;
use std::sync::Arc;
use tokio::sync::RwLock;

use mithril_common::{
    entities::{ProtocolMessage, ProtocolMessagePartKey},
    signable_builder::SignableSeedBuilder,
    StdResult,
};

use crate::{
    services::{EpochService, SingleSigner},
    store::ProtocolInitializerStorer,
};

/// SignableSeedBuilder service
pub struct SignableSeedBuilderService {
    epoch_service: Arc<RwLock<dyn EpochService>>,
    single_signer: Arc<dyn SingleSigner>,
    protocol_initializer_store: Arc<dyn ProtocolInitializerStorer>,
}

impl SignableSeedBuilderService {
    /// SignableSeedBuilderService factory
    pub fn new(
        epoch_service: Arc<RwLock<dyn EpochService>>,
        single_signer: Arc<dyn SingleSigner>,
        protocol_initializer_store: Arc<dyn ProtocolInitializerStorer>,
    ) -> Self {
        Self {
            epoch_service,
            single_signer,
            protocol_initializer_store,
        }
    }
}

#[async_trait]
impl SignableSeedBuilder for SignableSeedBuilderService {
    /// Compute seed protocol message
    async fn compute_seed_protocol_message(&self) -> StdResult<ProtocolMessage> {
        let mut protocol_message = ProtocolMessage::new();
        let epoch_service = self.epoch_service.read().await;
        let epoch = (*epoch_service).epoch_of_current_data()?;
        let next_signer_retrieval_epoch = epoch.offset_to_next_signer_retrieval_epoch();
        let next_protocol_initializer = self
            .protocol_initializer_store
            .get_protocol_initializer(next_signer_retrieval_epoch)
            .await?
            .ok_or_else(|| {
                anyhow!("protocol_initializer at epoch {next_signer_retrieval_epoch}")
            })?;

        let next_signers_with_stake = epoch_service.next_signers_with_stake().await?;
        let avk = self
            .single_signer
            .compute_aggregate_verification_key(
                &next_signers_with_stake,
                &next_protocol_initializer,
            )?
            .ok_or_else(|| anyhow!("next_signers avk".to_string()))?;
        protocol_message
            .set_message_part(ProtocolMessagePartKey::NextAggregateVerificationKey, avk);

        Ok(protocol_message)
    }
}

#[cfg(test)]
mod tests {

    use mockall::mock;

    use mithril_common::{
        crypto_helper::ProtocolInitializer,
        entities::{
            CardanoTransactionsSigningConfig, Epoch, EpochSettings, PartyId, ProtocolParameters,
            Signer, SignerWithStake,
        },
        test_utils::MithrilFixtureBuilder,
    };

    use crate::{services::MockSingleSigner, store::MockProtocolInitializerStorer};

    use super::*;

    mock! {
        pub EpochServiceImpl {}

        #[async_trait]
        impl EpochService for EpochServiceImpl {
            /// Inform the service a new epoch has been detected, telling it to update its
            /// internal state for the new epoch.
            fn inform_epoch_settings(&mut self, epoch_settings: EpochSettings) -> StdResult<()>;

            /// Get the current epoch for which the data stored in this service are computed.
            fn epoch_of_current_data(&self) -> StdResult<Epoch>;

            /// Get next protocol parameters used in next epoch (associated with the actual epoch)
            fn next_protocol_parameters(&self) -> StdResult<&'static ProtocolParameters>;

            /// Get signers for the current epoch
            fn current_signers(&self) -> StdResult<&'static Vec<Signer>>;

            /// Get signers for the next epoch
            fn next_signers(&self) -> StdResult<&'static Vec<Signer>>;

            /// Get signers with stake for the current epoch
            async fn current_signers_with_stake(&self) -> StdResult<Vec<SignerWithStake>>;

            /// Get signers with stake for the next epoch
            async fn next_signers_with_stake(&self) -> StdResult<Vec<SignerWithStake>>;

            /// Get the cardano transactions signing configuration for the current epoch
            fn cardano_transactions_signing_config(
                &self,
            ) -> StdResult<&'static Option<CardanoTransactionsSigningConfig>>;

            /// Get the cardano transactions signing configuration for the next epoch
            fn next_cardano_transactions_signing_config(
                &self,
            ) -> StdResult<&'static Option<CardanoTransactionsSigningConfig>>;

            /// Check if a signer is included in the current stake distribution
            fn is_signer_included_in_current_stake_distribution(
                &self,
                party_id: PartyId,
                protocol_initializer: ProtocolInitializer,
            ) -> StdResult<bool>;
        }
    }

    #[tokio::test]
    async fn test_compute_seed_protocol_message() {
        let epoch = Epoch(5);
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let next_fixture = MithrilFixtureBuilder::default().with_signers(4).build();
        let expected_next_aggregate_verification_key = next_fixture.compute_and_encode_avk();
        let expected_next_aggregate_verification_key_clone =
            expected_next_aggregate_verification_key.clone();
        let protocol_initializer = fixture.signers_fixture()[0].protocol_initializer.clone();
        let mut mock_epoch_service = MockEpochServiceImpl::new();
        mock_epoch_service
            .expect_epoch_of_current_data()
            .return_once(move || Ok(epoch))
            .once();
        mock_epoch_service
            .expect_next_signers_with_stake()
            .return_once(move || Ok(Vec::new()))
            .once();
        let mut mock_single_signer = MockSingleSigner::new();
        mock_single_signer
            .expect_compute_aggregate_verification_key()
            .return_once(move |_, _| Ok(Some(expected_next_aggregate_verification_key_clone)))
            .once();
        let mut mock_protocol_initializer_store = MockProtocolInitializerStorer::new();
        mock_protocol_initializer_store
            .expect_get_protocol_initializer()
            .return_once(move |_| Ok(Some(protocol_initializer)))
            .once();
        let epoch_service = Arc::new(RwLock::new(mock_epoch_service));
        let single_signer = Arc::new(mock_single_signer);
        let protocol_initializer_store = Arc::new(mock_protocol_initializer_store);
        let signable_seed_builder_service = SignableSeedBuilderService::new(
            epoch_service,
            single_signer,
            protocol_initializer_store,
        );

        let protocol_message = signable_seed_builder_service
            .compute_seed_protocol_message()
            .await
            .unwrap();

        assert_eq!(
            protocol_message
                .get_message_part(&ProtocolMessagePartKey::NextAggregateVerificationKey),
            Some(&expected_next_aggregate_verification_key)
        );
    }
}

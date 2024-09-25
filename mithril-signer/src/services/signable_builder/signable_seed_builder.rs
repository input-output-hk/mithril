//! ## SignerSignableSeedBuilder
//!
//! This service is responsible for computing the seed protocol message
//! that is used by the [SignableBuilder] to compute the final protocol message.
//!
use anyhow::anyhow;
use async_trait::async_trait;
use std::sync::Arc;
use tokio::sync::RwLock;

use mithril_common::{
    entities::ProtocolMessagePartValue, signable_builder::SignableSeedBuilder, StdResult,
};

use crate::{
    services::{EpochService, SingleSigner},
    store::ProtocolInitializerStorer,
};

/// SignableSeedBuilder signer implementation
pub struct SignerSignableSeedBuilder {
    epoch_service: Arc<RwLock<dyn EpochService>>,
    single_signer: Arc<dyn SingleSigner>,
    protocol_initializer_store: Arc<dyn ProtocolInitializerStorer>,
}

impl SignerSignableSeedBuilder {
    /// SignerSignableSeedBuilder factory
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
impl SignableSeedBuilder for SignerSignableSeedBuilder {
    async fn compute_next_aggregate_verification_key_protocol_message_part_value(
        &self,
    ) -> StdResult<ProtocolMessagePartValue> {
        let epoch_service = self.epoch_service.read().await;
        let epoch = (*epoch_service).epoch_of_current_data()?;
        let next_signer_retrieval_epoch = epoch.offset_to_next_signer_retrieval_epoch();
        let next_protocol_initializer = self
            .protocol_initializer_store
            .get_protocol_initializer(next_signer_retrieval_epoch)
            .await?
            .ok_or_else(|| {
                anyhow!("can not get protocol_initializer at epoch {next_signer_retrieval_epoch}")
            })?;

        let next_signers_with_stake = epoch_service.next_signers_with_stake().await?;
        let next_aggregate_verification_key = self
            .single_signer
            .compute_aggregate_verification_key(
                &next_signers_with_stake,
                &next_protocol_initializer,
            )?
            .ok_or_else(|| anyhow!("next_signers avk".to_string()))?;

        Ok(next_aggregate_verification_key)
    }

    /// Compute next protocol parameters protocol message part value
    async fn compute_next_protocol_parameters_protocol_message_part_value(
        &self,
    ) -> StdResult<ProtocolMessagePartValue> {
        let epoch_service = self.epoch_service.read().await;
        let next_protocol_parameters = epoch_service.next_protocol_parameters()?.compute_hash();

        Ok(next_protocol_parameters)
    }
}

#[cfg(test)]
mod tests {

    use mithril_common::{
        entities::Epoch, entities::ProtocolParameters, test_utils::MithrilFixtureBuilder,
    };

    use crate::{
        services::{mock_epoch_service::MockEpochServiceImpl, MockSingleSigner},
        store::MockProtocolInitializerStorer,
    };

    use super::*;

    #[tokio::test]
    async fn test_compute_next_aggregate_verification_key_protocol_message_value() {
        let epoch = Epoch(5);
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let next_fixture = MithrilFixtureBuilder::default().with_signers(4).build();
        let expected_next_aggregate_verification_key = next_fixture.compute_and_encode_avk();
        let expected_next_aggregate_verification_key_clone =
            expected_next_aggregate_verification_key.clone();
        let protocol_initializer = fixture.signers_fixture()[0].protocol_initializer.clone();
        let mock_epoch_service = MockEpochServiceImpl::new_with_config(|mock_epoch_service| {
            mock_epoch_service
                .expect_epoch_of_current_data()
                .return_once(move || Ok(epoch))
                .once();
            mock_epoch_service
                .expect_next_signers_with_stake()
                .return_once(move || Ok(Vec::new()))
                .once();
        });
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
        let signable_seed_builder = SignerSignableSeedBuilder::new(
            epoch_service,
            single_signer,
            protocol_initializer_store,
        );

        let next_aggregate_verification_key = signable_seed_builder
            .compute_next_aggregate_verification_key_protocol_message_part_value()
            .await
            .unwrap();

        assert_eq!(
            next_aggregate_verification_key,
            expected_next_aggregate_verification_key
        );
    }

    #[tokio::test]
    async fn test_compute_next_protocol_parameters_protocol_message_value() {
        const NEXT_PROTOCOL_PARAMETERS: ProtocolParameters = ProtocolParameters {
            k: 10,
            m: 10,
            phi_f: 1.0,
        };
        let expected_next_protocol_parameters = NEXT_PROTOCOL_PARAMETERS.compute_hash();
        let mock_epoch_service = MockEpochServiceImpl::new_with_config(|mock_epoch_service| {
            mock_epoch_service
                .expect_next_protocol_parameters()
                .return_once(|| Ok(&NEXT_PROTOCOL_PARAMETERS))
                .once();
        });
        let mock_single_signer = MockSingleSigner::new();
        let mock_protocol_initializer_store = MockProtocolInitializerStorer::new();
        let epoch_service = Arc::new(RwLock::new(mock_epoch_service));
        let single_signer = Arc::new(mock_single_signer);
        let protocol_initializer_store = Arc::new(mock_protocol_initializer_store);
        let signable_seed_builder = SignerSignableSeedBuilder::new(
            epoch_service,
            single_signer,
            protocol_initializer_store,
        );

        let next_protocol_parameters = signable_seed_builder
            .compute_next_protocol_parameters_protocol_message_part_value()
            .await
            .unwrap();

        assert_eq!(next_protocol_parameters, expected_next_protocol_parameters);
    }
}

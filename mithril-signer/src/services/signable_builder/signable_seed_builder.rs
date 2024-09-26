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
    entities::{ProtocolMessagePartValue, ProtocolParameters},
    signable_builder::SignableSeedBuilder,
    StdResult,
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

    async fn compute_next_protocol_parameters_protocol_message_part_value(
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
        let next_protocol_parameters: ProtocolParameters =
            next_protocol_initializer.get_protocol_parameters().into();

        Ok(next_protocol_parameters.compute_hash())
    }

    async fn compute_current_epoch_protocol_message_part_value(
        &self,
    ) -> StdResult<ProtocolMessagePartValue> {
        let epoch_service = self.epoch_service.read().await;
        let current_epoch = epoch_service.epoch_of_current_data()?.to_string();

        Ok(current_epoch)
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

    struct MockDependencyInjector {
        mock_epoch_service: MockEpochServiceImpl,
        mock_single_signer: MockSingleSigner,
        mock_protocol_initializer_store: MockProtocolInitializerStorer,
    }

    impl MockDependencyInjector {
        fn new() -> MockDependencyInjector {
            MockDependencyInjector {
                mock_epoch_service: MockEpochServiceImpl::new(),
                mock_single_signer: MockSingleSigner::new(),
                mock_protocol_initializer_store: MockProtocolInitializerStorer::new(),
            }
        }

        fn build_signable_builder_service(self) -> SignerSignableSeedBuilder {
            SignerSignableSeedBuilder::new(
                Arc::new(RwLock::new(self.mock_epoch_service)),
                Arc::new(self.mock_single_signer),
                Arc::new(self.mock_protocol_initializer_store),
            )
        }
    }

    #[tokio::test]
    async fn test_compute_next_aggregate_verification_key_protocol_message_value() {
        let epoch = Epoch(5);
        let next_fixture = MithrilFixtureBuilder::default().with_signers(4).build();
        let protocol_initializer = next_fixture.signers_fixture()[0]
            .protocol_initializer
            .clone();
        let expected_next_aggregate_verification_key = next_fixture.compute_and_encode_avk();
        let expected_next_aggregate_verification_key_clone =
            expected_next_aggregate_verification_key.clone();
        let mut mock_container = MockDependencyInjector::new();
        mock_container.mock_epoch_service =
            MockEpochServiceImpl::new_with_config(|mock_epoch_service| {
                mock_epoch_service
                    .expect_epoch_of_current_data()
                    .return_once(move || Ok(epoch))
                    .once();
                mock_epoch_service
                    .expect_next_signers_with_stake()
                    .return_once(move || Ok(Vec::new()))
                    .once();
            });
        mock_container
            .mock_single_signer
            .expect_compute_aggregate_verification_key()
            .return_once(move |_, _| Ok(Some(expected_next_aggregate_verification_key_clone)))
            .once();
        mock_container
            .mock_protocol_initializer_store
            .expect_get_protocol_initializer()
            .return_once(move |_| Ok(Some(protocol_initializer)))
            .once();
        let signable_seed_builder = mock_container.build_signable_builder_service();

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
        let epoch = Epoch(5);
        let next_fixture = MithrilFixtureBuilder::default().with_signers(4).build();
        let protocol_initializer = next_fixture.signers_fixture()[0]
            .protocol_initializer
            .clone();
        let expected_next_protocol_parameters =
            Into::<ProtocolParameters>::into(protocol_initializer.get_protocol_parameters())
                .compute_hash();
        let mut mock_container = MockDependencyInjector::new();
        mock_container.mock_epoch_service =
            MockEpochServiceImpl::new_with_config(|mock_epoch_service| {
                mock_epoch_service
                    .expect_epoch_of_current_data()
                    .return_once(move || Ok(epoch))
                    .once();
            });
        mock_container
            .mock_protocol_initializer_store
            .expect_get_protocol_initializer()
            .return_once(move |_| Ok(Some(protocol_initializer)))
            .once();
        let signable_seed_builder = mock_container.build_signable_builder_service();

        let next_protocol_parameters = signable_seed_builder
            .compute_next_protocol_parameters_protocol_message_part_value()
            .await
            .unwrap();

        assert_eq!(next_protocol_parameters, expected_next_protocol_parameters);
    }

    #[tokio::test]
    async fn test_compute_current_epoch_protocol_message_value() {
        let epoch = Epoch(5);
        let expected_current_epoch = epoch.to_string();
        let mut mock_container = MockDependencyInjector::new();
        mock_container.mock_epoch_service =
            MockEpochServiceImpl::new_with_config(|mock_epoch_service| {
                mock_epoch_service
                    .expect_epoch_of_current_data()
                    .return_once(move || Ok(epoch))
                    .once();
            });
        let signable_seed_builder = mock_container.build_signable_builder_service();

        let current_epoch = signable_seed_builder
            .compute_current_epoch_protocol_message_part_value()
            .await
            .unwrap();

        assert_eq!(current_epoch, expected_current_epoch);
    }
}

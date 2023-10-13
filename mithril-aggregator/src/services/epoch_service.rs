use anyhow::Context;
use async_trait::async_trait;
use slog_scope::debug;
use std::sync::Arc;
use thiserror::Error;

use mithril_common::crypto_helper::ProtocolAggregateVerificationKey;
use mithril_common::entities::{Epoch, ProtocolParameters, SignerWithStake};
use mithril_common::protocol::{MultiSigner as ProtocolMultiSigner, SignerBuilder};
use mithril_common::StdResult;

use crate::{ProtocolParametersStorer, VerificationKeyStorer};

/// Errors dedicated to the CertifierService.
#[derive(Debug, Error)]
pub enum EpochServiceError {
    /// One of the data that is held for an epoch duration by the service was not available.
    #[error("Epoch service could not obtain {1} for epoch {0}")]
    UnavailableData(Epoch, String),

    /// Raised when service has not collected data at least once.
    #[error("Epoch service was not initialized, the function `inform_epoch` must be called first")]
    NotYetInitialized,

    /// Raised when service has not computed data for its current epoch.
    #[error(
        "No data computed for epoch {0}, the function `precompute_epoch_data` must be called first"
    )]
    NotYetComputed(Epoch),
}

/// Service that aggregates all data that don't change in a given epoch.
#[async_trait]
pub trait EpochService: Sync + Send {
    /// Inform the service a new epoch has been detected, telling it to update its
    /// internal state for the new epoch.
    async fn inform_epoch(&mut self, epoch: Epoch) -> StdResult<()>;

    /// Insert future protocol parameters in the store based on this service current epoch.
    ///
    /// Note: must be called after `inform_epoch`.
    async fn update_protocol_parameters(&mut self) -> StdResult<()>;

    /// Inform the service that it can precompute data for its current epoch.
    ///
    /// Note: must be called after `inform_epoch`.
    async fn precompute_epoch_data(&mut self) -> StdResult<()>;

    /// Get the current epoch for which the data stored in this service are computed.
    fn epoch_of_current_data(&self) -> StdResult<Epoch>;

    /// Get protocol parameters for current epoch
    fn current_protocol_parameters(&self) -> StdResult<&ProtocolParameters>;

    /// Get next protocol parameters for next epoch
    fn next_protocol_parameters(&self) -> StdResult<&ProtocolParameters>;

    /// Get aggregate verification key for current epoch
    fn current_aggregate_verification_key(&self) -> StdResult<&ProtocolAggregateVerificationKey>;

    /// Get next aggregate verification key for next epoch
    fn next_aggregate_verification_key(&self) -> StdResult<&ProtocolAggregateVerificationKey>;

    /// Get signers with stake for the current epoch
    fn current_signers_with_stake(&self) -> StdResult<&Vec<SignerWithStake>>;

    /// Get signers with stake for the next epoch
    fn next_signers_with_stake(&self) -> StdResult<&Vec<SignerWithStake>>;

    /// Get the [protocol multi signer][ProtocolMultiSigner] for the current epoch
    fn protocol_multi_signer(&self) -> StdResult<&ProtocolMultiSigner>;
}

struct EpochData {
    epoch: Epoch,
    protocol_parameters: ProtocolParameters,
    next_protocol_parameters: ProtocolParameters,
    signers: Vec<SignerWithStake>,
    next_signers: Vec<SignerWithStake>,
}

struct ComputedEpochData {
    aggregate_verification_key: ProtocolAggregateVerificationKey,
    next_aggregate_verification_key: ProtocolAggregateVerificationKey,
    protocol_multi_signer: ProtocolMultiSigner,
}

/// Implementation of the [epoch service][EpochService].
pub struct MithrilEpochService {
    epoch_data: Option<EpochData>,
    computed_epoch_data: Option<ComputedEpochData>,
    protocol_parameters_store: Arc<dyn ProtocolParametersStorer>,
    verification_key_store: Arc<dyn VerificationKeyStorer>,
}

impl MithrilEpochService {
    /// Create a new service instance
    pub fn new(
        protocol_parameters_store: Arc<dyn ProtocolParametersStorer>,
        verification_key_store: Arc<dyn VerificationKeyStorer>,
    ) -> Self {
        Self {
            epoch_data: None,
            computed_epoch_data: None,
            protocol_parameters_store,
            verification_key_store,
        }
    }

    async fn get_signers_with_stake_at_epoch(
        &self,
        signer_retrieval_epoch: Epoch,
    ) -> StdResult<Vec<SignerWithStake>> {
        let signers = self
            .verification_key_store
            .get_signers(signer_retrieval_epoch)
            .await?
            .unwrap_or_default();

        Ok(signers)
    }

    fn unwrap_data(&self) -> Result<&EpochData, EpochServiceError> {
        self.epoch_data
            .as_ref()
            .ok_or(EpochServiceError::NotYetInitialized)
    }

    fn unwrap_computed_data(&self) -> Result<&ComputedEpochData, EpochServiceError> {
        let epoch = self.unwrap_data()?.epoch;

        self.computed_epoch_data
            .as_ref()
            .ok_or(EpochServiceError::NotYetComputed(epoch))
    }
}

#[async_trait]
impl EpochService for MithrilEpochService {
    async fn inform_epoch(&mut self, epoch: Epoch) -> StdResult<()> {
        debug!("EpochService::inform_epoch(epoch: {epoch:?})");

        let signer_retrieval_epoch =
            epoch.offset_to_signer_retrieval_epoch().with_context(|| {
                format!("EpochService could not compute signer retrieval epoch from epoch: {epoch}")
            })?;
        let next_signer_retrieval_epoch = epoch.offset_to_next_signer_retrieval_epoch();
        let current_protocol_parameters = self
            .protocol_parameters_store
            .get_protocol_parameters(signer_retrieval_epoch)
            .await
            .with_context(|| "Epoch service failed to obtains current protocol parameters")?
            .ok_or(EpochServiceError::UnavailableData(
                signer_retrieval_epoch,
                "protocol parameters".to_string(),
            ))?;
        let next_protocol_parameters = self
            .protocol_parameters_store
            .get_protocol_parameters(next_signer_retrieval_epoch)
            .await
            .with_context(|| "Epoch service failed to obtains next protocol parameters")?
            .ok_or(EpochServiceError::UnavailableData(
                signer_retrieval_epoch,
                "protocol parameters".to_string(),
            ))?;

        let current_signers = self
            .get_signers_with_stake_at_epoch(signer_retrieval_epoch)
            .await?;
        let next_signers = self
            .get_signers_with_stake_at_epoch(next_signer_retrieval_epoch)
            .await?;

        self.epoch_data = Some(EpochData {
            epoch,
            protocol_parameters: current_protocol_parameters,
            next_protocol_parameters,
            signers: current_signers,
            next_signers,
        });
        self.computed_epoch_data = None;

        Ok(())
    }

    async fn precompute_epoch_data(&mut self) -> StdResult<()> {
        debug!("EpochService::precompute_epoch_data");

        let data = self.unwrap_data().with_context(|| {
            "can't precompute epoch data if inform_epoch has not been called first"
        })?;

        let protocol_multi_signer = SignerBuilder::new(&data.signers, &data.protocol_parameters)
            .with_context(|| "Epoch service failed to build protocol multi signer")?
            .build_multi_signer();

        let next_protocol_multi_signer =
            SignerBuilder::new(&data.next_signers, &data.next_protocol_parameters)
                .with_context(|| "Epoch service failed to build next protocol multi signer")?
                .build_multi_signer();

        self.computed_epoch_data = Some(ComputedEpochData {
            aggregate_verification_key: protocol_multi_signer.compute_aggregate_verification_key(),
            next_aggregate_verification_key: next_protocol_multi_signer
                .compute_aggregate_verification_key(),
            protocol_multi_signer,
        });

        Ok(())
    }

    fn epoch_of_current_data(&self) -> StdResult<Epoch> {
        Ok(self.unwrap_data()?.epoch)
    }

    fn current_protocol_parameters(&self) -> StdResult<&ProtocolParameters> {
        Ok(&self.unwrap_data()?.protocol_parameters)
    }

    fn next_protocol_parameters(&self) -> StdResult<&ProtocolParameters> {
        Ok(&self.unwrap_data()?.next_protocol_parameters)
    }

    fn current_aggregate_verification_key(&self) -> StdResult<&ProtocolAggregateVerificationKey> {
        Ok(&self.unwrap_computed_data()?.aggregate_verification_key)
    }

    fn next_aggregate_verification_key(&self) -> StdResult<&ProtocolAggregateVerificationKey> {
        Ok(&self.unwrap_computed_data()?.next_aggregate_verification_key)
    }

    fn current_signers_with_stake(&self) -> StdResult<&Vec<SignerWithStake>> {
        Ok(&self.unwrap_data()?.signers)
    }

    fn next_signers_with_stake(&self) -> StdResult<&Vec<SignerWithStake>> {
        Ok(&self.unwrap_data()?.next_signers)
    }

    fn protocol_multi_signer(&self) -> StdResult<&ProtocolMultiSigner> {
        Ok(&self.unwrap_computed_data()?.protocol_multi_signer)
    }
}

#[cfg(test)]
pub struct FakeEpochService {
    epoch_data: Option<EpochData>,
    computed_epoch_data: Option<ComputedEpochData>,
    inform_epoch_error: Option<()>,
    precompute_epoch_data_error: Option<()>,
}

#[cfg(test)]
impl FakeEpochService {
    /// Note: protocol multi signers and current/next avk will be computed using the given protocol
    /// parameters and signers.
    pub fn with_data(
        epoch: Epoch,
        protocol_parameters: &ProtocolParameters,
        next_protocol_parameters: &ProtocolParameters,
        signers: &[SignerWithStake],
        next_signers: &[SignerWithStake],
    ) -> Self {
        let protocol_multi_signer = SignerBuilder::new(signers, protocol_parameters)
            .with_context(|| "Could not build protocol_multi_signer for epoch service")
            .unwrap()
            .build_multi_signer();
        let next_protocol_multi_signer = SignerBuilder::new(signers, protocol_parameters)
            .with_context(|| "Could not build protocol_multi_signer for epoch service")
            .unwrap()
            .build_multi_signer();

        Self {
            epoch_data: Some(EpochData {
                epoch,
                protocol_parameters: protocol_parameters.clone(),
                next_protocol_parameters: next_protocol_parameters.clone(),
                signers: signers.to_vec(),
                next_signers: next_signers.to_vec(),
            }),
            computed_epoch_data: Some(ComputedEpochData {
                aggregate_verification_key: protocol_multi_signer
                    .compute_aggregate_verification_key(),
                next_aggregate_verification_key: next_protocol_multi_signer
                    .compute_aggregate_verification_key(),
                protocol_multi_signer,
            }),
            inform_epoch_error: None,
            precompute_epoch_data_error: None,
        }
    }

    pub fn from_fixture(
        epoch: Epoch,
        fixture: &mithril_common::test_utils::MithrilFixture,
    ) -> Self {
        Self::with_data(
            epoch,
            &fixture.protocol_parameters(),
            &fixture.protocol_parameters(),
            &fixture.signers_with_stake(),
            &fixture.signers_with_stake(),
        )
    }

    /// Note: using this will make all 'get' method from [EpochService] trait
    /// return a [EpochServiceError::NotYetInitialized] error.
    pub fn without_data() -> Self {
        Self {
            epoch_data: None,
            computed_epoch_data: None,
            inform_epoch_error: None,
            precompute_epoch_data_error: None,
        }
    }

    pub fn enable_inform_epoch_error(&mut self) {
        self.inform_epoch_error = Some(());
    }

    pub fn enable_precompute_epoch_data_error(&mut self) {
        self.precompute_epoch_data_error = Some(());
    }

    fn unwrap_data(&self) -> Result<&EpochData, EpochServiceError> {
        self.epoch_data
            .as_ref()
            .ok_or(EpochServiceError::NotYetInitialized)
    }

    fn unwrap_computed_data(&self) -> Result<&ComputedEpochData, EpochServiceError> {
        let epoch = self.unwrap_data()?.epoch;

        self.computed_epoch_data
            .as_ref()
            .ok_or(EpochServiceError::NotYetComputed(epoch))
    }
}

#[cfg(test)]
#[async_trait]
impl EpochService for FakeEpochService {
    async fn inform_epoch(&mut self, epoch: Epoch) -> StdResult<()> {
        match self.inform_epoch_error {
            None => Ok(()),
            Some(_) => anyhow::bail!("inform_epoch fake error, given epoch: {epoch}"),
        }
    }

    async fn precompute_epoch_data(&mut self) -> StdResult<()> {
        match self.precompute_epoch_data_error {
            None => Ok(()),
            Some(_) => anyhow::bail!("precompute_epoch_data fake error"),
        }
    }

    fn epoch_of_current_data(&self) -> StdResult<Epoch> {
        Ok(self.unwrap_data()?.epoch)
    }

    fn current_protocol_parameters(&self) -> StdResult<&ProtocolParameters> {
        Ok(&self.unwrap_data()?.protocol_parameters)
    }

    fn next_protocol_parameters(&self) -> StdResult<&ProtocolParameters> {
        Ok(&self.unwrap_data()?.next_protocol_parameters)
    }

    fn current_aggregate_verification_key(&self) -> StdResult<&ProtocolAggregateVerificationKey> {
        Ok(&self.unwrap_computed_data()?.aggregate_verification_key)
    }

    fn next_aggregate_verification_key(&self) -> StdResult<&ProtocolAggregateVerificationKey> {
        Ok(&self.unwrap_computed_data()?.next_aggregate_verification_key)
    }

    fn current_signers_with_stake(&self) -> StdResult<&Vec<SignerWithStake>> {
        Ok(&self.unwrap_data()?.signers)
    }

    fn next_signers_with_stake(&self) -> StdResult<&Vec<SignerWithStake>> {
        Ok(&self.unwrap_data()?.next_signers)
    }

    fn protocol_multi_signer(&self) -> StdResult<&ProtocolMultiSigner> {
        Ok(&self.unwrap_computed_data()?.protocol_multi_signer)
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::PartyId;
    use mithril_common::store::adapter::MemoryAdapter;
    use mithril_common::test_utils::{MithrilFixture, MithrilFixtureBuilder};
    use std::collections::{BTreeSet, HashMap};

    use crate::{ProtocolParametersStore, VerificationKeyStore};

    use super::*;

    #[derive(Debug, Clone, PartialEq)]
    struct ExpectedEpochData {
        epoch: Epoch,
        protocol_parameters: ProtocolParameters,
        next_protocol_parameters: ProtocolParameters,
        signers: BTreeSet<SignerWithStake>,
        next_signers: BTreeSet<SignerWithStake>,
    }

    #[derive(Debug, Clone, PartialEq)]
    struct ExpectedComputedEpochData {
        aggregate_verification_key: ProtocolAggregateVerificationKey,
        next_aggregate_verification_key: ProtocolAggregateVerificationKey,
    }

    impl ExpectedEpochData {
        async fn from_service(service: &MithrilEpochService) -> StdResult<Self> {
            Ok(Self {
                epoch: service.epoch_of_current_data()?,
                protocol_parameters: service.current_protocol_parameters()?.clone(),
                next_protocol_parameters: service.next_protocol_parameters()?.clone(),
                signers: service
                    .current_signers_with_stake()?
                    .clone()
                    .into_iter()
                    .collect(),
                next_signers: service
                    .next_signers_with_stake()?
                    .clone()
                    .into_iter()
                    .collect(),
            })
        }
    }

    impl ExpectedComputedEpochData {
        async fn from_service(service: &MithrilEpochService) -> StdResult<Self> {
            Ok(Self {
                aggregate_verification_key: service.current_aggregate_verification_key()?.clone(),
                next_aggregate_verification_key: service.next_aggregate_verification_key()?.clone(),
            })
        }
    }

    fn map_signers_for_vkey_store(
        signers: &[SignerWithStake],
    ) -> HashMap<PartyId, SignerWithStake> {
        signers
            .iter()
            .cloned()
            .map(|s| (s.party_id.to_owned(), s))
            .collect()
    }

    async fn build_service(
        epoch: Epoch,
        current_epoch_fixture: &MithrilFixture,
        next_epoch_fixture: &MithrilFixture,
    ) -> MithrilEpochService {
        let signer_retrieval_epoch = epoch.offset_to_signer_retrieval_epoch().unwrap();
        let next_signer_retrieval_epoch = epoch.offset_to_next_signer_retrieval_epoch();

        let protocol_parameters_store = ProtocolParametersStore::new(
            Box::new(
                MemoryAdapter::new(Some(vec![
                    (
                        signer_retrieval_epoch,
                        current_epoch_fixture.protocol_parameters(),
                    ),
                    (
                        next_signer_retrieval_epoch,
                        next_epoch_fixture.protocol_parameters(),
                    ),
                ]))
                .unwrap(),
            ),
            None,
        );
        let vkey_store = VerificationKeyStore::new(Box::new(
            MemoryAdapter::new(Some(vec![
                (
                    signer_retrieval_epoch,
                    map_signers_for_vkey_store(&current_epoch_fixture.signers_with_stake()),
                ),
                (
                    next_signer_retrieval_epoch,
                    map_signers_for_vkey_store(&next_epoch_fixture.signers_with_stake()),
                ),
            ]))
            .unwrap(),
        ));

        MithrilEpochService::new(Arc::new(protocol_parameters_store), Arc::new(vkey_store))
    }

    #[tokio::test]
    async fn inform_epoch_get_data_from_its_dependencies() {
        let current_epoch_fixture = MithrilFixtureBuilder::default().with_signers(3).build();
        let next_epoch_fixture = MithrilFixtureBuilder::default()
            .with_protocol_parameters(ProtocolParameters::new(8, 80, 0.80))
            .with_signers(5)
            .build();

        let epoch = Epoch(5);
        let mut service = build_service(epoch, &current_epoch_fixture, &next_epoch_fixture).await;

        service
            .inform_epoch(epoch)
            .await
            .expect("inform_epoch should not fail");

        let data = ExpectedEpochData::from_service(&service)
            .await
            .expect("extracting data from service should not fail");

        assert_eq!(
            data,
            ExpectedEpochData {
                epoch,
                protocol_parameters: current_epoch_fixture.protocol_parameters(),
                next_protocol_parameters: next_epoch_fixture.protocol_parameters(),
                signers: current_epoch_fixture
                    .signers_with_stake()
                    .into_iter()
                    .collect(),
                next_signers: next_epoch_fixture
                    .signers_with_stake()
                    .into_iter()
                    .collect(),
            }
        );
    }

    #[tokio::test]
    async fn compute_data_with_data_from_inform_epoch() {
        let current_epoch_fixture = MithrilFixtureBuilder::default().with_signers(3).build();
        let next_epoch_fixture = MithrilFixtureBuilder::default()
            .with_protocol_parameters(ProtocolParameters::new(8, 80, 0.80))
            .with_signers(5)
            .build();

        let epoch = Epoch(5);
        let mut service = build_service(epoch, &current_epoch_fixture, &next_epoch_fixture).await;

        service
            .inform_epoch(epoch)
            .await
            .expect("inform_epoch should not fail");
        service
            .precompute_epoch_data()
            .await
            .expect("precompute_epoch_data should not fail");

        let data = ExpectedComputedEpochData::from_service(&service)
            .await
            .expect("extracting data from service should not fail");

        assert_eq!(
            data,
            ExpectedComputedEpochData {
                aggregate_verification_key: current_epoch_fixture.compute_avk(),
                next_aggregate_verification_key: next_epoch_fixture.compute_avk(),
            }
        );
    }

    #[tokio::test]
    async fn inform_epoch_reset_computed_data() {
        let fixture = MithrilFixtureBuilder::default().with_signers(3).build();
        let avk = fixture.compute_avk();
        let epoch = Epoch(4);
        let mut service = build_service(epoch, &fixture, &fixture).await;
        service.computed_epoch_data = Some(ComputedEpochData {
            aggregate_verification_key: avk.clone(),
            next_aggregate_verification_key: avk.clone(),
            protocol_multi_signer: SignerBuilder::new(
                &fixture.signers_with_stake(),
                &fixture.protocol_parameters(),
            )
            .unwrap()
            .build_multi_signer(),
        });

        service
            .inform_epoch(epoch)
            .await
            .expect("inform_epoch should not fail");

        assert!(service.computed_epoch_data.is_none());
    }

    #[tokio::test]
    async fn cant_get_data_if_inform_epoch_has_not_been_called() {
        let fixture = MithrilFixtureBuilder::default().with_signers(3).build();
        let service = build_service(Epoch(4), &fixture, &fixture).await;

        for (name, res) in [
            (
                "epoch_of_current_data",
                service.epoch_of_current_data().err(),
            ),
            (
                "current_protocol_parameters",
                service.current_protocol_parameters().err(),
            ),
            (
                "next_protocol_parameters",
                service.next_protocol_parameters().err(),
            ),
            (
                "current_signers_with_stake",
                service.current_signers_with_stake().err(),
            ),
            (
                "next_signers_with_stake",
                service.next_signers_with_stake().err(),
            ),
            (
                "current_aggregate_verification_key",
                service.current_aggregate_verification_key().err(),
            ),
            (
                "next_aggregate_verification_key",
                service.next_aggregate_verification_key().err(),
            ),
            (
                "protocol_multi_signer",
                service.protocol_multi_signer().err(),
            ),
        ] {
            let error =
                res.unwrap_or_else(|| panic!("getting {name} should have returned an error"));

            match error.downcast_ref::<EpochServiceError>() {
                Some(EpochServiceError::NotYetInitialized) => (),
                _ => panic!("Expected an NotYetInitialized error, got: {error:?}"),
            }
        }
    }

    #[tokio::test]
    async fn can_only_get_non_computed_data_if_inform_epoch_has_been_called_but_not_precompute_epoch_data(
    ) {
        let fixture = MithrilFixtureBuilder::default().with_signers(3).build();
        let mut service = build_service(Epoch(4), &fixture, &fixture).await;
        service.inform_epoch(Epoch(4)).await.unwrap();

        assert!(service.epoch_of_current_data().is_ok());
        assert!(service.current_protocol_parameters().is_ok());
        assert!(service.next_protocol_parameters().is_ok());
        assert!(service.current_signers_with_stake().is_ok());
        assert!(service.next_signers_with_stake().is_ok());

        for (name, res) in [
            (
                "current_aggregate_verification_key",
                service.current_aggregate_verification_key().err(),
            ),
            (
                "next_aggregate_verification_key",
                service.next_aggregate_verification_key().err(),
            ),
            (
                "protocol_multi_signer",
                service.protocol_multi_signer().err(),
            ),
        ] {
            let error =
                res.unwrap_or_else(|| panic!("getting {name} should have returned an error"));

            match error.downcast_ref::<EpochServiceError>() {
                Some(EpochServiceError::NotYetComputed(Epoch(4))) => (),
                _ => panic!("Expected an NotYetComputed error for epoch 4, got: {error:?}"),
            }
        }
    }
}

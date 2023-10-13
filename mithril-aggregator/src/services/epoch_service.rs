use anyhow::Context;
use async_trait::async_trait;
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

    /// Raised when service had not collected data at least once.
    #[error("Epoch service was not initialized, you can call inform_epoch to initialize it")]
    NotYetInitialized,
}

/// Service that aggregates all data that don't change in a given epoch.
#[async_trait]
pub trait EpochService: Sync + Send {
    /// Inform the service a new epoch has been detected, telling it to update its
    /// internal state for the new epoch.
    async fn inform_epoch(&mut self, epoch: Epoch) -> StdResult<()>;

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

/// Struct that aggregates all data that don't change in a given epoch.
struct EpochData {
    epoch: Epoch,
    protocol_parameters: ProtocolParameters,
    next_protocol_parameters: ProtocolParameters,
    aggregate_verification_key: ProtocolAggregateVerificationKey,
    next_aggregate_verification_key: ProtocolAggregateVerificationKey,
    signers: Vec<SignerWithStake>,
    next_signers: Vec<SignerWithStake>,
    protocol_multi_signer: ProtocolMultiSigner,
}

/// Implementation of the [epoch service][EpochService].
pub struct MithrilEpochService {
    current_epoch_data: Option<EpochData>,
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
            current_epoch_data: None,
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
            .ok_or(EpochServiceError::UnavailableData(
                signer_retrieval_epoch,
                "signers verification keys".to_string(),
            ))?;

        Ok(signers)
    }

    fn unwrap_data(&self) -> Result<&EpochData, EpochServiceError> {
        self.current_epoch_data
            .as_ref()
            .ok_or(EpochServiceError::NotYetInitialized)
    }
}

#[async_trait]
impl EpochService for MithrilEpochService {
    async fn inform_epoch(&mut self, epoch: Epoch) -> StdResult<()> {
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

        let protocol_multi_signer =
            SignerBuilder::new(&current_signers, &current_protocol_parameters)
                .with_context(|| "Epoch service failed to build protocol multi signer")?
                .build_multi_signer();

        let next_protocol_multi_signer =
            SignerBuilder::new(&next_signers, &next_protocol_parameters)
                .with_context(|| "Epoch service failed to build next protocol multi signer")?
                .build_multi_signer();

        self.current_epoch_data = Some(EpochData {
            epoch,
            protocol_parameters: current_protocol_parameters,
            next_protocol_parameters,
            aggregate_verification_key: protocol_multi_signer.compute_aggregate_verification_key(),
            next_aggregate_verification_key: next_protocol_multi_signer
                .compute_aggregate_verification_key(),
            signers: current_signers,
            next_signers,
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
        Ok(&self.unwrap_data()?.aggregate_verification_key)
    }

    fn next_aggregate_verification_key(&self) -> StdResult<&ProtocolAggregateVerificationKey> {
        Ok(&self.unwrap_data()?.next_aggregate_verification_key)
    }

    fn current_signers_with_stake(&self) -> StdResult<&Vec<SignerWithStake>> {
        Ok(&self.unwrap_data()?.signers)
    }

    fn next_signers_with_stake(&self) -> StdResult<&Vec<SignerWithStake>> {
        Ok(&self.unwrap_data()?.next_signers)
    }

    fn protocol_multi_signer(&self) -> StdResult<&ProtocolMultiSigner> {
        Ok(&self.unwrap_data()?.protocol_multi_signer)
    }
}

#[cfg(test)]
pub struct FakeEpochService {
    epoch_data: Option<EpochData>,
    inform_epoch_error: Option<()>,
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
                aggregate_verification_key: protocol_multi_signer
                    .compute_aggregate_verification_key(),
                next_aggregate_verification_key: next_protocol_multi_signer
                    .compute_aggregate_verification_key(),
                signers: signers.to_vec(),
                next_signers: next_signers.to_vec(),
                protocol_multi_signer,
            }),
            inform_epoch_error: None,
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
            inform_epoch_error: None,
        }
    }

    pub fn enable_inform_epoch_error(&mut self) {
        self.inform_epoch_error = Some(());
    }

    fn unwrap_data(&self) -> Result<&EpochData, EpochServiceError> {
        self.epoch_data
            .as_ref()
            .ok_or(EpochServiceError::NotYetInitialized)
    }
}

#[cfg(test)]
#[async_trait]
impl EpochService for FakeEpochService {
    async fn inform_epoch(&mut self, epoch: Epoch) -> StdResult<()> {
        match self.inform_epoch_error {
            None => Ok(()),
            Some(_) => anyhow::bail!("Inform epoch fake error, given epoch: {epoch}"),
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
        Ok(&self.unwrap_data()?.aggregate_verification_key)
    }

    fn next_aggregate_verification_key(&self) -> StdResult<&ProtocolAggregateVerificationKey> {
        Ok(&self.unwrap_data()?.next_aggregate_verification_key)
    }

    fn current_signers_with_stake(&self) -> StdResult<&Vec<SignerWithStake>> {
        Ok(&self.unwrap_data()?.signers)
    }

    fn next_signers_with_stake(&self) -> StdResult<&Vec<SignerWithStake>> {
        Ok(&self.unwrap_data()?.next_signers)
    }

    fn protocol_multi_signer(&self) -> StdResult<&ProtocolMultiSigner> {
        Ok(&self.unwrap_data()?.protocol_multi_signer)
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::PartyId;
    use mithril_common::store::adapter::MemoryAdapter;
    use mithril_common::test_utils::MithrilFixtureBuilder;
    use std::collections::{BTreeSet, HashMap};

    use crate::{ProtocolParametersStore, VerificationKeyStore};

    use super::*;

    #[derive(Debug, Clone, PartialEq)]
    struct ExpectedEpochData {
        epoch: Epoch,
        protocol_parameters: ProtocolParameters,
        next_protocol_parameters: ProtocolParameters,
        aggregate_verification_key: ProtocolAggregateVerificationKey,
        next_aggregate_verification_key: ProtocolAggregateVerificationKey,
        signers: BTreeSet<SignerWithStake>,
        next_signers: BTreeSet<SignerWithStake>,
    }

    impl ExpectedEpochData {
        async fn from_service(service: &MithrilEpochService) -> StdResult<Self> {
            Ok(ExpectedEpochData {
                epoch: service.epoch_of_current_data()?,
                protocol_parameters: service.current_protocol_parameters()?.clone(),
                next_protocol_parameters: service.next_protocol_parameters()?.clone(),
                aggregate_verification_key: service.current_aggregate_verification_key()?.clone(),
                next_aggregate_verification_key: service.next_aggregate_verification_key()?.clone(),
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

    fn map_signers_for_vkey_store(
        signers: &[SignerWithStake],
    ) -> HashMap<PartyId, SignerWithStake> {
        signers
            .iter()
            .cloned()
            .map(|s| (s.party_id.to_owned(), s))
            .collect()
    }

    #[tokio::test]
    async fn informs_epoch_get_data_from_its_dependencies() {
        let fixture = MithrilFixtureBuilder::default().with_signers(3).build();
        let fixture2 = MithrilFixtureBuilder::default()
            .with_protocol_parameters(ProtocolParameters::new(8, 80, 0.80))
            .with_signers(5)
            .build();

        let epoch = Epoch(5);
        let signer_retrieval_epoch = epoch.offset_to_signer_retrieval_epoch().unwrap();
        let next_signer_retrieval_epoch = epoch.offset_to_next_signer_retrieval_epoch();

        let expected = ExpectedEpochData {
            epoch,
            protocol_parameters: fixture.protocol_parameters(),
            next_protocol_parameters: fixture2.protocol_parameters(),
            aggregate_verification_key: fixture.compute_avk(),
            next_aggregate_verification_key: fixture2.compute_avk(),
            signers: fixture.signers_with_stake().into_iter().collect(),
            next_signers: fixture2.signers_with_stake().into_iter().collect(),
        };

        let protocol_parameters_store = ProtocolParametersStore::new(
            Box::new(
                MemoryAdapter::new(Some(vec![
                    (signer_retrieval_epoch, fixture.protocol_parameters()),
                    (next_signer_retrieval_epoch, fixture2.protocol_parameters()),
                ]))
                .unwrap(),
            ),
            None,
        );
        let vkey_store = VerificationKeyStore::new(Box::new(
            MemoryAdapter::new(Some(vec![
                (
                    signer_retrieval_epoch,
                    map_signers_for_vkey_store(&fixture.signers_with_stake()),
                ),
                (
                    next_signer_retrieval_epoch,
                    map_signers_for_vkey_store(&fixture2.signers_with_stake()),
                ),
            ]))
            .unwrap(),
        ));

        let mut service =
            MithrilEpochService::new(Arc::new(protocol_parameters_store), Arc::new(vkey_store));

        service
            .inform_epoch(epoch)
            .await
            .expect("inform_epoch should not fail");
        let data = ExpectedEpochData::from_service(&service)
            .await
            .expect("extracting data from service should not fail");

        assert_eq!(expected, data);
    }
}

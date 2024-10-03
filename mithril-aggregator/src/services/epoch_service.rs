use anyhow::Context;
use async_trait::async_trait;
use slog_scope::debug;
use std::collections::BTreeSet;
use std::sync::Arc;
use thiserror::Error;

use mithril_common::crypto_helper::ProtocolAggregateVerificationKey;
use mithril_common::entities::{
    CardanoTransactionsSigningConfig, Epoch, ProtocolParameters, SignedEntityConfig,
    SignedEntityTypeDiscriminants, Signer, SignerWithStake,
};
use mithril_common::protocol::{MultiSigner as ProtocolMultiSigner, SignerBuilder};
use mithril_common::{CardanoNetwork, StdResult};

use crate::entities::AggregatorEpochSettings;
use crate::{EpochSettingsStorer, VerificationKeyStorer};

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

    /// Insert future epoch settings in the store based on this service current epoch (epoch offset +2).
    ///
    /// Note: must be called after `inform_epoch`.
    async fn update_epoch_settings(&mut self) -> StdResult<()>;

    /// Inform the service that it can precompute data for its current epoch.
    ///
    /// Note: must be called after `inform_epoch`.
    async fn precompute_epoch_data(&mut self) -> StdResult<()>;

    /// Get the current epoch for which the data stored in this service are computed.
    fn epoch_of_current_data(&self) -> StdResult<Epoch>;

    /// Get protocol parameters used in current epoch (associated with the previous epoch)
    fn current_protocol_parameters(&self) -> StdResult<&ProtocolParameters>;

    /// Get next protocol parameters used in next epoch (associated with the actual epoch)
    fn next_protocol_parameters(&self) -> StdResult<&ProtocolParameters>;

    /// Get upcoming protocol parameters used in next epoch (associated with the next epoch)
    fn upcoming_protocol_parameters(&self) -> StdResult<&ProtocolParameters>;

    /// Get cardano transactions signing configuration used in current epoch
    fn current_cardano_transactions_signing_config(
        &self,
    ) -> StdResult<&CardanoTransactionsSigningConfig>;

    /// Get next cardano transactions signing configuration used in next epoch
    fn next_cardano_transactions_signing_config(
        &self,
    ) -> StdResult<&CardanoTransactionsSigningConfig>;

    /// Get aggregate verification key for current epoch
    fn current_aggregate_verification_key(&self) -> StdResult<&ProtocolAggregateVerificationKey>;

    /// Get next aggregate verification key for next epoch
    fn next_aggregate_verification_key(&self) -> StdResult<&ProtocolAggregateVerificationKey>;

    /// Get signers with stake for the current epoch
    fn current_signers_with_stake(&self) -> StdResult<&Vec<SignerWithStake>>;

    /// Get signers with stake for the next epoch
    fn next_signers_with_stake(&self) -> StdResult<&Vec<SignerWithStake>>;

    /// Get signers for the current epoch
    fn current_signers(&self) -> StdResult<&Vec<Signer>>;

    /// Get signers for the next epoch
    fn next_signers(&self) -> StdResult<&Vec<Signer>>;

    /// Get the [protocol multi signer][ProtocolMultiSigner] for the current epoch
    fn protocol_multi_signer(&self) -> StdResult<&ProtocolMultiSigner>;

    /// Get the [protocol multi signer][ProtocolMultiSigner] for the next epoch
    fn next_protocol_multi_signer(&self) -> StdResult<&ProtocolMultiSigner>;

    /// Get the [SignedEntityConfig] for the current epoch.
    fn signed_entity_config(&self) -> StdResult<&SignedEntityConfig>;
}

struct EpochData {
    epoch: Epoch,
    epoch_settings: AggregatorEpochSettings,
    next_epoch_settings: AggregatorEpochSettings,
    upcoming_epoch_settings: AggregatorEpochSettings,
    current_signers_with_stake: Vec<SignerWithStake>,
    next_signers_with_stake: Vec<SignerWithStake>,
    current_signers: Vec<Signer>,
    next_signers: Vec<Signer>,
    signed_entity_config: SignedEntityConfig,
}

struct ComputedEpochData {
    aggregate_verification_key: ProtocolAggregateVerificationKey,
    next_aggregate_verification_key: ProtocolAggregateVerificationKey,
    protocol_multi_signer: ProtocolMultiSigner,
    next_protocol_multi_signer: ProtocolMultiSigner,
}

/// Implementation of the [epoch service][EpochService].
pub struct MithrilEpochService {
    /// Epoch settings that will be inserted when inform_epoch is called
    future_epoch_settings: AggregatorEpochSettings,
    epoch_data: Option<EpochData>,
    computed_epoch_data: Option<ComputedEpochData>,
    epoch_settings_storer: Arc<dyn EpochSettingsStorer>,
    verification_key_store: Arc<dyn VerificationKeyStorer>,
    network: CardanoNetwork,
    allowed_discriminants: BTreeSet<SignedEntityTypeDiscriminants>,
}

impl MithrilEpochService {
    /// Create a new service instance
    pub fn new(
        future_epoch_settings: AggregatorEpochSettings,
        epoch_settings_storer: Arc<dyn EpochSettingsStorer>,
        verification_key_store: Arc<dyn VerificationKeyStorer>,
        network: CardanoNetwork,
        allowed_discriminants: BTreeSet<SignedEntityTypeDiscriminants>,
    ) -> Self {
        Self {
            future_epoch_settings,
            epoch_data: None,
            computed_epoch_data: None,
            epoch_settings_storer,
            verification_key_store,
            network,
            allowed_discriminants,
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

    async fn get_epoch_settings(
        &self,
        epoch: Epoch,
        name: &str,
    ) -> StdResult<AggregatorEpochSettings> {
        let epoch_settings = self
            .epoch_settings_storer
            .get_epoch_settings(epoch)
            .await
            .with_context(|| format!("Epoch service failed to obtain {name}"))?
            .ok_or(EpochServiceError::UnavailableData(epoch, name.to_string()))?;

        Ok(epoch_settings)
    }

    async fn insert_future_epoch_settings(&self, actual_epoch: Epoch) -> StdResult<()> {
        let recording_epoch = actual_epoch.offset_to_epoch_settings_recording_epoch();

        debug!(
            "EpochService: inserting epoch settings in epoch {}",
            recording_epoch;
            "epoch_settings" => ?self.future_epoch_settings
        );

        self.epoch_settings_storer
            .save_epoch_settings(
                recording_epoch,
                self.future_epoch_settings.clone(),
            )
            .await
            .with_context(|| format!("Epoch service failed to insert future_epoch_settings to epoch {recording_epoch}"))
            .map(|_| ())
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

        let epoch_settings = self
            .get_epoch_settings(signer_retrieval_epoch, "current epoch settings")
            .await?;

        let next_epoch_settings = self
            .get_epoch_settings(next_signer_retrieval_epoch, "next epoch settings")
            .await?;

        let upcoming_epoch_settings = self
            .get_epoch_settings(
                next_signer_retrieval_epoch.next(),
                "upcoming epoch settings",
            )
            .await?;

        let current_signers_with_stake = self
            .get_signers_with_stake_at_epoch(signer_retrieval_epoch)
            .await?;
        let next_signers_with_stake = self
            .get_signers_with_stake_at_epoch(next_signer_retrieval_epoch)
            .await?;
        let current_signers = Signer::vec_from(current_signers_with_stake.clone());
        let next_signers = Signer::vec_from(next_signers_with_stake.clone());

        let signed_entity_config = SignedEntityConfig {
            allowed_discriminants: self.allowed_discriminants.clone(),
            network: self.network,
            cardano_transactions_signing_config: epoch_settings
                .cardano_transactions_signing_config
                .clone(),
        };

        self.epoch_data = Some(EpochData {
            epoch,
            epoch_settings,
            next_epoch_settings,
            upcoming_epoch_settings,
            current_signers_with_stake,
            next_signers_with_stake,
            current_signers,
            next_signers,
            signed_entity_config,
        });
        self.computed_epoch_data = None;

        Ok(())
    }

    async fn update_epoch_settings(&mut self) -> StdResult<()> {
        debug!("EpochService::update_epoch_settings");

        let data = self.unwrap_data().with_context(|| {
            "can't update epoch settings if inform_epoch has not been called first"
        })?;

        self.insert_future_epoch_settings(data.epoch).await
    }

    async fn precompute_epoch_data(&mut self) -> StdResult<()> {
        debug!("EpochService::precompute_epoch_data");

        let data = self.unwrap_data().with_context(|| {
            "can't precompute epoch data if inform_epoch has not been called first"
        })?;

        let protocol_multi_signer = SignerBuilder::new(
            &data.current_signers_with_stake,
            &data.epoch_settings.protocol_parameters,
        )
        .with_context(|| "Epoch service failed to build protocol multi signer")?
        .build_multi_signer();

        let next_protocol_multi_signer = SignerBuilder::new(
            &data.next_signers_with_stake,
            &data.next_epoch_settings.protocol_parameters,
        )
        .with_context(|| "Epoch service failed to build next protocol multi signer")?
        .build_multi_signer();

        self.computed_epoch_data = Some(ComputedEpochData {
            aggregate_verification_key: protocol_multi_signer.compute_aggregate_verification_key(),
            next_aggregate_verification_key: next_protocol_multi_signer
                .compute_aggregate_verification_key(),
            protocol_multi_signer,
            next_protocol_multi_signer,
        });

        Ok(())
    }

    fn epoch_of_current_data(&self) -> StdResult<Epoch> {
        Ok(self.unwrap_data()?.epoch)
    }

    fn current_protocol_parameters(&self) -> StdResult<&ProtocolParameters> {
        Ok(&self.unwrap_data()?.epoch_settings.protocol_parameters)
    }

    fn next_protocol_parameters(&self) -> StdResult<&ProtocolParameters> {
        Ok(&self.unwrap_data()?.next_epoch_settings.protocol_parameters)
    }

    fn upcoming_protocol_parameters(&self) -> StdResult<&ProtocolParameters> {
        Ok(&self
            .unwrap_data()?
            .upcoming_epoch_settings
            .protocol_parameters)
    }

    fn current_cardano_transactions_signing_config(
        &self,
    ) -> StdResult<&CardanoTransactionsSigningConfig> {
        Ok(&self
            .unwrap_data()?
            .epoch_settings
            .cardano_transactions_signing_config)
    }

    fn next_cardano_transactions_signing_config(
        &self,
    ) -> StdResult<&CardanoTransactionsSigningConfig> {
        Ok(&self
            .unwrap_data()?
            .next_epoch_settings
            .cardano_transactions_signing_config)
    }

    fn current_aggregate_verification_key(&self) -> StdResult<&ProtocolAggregateVerificationKey> {
        Ok(&self.unwrap_computed_data()?.aggregate_verification_key)
    }

    fn next_aggregate_verification_key(&self) -> StdResult<&ProtocolAggregateVerificationKey> {
        Ok(&self.unwrap_computed_data()?.next_aggregate_verification_key)
    }

    fn current_signers_with_stake(&self) -> StdResult<&Vec<SignerWithStake>> {
        Ok(&self.unwrap_data()?.current_signers_with_stake)
    }

    fn next_signers_with_stake(&self) -> StdResult<&Vec<SignerWithStake>> {
        Ok(&self.unwrap_data()?.next_signers_with_stake)
    }

    fn current_signers(&self) -> StdResult<&Vec<Signer>> {
        Ok(&self.unwrap_data()?.current_signers)
    }

    fn next_signers(&self) -> StdResult<&Vec<Signer>> {
        Ok(&self.unwrap_data()?.next_signers)
    }

    fn protocol_multi_signer(&self) -> StdResult<&ProtocolMultiSigner> {
        Ok(&self.unwrap_computed_data()?.protocol_multi_signer)
    }

    fn next_protocol_multi_signer(&self) -> StdResult<&ProtocolMultiSigner> {
        Ok(&self.unwrap_computed_data()?.next_protocol_multi_signer)
    }

    fn signed_entity_config(&self) -> StdResult<&SignedEntityConfig> {
        Ok(&self.unwrap_data()?.signed_entity_config)
    }
}

#[cfg(test)]
pub struct FakeEpochService {
    epoch_data: Option<EpochData>,
    computed_epoch_data: Option<ComputedEpochData>,
    inform_epoch_error: bool,
    update_epoch_settings_error: bool,
    precompute_epoch_data_error: bool,
}

#[cfg(test)]
impl FakeEpochService {
    /// Note: protocol multi signers and current/next avk will be computed using the given protocol
    /// parameters and signers.
    pub fn with_data(
        epoch: Epoch,
        epoch_settings: &AggregatorEpochSettings,
        next_epoch_settings: &AggregatorEpochSettings,
        upcoming_epoch_settings: &AggregatorEpochSettings,
        current_signers_with_stake: &[SignerWithStake],
        next_signers_with_stake: &[SignerWithStake],
    ) -> Self {
        let protocol_multi_signer = SignerBuilder::new(
            current_signers_with_stake,
            &epoch_settings.protocol_parameters,
        )
        .with_context(|| "Could not build protocol_multi_signer for epoch service")
        .unwrap()
        .build_multi_signer();
        let next_protocol_multi_signer = SignerBuilder::new(
            next_signers_with_stake,
            &next_epoch_settings.protocol_parameters,
        )
        .with_context(|| "Could not build protocol_multi_signer for epoch service")
        .unwrap()
        .build_multi_signer();

        let current_signers_with_stake = current_signers_with_stake.to_vec();
        let next_signers_with_stake = next_signers_with_stake.to_vec();
        let current_signers = Signer::vec_from(current_signers_with_stake.clone());
        let next_signers = Signer::vec_from(next_signers_with_stake.clone());

        Self {
            epoch_data: Some(EpochData {
                epoch,
                epoch_settings: epoch_settings.clone(),
                next_epoch_settings: next_epoch_settings.clone(),
                upcoming_epoch_settings: upcoming_epoch_settings.clone(),
                current_signers_with_stake,
                next_signers_with_stake,
                current_signers,
                next_signers,
                signed_entity_config: SignedEntityConfig::dummy(),
            }),
            computed_epoch_data: Some(ComputedEpochData {
                aggregate_verification_key: protocol_multi_signer
                    .compute_aggregate_verification_key(),
                next_aggregate_verification_key: next_protocol_multi_signer
                    .compute_aggregate_verification_key(),
                protocol_multi_signer,
                next_protocol_multi_signer,
            }),
            inform_epoch_error: false,
            update_epoch_settings_error: false,
            precompute_epoch_data_error: false,
        }
    }

    pub fn from_fixture(
        epoch: Epoch,
        fixture: &mithril_common::test_utils::MithrilFixture,
    ) -> Self {
        use mithril_common::entities::CardanoTransactionsSigningConfig;

        let epoch_settings = AggregatorEpochSettings {
            protocol_parameters: fixture.protocol_parameters(),
            cardano_transactions_signing_config: CardanoTransactionsSigningConfig::dummy(),
        };
        let next_epoch_settings = AggregatorEpochSettings {
            protocol_parameters: fixture.protocol_parameters(),
            cardano_transactions_signing_config: CardanoTransactionsSigningConfig::dummy(),
        };
        let upcoming_epoch_settings = AggregatorEpochSettings {
            protocol_parameters: fixture.protocol_parameters(),
            cardano_transactions_signing_config: CardanoTransactionsSigningConfig::dummy(),
        };
        Self::with_data(
            epoch,
            &epoch_settings,
            &next_epoch_settings,
            &upcoming_epoch_settings,
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
            inform_epoch_error: false,
            update_epoch_settings_error: false,
            precompute_epoch_data_error: false,
        }
    }

    pub fn toggle_errors(
        &mut self,
        inform_epoch: bool,
        update_protocol_parameters: bool,
        precompute_epoch: bool,
    ) {
        self.inform_epoch_error = inform_epoch;
        self.update_epoch_settings_error = update_protocol_parameters;
        self.precompute_epoch_data_error = precompute_epoch;
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
        if self.inform_epoch_error {
            anyhow::bail!("inform_epoch fake error, given epoch: {epoch}");
        }
        Ok(())
    }

    async fn update_epoch_settings(&mut self) -> StdResult<()> {
        if self.update_epoch_settings_error {
            anyhow::bail!("update_epoch_settings fake error");
        }
        Ok(())
    }

    async fn precompute_epoch_data(&mut self) -> StdResult<()> {
        if self.precompute_epoch_data_error {
            anyhow::bail!("precompute_epoch_data fake error");
        }
        Ok(())
    }

    fn epoch_of_current_data(&self) -> StdResult<Epoch> {
        Ok(self.unwrap_data()?.epoch)
    }

    fn current_protocol_parameters(&self) -> StdResult<&ProtocolParameters> {
        Ok(&self.unwrap_data()?.epoch_settings.protocol_parameters)
    }

    fn next_protocol_parameters(&self) -> StdResult<&ProtocolParameters> {
        Ok(&self.unwrap_data()?.next_epoch_settings.protocol_parameters)
    }

    fn upcoming_protocol_parameters(&self) -> StdResult<&ProtocolParameters> {
        Ok(&self
            .unwrap_data()?
            .upcoming_epoch_settings
            .protocol_parameters)
    }

    fn current_cardano_transactions_signing_config(
        &self,
    ) -> StdResult<&CardanoTransactionsSigningConfig> {
        Ok(&self
            .unwrap_data()?
            .epoch_settings
            .cardano_transactions_signing_config)
    }

    fn next_cardano_transactions_signing_config(
        &self,
    ) -> StdResult<&CardanoTransactionsSigningConfig> {
        Ok(&self
            .unwrap_data()?
            .next_epoch_settings
            .cardano_transactions_signing_config)
    }

    fn current_aggregate_verification_key(&self) -> StdResult<&ProtocolAggregateVerificationKey> {
        Ok(&self.unwrap_computed_data()?.aggregate_verification_key)
    }

    fn next_aggregate_verification_key(&self) -> StdResult<&ProtocolAggregateVerificationKey> {
        Ok(&self.unwrap_computed_data()?.next_aggregate_verification_key)
    }

    fn current_signers_with_stake(&self) -> StdResult<&Vec<SignerWithStake>> {
        Ok(&self.unwrap_data()?.current_signers_with_stake)
    }

    fn next_signers_with_stake(&self) -> StdResult<&Vec<SignerWithStake>> {
        Ok(&self.unwrap_data()?.next_signers_with_stake)
    }

    fn current_signers(&self) -> StdResult<&Vec<Signer>> {
        Ok(&self.unwrap_data()?.current_signers)
    }

    fn next_signers(&self) -> StdResult<&Vec<Signer>> {
        Ok(&self.unwrap_data()?.next_signers)
    }

    fn protocol_multi_signer(&self) -> StdResult<&ProtocolMultiSigner> {
        Ok(&self.unwrap_computed_data()?.protocol_multi_signer)
    }

    fn next_protocol_multi_signer(&self) -> StdResult<&ProtocolMultiSigner> {
        Ok(&self.unwrap_computed_data()?.next_protocol_multi_signer)
    }

    fn signed_entity_config(&self) -> StdResult<&SignedEntityConfig> {
        Ok(&self.unwrap_data()?.signed_entity_config)
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::{BlockNumber, CardanoTransactionsSigningConfig, PartyId};
    use mithril_common::test_utils::{fake_data, MithrilFixture, MithrilFixtureBuilder};
    use mithril_persistence::store::adapter::MemoryAdapter;
    use std::collections::{BTreeSet, HashMap};

    use crate::services::epoch_service::tests::ServiceBuilderParameters::WithFutureProtocolParameters;
    use crate::store::FakeEpochSettingsStorer;
    use crate::VerificationKeyStore;

    use super::*;

    #[derive(Debug, Clone, PartialEq)]
    struct ExpectedEpochData {
        epoch: Epoch,
        protocol_parameters: ProtocolParameters,
        next_protocol_parameters: ProtocolParameters,
        cardano_signing_config: CardanoTransactionsSigningConfig,
        next_cardano_signing_config: CardanoTransactionsSigningConfig,
        upcoming_protocol_parameters: ProtocolParameters,
        current_signers_with_stake: BTreeSet<SignerWithStake>,
        next_signers_with_stake: BTreeSet<SignerWithStake>,
        current_signers: BTreeSet<Signer>,
        next_signers: BTreeSet<Signer>,
        signed_entity_config: SignedEntityConfig,
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
                upcoming_protocol_parameters: service.upcoming_protocol_parameters()?.clone(),
                cardano_signing_config: service
                    .current_cardano_transactions_signing_config()?
                    .clone(),
                next_cardano_signing_config: service
                    .next_cardano_transactions_signing_config()?
                    .clone(),
                current_signers_with_stake: service
                    .current_signers_with_stake()?
                    .clone()
                    .into_iter()
                    .collect(),
                next_signers_with_stake: service
                    .next_signers_with_stake()?
                    .clone()
                    .into_iter()
                    .collect(),
                current_signers: service.current_signers()?.clone().into_iter().collect(),
                next_signers: service.next_signers()?.clone().into_iter().collect(),
                signed_entity_config: service.signed_entity_config()?.clone(),
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

    struct EpochServiceBuilder {
        cardano_transactions_signing_config: CardanoTransactionsSigningConfig,
        future_protocol_parameters: ProtocolParameters,
        network: CardanoNetwork,
        allowed_discriminants: BTreeSet<SignedEntityTypeDiscriminants>,
        signer_retrieval_epoch: Epoch,
        next_signer_retrieval_epoch: Epoch,
        signers_with_stake: Vec<SignerWithStake>,
        next_signers_with_stake: Vec<SignerWithStake>,
        stored_epoch_settings: AggregatorEpochSettings,
        stored_next_epoch_settings: AggregatorEpochSettings,
        stored_upcoming_epoch_settings: AggregatorEpochSettings,
    }

    impl EpochServiceBuilder {
        fn new(epoch: Epoch, current_epoch_fixture: MithrilFixture) -> Self {
            let next_epoch_fixture = current_epoch_fixture.clone();
            Self {
                // Aggregator configuration
                cardano_transactions_signing_config: CardanoTransactionsSigningConfig::dummy(),
                future_protocol_parameters: current_epoch_fixture.protocol_parameters(),
                network: CardanoNetwork::TestNet(0),
                allowed_discriminants: BTreeSet::new(),
                // Epoch used for verification keys and database
                signer_retrieval_epoch: epoch.offset_to_signer_retrieval_epoch().unwrap(),
                next_signer_retrieval_epoch: epoch.offset_to_next_signer_retrieval_epoch(),
                // Signers in verification key store
                signers_with_stake: current_epoch_fixture.signers_with_stake(),
                next_signers_with_stake: next_epoch_fixture.signers_with_stake(),
                // Database data
                stored_epoch_settings: AggregatorEpochSettings {
                    protocol_parameters: current_epoch_fixture.protocol_parameters(),
                    cardano_transactions_signing_config: CardanoTransactionsSigningConfig::dummy(),
                },
                stored_next_epoch_settings: AggregatorEpochSettings {
                    protocol_parameters: current_epoch_fixture.protocol_parameters(),
                    cardano_transactions_signing_config: CardanoTransactionsSigningConfig::dummy(),
                },
                stored_upcoming_epoch_settings: AggregatorEpochSettings {
                    protocol_parameters: current_epoch_fixture.protocol_parameters(),
                    cardano_transactions_signing_config: CardanoTransactionsSigningConfig::dummy(),
                },
            }
        }

        fn build(self) -> MithrilEpochService {
            let epoch_settings_storer = FakeEpochSettingsStorer::new(vec![
                (self.signer_retrieval_epoch, self.stored_epoch_settings),
                (
                    self.next_signer_retrieval_epoch,
                    self.stored_next_epoch_settings.clone(),
                ),
                (
                    self.next_signer_retrieval_epoch.next(),
                    self.stored_upcoming_epoch_settings.clone(),
                ),
            ]);
            let vkey_store = VerificationKeyStore::new(Box::new(
                MemoryAdapter::new(Some(vec![
                    (
                        self.signer_retrieval_epoch,
                        map_signers_for_vkey_store(&self.signers_with_stake),
                    ),
                    (
                        self.next_signer_retrieval_epoch,
                        map_signers_for_vkey_store(&self.next_signers_with_stake),
                    ),
                ]))
                .unwrap(),
            ));
            MithrilEpochService::new(
                AggregatorEpochSettings {
                    protocol_parameters: self.future_protocol_parameters,
                    cardano_transactions_signing_config: self.cardano_transactions_signing_config,
                },
                Arc::new(epoch_settings_storer),
                Arc::new(vkey_store),
                self.network,
                self.allowed_discriminants,
            )
        }
    }

    // TODO: Use EpochServiceBuilder and remove ServiceBuilderParameters
    enum ServiceBuilderParameters {
        DifferentFixtureForSecondEpoch(MithrilFixture),
        UpcomingProtocolParameters(ProtocolParameters),
        WithFutureProtocolParameters(ProtocolParameters),
    }

    // TODO: Use EpochServiceBuilder and remove build_service
    /// By default will copy data from the given fixture for all epochs, can be fined tuned
    /// with the [ServiceBuilderParameters].
    async fn build_service(
        epoch: Epoch,
        current_epoch_fixture: &MithrilFixture,
        additional_params: &[ServiceBuilderParameters],
    ) -> MithrilEpochService {
        let mut builder = EpochServiceBuilder::new(epoch, current_epoch_fixture.clone());
        for params in additional_params {
            match params {
                ServiceBuilderParameters::DifferentFixtureForSecondEpoch(fixture) => {
                    builder.stored_next_epoch_settings = AggregatorEpochSettings {
                        protocol_parameters: fixture.protocol_parameters(),
                        cardano_transactions_signing_config:
                            CardanoTransactionsSigningConfig::dummy(),
                    };
                    builder.next_signers_with_stake = fixture.signers_with_stake().clone();
                }
                ServiceBuilderParameters::UpcomingProtocolParameters(params) => {
                    builder.stored_upcoming_epoch_settings = AggregatorEpochSettings {
                        protocol_parameters: params.clone(),
                        cardano_transactions_signing_config:
                            CardanoTransactionsSigningConfig::dummy(),
                    };
                }
                ServiceBuilderParameters::WithFutureProtocolParameters(params) => {
                    builder.future_protocol_parameters = params.clone();
                }
            }
        }

        builder.build()
    }

    #[tokio::test]
    async fn inform_epoch_get_data_from_its_dependencies() {
        let current_epoch_fixture = MithrilFixtureBuilder::default().with_signers(3).build();
        let next_epoch_fixture = MithrilFixtureBuilder::default()
            .with_protocol_parameters(ProtocolParameters::new(8, 80, 0.80))
            .with_signers(5)
            .build();
        let upcoming_protocol_parameters = fake_data::protocol_parameters();

        let epoch = Epoch(5);
        let builder = EpochServiceBuilder {
            next_signers_with_stake: next_epoch_fixture.signers_with_stake(),
            stored_next_epoch_settings: AggregatorEpochSettings {
                protocol_parameters: next_epoch_fixture.protocol_parameters(),
                ..AggregatorEpochSettings::dummy()
            },
            stored_upcoming_epoch_settings: AggregatorEpochSettings {
                protocol_parameters: upcoming_protocol_parameters.clone(),
                ..AggregatorEpochSettings::dummy()
            },
            network: SignedEntityConfig::dummy().network,
            allowed_discriminants: SignedEntityConfig::dummy().allowed_discriminants,
            ..EpochServiceBuilder::new(epoch, current_epoch_fixture.clone())
        };

        let mut service = builder.build();

        service
            .inform_epoch(epoch)
            .await
            .expect("inform_epoch should not fail");

        let data = ExpectedEpochData::from_service(&service)
            .await
            .expect("extracting data from service should not fail");

        assert_eq!(
            data.clone(),
            ExpectedEpochData {
                epoch,
                protocol_parameters: current_epoch_fixture.protocol_parameters(),
                next_protocol_parameters: next_epoch_fixture.protocol_parameters(),
                upcoming_protocol_parameters,
                cardano_signing_config: CardanoTransactionsSigningConfig::dummy(),
                next_cardano_signing_config: CardanoTransactionsSigningConfig::dummy(),
                current_signers_with_stake: current_epoch_fixture
                    .signers_with_stake()
                    .into_iter()
                    .collect(),
                next_signers_with_stake: next_epoch_fixture
                    .signers_with_stake()
                    .into_iter()
                    .collect(),
                current_signers: current_epoch_fixture.signers().into_iter().collect(),
                next_signers: next_epoch_fixture.signers().into_iter().collect(),
                signed_entity_config: SignedEntityConfig::dummy(),
            }
        );
    }

    #[tokio::test]
    async fn inform_epoch_get_signed_entity_config_from_its_dependencies_and_store() {
        let epoch = Epoch(5);

        let cardano_transactions_signing_config =
            CardanoTransactionsSigningConfig::new(BlockNumber(29), BlockNumber(986));
        let network = CardanoNetwork::TestNet(27);
        let allowed_discriminants = BTreeSet::from([
            SignedEntityTypeDiscriminants::CardanoTransactions,
            SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
        ]);

        let builder = EpochServiceBuilder {
            network,
            allowed_discriminants: allowed_discriminants.clone(),
            stored_epoch_settings: AggregatorEpochSettings {
                cardano_transactions_signing_config: cardano_transactions_signing_config.clone(),
                ..AggregatorEpochSettings::dummy()
            },
            ..EpochServiceBuilder::new(epoch, MithrilFixtureBuilder::default().build())
        };

        let mut service = builder.build();

        service
            .inform_epoch(epoch)
            .await
            .expect("inform_epoch should not fail");

        let signed_entity_config = service
            .signed_entity_config()
            .expect("extracting data from service should not fail");

        assert_eq!(
            signed_entity_config.clone(),
            SignedEntityConfig {
                allowed_discriminants,
                network,
                cardano_transactions_signing_config,
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
        let mut service = build_service(
            epoch,
            &current_epoch_fixture,
            &[ServiceBuilderParameters::DifferentFixtureForSecondEpoch(
                next_epoch_fixture.clone(),
            )],
        )
        .await;

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
        let mut service = build_service(epoch, &fixture, &[]).await;
        let signer_builder = SignerBuilder::new(
            &fixture.signers_with_stake(),
            &fixture.protocol_parameters(),
        )
        .unwrap();
        service.computed_epoch_data = Some(ComputedEpochData {
            aggregate_verification_key: avk.clone(),
            next_aggregate_verification_key: avk.clone(),
            protocol_multi_signer: signer_builder.build_multi_signer(),
            next_protocol_multi_signer: signer_builder.build_multi_signer(),
        });

        service
            .inform_epoch(epoch)
            .await
            .expect("inform_epoch should not fail");

        assert!(service.computed_epoch_data.is_none());
    }

    #[tokio::test]
    async fn update_epoch_settings_insert_future_epoch_settings_in_the_store() {
        let fixture = MithrilFixtureBuilder::default().with_signers(3).build();
        let future_protocol_parameters = ProtocolParameters::new(6, 89, 0.124);
        let epoch = Epoch(4);
        let mut service = build_service(
            epoch,
            &fixture,
            &[WithFutureProtocolParameters(
                future_protocol_parameters.clone(),
            )],
        )
        .await;

        service
            .inform_epoch(epoch)
            .await
            .expect("inform_epoch should not fail");
        service
            .update_epoch_settings()
            .await
            .expect("update_epoch_settings should not fail");

        let inserted_epoch_settings = service
            .epoch_settings_storer
            .get_epoch_settings(epoch.offset_to_epoch_settings_recording_epoch())
            .await
            .unwrap_or_else(|_| {
                panic!(
                    "epoch settings should have been inserted for epoch {}",
                    epoch.offset_to_epoch_settings_recording_epoch()
                )
            })
            .unwrap();

        assert_eq!(
            inserted_epoch_settings.protocol_parameters,
            future_protocol_parameters
        );
    }

    #[tokio::test]
    async fn cant_get_data_if_inform_epoch_has_not_been_called() {
        let fixture = MithrilFixtureBuilder::default().with_signers(3).build();
        let service = build_service(Epoch(4), &fixture, &[]).await;

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
                "upcoming_protocol_parameters",
                service.upcoming_protocol_parameters().err(),
            ),
            (
                "current_cardano_transactions_signing_config",
                service.current_cardano_transactions_signing_config().err(),
            ),
            (
                "next_cardano_transactions_signing_config",
                service.next_cardano_transactions_signing_config().err(),
            ),
            (
                "current_signers_with_stake",
                service.current_signers_with_stake().err(),
            ),
            (
                "next_signers_with_stake",
                service.next_signers_with_stake().err(),
            ),
            ("current_signers", service.current_signers().err()),
            ("next_signers", service.next_signers().err()),
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
            (
                "next_protocol_multi_signer",
                service.next_protocol_multi_signer().err(),
            ),
            ("signed_entity_config", service.signed_entity_config().err()),
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
        let mut service = build_service(Epoch(4), &fixture, &[]).await;
        service.inform_epoch(Epoch(4)).await.unwrap();

        assert!(service.epoch_of_current_data().is_ok());
        assert!(service.current_protocol_parameters().is_ok());
        assert!(service.next_protocol_parameters().is_ok());
        assert!(service.upcoming_protocol_parameters().is_ok());
        assert!(service
            .current_cardano_transactions_signing_config()
            .is_ok());
        assert!(service.next_cardano_transactions_signing_config().is_ok());
        assert!(service.current_signers_with_stake().is_ok());
        assert!(service.next_signers_with_stake().is_ok());
        assert!(service.current_signers().is_ok());
        assert!(service.next_signers().is_ok());
        assert!(service.signed_entity_config().is_ok());

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
            (
                "next_protocol_multi_signer",
                service.next_protocol_multi_signer().err(),
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

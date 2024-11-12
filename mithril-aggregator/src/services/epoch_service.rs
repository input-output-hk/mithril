use anyhow::{anyhow, Context};
use async_trait::async_trait;
use mithril_common::chain_observer::ChainObserver;
use mithril_common::era::{EraChecker, SupportedEra};
use slog::{debug, Logger};
use std::collections::BTreeSet;
use std::sync::Arc;
use thiserror::Error;

use mithril_common::crypto_helper::ProtocolAggregateVerificationKey;
use mithril_common::entities::{
    CardanoEra, CardanoTransactionsSigningConfig, Epoch, ProtocolParameters, SignedEntityConfig,
    SignedEntityTypeDiscriminants, Signer, SignerWithStake, Stake, TotalSPOs,
};
use mithril_common::logging::LoggerExtensions;
use mithril_common::protocol::{MultiSigner as ProtocolMultiSigner, SignerBuilder};
use mithril_common::{CardanoNetwork, StdResult};

use crate::{
    entities::AggregatorEpochSettings, services::StakeDistributionService, EpochSettingsStorer,
    VerificationKeyStorer,
};

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

    /// Get the current Cardano era.
    fn cardano_era(&self) -> StdResult<CardanoEra>;

    /// Get the current Mithril era.
    fn mithril_era(&self) -> StdResult<SupportedEra>;

    /// Get the current epoch for which the data stored in this service are computed.
    fn epoch_of_current_data(&self) -> StdResult<Epoch>;

    /// Get protocol parameters used for signing in the current epoch.
    fn current_protocol_parameters(&self) -> StdResult<&ProtocolParameters>;

    /// Get protocol parameters used for signing in the next epoch.
    fn next_protocol_parameters(&self) -> StdResult<&ProtocolParameters>;

    /// Get protocol parameters for signer registration.
    fn signer_registration_protocol_parameters(&self) -> StdResult<&ProtocolParameters>;

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

    /// Get the total number of SPOs for the current epoch in the Cardano stake distribution.
    fn total_spo(&self) -> StdResult<TotalSPOs>;

    /// Get the total stake for the current epoch in the Cardano stake distribution.
    fn total_stake(&self) -> StdResult<Stake>;
}

struct EpochData {
    cardano_era: CardanoEra,
    mithril_era: SupportedEra,
    epoch: Epoch,
    current_epoch_settings: AggregatorEpochSettings,
    next_epoch_settings: AggregatorEpochSettings,
    signer_registration_epoch_settings: AggregatorEpochSettings,
    current_signers_with_stake: Vec<SignerWithStake>,
    next_signers_with_stake: Vec<SignerWithStake>,
    current_signers: Vec<Signer>,
    next_signers: Vec<Signer>,
    signed_entity_config: SignedEntityConfig,
    total_spo: TotalSPOs,
    total_stake: Stake,
}

struct ComputedEpochData {
    aggregate_verification_key: ProtocolAggregateVerificationKey,
    next_aggregate_verification_key: ProtocolAggregateVerificationKey,
    protocol_multi_signer: ProtocolMultiSigner,
    next_protocol_multi_signer: ProtocolMultiSigner,
}

/// Dependencies required by the [MithrilEpochService].
pub struct EpochServiceDependencies {
    epoch_settings_storer: Arc<dyn EpochSettingsStorer>,
    verification_key_store: Arc<dyn VerificationKeyStorer>,
    chain_observer: Arc<dyn ChainObserver>,
    era_checker: Arc<EraChecker>,
    stake_distribution_service: Arc<dyn StakeDistributionService>,
}

impl EpochServiceDependencies {
    /// Create a new instance of [EpochServiceDependencies].
    pub fn new(
        epoch_settings_storer: Arc<dyn EpochSettingsStorer>,
        verification_key_store: Arc<dyn VerificationKeyStorer>,
        chain_observer: Arc<dyn ChainObserver>,
        era_checker: Arc<EraChecker>,
        stake_distribution_service: Arc<dyn StakeDistributionService>,
    ) -> Self {
        Self {
            epoch_settings_storer,
            verification_key_store,
            chain_observer,
            era_checker,
            stake_distribution_service,
        }
    }
}

/// Implementation of the [epoch service][EpochService].
pub struct MithrilEpochService {
    /// Epoch settings that will be inserted when inform_epoch is called
    future_epoch_settings: AggregatorEpochSettings,
    epoch_data: Option<EpochData>,
    computed_epoch_data: Option<ComputedEpochData>,
    epoch_settings_storer: Arc<dyn EpochSettingsStorer>,
    verification_key_store: Arc<dyn VerificationKeyStorer>,
    chain_observer: Arc<dyn ChainObserver>,
    era_checker: Arc<EraChecker>,
    stake_distribution_service: Arc<dyn StakeDistributionService>,
    network: CardanoNetwork,
    allowed_signed_entity_discriminants: BTreeSet<SignedEntityTypeDiscriminants>,
    logger: Logger,
}

impl MithrilEpochService {
    /// Create a new service instance
    pub fn new(
        future_epoch_settings: AggregatorEpochSettings,
        dependencies: EpochServiceDependencies,
        network: CardanoNetwork,
        allowed_discriminants: BTreeSet<SignedEntityTypeDiscriminants>,
        logger: Logger,
    ) -> Self {
        Self {
            future_epoch_settings,
            epoch_data: None,
            computed_epoch_data: None,
            epoch_settings_storer: dependencies.epoch_settings_storer,
            verification_key_store: dependencies.verification_key_store,
            chain_observer: dependencies.chain_observer,
            era_checker: dependencies.era_checker,
            stake_distribution_service: dependencies.stake_distribution_service,
            network,
            allowed_signed_entity_discriminants: allowed_discriminants,
            logger: logger.new_with_component_name::<Self>(),
        }
    }

    async fn get_cardano_era(&self) -> StdResult<CardanoEra> {
        let cardano_era = self
            .chain_observer
            .get_current_era()
            .await?
            .ok_or_else(|| anyhow!("No Cardano era returned by the chain observer".to_string()))?;

        Ok(cardano_era)
    }

    async fn get_total_spo_and_total_stake(&self, epoch: Epoch) -> StdResult<(TotalSPOs, Stake)> {
        let stake_distribution = self
            .stake_distribution_service
            .get_stake_distribution(epoch)
            .await
            .with_context(|| {
                format!("Epoch service failed to obtain the stake distribution for epoch: {epoch}")
            })?;

        Ok((
            stake_distribution.len() as TotalSPOs,
            stake_distribution.values().sum(),
        ))
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
            self.logger, "Inserting epoch settings in epoch {recording_epoch}";
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
        debug!(self.logger, ">> inform_epoch(epoch: {epoch:?})");

        let cardano_era = self.get_cardano_era().await?;

        let mithril_era = self.era_checker.current_era();

        let signer_retrieval_epoch =
            epoch.offset_to_signer_retrieval_epoch().with_context(|| {
                format!("EpochService could not compute signer retrieval epoch from epoch: {epoch}")
            })?;
        let next_signer_retrieval_epoch = epoch.offset_to_next_signer_retrieval_epoch();

        let current_epoch_settings = self
            .get_epoch_settings(signer_retrieval_epoch, "current epoch settings")
            .await?;

        let next_epoch_settings = self
            .get_epoch_settings(next_signer_retrieval_epoch, "next epoch settings")
            .await?;

        let signer_registration_epoch_settings = self
            .get_epoch_settings(
                next_signer_retrieval_epoch.next(),
                "signer registration epoch settings",
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
            allowed_discriminants: self.allowed_signed_entity_discriminants.clone(),
            network: self.network,
            cardano_transactions_signing_config: current_epoch_settings
                .cardano_transactions_signing_config
                .clone(),
        };

        let (total_spo, total_stake) = self.get_total_spo_and_total_stake(epoch).await?;

        self.epoch_data = Some(EpochData {
            cardano_era,
            mithril_era,
            epoch,
            current_epoch_settings,
            next_epoch_settings,
            signer_registration_epoch_settings,
            current_signers_with_stake,
            next_signers_with_stake,
            current_signers,
            next_signers,
            signed_entity_config,
            total_spo,
            total_stake,
        });
        self.computed_epoch_data = None;

        Ok(())
    }

    async fn update_epoch_settings(&mut self) -> StdResult<()> {
        debug!(self.logger, ">> update_epoch_settings");

        let data = self.unwrap_data().with_context(|| {
            "can't update epoch settings if inform_epoch has not been called first"
        })?;

        self.insert_future_epoch_settings(data.epoch).await
    }

    async fn precompute_epoch_data(&mut self) -> StdResult<()> {
        debug!(self.logger, ">> precompute_epoch_data");

        let data = self.unwrap_data().with_context(|| {
            "can't precompute epoch data if inform_epoch has not been called first"
        })?;

        let protocol_multi_signer = SignerBuilder::new(
            &data.current_signers_with_stake,
            &data.current_epoch_settings.protocol_parameters,
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

    fn cardano_era(&self) -> StdResult<CardanoEra> {
        Ok(self.unwrap_data()?.cardano_era.clone())
    }

    fn mithril_era(&self) -> StdResult<SupportedEra> {
        Ok(self.unwrap_data()?.mithril_era)
    }

    fn epoch_of_current_data(&self) -> StdResult<Epoch> {
        Ok(self.unwrap_data()?.epoch)
    }

    fn current_protocol_parameters(&self) -> StdResult<&ProtocolParameters> {
        Ok(&self
            .unwrap_data()?
            .current_epoch_settings
            .protocol_parameters)
    }

    fn next_protocol_parameters(&self) -> StdResult<&ProtocolParameters> {
        Ok(&self.unwrap_data()?.next_epoch_settings.protocol_parameters)
    }

    fn signer_registration_protocol_parameters(&self) -> StdResult<&ProtocolParameters> {
        Ok(&self
            .unwrap_data()?
            .signer_registration_epoch_settings
            .protocol_parameters)
    }

    fn current_cardano_transactions_signing_config(
        &self,
    ) -> StdResult<&CardanoTransactionsSigningConfig> {
        Ok(&self
            .unwrap_data()?
            .current_epoch_settings
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

    fn total_spo(&self) -> StdResult<TotalSPOs> {
        Ok(self.unwrap_data()?.total_spo)
    }

    fn total_stake(&self) -> StdResult<Stake> {
        Ok(self.unwrap_data()?.total_stake)
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
pub struct FakeEpochServiceBuilder {
    pub cardano_era: CardanoEra,
    pub mithril_era: SupportedEra,
    pub epoch: Epoch,
    pub current_epoch_settings: AggregatorEpochSettings,
    pub next_epoch_settings: AggregatorEpochSettings,
    pub signer_registration_epoch_settings: AggregatorEpochSettings,
    pub current_signers_with_stake: Vec<SignerWithStake>,
    pub next_signers_with_stake: Vec<SignerWithStake>,
    pub signed_entity_config: SignedEntityConfig,
    pub total_spo: TotalSPOs,
    pub total_stake: Stake,
}

#[cfg(test)]
impl FakeEpochServiceBuilder {
    pub fn dummy(epoch: Epoch) -> Self {
        use mithril_common::test_utils::fake_data;
        let signers = fake_data::signers_with_stakes(3);

        Self {
            cardano_era: "DummyEra".to_string(),
            mithril_era: SupportedEra::dummy(),
            epoch,
            current_epoch_settings: AggregatorEpochSettings::dummy(),
            next_epoch_settings: AggregatorEpochSettings::dummy(),
            signer_registration_epoch_settings: AggregatorEpochSettings::dummy(),
            current_signers_with_stake: signers.clone(),
            next_signers_with_stake: signers,
            signed_entity_config: SignedEntityConfig::dummy(),
            total_spo: 0,
            total_stake: 0,
        }
    }

    pub fn build(self) -> FakeEpochService {
        let current_signers = Signer::vec_from(self.current_signers_with_stake.clone());
        let next_signers = Signer::vec_from(self.next_signers_with_stake.clone());

        let protocol_multi_signer = SignerBuilder::new(
            &self.current_signers_with_stake,
            &self.current_epoch_settings.protocol_parameters,
        )
        .with_context(|| "Could not build protocol_multi_signer for epoch service")
        .unwrap()
        .build_multi_signer();
        let next_protocol_multi_signer = SignerBuilder::new(
            &self.next_signers_with_stake,
            &self.next_epoch_settings.protocol_parameters,
        )
        .with_context(|| "Could not build protocol_multi_signer for epoch service")
        .unwrap()
        .build_multi_signer();

        FakeEpochService {
            epoch_data: Some(EpochData {
                cardano_era: self.cardano_era,
                mithril_era: self.mithril_era,
                epoch: self.epoch,
                current_epoch_settings: self.current_epoch_settings,
                next_epoch_settings: self.next_epoch_settings,
                signer_registration_epoch_settings: self.signer_registration_epoch_settings,
                current_signers_with_stake: self.current_signers_with_stake,
                next_signers_with_stake: self.next_signers_with_stake,
                current_signers,
                next_signers,
                signed_entity_config: self.signed_entity_config,
                total_spo: self.total_spo,
                total_stake: self.total_stake,
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
}

#[cfg(test)]
impl FakeEpochService {
    pub fn from_fixture(
        epoch: Epoch,
        fixture: &mithril_common::test_utils::MithrilFixture,
    ) -> Self {
        use mithril_common::entities::CardanoTransactionsSigningConfig;

        let current_epoch_settings = AggregatorEpochSettings {
            protocol_parameters: fixture.protocol_parameters(),
            cardano_transactions_signing_config: CardanoTransactionsSigningConfig::dummy(),
        };
        let next_epoch_settings = AggregatorEpochSettings {
            protocol_parameters: fixture.protocol_parameters(),
            cardano_transactions_signing_config: CardanoTransactionsSigningConfig::dummy(),
        };
        let signer_registration_epoch_settings = AggregatorEpochSettings {
            protocol_parameters: fixture.protocol_parameters(),
            cardano_transactions_signing_config: CardanoTransactionsSigningConfig::dummy(),
        };

        FakeEpochServiceBuilder {
            current_epoch_settings,
            next_epoch_settings,
            signer_registration_epoch_settings,
            current_signers_with_stake: fixture.signers_with_stake(),
            next_signers_with_stake: fixture.signers_with_stake(),
            ..FakeEpochServiceBuilder::dummy(epoch)
        }
        .build()
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

    fn cardano_era(&self) -> StdResult<CardanoEra> {
        Ok(self.unwrap_data()?.cardano_era.clone())
    }

    fn mithril_era(&self) -> StdResult<SupportedEra> {
        Ok(self.unwrap_data()?.mithril_era)
    }

    fn epoch_of_current_data(&self) -> StdResult<Epoch> {
        Ok(self.unwrap_data()?.epoch)
    }

    fn current_protocol_parameters(&self) -> StdResult<&ProtocolParameters> {
        Ok(&self
            .unwrap_data()?
            .current_epoch_settings
            .protocol_parameters)
    }

    fn next_protocol_parameters(&self) -> StdResult<&ProtocolParameters> {
        Ok(&self.unwrap_data()?.next_epoch_settings.protocol_parameters)
    }

    fn signer_registration_protocol_parameters(&self) -> StdResult<&ProtocolParameters> {
        Ok(&self
            .unwrap_data()?
            .signer_registration_epoch_settings
            .protocol_parameters)
    }

    fn current_cardano_transactions_signing_config(
        &self,
    ) -> StdResult<&CardanoTransactionsSigningConfig> {
        Ok(&self
            .unwrap_data()?
            .current_epoch_settings
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

    fn total_spo(&self) -> StdResult<u32> {
        Ok(self.unwrap_data()?.total_spo)
    }

    fn total_stake(&self) -> StdResult<u64> {
        Ok(self.unwrap_data()?.total_stake)
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::chain_observer::FakeObserver;
    use mithril_common::entities::{
        BlockNumber, CardanoTransactionsSigningConfig, PartyId, Stake, StakeDistribution,
    };
    use mithril_common::era::SupportedEra;
    use mithril_common::test_utils::{
        fake_data, MithrilFixture, MithrilFixtureBuilder, StakeDistributionGenerationMethod,
    };
    use mithril_persistence::store::adapter::MemoryAdapter;
    use std::collections::{BTreeSet, HashMap};

    use crate::services::MockStakeDistributionService;
    use crate::store::FakeEpochSettingsStorer;
    use crate::test_tools::TestLogger;
    use crate::VerificationKeyStore;

    use super::*;

    fn build_uniform_stake_distribution(
        total_spo: TotalSPOs,
        stake_by_spo: Stake,
    ) -> StakeDistribution {
        let fixture = MithrilFixtureBuilder::default()
            .with_signers(total_spo as usize)
            .with_stake_distribution(StakeDistributionGenerationMethod::Uniform(stake_by_spo))
            .build();

        fixture.stake_distribution()
    }

    #[derive(Debug, Clone, PartialEq)]
    struct ExpectedEpochData {
        cardano_era: CardanoEra,
        mithril_era: SupportedEra,
        epoch: Epoch,
        protocol_parameters: ProtocolParameters,
        next_protocol_parameters: ProtocolParameters,
        cardano_signing_config: CardanoTransactionsSigningConfig,
        next_cardano_signing_config: CardanoTransactionsSigningConfig,
        signer_registration_protocol_parameters: ProtocolParameters,
        current_signers_with_stake: BTreeSet<SignerWithStake>,
        next_signers_with_stake: BTreeSet<SignerWithStake>,
        current_signers: BTreeSet<Signer>,
        next_signers: BTreeSet<Signer>,
        signed_entity_config: SignedEntityConfig,
        total_spo: TotalSPOs,
        total_stake: Stake,
    }

    #[derive(Debug, Clone, PartialEq)]
    struct ExpectedComputedEpochData {
        aggregate_verification_key: ProtocolAggregateVerificationKey,
        next_aggregate_verification_key: ProtocolAggregateVerificationKey,
    }

    impl ExpectedEpochData {
        async fn from_service(service: &MithrilEpochService) -> StdResult<Self> {
            Ok(Self {
                cardano_era: service.cardano_era()?,
                mithril_era: service.mithril_era()?,
                epoch: service.epoch_of_current_data()?,
                protocol_parameters: service.current_protocol_parameters()?.clone(),
                next_protocol_parameters: service.next_protocol_parameters()?.clone(),
                signer_registration_protocol_parameters: service
                    .signer_registration_protocol_parameters()?
                    .clone(),
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
                total_spo: service.total_spo()?,
                total_stake: service.total_stake()?,
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
        cardano_era: CardanoEra,
        mithril_era: SupportedEra,
        current_epoch: Epoch,
        signers_with_stake: Vec<SignerWithStake>,
        next_signers_with_stake: Vec<SignerWithStake>,
        stored_current_epoch_settings: AggregatorEpochSettings,
        stored_next_epoch_settings: AggregatorEpochSettings,
        stored_signer_registration_epoch_settings: AggregatorEpochSettings,
        total_spo: TotalSPOs,
        total_stake: Stake,
    }

    impl EpochServiceBuilder {
        fn new(epoch: Epoch, epoch_fixture: MithrilFixture) -> Self {
            Self {
                cardano_transactions_signing_config: CardanoTransactionsSigningConfig::dummy(),
                future_protocol_parameters: epoch_fixture.protocol_parameters(),
                network: CardanoNetwork::TestNet(0),
                allowed_discriminants: BTreeSet::new(),
                cardano_era: String::new(),
                mithril_era: SupportedEra::dummy(),
                current_epoch: epoch,
                signers_with_stake: epoch_fixture.signers_with_stake(),
                next_signers_with_stake: epoch_fixture.signers_with_stake(),
                stored_current_epoch_settings: AggregatorEpochSettings {
                    protocol_parameters: epoch_fixture.protocol_parameters(),
                    cardano_transactions_signing_config: CardanoTransactionsSigningConfig::dummy(),
                },
                stored_next_epoch_settings: AggregatorEpochSettings {
                    protocol_parameters: epoch_fixture.protocol_parameters(),
                    cardano_transactions_signing_config: CardanoTransactionsSigningConfig::dummy(),
                },
                stored_signer_registration_epoch_settings: AggregatorEpochSettings {
                    protocol_parameters: epoch_fixture.protocol_parameters(),
                    cardano_transactions_signing_config: CardanoTransactionsSigningConfig::dummy(),
                },
                total_spo: 1,
                total_stake: 0,
            }
        }

        async fn build(self) -> MithrilEpochService {
            let signer_retrieval_epoch = self
                .current_epoch
                .offset_to_signer_retrieval_epoch()
                .unwrap();
            let next_signer_retrieval_epoch =
                self.current_epoch.offset_to_next_signer_retrieval_epoch();

            let epoch_settings_storer = FakeEpochSettingsStorer::new(vec![
                (signer_retrieval_epoch, self.stored_current_epoch_settings),
                (
                    next_signer_retrieval_epoch,
                    self.stored_next_epoch_settings.clone(),
                ),
                (
                    next_signer_retrieval_epoch.next(),
                    self.stored_signer_registration_epoch_settings.clone(),
                ),
            ]);
            let vkey_store = VerificationKeyStore::new(Box::new(
                MemoryAdapter::new(Some(vec![
                    (
                        signer_retrieval_epoch,
                        map_signers_for_vkey_store(&self.signers_with_stake),
                    ),
                    (
                        next_signer_retrieval_epoch,
                        map_signers_for_vkey_store(&self.next_signers_with_stake),
                    ),
                ]))
                .unwrap(),
            ));
            let chain_observer = FakeObserver::default();
            chain_observer.set_current_era(self.cardano_era).await;
            let era_checker = EraChecker::new(self.mithril_era, Epoch::default());

            let stake_distribution_service = {
                assert!(
                self.total_stake % self.total_spo as u64 == 0,
                "'total_stake' must be a multiple of 'total_spo' to create a uniform stake distribution"
            );
                let stake_per_spo = self.total_stake / self.total_spo as u64;

                let stake_distribution =
                    build_uniform_stake_distribution(self.total_spo, stake_per_spo);

                let mut stake_distribution_service = MockStakeDistributionService::new();
                stake_distribution_service
                    .expect_get_stake_distribution()
                    .returning(move |_| Ok(stake_distribution.clone()));

                stake_distribution_service
            };

            MithrilEpochService::new(
                AggregatorEpochSettings {
                    protocol_parameters: self.future_protocol_parameters,
                    cardano_transactions_signing_config: self.cardano_transactions_signing_config,
                },
                EpochServiceDependencies::new(
                    Arc::new(epoch_settings_storer),
                    Arc::new(vkey_store),
                    Arc::new(chain_observer),
                    Arc::new(era_checker),
                    Arc::new(stake_distribution_service),
                ),
                self.network,
                self.allowed_discriminants,
                TestLogger::stdout(),
            )
        }
    }

    #[tokio::test]
    async fn inform_epoch_get_data_from_its_dependencies() {
        let current_epoch_fixture = MithrilFixtureBuilder::default().with_signers(3).build();
        let next_epoch_fixture = MithrilFixtureBuilder::default()
            .with_protocol_parameters(ProtocolParameters::new(8, 80, 0.80))
            .with_signers(5)
            .build();
        let signer_registration_protocol_parameters = fake_data::protocol_parameters();

        let epoch = Epoch(5);
        let builder = EpochServiceBuilder {
            next_signers_with_stake: next_epoch_fixture.signers_with_stake(),
            stored_next_epoch_settings: AggregatorEpochSettings {
                protocol_parameters: next_epoch_fixture.protocol_parameters(),
                ..AggregatorEpochSettings::dummy()
            },
            stored_signer_registration_epoch_settings: AggregatorEpochSettings {
                protocol_parameters: signer_registration_protocol_parameters.clone(),
                ..AggregatorEpochSettings::dummy()
            },
            network: SignedEntityConfig::dummy().network,
            allowed_discriminants: SignedEntityConfig::dummy().allowed_discriminants,
            cardano_era: "CardanoEra".to_string(),
            mithril_era: SupportedEra::eras()[1],
            total_spo: 40,
            total_stake: 20_000_000,
            ..EpochServiceBuilder::new(epoch, current_epoch_fixture.clone())
        };

        let mut service = builder.build().await;

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
                cardano_era: "CardanoEra".to_string(),
                mithril_era: SupportedEra::eras()[1],
                epoch,
                protocol_parameters: current_epoch_fixture.protocol_parameters(),
                next_protocol_parameters: next_epoch_fixture.protocol_parameters(),
                signer_registration_protocol_parameters,
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
                total_spo: 40,
                total_stake: 20_000_000,
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

        let mut service = EpochServiceBuilder {
            network,
            allowed_discriminants: allowed_discriminants.clone(),
            stored_current_epoch_settings: AggregatorEpochSettings {
                cardano_transactions_signing_config: cardano_transactions_signing_config.clone(),
                ..AggregatorEpochSettings::dummy()
            },
            ..EpochServiceBuilder::new(epoch, MithrilFixtureBuilder::default().build())
        }
        .build()
        .await;

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
        let mut service = EpochServiceBuilder {
            stored_next_epoch_settings: AggregatorEpochSettings {
                protocol_parameters: next_epoch_fixture.protocol_parameters(),
                cardano_transactions_signing_config: CardanoTransactionsSigningConfig::dummy(),
            },
            next_signers_with_stake: next_epoch_fixture.signers_with_stake().clone(),
            ..EpochServiceBuilder::new(epoch, current_epoch_fixture.clone())
        }
        .build()
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
        let mut service = EpochServiceBuilder::new(epoch, fixture.clone())
            .build()
            .await;
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
        let future_protocol_parameters = ProtocolParameters::new(6, 89, 0.124);
        let epoch = Epoch(4);
        let mut service = EpochServiceBuilder {
            future_protocol_parameters: future_protocol_parameters.clone(),
            ..EpochServiceBuilder::new(epoch, MithrilFixtureBuilder::default().build())
        }
        .build()
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
        let service = EpochServiceBuilder::new(Epoch(4), fixture.clone())
            .build()
            .await;

        for (name, res) in [
            ("cardano_era", service.cardano_era().err()),
            ("mithril_era", service.mithril_era().err()),
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
                "signer_registration_protocol_parameters",
                service.signer_registration_protocol_parameters().err(),
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
            ("total_spo", service.total_spo().err()),
            ("total_stake", service.total_stake().err()),
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
        let mut service = EpochServiceBuilder::new(Epoch(4), fixture.clone())
            .build()
            .await;
        service.inform_epoch(Epoch(4)).await.unwrap();

        assert!(service.cardano_era().is_ok());
        assert!(service.mithril_era().is_ok());
        assert!(service.epoch_of_current_data().is_ok());
        assert!(service.current_protocol_parameters().is_ok());
        assert!(service.next_protocol_parameters().is_ok());
        assert!(service.signer_registration_protocol_parameters().is_ok());
        assert!(service
            .current_cardano_transactions_signing_config()
            .is_ok());
        assert!(service.next_cardano_transactions_signing_config().is_ok());
        assert!(service.current_signers_with_stake().is_ok());
        assert!(service.next_signers_with_stake().is_ok());
        assert!(service.current_signers().is_ok());
        assert!(service.next_signers().is_ok());
        assert!(service.signed_entity_config().is_ok());
        assert!(service.total_spo().is_ok());
        assert!(service.total_stake().is_ok());

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

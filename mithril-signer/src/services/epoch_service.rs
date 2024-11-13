use anyhow::anyhow;
use async_trait::async_trait;
use slog::{debug, trace, warn, Logger};
use std::collections::BTreeSet;
use std::sync::Arc;
use thiserror::Error;

use crate::dependency_injection::EpochServiceWrapper;
use crate::entities::SignerEpochSettings;
use crate::services::SignedEntityConfigProvider;
use crate::store::ProtocolInitializerStorer;
use crate::RunnerError;
use mithril_common::crypto_helper::ProtocolInitializer;
use mithril_common::entities::{
    CardanoTransactionsSigningConfig, Epoch, PartyId, ProtocolParameters, SignedEntityConfig,
    SignedEntityTypeDiscriminants, Signer, SignerWithStake,
};
use mithril_common::logging::LoggerExtensions;
use mithril_common::StdResult;
use mithril_persistence::store::StakeStorer;

/// Errors dedicated to the EpochService.
#[derive(Debug, Error)]
pub enum EpochServiceError {
    /// Raised when service has not collected data at least once.
    #[error("Epoch service was not initialized, the function `inform_epoch_settings` must be called first")]
    NotYetInitialized,
}

/// Service that aggregates all data that don't change in a given epoch.
#[async_trait]
pub trait EpochService: Sync + Send {
    /// Inform the service a new epoch has been detected, telling it to update its
    /// internal state for the new epoch.
    async fn inform_epoch_settings(
        &mut self,
        epoch_settings: SignerEpochSettings,
        allowed_discriminants: BTreeSet<SignedEntityTypeDiscriminants>,
    ) -> StdResult<()>;

    /// Get the current epoch for which the data stored in this service are computed.
    fn epoch_of_current_data(&self) -> StdResult<Epoch>;

    /// Get protocol parameters for registration.
    fn registration_protocol_parameters(&self) -> StdResult<&ProtocolParameters>;

    /// Get the protocol initializer for the current epoch if any
    ///
    /// `None` if the signer can't sign for the current epoch.
    fn protocol_initializer(&self) -> StdResult<&Option<ProtocolInitializer>>;

    /// Get signers for the current epoch
    fn current_signers(&self) -> StdResult<&Vec<Signer>>;

    /// Get signers for the next epoch
    fn next_signers(&self) -> StdResult<&Vec<Signer>>;

    /// Get signers with stake for the current epoch
    async fn current_signers_with_stake(&self) -> StdResult<Vec<SignerWithStake>>;

    /// Get signers with stake for the next epoch
    async fn next_signers_with_stake(&self) -> StdResult<Vec<SignerWithStake>>;

    /// Get the list of signed entity types that are allowed to sign for the current epoch
    fn allowed_discriminants(&self) -> StdResult<&BTreeSet<SignedEntityTypeDiscriminants>>;

    /// Get the cardano transactions signing configuration for the current epoch
    fn cardano_transactions_signing_config(
        &self,
    ) -> StdResult<&Option<CardanoTransactionsSigningConfig>>;

    /// Get the cardano transactions signing configuration for the next epoch
    fn next_cardano_transactions_signing_config(
        &self,
    ) -> StdResult<&Option<CardanoTransactionsSigningConfig>>;

    /// Check if the given signer can sign for the current epoch
    fn can_signer_sign_current_epoch(&self, party_id: PartyId) -> StdResult<bool>;
}

pub(crate) struct EpochData {
    pub epoch: Epoch,
    pub registration_protocol_parameters: ProtocolParameters,
    pub protocol_initializer: Option<ProtocolInitializer>,
    pub current_signers: Vec<Signer>,
    pub next_signers: Vec<Signer>,
    pub allowed_discriminants: BTreeSet<SignedEntityTypeDiscriminants>,
    pub cardano_transactions_signing_config: Option<CardanoTransactionsSigningConfig>,
    pub next_cardano_transactions_signing_config: Option<CardanoTransactionsSigningConfig>,
}

/// Implementation of the [epoch service][EpochService].
pub struct MithrilEpochService {
    stake_storer: Arc<dyn StakeStorer>,
    protocol_initializer_store: Arc<dyn ProtocolInitializerStorer>,
    epoch_data: Option<EpochData>,
    logger: Logger,
}

impl MithrilEpochService {
    /// Create a new service instance
    pub fn new(
        stake_storer: Arc<dyn StakeStorer>,
        protocol_initializer_store: Arc<dyn ProtocolInitializerStorer>,
        logger: Logger,
    ) -> Self {
        Self {
            stake_storer,
            protocol_initializer_store,
            epoch_data: None,
            logger: logger.new_with_component_name::<Self>(),
        }
    }

    async fn associate_signers_with_stake(
        &self,
        epoch: Epoch,
        signers: &[Signer],
    ) -> StdResult<Vec<SignerWithStake>> {
        debug!(
            self.logger,
            ">> associate_signers_with_stake(epoch:{epoch})"
        );

        let stakes = self
            .stake_storer
            .get_stakes(epoch)
            .await?
            .ok_or_else(|| RunnerError::NoValueError(format!("stakes at epoch {epoch}")))?;

        let mut signers_with_stake = vec![];

        for signer in signers {
            let stake = stakes
                .get(&*signer.party_id)
                .ok_or_else(|| RunnerError::NoStakeForSigner(signer.party_id.to_string()))?;

            signers_with_stake.push(SignerWithStake::new(
                signer.party_id.to_owned(),
                signer.verification_key.to_owned(),
                signer.verification_key_signature.to_owned(),
                signer.operational_certificate.to_owned(),
                signer.kes_period.to_owned(),
                *stake,
            ));
            trace!(
                self.logger,
                " > Associating signer_id {} with stake {}",
                signer.party_id,
                *stake
            );
        }

        Ok(signers_with_stake)
    }

    fn is_signer_included_in_current_stake_distribution(
        &self,
        party_id: PartyId,
        protocol_initializer: &ProtocolInitializer,
    ) -> StdResult<bool> {
        Ok(self.current_signers()?.iter().any(|s| {
            s.party_id == party_id
                && s.verification_key == protocol_initializer.verification_key().into()
        }))
    }

    fn unwrap_data(&self) -> Result<&EpochData, EpochServiceError> {
        self.epoch_data
            .as_ref()
            .ok_or(EpochServiceError::NotYetInitialized)
    }
}

#[async_trait]
impl EpochService for MithrilEpochService {
    async fn inform_epoch_settings(
        &mut self,
        epoch_settings: SignerEpochSettings,
        allowed_discriminants: BTreeSet<SignedEntityTypeDiscriminants>,
    ) -> StdResult<()> {
        debug!(self.logger, ">> inform_epoch_settings"; "epoch_settings" => ?epoch_settings);

        let epoch = epoch_settings.epoch;
        let protocol_initializer = self
            .protocol_initializer_store
            .get_protocol_initializer(epoch.offset_to_signer_retrieval_epoch()?)
            .await?;

        self.epoch_data = Some(EpochData {
            epoch,
            registration_protocol_parameters: epoch_settings.registration_protocol_parameters,
            protocol_initializer,
            current_signers: epoch_settings.current_signers,
            next_signers: epoch_settings.next_signers,
            allowed_discriminants,
            cardano_transactions_signing_config: epoch_settings.cardano_transactions_signing_config,
            next_cardano_transactions_signing_config: epoch_settings
                .next_cardano_transactions_signing_config,
        });

        Ok(())
    }

    fn epoch_of_current_data(&self) -> StdResult<Epoch> {
        Ok(self.unwrap_data()?.epoch)
    }

    fn registration_protocol_parameters(&self) -> StdResult<&ProtocolParameters> {
        Ok(&self.unwrap_data()?.registration_protocol_parameters)
    }

    fn protocol_initializer(&self) -> StdResult<&Option<ProtocolInitializer>> {
        Ok(&self.unwrap_data()?.protocol_initializer)
    }

    fn current_signers(&self) -> StdResult<&Vec<Signer>> {
        Ok(&self.unwrap_data()?.current_signers)
    }

    fn next_signers(&self) -> StdResult<&Vec<Signer>> {
        Ok(&self.unwrap_data()?.next_signers)
    }

    async fn current_signers_with_stake(&self) -> StdResult<Vec<SignerWithStake>> {
        let current_epoch = self.epoch_of_current_data()?;
        self.associate_signers_with_stake(
            current_epoch.offset_to_signer_retrieval_epoch()?,
            self.current_signers()?,
        )
        .await
    }

    async fn next_signers_with_stake(&self) -> StdResult<Vec<SignerWithStake>> {
        let current_epoch = self.epoch_of_current_data()?;
        self.associate_signers_with_stake(
            current_epoch.offset_to_next_signer_retrieval_epoch(),
            self.next_signers()?,
        )
        .await
    }

    fn allowed_discriminants(&self) -> StdResult<&BTreeSet<SignedEntityTypeDiscriminants>> {
        Ok(&self.unwrap_data()?.allowed_discriminants)
    }

    fn cardano_transactions_signing_config(
        &self,
    ) -> StdResult<&Option<CardanoTransactionsSigningConfig>> {
        Ok(&self.unwrap_data()?.cardano_transactions_signing_config)
    }

    fn next_cardano_transactions_signing_config(
        &self,
    ) -> StdResult<&Option<CardanoTransactionsSigningConfig>> {
        Ok(&self.unwrap_data()?.next_cardano_transactions_signing_config)
    }

    fn can_signer_sign_current_epoch(&self, party_id: PartyId) -> StdResult<bool> {
        let epoch = self.epoch_of_current_data()?;
        if let Some(protocol_initializer) = self.protocol_initializer()? {
            debug!(
                self.logger,
                " > Got protocol initializer for this epoch ({epoch})"
            );
            if self
                .is_signer_included_in_current_stake_distribution(party_id, protocol_initializer)?
            {
                return Ok(true);
            } else {
                debug!(
                    self.logger,
                    " > Signer not in current stake distribution. Can NOT sign"
                );
            }
        } else {
            warn!(
                self.logger,
                " > NO protocol initializer found for this epoch ({epoch})",
            );
        }

        Ok(false)
    }
}

/// Simple wrapper to the [EpochService] to implement the [SignedEntityConfigProvider] trait.
///
/// Needed because the epoch service is wrapped in an Arc<RwLock<>> in the dependencies, making
/// direct usage of implemented traits methods difficult.
pub struct SignerSignedEntityConfigProvider {
    epoch_service: EpochServiceWrapper,
}

impl SignerSignedEntityConfigProvider {
    /// Create a new instance of the `SignerSignedEntityConfigProvider`.
    pub fn new(epoch_service: EpochServiceWrapper) -> Self {
        Self { epoch_service }
    }
}

#[async_trait]
impl SignedEntityConfigProvider for SignerSignedEntityConfigProvider {
    async fn get(&self) -> StdResult<SignedEntityConfig> {
        let epoch_service = self.epoch_service.read().await;
        let cardano_transactions_signing_config =
            match epoch_service.cardano_transactions_signing_config()? {
                Some(config) => Ok(config.clone()),
                None => Err(anyhow!("No cardano transaction signing config available")),
            }?;

        Ok(SignedEntityConfig {
            allowed_discriminants: epoch_service.allowed_discriminants()?.clone(),
            cardano_transactions_signing_config,
        })
    }
}

#[cfg(test)]
impl MithrilEpochService {
    /// `TEST ONLY` - Create a new instance of the service using dumb dependencies.
    pub fn new_with_dumb_dependencies() -> Self {
        use crate::database::repository::StakePoolStore;
        use crate::database::test_helper::main_db_connection;
        use crate::store::ProtocolInitializerStore;
        use crate::test_tools::TestLogger;
        use mithril_persistence::store::adapter::DumbStoreAdapter;

        let sqlite_connection = Arc::new(main_db_connection().unwrap());
        let stake_store = Arc::new(StakePoolStore::new(sqlite_connection, None));
        let protocol_initializer_store = Arc::new(ProtocolInitializerStore::new(
            Box::new(DumbStoreAdapter::new()),
            None,
        ));

        Self::new(
            stake_store,
            protocol_initializer_store,
            TestLogger::stdout(),
        )
    }

    /// `TEST ONLY` - Set all data to either default values, empty values, or fake values
    /// if no default/empty can be set.
    pub fn set_data_to_default_or_fake(mut self, epoch: Epoch) -> Self {
        use mithril_common::test_utils::fake_data;

        let epoch_data = EpochData {
            epoch,
            registration_protocol_parameters: fake_data::protocol_parameters(),
            protocol_initializer: None,
            current_signers: vec![],
            next_signers: vec![],
            allowed_discriminants: BTreeSet::new(),
            cardano_transactions_signing_config: None,
            next_cardano_transactions_signing_config: None,
        };
        self.epoch_data = Some(epoch_data);
        self
    }

    /// `TEST ONLY` - Alter the data stored in the service, won't do anything if no data is stored.
    pub(crate) fn alter_data(mut self, data_config: impl FnOnce(&mut EpochData)) -> Self {
        if let Some(data) = self.epoch_data.as_mut() {
            data_config(data);
        }
        self
    }
}

#[cfg(test)]
pub mod mock_epoch_service {
    use mockall::mock;

    use super::*;

    mock! {
        pub EpochServiceImpl {}

        #[async_trait]
        impl EpochService for EpochServiceImpl {
            async fn inform_epoch_settings(
                &mut self,
                epoch_settings: SignerEpochSettings,
                allowed_discriminants: BTreeSet<SignedEntityTypeDiscriminants>,
            ) -> StdResult<()>;

            fn epoch_of_current_data(&self) -> StdResult<Epoch>;

            fn registration_protocol_parameters(&self) -> StdResult<&'static ProtocolParameters>;

            fn protocol_initializer(&self) -> StdResult<&'static Option<ProtocolInitializer>>;

            fn current_signers(&self) -> StdResult<&'static Vec<Signer>>;

            fn next_signers(&self) -> StdResult<&'static Vec<Signer>>;

            async fn current_signers_with_stake(&self) -> StdResult<Vec<SignerWithStake>>;

            async fn next_signers_with_stake(&self) -> StdResult<Vec<SignerWithStake>>;

            fn allowed_discriminants(&self) -> StdResult<&'static BTreeSet<SignedEntityTypeDiscriminants>>;

            fn cardano_transactions_signing_config(
                &self,
            ) -> StdResult<&'static Option<CardanoTransactionsSigningConfig>>;

            fn next_cardano_transactions_signing_config(
                &self,
            ) -> StdResult<&'static Option<CardanoTransactionsSigningConfig>>;

            fn can_signer_sign_current_epoch(&self, party_id: PartyId) -> StdResult<bool>;
        }
    }

    impl MockEpochServiceImpl {
        pub fn new_with_config(
            config: impl FnOnce(&mut MockEpochServiceImpl),
        ) -> MockEpochServiceImpl {
            let mut epoch_service_mock = MockEpochServiceImpl::new();
            config(&mut epoch_service_mock);

            epoch_service_mock
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;
    use tokio::sync::RwLock;

    use mithril_common::entities::{Epoch, StakeDistribution};
    use mithril_common::test_utils::{fake_data, MithrilFixtureBuilder};
    use mithril_persistence::store::adapter::{DumbStoreAdapter, MemoryAdapter};

    use crate::database::repository::StakePoolStore;
    use crate::database::test_helper::main_db_connection;
    use crate::entities::SignerEpochSettings;
    use crate::services::MithrilProtocolInitializerBuilder;
    use crate::store::ProtocolInitializerStore;
    use crate::test_tools::TestLogger;

    use super::*;

    #[test]
    fn test_is_signer_included_in_current_stake_distribution_returns_error_when_epoch_settings_is_not_set(
    ) {
        let party_id = "party_id".to_string();
        let protocol_initializer = MithrilProtocolInitializerBuilder::build(
            &100,
            &fake_data::protocol_parameters(),
            None,
            None,
        )
        .unwrap();
        let stake_store = Arc::new(StakePoolStore::new(
            Arc::new(main_db_connection().unwrap()),
            None,
        ));
        let protocol_initializer_store = Arc::new(ProtocolInitializerStore::new(
            Box::new(DumbStoreAdapter::new()),
            None,
        ));
        let service = MithrilEpochService::new(
            stake_store,
            protocol_initializer_store,
            TestLogger::stdout(),
        );

        service
            .is_signer_included_in_current_stake_distribution(party_id, &protocol_initializer)
            .expect_err("can_signer_sign should return error when epoch settings is not set");
    }

    #[tokio::test]
    async fn test_is_signer_included_in_current_stake_distribution_returns_true_when_signer_verification_key_and_pool_id_found(
    ) {
        let fixtures = MithrilFixtureBuilder::default().with_signers(10).build();
        let protocol_initializer = fixtures.signers_fixture()[0]
            .protocol_initializer
            .to_owned();
        let epoch = Epoch(12);
        let signers = fixtures.signers();
        let stake_store = Arc::new(StakePoolStore::new(
            Arc::new(main_db_connection().unwrap()),
            None,
        ));
        let protocol_initializer_store = Arc::new(ProtocolInitializerStore::new(
            Box::new(DumbStoreAdapter::new()),
            None,
        ));

        let epoch_settings = SignerEpochSettings {
            epoch,
            current_signers: signers[..5].to_vec(),
            ..SignerEpochSettings::dummy().clone()
        };

        let mut service = MithrilEpochService::new(
            stake_store,
            protocol_initializer_store,
            TestLogger::stdout(),
        );
        service
            .inform_epoch_settings(epoch_settings.clone(), BTreeSet::new())
            .await
            .unwrap();

        let party_id = fixtures.signers_fixture()[0].party_id();
        assert!(service
            .is_signer_included_in_current_stake_distribution(
                party_id.clone(),
                &protocol_initializer
            )
            .unwrap());

        let party_id_not_included = fixtures.signers_fixture()[6].party_id();
        assert!(!service
            .is_signer_included_in_current_stake_distribution(
                party_id_not_included,
                &protocol_initializer
            )
            .unwrap());

        let protocol_initializer_not_included = fixtures.signers_fixture()[6]
            .protocol_initializer
            .to_owned();
        assert!(!service
            .is_signer_included_in_current_stake_distribution(
                party_id,
                &protocol_initializer_not_included
            )
            .unwrap());
    }

    mod can_signer_sign_current_epoch {
        use super::*;

        #[test]
        fn cant_sign_if_no_protocol_initializer_are_stored() {
            let epoch = Epoch(12);
            let fixtures = MithrilFixtureBuilder::default().with_signers(10).build();

            let epoch_service = MithrilEpochService::new_with_dumb_dependencies()
                .set_data_to_default_or_fake(epoch)
                .alter_data(|data| {
                    data.protocol_initializer = None;
                    data.current_signers = fixtures.signers();
                });

            let can_sign_current_epoch = epoch_service
                .can_signer_sign_current_epoch(fixtures.signers()[0].party_id.clone())
                .unwrap();
            assert!(!can_sign_current_epoch);
        }

        #[test]
        fn cant_sign_if_stored_protocol_initializer_belong_to_another_party() {
            let epoch = Epoch(12);
            let fixtures = MithrilFixtureBuilder::default().with_signers(10).build();

            let epoch_service = MithrilEpochService::new_with_dumb_dependencies()
                .set_data_to_default_or_fake(epoch)
                .alter_data(|data| {
                    data.protocol_initializer =
                        Some(fixtures.signers_fixture()[2].protocol_initializer.clone());
                    data.current_signers = fixtures.signers();
                });

            let can_sign_current_epoch = epoch_service
                .can_signer_sign_current_epoch(fixtures.signers()[0].party_id.clone())
                .unwrap();
            assert!(!can_sign_current_epoch);
        }

        #[test]
        fn cant_sign_if_not_in_current_signers() {
            let epoch = Epoch(12);
            let fixtures = MithrilFixtureBuilder::default().with_signers(10).build();

            let epoch_service = MithrilEpochService::new_with_dumb_dependencies()
                .set_data_to_default_or_fake(epoch)
                .alter_data(|data| {
                    data.protocol_initializer =
                        Some(fixtures.signers_fixture()[0].protocol_initializer.clone());
                    data.current_signers = fixtures.signers()[2..].to_vec();
                });

            let can_sign_current_epoch = epoch_service
                .can_signer_sign_current_epoch(fixtures.signers()[0].party_id.clone())
                .unwrap();
            assert!(!can_sign_current_epoch);
        }

        #[test]
        fn can_sign_if_in_current_signers_and_use_the_stored_protocol_initializer() {
            let epoch = Epoch(12);
            let fixtures = MithrilFixtureBuilder::default().with_signers(10).build();

            let epoch_service = MithrilEpochService::new_with_dumb_dependencies()
                .set_data_to_default_or_fake(epoch)
                .alter_data(|data| {
                    data.protocol_initializer =
                        Some(fixtures.signers_fixture()[0].protocol_initializer.clone());
                    data.current_signers = fixtures.signers();
                });

            let can_sign_current_epoch = epoch_service
                .can_signer_sign_current_epoch(fixtures.signers()[0].party_id.clone())
                .unwrap();
            assert!(can_sign_current_epoch);
        }
    }

    #[tokio::test]
    async fn test_retrieve_data_return_error_before_register_epoch_settings_was_call() {
        let epoch = Epoch(12);
        // Signers and stake distribution
        let signers = fake_data::signers(10);
        let stake_distribution: StakeDistribution = signers
            .iter()
            .enumerate()
            .map(|(i, signer)| (signer.party_id.clone(), (i + 1) as u64 * 100))
            .collect();

        // Init stores
        let stake_store = Arc::new(StakePoolStore::new(
            Arc::new(main_db_connection().unwrap()),
            None,
        ));
        stake_store
            .save_stakes(epoch, stake_distribution.clone())
            .await
            .expect("save_stakes should not fail");
        let protocol_initializer_store = Arc::new(ProtocolInitializerStore::new(
            Box::new(DumbStoreAdapter::new()),
            None,
        ));

        // Build service and register epoch settings
        let service = MithrilEpochService::new(
            stake_store,
            protocol_initializer_store,
            TestLogger::stdout(),
        );
        assert!(service.epoch_of_current_data().is_err());
        assert!(service.registration_protocol_parameters().is_err());
        assert!(service.protocol_initializer().is_err());
        assert!(service.current_signers().is_err());
        assert!(service.next_signers().is_err());
        assert!(service.current_signers_with_stake().await.is_err());
        assert!(service.next_signers_with_stake().await.is_err());
        assert!(service.cardano_transactions_signing_config().is_err());
        assert!(service.next_cardano_transactions_signing_config().is_err());
    }

    #[tokio::test]
    async fn test_data_are_available_after_register_epoch_settings_call() {
        let epoch = Epoch(12);
        // Signers and stake distribution
        let signers = fake_data::signers(10);

        // Init stores
        let stake_store = Arc::new(StakePoolStore::new(
            Arc::new(main_db_connection().unwrap()),
            None,
        ));
        let protocol_initializer_store = Arc::new(ProtocolInitializerStore::new(
            Box::new(DumbStoreAdapter::new()),
            None,
        ));

        // Epoch settings
        let epoch_settings = SignerEpochSettings {
            epoch,
            current_signers: signers[2..5].to_vec(),
            next_signers: signers[3..7].to_vec(),
            ..SignerEpochSettings::dummy().clone()
        };

        // Build service and register epoch settings
        let mut service = MithrilEpochService::new(
            stake_store,
            protocol_initializer_store,
            TestLogger::stdout(),
        );

        service
            .inform_epoch_settings(
                epoch_settings.clone(),
                BTreeSet::from([SignedEntityTypeDiscriminants::CardanoImmutableFilesFull]),
            )
            .await
            .unwrap();

        // Check current_signers
        {
            let current_signers = service.current_signers().unwrap();
            let expected_current_signers = epoch_settings.current_signers.clone();
            assert_eq!(expected_current_signers, *current_signers);
        }
        // Check next_signers
        {
            let next_signers = service.next_signers().unwrap();
            let expected_next_signers = epoch_settings.next_signers.clone();
            assert_eq!(expected_next_signers, *next_signers);
        }

        // Check other data
        assert_eq!(
            epoch_settings.epoch,
            service.epoch_of_current_data().unwrap()
        );
        assert_eq!(
            epoch_settings.registration_protocol_parameters,
            *service.registration_protocol_parameters().unwrap()
        );
        assert!(
            service.protocol_initializer().unwrap().is_none(),
            "protocol_initializer should be None since nothing was in store"
        );

        // Check allowed_discriminants
        assert_eq!(
            BTreeSet::from([SignedEntityTypeDiscriminants::CardanoImmutableFilesFull]),
            *service.allowed_discriminants().unwrap()
        );

        // Check cardano_transactions_signing_config
        assert_eq!(
            epoch_settings.cardano_transactions_signing_config,
            *service.cardano_transactions_signing_config().unwrap()
        );
        // Check next_cardano_transactions_signing_config
        assert_eq!(
            epoch_settings.next_cardano_transactions_signing_config,
            *service.next_cardano_transactions_signing_config().unwrap()
        );
    }

    #[tokio::test]
    async fn test_signers_with_stake_are_available_after_register_epoch_settings_call() {
        fn build_stake_distribution(signers: &[Signer], first_stake: u64) -> StakeDistribution {
            signers
                .iter()
                .enumerate()
                .map(|(i, signer)| (signer.party_id.clone(), first_stake + i as u64))
                .collect()
        }

        let epoch = Epoch(12);

        // Signers and stake distribution
        let signers = fake_data::signers(10);
        let stake_distribution: StakeDistribution = build_stake_distribution(&signers, 100);
        let next_stake_distribution: StakeDistribution = build_stake_distribution(&signers, 500);

        let stake_store = {
            let store = Arc::new(StakePoolStore::new(
                Arc::new(main_db_connection().unwrap()),
                None,
            ));
            store
                .save_stakes(
                    epoch.offset_to_signer_retrieval_epoch().unwrap(),
                    stake_distribution.clone(),
                )
                .await
                .unwrap();
            store
                .save_stakes(
                    epoch.offset_to_next_signer_retrieval_epoch(),
                    next_stake_distribution.clone(),
                )
                .await
                .unwrap();
            store
        };
        let protocol_initializer_store = Arc::new(ProtocolInitializerStore::new(
            Box::new(DumbStoreAdapter::new()),
            None,
        ));

        // Epoch settings
        let epoch_settings = SignerEpochSettings {
            epoch,
            current_signers: signers[2..5].to_vec(),
            next_signers: signers[3..7].to_vec(),
            ..SignerEpochSettings::dummy().clone()
        };

        // Build service and register epoch settings
        let mut service = MithrilEpochService::new(
            stake_store,
            protocol_initializer_store,
            TestLogger::stdout(),
        );
        service
            .inform_epoch_settings(epoch_settings.clone(), BTreeSet::new())
            .await
            .unwrap();

        // Check current signers with stake
        {
            let current_signers = service.current_signers_with_stake().await.unwrap();

            assert_eq!(epoch_settings.current_signers.len(), current_signers.len());
            for signer in current_signers {
                let expected_stake = stake_distribution.get(&signer.party_id).unwrap();
                assert_eq!(expected_stake, &signer.stake);
            }
        }

        // Check next signers with stake
        {
            let next_signers = service.next_signers_with_stake().await.unwrap();

            assert_eq!(epoch_settings.next_signers.len(), next_signers.len());
            for signer in next_signers {
                let expected_stake = next_stake_distribution.get(&signer.party_id).unwrap();
                assert_eq!(expected_stake, &signer.stake);
            }
        }
    }

    #[tokio::test]
    async fn test_protocol_initializer_is_available_after_register_epoch_settings_call_if_in_store()
    {
        let epoch = Epoch(12);
        let stake_store = Arc::new(StakePoolStore::new(
            Arc::new(main_db_connection().unwrap()),
            None,
        ));
        let protocol_initializer_store = Arc::new(ProtocolInitializerStore::new(
            Box::new(
                MemoryAdapter::new(Some(vec![(
                    epoch.offset_to_signer_retrieval_epoch().unwrap(),
                    fake_data::protocol_initializer("seed", 1245),
                )]))
                .unwrap(),
            ),
            None,
        ));

        let mut service = MithrilEpochService::new(
            stake_store,
            protocol_initializer_store,
            TestLogger::stdout(),
        );
        let epoch_settings = SignerEpochSettings {
            epoch,
            ..SignerEpochSettings::dummy().clone()
        };
        service
            .inform_epoch_settings(epoch_settings, BTreeSet::new())
            .await
            .unwrap();

        let protocol_initializer = service.protocol_initializer().unwrap();
        assert_eq!(
            Some(1245),
            protocol_initializer.as_ref().map(|p| p.get_stake()),
        );
    }

    #[tokio::test]
    async fn is_source_of_signed_entity_config() {
        let stake_store = Arc::new(StakePoolStore::new(
            Arc::new(main_db_connection().unwrap()),
            None,
        ));
        let protocol_initializer_store = Arc::new(ProtocolInitializerStore::new(
            Box::new(DumbStoreAdapter::new()),
            None,
        ));
        let epoch_service = Arc::new(RwLock::new(MithrilEpochService::new(
            stake_store,
            protocol_initializer_store,
            TestLogger::stdout(),
        )));
        let config_provider = SignerSignedEntityConfigProvider {
            epoch_service: epoch_service.clone(),
        };

        // Fail before the first `inform_epoch_settings`
        {
            config_provider.get().await.expect_err(
                "Should fail since sources data are not set before the first inform_epoch_settings",
            );
        }
        // Fail after `inform_epoch_settings` if `cardano_transactions_signing_config` is not set
        {
            let allowed_discriminants =
                BTreeSet::from([SignedEntityTypeDiscriminants::CardanoImmutableFilesFull]);

            epoch_service
                .write()
                .await
                .inform_epoch_settings(
                    SignerEpochSettings {
                        cardano_transactions_signing_config: None,
                        ..SignerEpochSettings::dummy()
                    },
                    allowed_discriminants.clone(),
                )
                .await
                .unwrap();

            config_provider
                .get()
                .await
                .expect_err("Should fail since cardano_transactions_signing_config is not set");
        }
        // Success after `inform_epoch_settings` if `cardano_transactions_signing_config` is set
        {
            let allowed_discriminants =
                BTreeSet::from([SignedEntityTypeDiscriminants::CardanoImmutableFilesFull]);
            epoch_service
                .write()
                .await
                .inform_epoch_settings(
                    SignerEpochSettings {
                        cardano_transactions_signing_config: Some(
                            CardanoTransactionsSigningConfig::dummy(),
                        ),
                        ..SignerEpochSettings::dummy()
                    },
                    allowed_discriminants.clone(),
                )
                .await
                .unwrap();

            let config = config_provider.get().await.unwrap();

            assert_eq!(
                SignedEntityConfig {
                    allowed_discriminants,
                    cardano_transactions_signing_config: CardanoTransactionsSigningConfig::dummy(),
                },
                config
            );
        }
    }
}

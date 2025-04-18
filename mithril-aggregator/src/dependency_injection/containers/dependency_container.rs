use slog::Logger;
use std::sync::Arc;
use tokio::sync::RwLock;

use mithril_common::{
    api_version::APIVersionProvider,
    chain_observer::ChainObserver,
    entities::{
        CardanoTransactionsSigningConfig, Epoch, ProtocolParameters, SignerWithStake,
        StakeDistribution,
    },
    era::{EraChecker, EraReader},
    signable_builder::SignableBuilderService,
    test_utils::MithrilFixture,
    TickerService,
};

use mithril_persistence::store::StakeStorer;
use mithril_signed_entity_lock::SignedEntityTypeLock;

use crate::{
    database::repository::{
        CertificateRepository, SignedEntityStorer, SignerGetter, StakePoolStore,
    },
    entities::AggregatorEpochSettings,
    event_store::{EventMessage, TransmitterService},
    services::{
        CertifierService, EpochService, MessageService, ProverService, SignedEntityService,
        SignerRecorder, SignerSynchronizer, StakeDistributionService, UpkeepService,
    },
    EpochSettingsStorer, MetricsService, SignerRegisterer, SignerRegistrationRoundOpener,
    SingleSignatureAuthenticator, VerificationKeyStorer,
};

/// EpochServiceWrapper wraps
pub type EpochServiceWrapper = Arc<RwLock<dyn EpochService>>;

/// DependencyManager handles the dependencies
pub struct DependencyContainer {
    /// Application root logger
    pub(crate) root_logger: Logger,

    /// Stake Store used by the StakeDistributionService
    /// It shall be a private dependency.
    pub(crate) stake_store: Arc<StakePoolStore>,

    /// It shall be a private dependency.
    /// Certificate store.
    pub certificate_repository: Arc<CertificateRepository>,

    /// Verification key store.
    pub verification_key_store: Arc<dyn VerificationKeyStorer>,

    /// Epoch settings storer.
    pub epoch_settings_storer: Arc<dyn EpochSettingsStorer>,

    /// Chain observer service.
    pub(crate) chain_observer: Arc<dyn ChainObserver>,

    /// Signer registerer service
    pub signer_registerer: Arc<dyn SignerRegisterer>,

    /// Signer synchronizer service
    pub(crate) signer_synchronizer: Arc<dyn SignerSynchronizer>,

    /// Signer registration round opener service
    pub(crate) signer_registration_round_opener: Arc<dyn SignerRegistrationRoundOpener>,

    /// Era checker service
    pub(crate) era_checker: Arc<EraChecker>,

    /// Era reader service
    pub(crate) era_reader: Arc<EraReader>,

    /// Event Transmitter Service
    pub(crate) event_transmitter: Arc<TransmitterService<EventMessage>>,

    /// API Version provider
    pub(crate) api_version_provider: Arc<APIVersionProvider>,

    /// Stake Distribution Service
    pub(crate) stake_distribution_service: Arc<dyn StakeDistributionService>,

    /// Signer Recorder
    pub(crate) signer_recorder: Arc<dyn SignerRecorder>,

    /// Signable Builder Service
    pub signable_builder_service: Arc<dyn SignableBuilderService>,

    /// Signed Entity Service
    pub signed_entity_service: Arc<dyn SignedEntityService>,

    /// Certifier Service
    pub certifier_service: Arc<dyn CertifierService>,

    /// Epoch service
    pub(crate) epoch_service: EpochServiceWrapper,

    /// Ticker Service
    pub(crate) ticker_service: Arc<dyn TickerService>,

    /// Signed Entity storer
    pub signed_entity_storer: Arc<dyn SignedEntityStorer>,

    /// Signer getter service
    pub(crate) signer_getter: Arc<dyn SignerGetter>,

    /// HTTP message service
    pub message_service: Arc<dyn MessageService>,

    /// Prover service
    pub prover_service: Arc<dyn ProverService>,

    /// Signed Entity Type Lock
    pub signed_entity_type_lock: Arc<SignedEntityTypeLock>,

    /// Upkeep service
    pub(crate) upkeep_service: Arc<dyn UpkeepService>,

    /// Single signer authenticator
    pub(crate) single_signer_authenticator: Arc<SingleSignatureAuthenticator>,

    /// Metrics service
    pub(crate) metrics_service: Arc<MetricsService>,
}

#[doc(hidden)]
impl DependencyContainer {
    /// `TEST METHOD ONLY`
    ///
    /// Get the first two epochs that will be used by a newly started aggregator
    pub async fn get_genesis_epochs(&self) -> (Epoch, Epoch) {
        let current_epoch = self
            .chain_observer
            .get_current_epoch()
            .await
            .expect("get_current_epoch should not fail")
            .expect("an epoch should've been set to the chain observer");
        let work_epoch = current_epoch
            .offset_to_signer_retrieval_epoch()
            .expect("epoch.offset_by SIGNER_EPOCH_RETRIEVAL_OFFSET should not fail");
        let epoch_to_sign = current_epoch.offset_to_next_signer_retrieval_epoch();

        (work_epoch, epoch_to_sign)
    }

    /// `TEST METHOD ONLY`
    ///
    /// Fill the stores of a [DependencyManager] in a way to simulate an aggregator state
    /// using the data from a precomputed fixture.
    pub async fn init_state_from_fixture(
        &self,
        fixture: &MithrilFixture,
        cardano_transactions_signing_config: &CardanoTransactionsSigningConfig,
        target_epochs: &[Epoch],
    ) {
        for epoch in target_epochs {
            self.epoch_settings_storer
                .save_epoch_settings(
                    *epoch,
                    AggregatorEpochSettings {
                        protocol_parameters: fixture.protocol_parameters(),
                        cardano_transactions_signing_config: cardano_transactions_signing_config
                            .clone(),
                    },
                )
                .await
                .expect("save_epoch_settings should not fail");
            self.fill_verification_key_store(*epoch, &fixture.signers_with_stake())
                .await;
            self.fill_stakes_store(*epoch, fixture.signers_with_stake())
                .await;
        }
    }

    /// `TEST METHOD ONLY`
    ///
    /// Fill the stores of a [DependencyManager] in a way to simulate an aggregator genesis state.
    ///
    /// For the current and the next epoch:
    /// * Fill the [VerificationKeyStorer] with the given signers keys.
    /// * Fill the [StakeStore] with the given signers stakes.
    /// * Fill the [ProtocolParametersStore] with the given parameters.
    pub async fn prepare_for_genesis(
        &self,
        genesis_signers: Vec<SignerWithStake>,
        second_epoch_signers: Vec<SignerWithStake>,
        genesis_protocol_parameters: &ProtocolParameters,
        cardano_transactions_signing_config: &CardanoTransactionsSigningConfig,
    ) {
        self.init_epoch_settings_storer(&AggregatorEpochSettings {
            protocol_parameters: genesis_protocol_parameters.clone(),
            cardano_transactions_signing_config: cardano_transactions_signing_config.clone(),
        })
        .await;

        let (work_epoch, epoch_to_sign) = self.get_genesis_epochs().await;
        for (epoch, signers) in [
            (work_epoch, genesis_signers),
            (epoch_to_sign, second_epoch_signers),
        ] {
            self.fill_verification_key_store(epoch, &signers).await;
            self.fill_stakes_store(epoch, signers).await;
        }
    }

    /// `TEST METHOD ONLY`
    ///
    /// Fill up to the first three epochs of the [EpochSettingsStorer] with the given value.
    pub async fn init_epoch_settings_storer(&self, epoch_settings: &AggregatorEpochSettings) {
        let (work_epoch, epoch_to_sign) = self.get_genesis_epochs().await;
        let mut epochs_to_save = Vec::new();
        epochs_to_save.push(work_epoch);
        epochs_to_save.push(epoch_to_sign);
        epochs_to_save.push(epoch_to_sign.next());
        for epoch in epochs_to_save {
            self.epoch_settings_storer
                .save_epoch_settings(epoch, epoch_settings.clone())
                .await
                .expect("save_epoch_settings should not fail");
        }
    }

    async fn fill_verification_key_store(&self, target_epoch: Epoch, signers: &[SignerWithStake]) {
        for signer in signers {
            self.signer_recorder
                .record_signer_registration(signer.party_id.clone())
                .await
                .expect("record_signer_registration should not fail");
            self.verification_key_store
                .save_verification_key(target_epoch, signer.clone())
                .await
                .expect("save_verification_key should not fail");
        }
    }

    async fn fill_stakes_store(&self, target_epoch: Epoch, signers: Vec<SignerWithStake>) {
        let _ = self
            .stake_store
            .save_stakes(
                target_epoch,
                signers
                    .iter()
                    .map(|s| s.into())
                    .collect::<StakeDistribution>(),
            )
            .await
            .expect("save_stakes should not fail");
    }
}

#[cfg(test)]
pub(crate) mod tests {

    use std::{path::PathBuf, sync::Arc};

    use crate::{
        dependency_injection::DependenciesBuilder, DependencyContainer, ServeCommandConfiguration,
    };

    /// Initialize dependency container with a unique temporary snapshot directory build from test path.
    /// This macro should used directly in a function test to be able to retrieve the function name.
    #[macro_export]
    macro_rules! initialize_dependencies {
        () => {{
            initialize_dependencies(mithril_common::temp_dir!())
        }};
    }

    pub async fn initialize_dependencies(tmp_path: PathBuf) -> DependencyContainer {
        let config = ServeCommandConfiguration::new_sample(tmp_path);

        let mut builder = DependenciesBuilder::new_with_stdout_logger(Arc::new(config));

        builder.build_dependency_container().await.unwrap()
    }
}

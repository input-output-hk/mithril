use mithril_common::{
    api_version::APIVersionProvider,
    certificate_chain::CertificateVerifier,
    chain_observer::ChainObserver,
    crypto_helper::ProtocolGenesisVerifier,
    digesters::{ImmutableDigester, ImmutableFileObserver},
    entities::{Certificate, Epoch, ProtocolParameters, SignerWithStake, StakeDistribution},
    era::{EraChecker, EraReader},
    store::StakeStorer,
    test_utils::MithrilFixture,
    BeaconProvider,
};
use sqlite::Connection;

use std::{collections::HashMap, sync::Arc};
use tokio::sync::{Mutex, RwLock};

use crate::{
    artifact_builder::ArtifactBuilderService, certifier_service::CertifierService,
    configuration::*, database::provider::StakePoolStore, signable_builder::SignableBuilderService,
    signer_registerer::SignerRecorder, ticker_service::TickerService, CertificatePendingStore,
    CertificateStore, ProtocolParametersStore, ProtocolParametersStorer, SignerRegisterer,
    SignerRegistrationRoundOpener, Snapshotter, VerificationKeyStore, VerificationKeyStorer,
};
use crate::{event_store::EventMessage, snapshot_stores::SnapshotStore};
use crate::{event_store::TransmitterService, multi_signer::MultiSigner};
use crate::{
    snapshot_uploaders::SnapshotUploader, stake_distribution_service::StakeDistributionService,
};

/// MultiSignerWrapper wraps a MultiSigner
pub type MultiSignerWrapper = Arc<RwLock<dyn MultiSigner>>;

/// DependencyManager handles the dependencies
pub struct DependencyManager {
    /// Configuration structure.
    pub config: Configuration,

    /// SQLite database connection
    /// This is not a real service but is is needed to instantiate all store
    /// services. Shall be private dependency.
    pub sqlite_connection: Arc<Mutex<Connection>>,

    /// Stake Store used by the StakeDistributionService
    /// It shall be a private dependency.
    pub stake_store: Arc<StakePoolStore>,

    /// Snapshot store.
    pub snapshot_store: Arc<dyn SnapshotStore>,

    /// Snapshot uploader service.
    pub snapshot_uploader: Arc<dyn SnapshotUploader>,

    /// Multisigner service.
    pub multi_signer: MultiSignerWrapper,

    /// Certificate pending store.
    pub certificate_pending_store: Arc<CertificatePendingStore>,

    /// Certificate store.
    pub certificate_store: Arc<CertificateStore>,

    /// Verification key store.
    pub verification_key_store: Arc<VerificationKeyStore>,

    /// Protocol parameter store.
    pub protocol_parameters_store: Arc<ProtocolParametersStore>,

    /// Chain observer service.
    pub chain_observer: Arc<dyn ChainObserver>,

    /// Beacon provider service.
    pub beacon_provider: Arc<dyn BeaconProvider>,

    /// Immutable file observer service.
    pub immutable_file_observer: Arc<dyn ImmutableFileObserver>,

    /// Digester service.
    pub digester: Arc<dyn ImmutableDigester>,

    /// Snapshotter service.
    pub snapshotter: Arc<dyn Snapshotter>,

    /// Certificate verifier service.
    pub certificate_verifier: Arc<dyn CertificateVerifier>,

    /// Genesis signature verifier service.
    pub genesis_verifier: Arc<ProtocolGenesisVerifier>,

    /// Signer registerer service
    pub signer_registerer: Arc<dyn SignerRegisterer>,

    /// Signer registration round opener service
    pub signer_registration_round_opener: Arc<dyn SignerRegistrationRoundOpener>,

    /// Era checker service
    pub era_checker: Arc<EraChecker>,

    /// Era reader service
    pub era_reader: Arc<EraReader>,

    /// Event Transmitter Service
    pub event_transmitter: Arc<TransmitterService<EventMessage>>,

    /// API Version provider
    pub api_version_provider: Arc<APIVersionProvider>,

    /// Stake Distribution Service
    pub stake_distribution_service: Arc<dyn StakeDistributionService>,

    /// Signer Recorder
    pub signer_recorder: Arc<dyn SignerRecorder>,

    /// Signable Builder Service
    pub signable_builder_service: Arc<dyn SignableBuilderService>,

    /// Artifact Builder Service
    pub artifact_builder_service: Arc<dyn ArtifactBuilderService>,

    /// Certifier Service
    pub certifier_service: Arc<dyn CertifierService>,

    /// Ticker Service
    pub ticker_service: Arc<dyn TickerService>,
}

#[doc(hidden)]
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SimulateFromChainParams {
    /// Will set the multi_signer protocol parameters & beacon to the one in contained in the latest certificate.
    SetupMultiSigner,
}

#[doc(hidden)]
impl DependencyManager {
    /// Get the first two epochs that will be used by a newly started aggregator
    async fn get_genesis_epochs(&self) -> (Epoch, Epoch) {
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
    pub async fn init_state_from_fixture(&self, fixture: &MithrilFixture, target_epochs: &[Epoch]) {
        for epoch in target_epochs {
            self.protocol_parameters_store
                .save_protocol_parameters(*epoch, fixture.protocol_parameters())
                .await
                .expect("save_protocol_parameters should not fail");
            self.fill_verification_key_store(*epoch, &fixture.signers_with_stake())
                .await;
            self.fill_stakes_store(*epoch, fixture.signers_with_stake())
                .await;
        }
    }

    /// `TEST METHOD ONLY`
    ///
    /// Fill the stores of a [DependencyManager] in a way to simulate an aggregator genesis state
    /// using the data from a precomputed certificate_chain.
    ///
    /// Arguments:
    pub async fn init_state_from_chain(
        &self,
        certificate_chain: &[Certificate],
        additional_params: Vec<SimulateFromChainParams>,
    ) {
        if certificate_chain.is_empty() {
            panic!("The given certificate chain must contains at least one certificate");
        }

        let mut certificate_chain = certificate_chain.to_vec();
        certificate_chain.sort_by(|left, right| {
            left.beacon
                .partial_cmp(&right.beacon)
                .expect("The given certificates should all share the same network")
        });
        let last_certificate = certificate_chain.last().unwrap().clone();
        let last_beacon = last_certificate.beacon.clone();
        let last_protocol_parameters = last_certificate.metadata.protocol_parameters.clone();

        let mut parameters_per_epoch: HashMap<Epoch, (Vec<SignerWithStake>, ProtocolParameters)> =
            HashMap::new();
        for certificate in certificate_chain.iter() {
            if parameters_per_epoch.contains_key(&certificate.beacon.epoch) {
                continue;
            }

            parameters_per_epoch.insert(
                certificate.beacon.epoch,
                (
                    certificate.metadata.signers.clone(),
                    certificate.metadata.protocol_parameters.clone(),
                ),
            );
        }

        for (epoch, params) in parameters_per_epoch {
            self.protocol_parameters_store
                .save_protocol_parameters(epoch, params.1)
                .await
                .expect("save_protocol_parameters should not fail");
            self.fill_verification_key_store(epoch, &params.0).await;
            self.fill_stakes_store(epoch, params.0.to_vec()).await;
        }

        for certificate in certificate_chain {
            self.certificate_store
                .save(certificate.to_owned())
                .await
                .expect("certificate_store::save should not fail");
        }

        if additional_params.contains(&SimulateFromChainParams::SetupMultiSigner) {
            let mut multi_signer = self.multi_signer.write().await;

            multi_signer
                .update_current_beacon(last_beacon)
                .await
                .expect("setting the beacon should not fail");
            multi_signer
                .update_protocol_parameters(&last_protocol_parameters.into())
                .await
                .expect("updating protocol parameters should not fail");
        }
    }

    /// `TEST METHOD ONLY`
    ///
    /// Fill the stores of a [DependencyManager] in a way to simulate an aggregator genesis state.
    ///
    /// For the current and the next epoch:
    /// * Fill the [VerificationKeyStore] with the given signers keys.
    /// * Fill the [StakeStore] with the given signers stakes.
    /// * Fill the [ProtocolParametersStore] with the given parameters.
    pub async fn prepare_for_genesis(
        &self,
        genesis_signers: Vec<SignerWithStake>,
        second_epoch_signers: Vec<SignerWithStake>,
        genesis_protocol_parameters: &ProtocolParameters,
    ) {
        self.init_protocol_parameter_store(genesis_protocol_parameters)
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
    /// Fill up to the first three epochs of the [ProtocolParametersStore] with the given value.
    pub async fn init_protocol_parameter_store(&self, protocol_parameters: &ProtocolParameters) {
        let (work_epoch, epoch_to_sign) = self.get_genesis_epochs().await;
        let mut epochs_to_save = Vec::new();
        epochs_to_save.push(work_epoch);
        epochs_to_save.push(epoch_to_sign);
        epochs_to_save.push(epoch_to_sign.next());
        for epoch in epochs_to_save {
            self.protocol_parameters_store
                .save_protocol_parameters(epoch, protocol_parameters.clone())
                .await
                .expect("save_protocol_parameters should not fail");
        }
    }

    async fn fill_verification_key_store(&self, target_epoch: Epoch, signers: &[SignerWithStake]) {
        for signer in signers {
            self.signer_recorder
                .record_signer_id(signer.party_id.clone())
                .await
                .expect("record_signer_id should not fail");
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
pub mod tests {
    use crate::{
        dependency_injection::DependenciesBuilder, AggregatorConfig, Configuration,
        DependencyManager,
    };
    use mithril_common::CardanoNetwork;

    pub async fn initialize_dependencies() -> (DependencyManager, AggregatorConfig) {
        let config = Configuration::new_sample();
        let mut builder = DependenciesBuilder::new(config);
        let dependency_manager = builder.build_dependency_container().await.unwrap();

        let config = AggregatorConfig::new(
            dependency_manager.config.run_interval,
            CardanoNetwork::TestNet(42),
            dependency_manager.config.db_directory.as_path(),
        );

        (dependency_manager, config)
    }
}

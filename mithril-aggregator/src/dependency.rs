use mithril_common::certificate_chain::CertificateVerifier;
use mithril_common::crypto_helper::ProtocolGenesisVerifier;
use std::sync::Arc;
use tokio::sync::RwLock;

use mithril_common::chain_observer::ChainObserver;
use mithril_common::digesters::{ImmutableDigester, ImmutableFileObserver};
use mithril_common::entities::{
    Epoch, ProtocolParameters, Signer, SignerWithStake, StakeDistribution,
};
use mithril_common::store::{StakeStore, StakeStorer};
use mithril_common::BeaconProvider;

use crate::configuration::*;
use crate::multi_signer::MultiSigner;
use crate::snapshot_stores::SnapshotStore;
use crate::snapshot_uploaders::SnapshotUploader;
use crate::{
    CertificatePendingStore, CertificateStore, ProtocolParametersStore, ProtocolParametersStorer,
    SingleSignatureStore, Snapshotter, VerificationKeyStore, VerificationKeyStorer,
};

/// MultiSignerWrapper wraps a MultiSigner
pub type MultiSignerWrapper = Arc<RwLock<dyn MultiSigner>>;

/// DependencyManager handles the dependencies
pub struct DependencyManager {
    /// Configuration structure.
    pub config: Configuration,

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

    /// Stake store.
    pub stake_store: Arc<StakeStore>,

    /// Signer single signature store.
    pub single_signature_store: Arc<SingleSignatureStore>,

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
        let epoch_to_sign = current_epoch
            .offset_to_next_signer_retrieval_epoch()
            .expect("epoch.offset_by NEXT_SIGNER_EPOCH_RETRIEVAL_OFFSET should not fail");

        (work_epoch, epoch_to_sign)
    }

    /// `TEST METHOD ONLY`
    ///
    /// Fill the stores of a [DependencyManager] in a way to simulate an aggregator genesis state.
    ///
    /// For the current and the next epoch:
    /// * Fill the [VerificationKeyStore] with the given signers keys.
    /// * Fill the [StakeStore] with the given signers stakes.
    /// * Fill the [ProtocolParametersStore] with the given parameters.
    pub async fn simulate_genesis(
        &self,
        genesis_signers: Vec<SignerWithStake>,
        second_epoch_signers: Vec<SignerWithStake>,
        genesis_protocol_parameters: &ProtocolParameters,
    ) {
        let (work_epoch, epoch_to_sign) = self.get_genesis_epochs().await;
        for (epoch, signers) in [
            (work_epoch, genesis_signers),
            (epoch_to_sign, second_epoch_signers),
        ] {
            for signer in signers
                .clone()
                .into_iter()
                .map(|s| s.into())
                .collect::<Vec<Signer>>()
            {
                self.verification_key_store
                    .save_verification_key(epoch, signer.clone())
                    .await
                    .expect("save_verification_key should not fail");
            }

            self.stake_store
                .save_stakes(
                    epoch,
                    signers
                        .clone()
                        .iter()
                        .map(|s| s.into())
                        .collect::<StakeDistribution>(),
                )
                .await
                .expect("save_stakes should not fail");
        }

        self.init_protocol_parameter_store(genesis_protocol_parameters)
            .await;
    }

    /// `TEST METHOD ONLY`
    ///
    /// Fill the first two epoch of the [ProtocolParametersStore] with the given value.
    pub async fn init_protocol_parameter_store(&self, protocol_parameters: &ProtocolParameters) {
        let (work_epoch, epoch_to_sign) = self.get_genesis_epochs().await;
        for epoch in [work_epoch, epoch_to_sign] {
            self.protocol_parameters_store
                .save_protocol_parameters(epoch, protocol_parameters.clone())
                .await
                .expect("save_protocol_parameters should not fail");
        }
    }
}

#[cfg(test)]
pub mod tests {
    use crate::{
        AggregatorConfig, CertificatePendingStore, CertificateStore, Configuration,
        DependencyManager, DumbSnapshotUploader, DumbSnapshotter, LocalSnapshotStore,
        MultiSignerImpl, ProtocolParametersStore, SingleSignatureStore, SnapshotStoreType,
        SnapshotUploaderType, VerificationKeyStore,
    };
    use mithril_common::certificate_chain::MithrilCertificateVerifier;
    use mithril_common::crypto_helper::{key_encode_hex, ProtocolGenesisSigner};
    use mithril_common::digesters::{DumbImmutableDigester, DumbImmutableFileObserver};
    use mithril_common::{
        chain_observer::FakeObserver,
        fake_data,
        store::{adapter::MemoryAdapter, StakeStore},
        BeaconProviderImpl, CardanoNetwork,
    };
    use std::{path::PathBuf, sync::Arc};
    use tokio::sync::RwLock;

    pub async fn initialize_dependencies() -> (DependencyManager, AggregatorConfig) {
        let genesis_signer = ProtocolGenesisSigner::create_deterministic_genesis_signer();
        let genesis_verifier = Arc::new(genesis_signer.create_genesis_verifier());
        let genesis_verification_key = genesis_verifier.to_verification_key();
        let config = Configuration {
            cardano_cli_path: PathBuf::new(),
            cardano_node_socket_path: PathBuf::new(),
            network_magic: Some(42),
            network: "whatever".to_string(),
            protocol_parameters: fake_data::protocol_parameters(),
            url_snapshot_manifest: "https://storage.googleapis.com/cardano-testnet/snapshots.json"
                .to_string(),
            snapshot_store_type: SnapshotStoreType::Local,
            snapshot_uploader_type: SnapshotUploaderType::Local,
            server_ip: "0.0.0.0".to_string(),
            server_port: 8000,
            run_interval: 5000,
            db_directory: PathBuf::new(),
            snapshot_directory: PathBuf::new(),
            data_stores_directory: PathBuf::new(),
            genesis_verification_key: key_encode_hex(&genesis_verification_key).unwrap(),
            store_retention_limit: None,
        };
        let snapshot_store = Arc::new(LocalSnapshotStore::new(
            Box::new(MemoryAdapter::new(None).unwrap()),
            20,
        ));
        let snapshot_uploader = Arc::new(DumbSnapshotUploader::new());
        let certificate_pending_store = Arc::new(CertificatePendingStore::new(Box::new(
            MemoryAdapter::new(None).unwrap(),
        )));
        let certificate_store = Arc::new(CertificateStore::new(Box::new(
            MemoryAdapter::new(None).unwrap(),
        )));
        let verification_key_store = Arc::new(VerificationKeyStore::new(
            Box::new(MemoryAdapter::new(None).unwrap()),
            config.store_retention_limit,
        ));
        let stake_store = Arc::new(StakeStore::new(
            Box::new(MemoryAdapter::new(None).unwrap()),
            config.store_retention_limit,
        ));
        let single_signature_store = Arc::new(SingleSignatureStore::new(
            Box::new(MemoryAdapter::new(None).unwrap()),
            config.store_retention_limit,
        ));
        let protocol_parameters_store = Arc::new(ProtocolParametersStore::new(
            Box::new(MemoryAdapter::new(None).unwrap()),
            None,
        ));
        let chain_observer = Arc::new(FakeObserver::default());
        let multi_signer = MultiSignerImpl::new(
            verification_key_store.clone(),
            stake_store.clone(),
            single_signature_store.clone(),
            protocol_parameters_store.clone(),
            chain_observer,
        );
        let multi_signer = Arc::new(RwLock::new(multi_signer));
        let immutable_file_observer = Arc::new(DumbImmutableFileObserver::default());
        let chain_observer = Arc::new(FakeObserver::default());
        let beacon_provider = Arc::new(BeaconProviderImpl::new(
            chain_observer.clone(),
            immutable_file_observer.clone(),
            CardanoNetwork::TestNet(42),
        ));
        let certificate_verifier = Arc::new(MithrilCertificateVerifier::new(slog_scope::logger()));
        let dependency_manager = DependencyManager {
            config,
            snapshot_store,
            snapshot_uploader,
            multi_signer,
            certificate_pending_store,
            certificate_store,
            verification_key_store,
            stake_store,
            single_signature_store,
            protocol_parameters_store,
            chain_observer,
            beacon_provider,
            immutable_file_observer,
            digester: Arc::new(DumbImmutableDigester::new("digest", true)),
            snapshotter: Arc::new(DumbSnapshotter::new()),
            certificate_verifier,
            genesis_verifier,
        };

        let config = AggregatorConfig::new(
            dependency_manager.config.run_interval,
            CardanoNetwork::TestNet(42),
            dependency_manager.config.db_directory.as_path(),
        );

        (dependency_manager, config)
    }
}

use mithril_common::digesters::Digester;
use std::sync::Arc;
use tokio::sync::RwLock;

use mithril_common::chain_observer::ChainObserver;
use mithril_common::store::StakeStore;

use super::entities::*;
use super::multi_signer::MultiSigner;
use super::snapshot_stores::SnapshotStore;
use crate::beacon_provider::ImmutableFileObserver;
use crate::beacon_store::BeaconStore;
use crate::snapshot_uploaders::SnapshotUploader;
use crate::{
    BeaconProvider, CertificatePendingStore, CertificateStore, SingleSignatureStore, Snapshotter,
    VerificationKeyStore,
};

/// BeaconStoreWrapper wraps a BeaconStore
pub type BeaconStoreWrapper = Arc<dyn BeaconStore>;

///  SnapshotStoreWrapper wraps a SnapshotStore
pub type SnapshotStoreWrapper = Arc<dyn SnapshotStore>;

///  SnapshotUploaderWrapper wraps a SnapshotUploader
pub type SnapshotUploaderWrapper = Arc<dyn SnapshotUploader>;

/// MultiSignerWrapper wraps a MultiSigner
pub type MultiSignerWrapper = Arc<RwLock<dyn MultiSigner>>;

/// CertificatePendingStoreWrapper wraps a CertificatePendingStore
pub type CertificatePendingStoreWrapper = Arc<CertificatePendingStore>;

///  CertificateStoreWrapper wraps a CertificateStore
pub type CertificateStoreWrapper = Arc<RwLock<CertificateStore>>;

///  VerificationKeyStoreWrapper wraps a VerificationKeyStore
pub type VerificationKeyStoreWrapper = Arc<RwLock<VerificationKeyStore>>;

///  StakeStoreWrapper wraps a StakeStore
pub type StakeStoreWrapper = Arc<RwLock<StakeStore>>;

///  SingleSignatureStoreWrapper wraps a SingleSignatureStore
pub type SingleSignatureStoreWrapper = Arc<RwLock<SingleSignatureStore>>;

///  ChainObserverWrapper wraps a ChainObserver
pub type ChainObserverWrapper = Arc<RwLock<dyn ChainObserver>>;

/// BeaconProviderWrapper wraps a BeaconProvider
pub type BeaconProviderWrapper = Arc<RwLock<dyn BeaconProvider>>;

/// BeaconProviderWrapper wraps a BeaconProvider
pub type ImmutableFileObserverWrapper = Arc<RwLock<dyn ImmutableFileObserver>>;

/// DigesterWrapper wraps a Digester
pub type DigesterWrapper = Arc<dyn Digester>;

// DigesterWrapper wraps a Digester
pub type SnapshotterWrapper = Arc<dyn Snapshotter>;

/// DependencyManager handles the dependencies
pub struct DependencyManager {
    pub config: Config,
    pub snapshot_store: Option<SnapshotStoreWrapper>,
    pub snapshot_uploader: Option<SnapshotUploaderWrapper>,
    pub multi_signer: Option<MultiSignerWrapper>,
    pub beacon_store: Option<BeaconStoreWrapper>,
    pub certificate_pending_store: Option<CertificatePendingStoreWrapper>,
    pub certificate_store: Option<CertificateStoreWrapper>,
    pub verification_key_store: Option<VerificationKeyStoreWrapper>,
    pub stake_store: Option<StakeStoreWrapper>,
    pub single_signature_store: Option<SingleSignatureStoreWrapper>,
    pub chain_observer: Option<ChainObserverWrapper>,
    pub beacon_provider: Option<BeaconProviderWrapper>,
    pub immutable_file_observer: Option<ImmutableFileObserverWrapper>,
    pub digester: Option<DigesterWrapper>,
    pub snapshotter: Option<SnapshotterWrapper>,
}

impl DependencyManager {
    /// DependencyManager factory
    pub fn new(config: Config) -> Self {
        Self {
            config,
            snapshot_store: None,
            snapshot_uploader: None,
            multi_signer: None,
            beacon_store: None,
            certificate_pending_store: None,
            certificate_store: None,
            verification_key_store: None,
            stake_store: None,
            single_signature_store: None,
            chain_observer: None,
            beacon_provider: None,
            immutable_file_observer: None,
            digester: None,
            snapshotter: None,
        }
    }

    /// With Snapshotter
    pub fn with_snapshotter(&mut self, snapshotter: SnapshotterWrapper) -> &mut Self {
        self.snapshotter = Some(snapshotter);
        self
    }

    /// With Digester
    pub fn with_digester(&mut self, digester: DigesterWrapper) -> &mut Self {
        self.digester = Some(digester);
        self
    }

    /// With BeaconProvider middleware
    pub fn with_beacon_provider(&mut self, beacon_provider: BeaconProviderWrapper) -> &mut Self {
        self.beacon_provider = Some(beacon_provider);
        self
    }

    /// With SnapshotStore middleware
    pub fn with_snapshot_store(&mut self, snapshot_store: SnapshotStoreWrapper) -> &mut Self {
        self.snapshot_store = Some(snapshot_store);
        self
    }

    /// With SnapshotUploader middleware
    pub fn with_snapshot_uploader(
        &mut self,
        snapshot_uploader: SnapshotUploaderWrapper,
    ) -> &mut Self {
        self.snapshot_uploader = Some(snapshot_uploader);
        self
    }

    /// With MultiSigner middleware
    pub fn with_multi_signer(&mut self, multi_signer: MultiSignerWrapper) -> &mut Self {
        self.multi_signer = Some(multi_signer);
        self
    }

    /// With BeaconStore middleware
    pub fn with_beacon_store(&mut self, beacon_store: BeaconStoreWrapper) -> &mut Self {
        self.beacon_store = Some(beacon_store);
        self
    }

    /// With CertificatePendingStore middleware
    pub fn with_certificate_pending_store(
        &mut self,
        certificate_pending_store: CertificatePendingStoreWrapper,
    ) -> &mut Self {
        self.certificate_pending_store = Some(certificate_pending_store);
        self
    }

    /// With CertificateStore middleware
    pub fn with_certificate_store(
        &mut self,
        certificate_store: CertificateStoreWrapper,
    ) -> &mut Self {
        self.certificate_store = Some(certificate_store);
        self
    }

    /// With VerificationKeyStore middleware
    pub fn with_verification_key_store(
        &mut self,
        verification_key_store: VerificationKeyStoreWrapper,
    ) -> &mut Self {
        self.verification_key_store = Some(verification_key_store);
        self
    }

    /// With StakeStore middleware
    pub fn with_stake_store(&mut self, stake_store: StakeStoreWrapper) -> &mut Self {
        self.stake_store = Some(stake_store);
        self
    }

    /// With SingleSignatureStore middleware
    pub fn with_single_signature_store(
        &mut self,
        single_signature_store: SingleSignatureStoreWrapper,
    ) -> &mut Self {
        self.single_signature_store = Some(single_signature_store);
        self
    }

    /// With ChainObserver middleware
    pub fn with_chain_observer(&mut self, chain_observer: ChainObserverWrapper) -> &mut Self {
        self.chain_observer = Some(chain_observer);
        self
    }

    pub fn with_immutable_file_observer(
        &mut self,
        immutable_file_observer: ImmutableFileObserverWrapper,
    ) -> &mut Self {
        self.immutable_file_observer = Some(immutable_file_observer);
        self
    }

    #[cfg(test)]
    pub fn fake() -> DependencyManager {
        use std::path::PathBuf;

        let config = Config {
            cardano_node_socket_path: PathBuf::new().join("/tmp/cardano.sock"),
            cardano_cli_path: PathBuf::new().join("cardano-cli"),
            network_magic: Some(42),
            network: "testnet".to_string(),
            run_interval: 1000,
            url_snapshot_manifest: "https://storage.googleapis.com/cardano-testnet/snapshots.json"
                .to_string(),
            snapshot_store_type: SnapshotStoreType::Local,
            snapshot_uploader_type: SnapshotUploaderType::Local,
            server_url: "http://0.0.0.0:8080".to_string(),
            db_directory: Default::default(),
            snapshot_directory: Default::default(),
            snapshot_store_directory: std::env::temp_dir().join("mithril_test_snapshots_db"),
            pending_certificate_store_directory: std::env::temp_dir()
                .join("mithril_test_pending_cert_db"),
            certificate_store_directory: std::env::temp_dir().join("mithril_test_cert_db"),
            verification_key_store_directory: std::env::temp_dir()
                .join("mithril_test_verification_key_db"),
            stake_store_directory: std::env::temp_dir().join("mithril_test_stake_db"),
            single_signature_store_directory: std::env::temp_dir()
                .join("mithril_test_single_signature_db"),
        };
        DependencyManager::new(config)
    }
}

#[cfg(test)]
pub mod tests {
    use std::{path::PathBuf, sync::Arc};

    use crate::{
        beacon_provider::DumbImmutableFileObserver, AggregatorConfig, BeaconProviderImpl,
        CertificatePendingStore, CertificateStore, Config, DependencyManager, DumbSnapshotUploader,
        MemoryBeaconStore, MultiSigner, MultiSignerImpl, SingleSignatureStore, SnapshotStoreType,
        SnapshotUploaderType, VerificationKeyStore,
    };
    use mithril_common::{
        chain_observer::FakeObserver,
        fake_data,
        store::{adapter::MemoryAdapter, StakeStore},
        CardanoNetwork,
    };
    use tokio::sync::RwLock;

    pub async fn initialize_dependencies() -> (Arc<DependencyManager>, AggregatorConfig) {
        let config: Config = Config {
            cardano_cli_path: PathBuf::new(),
            cardano_node_socket_path: PathBuf::new(),
            network_magic: Some(42),
            network: "whatever".to_string(),
            url_snapshot_manifest: "https://storage.googleapis.com/cardano-testnet/snapshots.json"
                .to_string(),
            snapshot_store_type: SnapshotStoreType::Local,
            snapshot_uploader_type: SnapshotUploaderType::Local,
            server_url: "http://0.0.0.0:8000".to_string(),
            run_interval: 5000,
            db_directory: PathBuf::new(),
            snapshot_directory: PathBuf::new(),
            snapshot_store_directory: PathBuf::new(),
            pending_certificate_store_directory: PathBuf::new(),
            certificate_store_directory: PathBuf::new(),
            verification_key_store_directory: PathBuf::new(),
            stake_store_directory: PathBuf::new(),
            single_signature_store_directory: PathBuf::new(),
        };
        let certificate_pending_store = Arc::new(CertificatePendingStore::new(Box::new(
            MemoryAdapter::new(None).unwrap(),
        )));
        let certificate_store = Arc::new(RwLock::new(CertificateStore::new(Box::new(
            MemoryAdapter::new(None).unwrap(),
        ))));
        let verification_key_store = Arc::new(RwLock::new(VerificationKeyStore::new(Box::new(
            MemoryAdapter::new(None).unwrap(),
        ))));
        let stake_store = Arc::new(RwLock::new(StakeStore::new(Box::new(
            MemoryAdapter::new(None).unwrap(),
        ))));
        let single_signature_store = Arc::new(RwLock::new(SingleSignatureStore::new(Box::new(
            MemoryAdapter::new(None).unwrap(),
        ))));
        let beacon_store = Arc::new(MemoryBeaconStore::new());
        let multi_signer = async {
            let protocol_parameters = fake_data::protocol_parameters();
            let mut multi_signer = MultiSignerImpl::new(
                beacon_store.clone(),
                verification_key_store.clone(),
                stake_store.clone(),
                single_signature_store.clone(),
            );
            multi_signer
                .update_protocol_parameters(&protocol_parameters.into())
                .await
                .expect("fake update protocol parameters failed");

            multi_signer
        };
        let multi_signer = Arc::new(RwLock::new(multi_signer.await));
        let immutable_file_observer = Arc::new(RwLock::new(DumbImmutableFileObserver::default()));
        let chain_observer = Arc::new(RwLock::new(FakeObserver::default()));
        let beacon_provider = Arc::new(RwLock::new(BeaconProviderImpl::new(
            chain_observer.clone(),
            immutable_file_observer.clone(),
            mithril_common::CardanoNetwork::TestNet(42),
        )));
        let snapshot_uploader = Arc::new(DumbSnapshotUploader::new());
        let mut dependency_manager = DependencyManager::new(config.clone());
        dependency_manager
            //.with_snapshot_store(snapshot_store.clone())
            //.with_snapshot_uploader(snapshot_uploader.clone())
            .with_multi_signer(multi_signer)
            .with_beacon_store(beacon_store.clone())
            .with_certificate_pending_store(certificate_pending_store.clone())
            .with_certificate_store(certificate_store.clone())
            .with_verification_key_store(verification_key_store.clone())
            .with_stake_store(stake_store.clone())
            .with_single_signature_store(single_signature_store.clone())
            .with_chain_observer(chain_observer)
            .with_beacon_provider(beacon_provider)
            .with_immutable_file_observer(immutable_file_observer)
            .with_snapshot_uploader(snapshot_uploader);

        let dependency_manager = Arc::new(dependency_manager);
        let config = AggregatorConfig::new(
            dependency_manager.config.run_interval,
            CardanoNetwork::TestNet(42),
            dependency_manager.config.db_directory.as_path(),
        );

        (dependency_manager, config)
    }
}

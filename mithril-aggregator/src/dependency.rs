use mithril_common::digesters::Digester;
use std::sync::Arc;
use tokio::sync::RwLock;

use mithril_common::chain_observer::ChainObserver;
use mithril_common::store::stake_store::StakeStore;

use super::entities::*;
use super::multi_signer::MultiSigner;
use super::snapshot_stores::SnapshotStore;
use crate::beacon_provider::ImmutableFileObserver;
use crate::beacon_store::BeaconStore;
use crate::snapshot_uploaders::SnapshotUploader;
use crate::{
    BeaconProvider, CertificatePendingStore, CertificateStore, SingleSignatureStore,
    VerificationKeyStore,
};

/// BeaconStoreWrapper wraps a BeaconStore
pub type BeaconStoreWrapper = Arc<RwLock<dyn BeaconStore>>;

///  SnapshotStoreWrapper wraps a SnapshotStore
pub type SnapshotStoreWrapper = Arc<RwLock<dyn SnapshotStore>>;

///  SnapshotUploaderWrapper wraps a SnapshotUploader
pub type SnapshotUploaderWrapper = Arc<RwLock<dyn SnapshotUploader>>;

/// MultiSignerWrapper wraps a MultiSigner
pub type MultiSignerWrapper = Arc<RwLock<dyn MultiSigner>>;

/// CertificatePendingStoreWrapper wraps a CertificatePendingStore
pub type CertificatePendingStoreWrapper = Arc<RwLock<CertificatePendingStore>>;

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

pub type DigesterWrapper = Arc<dyn Digester>;
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
        }
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

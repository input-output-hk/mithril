use mithril_common::digesters::{ImmutableDigester, ImmutableFileObserver};
use mithril_common::BeaconProvider;
use std::sync::Arc;
use tokio::sync::RwLock;

use mithril_common::chain_observer::ChainObserver;
use mithril_common::store::StakeStore;

use crate::entities::*;
use crate::multi_signer::MultiSigner;
use crate::snapshot_stores::SnapshotStore;
use crate::snapshot_uploaders::SnapshotUploader;
use crate::{
    CertificatePendingStore, CertificateStore, SingleSignatureStore, Snapshotter,
    VerificationKeyStore,
};

///  SnapshotStoreWrapper wraps a SnapshotStore
pub type SnapshotStoreWrapper = Arc<dyn SnapshotStore>;

///  SnapshotUploaderWrapper wraps a SnapshotUploader
pub type SnapshotUploaderWrapper = Arc<dyn SnapshotUploader>;

/// MultiSignerWrapper wraps a MultiSigner
pub type MultiSignerWrapper = Arc<RwLock<dyn MultiSigner>>;

/// CertificatePendingStoreWrapper wraps a CertificatePendingStore
pub type CertificatePendingStoreWrapper = Arc<CertificatePendingStore>;

///  CertificateStoreWrapper wraps a CertificateStore
pub type CertificateStoreWrapper = Arc<CertificateStore>;

///  VerificationKeyStoreWrapper wraps a VerificationKeyStore
pub type VerificationKeyStoreWrapper = Arc<VerificationKeyStore>;

///  StakeStoreWrapper wraps a StakeStore
pub type StakeStoreWrapper = Arc<StakeStore>;

///  SingleSignatureStoreWrapper wraps a SingleSignatureStore
pub type SingleSignatureStoreWrapper = Arc<SingleSignatureStore>;

///  ChainObserverWrapper wraps a ChainObserver
pub type ChainObserverWrapper = Arc<dyn ChainObserver>;

/// BeaconProviderWrapper wraps a BeaconProvider
pub type BeaconProviderWrapper = Arc<dyn BeaconProvider>;

/// BeaconProviderWrapper wraps a BeaconProvider
pub type ImmutableFileObserverWrapper = Arc<dyn ImmutableFileObserver>;

/// DigesterWrapper wraps a Digester
pub type DigesterWrapper = Arc<dyn ImmutableDigester>;

// DigesterWrapper wraps a Digester
pub type SnapshotterWrapper = Arc<dyn Snapshotter>;

/// DependencyManager handles the dependencies
pub struct DependencyManager {
    pub config: Config,
    pub snapshot_store: SnapshotStoreWrapper,
    pub snapshot_uploader: SnapshotUploaderWrapper,
    pub multi_signer: MultiSignerWrapper,
    pub certificate_pending_store: CertificatePendingStoreWrapper,
    pub certificate_store: CertificateStoreWrapper,
    pub verification_key_store: VerificationKeyStoreWrapper,
    pub stake_store: StakeStoreWrapper,
    pub single_signature_store: SingleSignatureStoreWrapper,
    pub chain_observer: ChainObserverWrapper,
    pub beacon_provider: BeaconProviderWrapper,
    pub immutable_file_observer: ImmutableFileObserverWrapper,
    pub digester: DigesterWrapper,
    pub snapshotter: SnapshotterWrapper,
}

#[cfg(test)]
pub mod tests {
    use crate::{
        AggregatorConfig, CertificatePendingStore, CertificateStore, Config, DependencyManager,
        DumbSnapshotUploader, DumbSnapshotter, LocalSnapshotStore, MultiSigner, MultiSignerImpl,
        SingleSignatureStore, SnapshotStoreType, SnapshotUploaderType, VerificationKeyStore,
    };
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
        let verification_key_store = Arc::new(VerificationKeyStore::new(Box::new(
            MemoryAdapter::new(None).unwrap(),
        )));
        let stake_store = Arc::new(StakeStore::new(Box::new(MemoryAdapter::new(None).unwrap())));
        let single_signature_store = Arc::new(SingleSignatureStore::new(Box::new(
            MemoryAdapter::new(None).unwrap(),
        )));
        let multi_signer = async {
            let protocol_parameters = fake_data::protocol_parameters();
            let mut multi_signer = MultiSignerImpl::new(
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
        let immutable_file_observer = Arc::new(DumbImmutableFileObserver::default());
        let chain_observer = Arc::new(FakeObserver::default());
        let beacon_provider = Arc::new(BeaconProviderImpl::new(
            chain_observer.clone(),
            immutable_file_observer.clone(),
            CardanoNetwork::TestNet(42),
        ));
        let dependency_manager = DependencyManager {
            config,
            snapshot_store: snapshot_store.clone(),
            snapshot_uploader: snapshot_uploader.clone(),
            multi_signer,
            certificate_pending_store: certificate_pending_store.clone(),
            certificate_store: certificate_store.clone(),
            verification_key_store: verification_key_store.clone(),
            stake_store: stake_store.clone(),
            single_signature_store: single_signature_store.clone(),
            chain_observer,
            beacon_provider,
            immutable_file_observer,
            digester: Arc::new(DumbImmutableDigester::new("digest", true)),
            snapshotter: Arc::new(DumbSnapshotter::new()),
        };

        let config = AggregatorConfig::new(
            dependency_manager.config.run_interval,
            CardanoNetwork::TestNet(42),
            dependency_manager.config.db_directory.as_path(),
        );

        (dependency_manager, config)
    }
}

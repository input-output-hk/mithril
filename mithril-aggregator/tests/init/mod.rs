use std::{path::PathBuf, sync::Arc};

use mithril_aggregator::{
    AggregatorConfig, BeaconProviderImpl, CertificatePendingStore, CertificateStore, Config,
    DependencyManager, DumbImmutableFileObserver, DumbSnapshotUploader, DumbSnapshotter,
    LocalSnapshotStore, MemoryBeaconStore, MultiSigner, MultiSignerImpl, SingleSignatureStore,
    SnapshotStoreType, SnapshotUploaderType, VerificationKeyStore,
};
use mithril_common::{
    chain_observer::FakeObserver,
    crypto_helper::tests_setup::setup_protocol_parameters,
    digesters::DumbDigester,
    store::{adapter::MemoryAdapter, StakeStore},
    CardanoNetwork,
};
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
    let certificate_pending_store = Arc::new(CertificatePendingStore::new(Box::new(
        MemoryAdapter::new(None).unwrap(),
    )));
    let certificate_store = Arc::new(CertificateStore::new(Box::new(
        MemoryAdapter::new(None).unwrap(),
    )));
    let verification_key_store = Arc::new(VerificationKeyStore::new(Box::new(
        MemoryAdapter::new(None).unwrap(),
    )));
    let stake_store = Arc::new(RwLock::new(StakeStore::new(Box::new(
        MemoryAdapter::new(None).unwrap(),
    ))));
    let single_signature_store = Arc::new(SingleSignatureStore::new(Box::new(
        MemoryAdapter::new(None).unwrap(),
    )));
    let beacon_store = Arc::new(MemoryBeaconStore::new());
    let multi_signer = async {
        let protocol_parameters = setup_protocol_parameters();
        let mut multi_signer = MultiSignerImpl::new(
            beacon_store.clone(),
            verification_key_store.clone(),
            stake_store.clone(),
            single_signature_store.clone(),
        );
        multi_signer
            .update_protocol_parameters(&protocol_parameters)
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
        mithril_common::CardanoNetwork::TestNet(42),
    ));
    let digester = Arc::new(DumbDigester::default());
    let snapshotter = Arc::new(DumbSnapshotter::new());
    let snapshot_uploader = Arc::new(DumbSnapshotUploader::new());
    let snapshot_store = Arc::new(LocalSnapshotStore::new(
        Box::new(MemoryAdapter::new(None).expect("memory adapter init should not fail")),
        5,
    ));
    let mut dependency_manager = DependencyManager::new(config.clone());
    dependency_manager
        .with_snapshot_store(snapshot_store)
        .with_snapshot_uploader(snapshot_uploader)
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
        .with_digester(digester)
        .with_snapshotter(snapshotter);

    let config = AggregatorConfig::new(
        dependency_manager.config.run_interval,
        CardanoNetwork::TestNet(42),
        dependency_manager.config.db_directory.as_path(),
    );

    (dependency_manager, config)
}

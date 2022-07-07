use std::{path::PathBuf, sync::Arc};

use mithril_aggregator::{
    beacon_provider::DumbImmutableFileObserver,
    runtime::{AggregatorRunner, AggregatorRunnerTrait},
    AggregatorConfig, BeaconProviderImpl, CertificatePendingStore, CertificateStore, Config,
    DependencyManager, MemoryBeaconStore, MultiSignerImpl, SingleSignatureStore, SnapshotStoreType,
    SnapshotUploaderType, VerificationKeyStore,
};
use mithril_common::{
    chain_observer::FakeObserver,
    entities::Beacon,
    store::{adapter::MemoryAdapter, stake_store::StakeStore},
    CardanoNetwork,
};
use tokio::sync::RwLock;

fn initialize_dependencies() -> (Arc<DependencyManager>, AggregatorConfig) {
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
    let certificate_pending_store = Arc::new(RwLock::new(CertificatePendingStore::new(Box::new(
        MemoryAdapter::new(None).unwrap(),
    ))));
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
    let beacon_store = Arc::new(RwLock::new(MemoryBeaconStore::new()));
    let multi_signer = Arc::new(RwLock::new(MultiSignerImpl::new(
        beacon_store.clone(),
        verification_key_store.clone(),
        stake_store.clone(),
        single_signature_store.clone(),
    )));
    let immutable_file_observer = Arc::new(RwLock::new(DumbImmutableFileObserver::new()));
    let chain_observer = Arc::new(RwLock::new(FakeObserver::new()));
    let beacon_provider = Arc::new(RwLock::new(BeaconProviderImpl::new(
        chain_observer.clone(),
        immutable_file_observer.clone(),
        mithril_common::CardanoNetwork::TestNet(42),
    )));
    let mut dependency_manager = DependencyManager::new(config.clone());
    dependency_manager
        //.with_snapshot_store(snapshot_store.clone())
        //.with_snapshot_uploader(snapshot_uploader.clone())
        .with_multi_signer(multi_signer.clone())
        .with_beacon_store(beacon_store.clone())
        .with_certificate_pending_store(certificate_pending_store.clone())
        .with_certificate_store(certificate_store.clone())
        .with_verification_key_store(verification_key_store.clone())
        .with_stake_store(stake_store.clone())
        .with_single_signature_store(single_signature_store.clone())
        .with_chain_observer(chain_observer.clone())
        .with_beacon_provider(beacon_provider.clone())
        .with_immutable_file_observer(immutable_file_observer);

    let dependency_manager = Arc::new(dependency_manager);
    let config = AggregatorConfig::new(
        dependency_manager.config.run_interval,
        CardanoNetwork::TestNet(42),
        dependency_manager.config.db_directory.as_path(),
        dependency_manager
            .config
            .snapshot_directory
            .as_path()
            .as_ref(),
        dependency_manager.clone(),
    );

    (dependency_manager, config)
}

#[tokio::test]
async fn test_is_new_beacon() {
    let (_, config) = initialize_dependencies();
    let runner = AggregatorRunner::new(config);

    // No beacon means the current beacon is newer
    let res = runner.is_new_beacon(None).await;
    assert!(res.unwrap().is_some());

    // old beacon means the current beacon is newer
    let beacon = Beacon {
        network: "testnet".to_string(),
        epoch: 0,
        immutable_file_number: 0,
    };
    let res = runner.is_new_beacon(Some(beacon)).await;
    assert!(res.unwrap().is_some());

    // new beacon mens the current beacon is not new
    let beacon = Beacon {
        network: "whatever".to_string(),
        epoch: 9206230,
        immutable_file_number: 10000,
    };
    let res = runner.is_new_beacon(Some(beacon)).await;
    assert!(res.unwrap().is_none());
}

use std::{path::PathBuf, sync::Arc};

use mithril_aggregator::{
    beacon_provider::DumbImmutableFileObserver,
    runtime::{AggregatorRunner, AggregatorRunnerTrait},
    AggregatorConfig, BeaconProviderImpl, CertificatePendingStore, CertificateStore, Config,
    DependencyManager, MemoryBeaconStore, MultiSigner, MultiSignerImpl, SingleSignatureStore,
    SnapshotStoreType, SnapshotUploaderType, VerificationKeyStore,
};
use mithril_common::{
    chain_observer::FakeObserver,
    digesters::DigesterResult,
    entities::Beacon,
    fake_data,
    store::{
        adapter::MemoryAdapter,
        stake_store::{StakeStore, StakeStorer},
    },
    CardanoNetwork,
};
use tokio::sync::RwLock;

async fn initialize_dependencies() -> (Arc<DependencyManager>, AggregatorConfig) {
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
    let (_, config) = initialize_dependencies().await;
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

#[tokio::test]
async fn test_update_beacon() {
    let (deps, config) = initialize_dependencies().await;
    let runner = AggregatorRunner::new(config);
    let beacon = runner.is_new_beacon(None).await.unwrap().unwrap();
    let res = runner.update_beacon(&beacon).await;

    assert!(res.is_ok());
    let stored_beacon = deps
        .beacon_store
        .as_ref()
        .unwrap()
        .read()
        .await
        .get_current_beacon()
        .await
        .unwrap()
        .unwrap();

    assert_eq!(beacon, stored_beacon);
}
#[tokio::test]
async fn test_update_stake_distribution() {
    let (deps, config) = initialize_dependencies().await;
    let runner = AggregatorRunner::new(config);
    let beacon = runner.is_new_beacon(None).await.unwrap().unwrap();
    let res = runner.update_stake_distribution(&beacon).await;

    assert!(res.is_ok());

    let current_stake_distribution = deps
        .chain_observer
        .as_ref()
        .unwrap()
        .read()
        .await
        .get_current_stake_distribution()
        .await
        .unwrap()
        .unwrap();

    let saved_stake_distribution = deps
        .stake_store
        .as_ref()
        .unwrap()
        .read()
        .await
        .get_stakes(beacon.epoch)
        .await
        .unwrap()
        .unwrap();

    assert_eq!(
        current_stake_distribution.len(),
        saved_stake_distribution.len()
    );

    for (party_id, stake) in current_stake_distribution.iter() {
        let signer_with_stake = saved_stake_distribution.get(party_id).unwrap();
        assert_eq!(stake, &signer_with_stake.stake);
    }
}

#[tokio::test]
async fn test_create_new_pending_certificate_from_multisigner() {
    let (_deps, config) = initialize_dependencies().await;
    let runner = AggregatorRunner::new(config);
    let beacon = runner.is_new_beacon(None).await.unwrap().unwrap();
    runner.update_beacon(&beacon).await.unwrap();
    let certificate = runner
        .create_new_pending_certificate_from_multisigner(beacon.clone())
        .await
        .unwrap();

    assert_eq!(beacon, certificate.beacon);
}

#[tokio::test]
async fn test_update_message_in_multisigner() {
    let (deps, config) = initialize_dependencies().await;
    let runner = AggregatorRunner::new(config);
    let beacon = runner.is_new_beacon(None).await.unwrap().unwrap();
    let digester_result = DigesterResult {
        digest: "1+2+3+4=10".to_string(),
        last_immutable_file_number: beacon.immutable_file_number,
    };
    runner.update_beacon(&beacon).await.unwrap();

    assert!(runner
        .update_message_in_multisigner(digester_result)
        .await
        .is_ok());
    let message = deps
        .multi_signer
        .as_ref()
        .unwrap()
        .read()
        .await
        .get_current_message()
        .await
        .unwrap();

    assert_eq!("1+2+3+4=10".to_string(), message);
}

#[tokio::test]
async fn test_save_pending_certificate() {
    let (deps, config) = initialize_dependencies().await;
    let runner = AggregatorRunner::new(config);
    let beacon = runner.is_new_beacon(None).await.unwrap().unwrap();
    runner.update_beacon(&beacon).await.unwrap();
    let pending_certificate = runner
        .create_new_pending_certificate_from_multisigner(beacon.clone())
        .await
        .unwrap();
    let res = runner
        .save_pending_certificate(pending_certificate.clone())
        .await;
    assert!(res.is_ok());

    let saved_cert = deps
        .certificate_pending_store
        .as_ref()
        .unwrap()
        .read()
        .await
        .get()
        .await
        .unwrap()
        .unwrap();

    assert_eq!(pending_certificate, saved_cert);
}

#[tokio::test]
async fn test_drop_pending_certificate() {
    let (deps, config) = initialize_dependencies().await;
    let runner = AggregatorRunner::new(config);
    let beacon = runner.is_new_beacon(None).await.unwrap().unwrap();
    runner.update_beacon(&beacon).await.unwrap();
    let pending_certificate = runner
        .create_new_pending_certificate_from_multisigner(beacon.clone())
        .await
        .unwrap();
    runner
        .save_pending_certificate(pending_certificate.clone())
        .await
        .unwrap();
    let cert = runner.drop_pending_certificate().await.unwrap();
    assert_eq!(pending_certificate, cert);
    let maybe_saved_cert = deps
        .certificate_pending_store
        .as_ref()
        .unwrap()
        .read()
        .await
        .get()
        .await
        .unwrap();
    assert!(maybe_saved_cert.is_none());
}

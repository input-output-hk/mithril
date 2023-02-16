use mithril_aggregator::event_store::{EventMessage, TransmitterService};
use mithril_aggregator::{
    AggregatorConfig, CertificatePendingStore, CertificateStore, Configuration, DependencyManager,
    DumbSnapshotUploader, DumbSnapshotter, LocalSnapshotStore, MithrilSignerRegisterer,
    MultiSignerImpl, ProtocolParametersStore, SingleSignatureStore, SnapshotStoreType,
    SnapshotUploaderType, VerificationKeyStore,
};
use mithril_common::certificate_chain::MithrilCertificateVerifier;
use mithril_common::chain_observer::FakeObserver;
use mithril_common::crypto_helper::{key_encode_hex, ProtocolGenesisSigner};
use mithril_common::digesters::{DumbImmutableDigester, DumbImmutableFileObserver};
use mithril_common::entities::ProtocolParameters;
use mithril_common::era::{adapters::EraReaderAdapterType, EraReader};
use mithril_common::era::{adapters::EraReaderBootstrapAdapter, EraChecker};
use mithril_common::store::adapter::MemoryAdapter;
use mithril_common::store::StakeStore;
use mithril_common::{BeaconProvider, BeaconProviderImpl, CardanoNetwork};
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::mpsc::UnboundedReceiver;
use tokio::sync::RwLock;

pub async fn initialize_dependencies(
    default_protocol_parameters: ProtocolParameters,
    snapshot_uploader: Arc<DumbSnapshotUploader>,
    chain_observer: Arc<FakeObserver>,
    immutable_file_observer: Arc<DumbImmutableFileObserver>,
    digester: Arc<DumbImmutableDigester>,
    snapshotter: Arc<DumbSnapshotter>,
    genesis_signer: Arc<ProtocolGenesisSigner>,
) -> (
    Arc<DependencyManager>,
    AggregatorConfig,
    UnboundedReceiver<EventMessage>,
) {
    let genesis_verifier = Arc::new(genesis_signer.create_genesis_verifier());
    let genesis_verification_key = genesis_verifier.to_verification_key();
    let config: Configuration = Configuration {
        cardano_cli_path: PathBuf::new(),
        cardano_node_socket_path: PathBuf::new(),
        network_magic: Some(42),
        network: "whatever".to_string(),
        protocol_parameters: default_protocol_parameters,
        url_snapshot_manifest: "https://storage.googleapis.com/cardano-testnet/snapshots.json"
            .to_string(),
        snapshot_store_type: SnapshotStoreType::Local,
        snapshot_uploader_type: SnapshotUploaderType::Local,
        snapshot_bucket_name: None,
        server_ip: "0.0.0.0".to_string(),
        server_port: 8000,
        run_interval: 5000,
        db_directory: PathBuf::new(),
        snapshot_directory: PathBuf::new(),
        data_stores_directory: PathBuf::new(),
        genesis_verification_key: key_encode_hex(genesis_verification_key).unwrap(),
        store_retention_limit: None,
        era_reader_adapter_type: EraReaderAdapterType::Bootstrap,
        era_reader_adapter_params: None,
    };
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
    let multi_signer = MultiSignerImpl::new(
        verification_key_store.clone(),
        stake_store.clone(),
        single_signature_store.clone(),
        protocol_parameters_store.clone(),
    );
    let multi_signer = Arc::new(RwLock::new(multi_signer));
    let beacon_provider = Arc::new(BeaconProviderImpl::new(
        chain_observer.clone(),
        immutable_file_observer.clone(),
        CardanoNetwork::TestNet(42),
    ));
    let snapshot_store = Arc::new(LocalSnapshotStore::new(
        Box::new(MemoryAdapter::new(None).expect("memory adapter init should not fail")),
        5,
    ));
    let certificate_verifier = Arc::new(MithrilCertificateVerifier::new(slog_scope::logger()));
    let signer_registerer = Arc::new(MithrilSignerRegisterer::new(
        chain_observer.clone(),
        verification_key_store.clone(),
    ));
    let era_reader = Arc::new(EraReader::new(Box::new(EraReaderBootstrapAdapter)));
    let era_epoch_token = era_reader
        .read_era_epoch_token(beacon_provider.get_current_beacon().await.unwrap().epoch)
        .await
        .unwrap();
    let era_checker = Arc::new(EraChecker::new(
        era_epoch_token.get_current_supported_era().unwrap(),
        era_epoch_token.get_current_epoch(),
    ));
    let (tx, rx) = tokio::sync::mpsc::unbounded_channel();
    let event_transmitter = Arc::new(TransmitterService::new(tx));

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
        digester,
        snapshotter,
        certificate_verifier,
        genesis_verifier,
        signer_registerer: signer_registerer.clone(),
        signer_registration_round_opener: signer_registerer,
        era_checker,
        era_reader,
        event_transmitter,
    };

    let config = AggregatorConfig::new(
        dependency_manager.config.run_interval,
        CardanoNetwork::TestNet(42),
        dependency_manager.config.db_directory.as_path(),
    );

    (Arc::new(dependency_manager), config, rx)
}

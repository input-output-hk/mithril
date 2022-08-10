// I don't know why but all types declared here are marked as dead code by clippy
#![allow(dead_code)]

use slog::Drain;
use std::time::Duration;
use std::{path::PathBuf, sync::Arc};

use mithril_aggregator::{
    AggregatorConfig, AggregatorRunner, AggregatorRuntime, CertificatePendingStore,
    CertificateStore, Config, DependencyManager, DumbSnapshotUploader, DumbSnapshotter,
    LocalSnapshotStore, MultiSigner, MultiSignerImpl, ProtocolParametersStore,
    SingleSignatureStore, SnapshotStoreType, SnapshotUploaderType, VerificationKeyStore,
};
use mithril_common::crypto_helper::{
    key_encode_hex, ProtocolInitializer, ProtocolPartyId, ProtocolSigner,
    ProtocolSignerVerificationKey, ProtocolStake,
};
use mithril_common::digesters::DumbImmutableFileObserver;
use mithril_common::entities::{Certificate, ImmutableFileNumber, SingleSignatures};
use mithril_common::{
    chain_observer::FakeObserver,
    digesters::DumbImmutableDigester,
    fake_data,
    store::{adapter::MemoryAdapter, StakeStore},
    BeaconProviderImpl, CardanoNetwork,
};
use tokio::sync::RwLock;

pub type TestSigner = (
    ProtocolPartyId,
    ProtocolStake,
    ProtocolSignerVerificationKey,
    ProtocolSigner,
    ProtocolInitializer,
);

/// Simple struct to give a more helpful error message when ticking the state machine
pub struct TickCounter {
    tick_no: u64,
}

impl TickCounter {
    pub fn new() -> Self {
        Self { tick_no: 0 }
    }

    pub fn get_message(&mut self) -> String {
        self.tick_no += 1;

        format!(
            "Ticking the state machine should not fail (tick n° {})",
            self.tick_no
        )
    }
}

pub struct RuntimeTester {
    pub snapshot_uploader: Arc<DumbSnapshotUploader>,
    pub chain_observer: Arc<FakeObserver>,
    pub immutable_file_observer: Arc<DumbImmutableFileObserver>,
    pub digester: Arc<DumbImmutableDigester>,
    pub snapshotter: Arc<DumbSnapshotter>,
    pub deps: Arc<DependencyManager>,
    pub runtime: AggregatorRuntime,
    tick_counter: TickCounter,
    logs_guard: slog_scope::GlobalLoggerGuard,
}

impl RuntimeTester {
    pub async fn build() -> Self {
        let snapshot_uploader = Arc::new(DumbSnapshotUploader::new());
        let chain_observer = Arc::new(FakeObserver::default());
        let immutable_file_observer = Arc::new(DumbImmutableFileObserver::default());
        let digester = Arc::new(DumbImmutableDigester::default());
        let snapshotter = Arc::new(DumbSnapshotter::new());
        let (deps, config) = initialize_dependencies(
            snapshot_uploader.clone(),
            chain_observer.clone(),
            immutable_file_observer.clone(),
            digester.clone(),
            snapshotter.clone(),
        )
        .await;
        let runner = Arc::new(AggregatorRunner::new(config.clone(), deps.clone()));
        let runtime =
            AggregatorRuntime::new(Duration::from_millis(config.interval), None, runner.clone())
                .await
                .expect("Instantiating the Runtime should not fail.");

        let decorator = slog_term::PlainDecorator::new(slog_term::TestStdoutWriter);
        let drain = slog_term::CompactFormat::new(decorator).build().fuse();
        let drain = slog_async::Async::new(drain).build().fuse();
        let log = slog_scope::set_global_logger(slog::Logger::root(Arc::new(drain), slog::o!()));

        Self {
            snapshot_uploader,
            chain_observer,
            immutable_file_observer,
            digester,
            snapshotter,
            deps,
            runtime,
            tick_counter: TickCounter::new(),
            logs_guard: log,
        }
    }

    /// cycle the runtime once
    pub async fn cycle(&mut self, expected_state: &str) {
        self.runtime
            .cycle()
            .await
            .unwrap_or_else(|_| panic!("{}", self.tick_counter.get_message()));
        assert_eq!(expected_state, self.runtime.get_state());
    }

    /// Increase the immutable file number of the beacon, returns the new number.
    pub async fn increase_immutable_number(&self) -> Result<ImmutableFileNumber, String> {
        let new_immutable_number = self.immutable_file_observer.increase().await.unwrap();
        assert_eq!(
            new_immutable_number,
            self.deps
                .beacon_provider
                .get_current_beacon()
                .await
                .map_err(|e| format!("Querying the current beacon should not fail: {:?}", e))?
                .immutable_file_number
        );

        Ok(new_immutable_number)
    }

    /// Register the given signers in the multi-signers
    pub async fn register_signers(&self, signers: &[TestSigner]) -> Result<(), String> {
        let mut multisigner = self.deps.multi_signer.write().await;

        for (party_id, _stakes, verification_key, _signer, _initializer) in signers {
            multisigner
                .register_signer(party_id.to_owned(), verification_key)
                .await
                .map_err(|e| format!("Registering a signer should not fail: {:?}", e))?;
        }

        Ok(())
    }

    /// "Send", actually register, the given single signatures in the multi-signers
    pub async fn send_single_signatures(&self, signers: &[TestSigner]) -> Result<(), String> {
        let mut multisigner = self.deps.multi_signer.write().await;
        let message = multisigner
            .get_current_message()
            .await
            .ok_or("There should be a message to be signed.")?;

        for (party_id, _stakes, _verification_key, protocol_signer, _initializer) in signers {
            if let Some(signature) = protocol_signer.sign(message.compute_hash().as_bytes()) {
                let single_signatures = SingleSignatures::new(
                    party_id.to_string(),
                    key_encode_hex(&signature).expect("hex encoding should not fail"),
                    signature.indexes,
                );

                multisigner
                    .register_single_signature(&single_signatures)
                    .await
                    .map_err(|e| {
                        format!(
                            "registering a winning lottery signature should not fail: {:?}",
                            e
                        )
                    })?;
            }
        }

        Ok(())
    }

    /// Get the last 'n' certificates from the certificate store
    pub async fn get_last_certificates(
        &self,
        number_of_cert_to_fetch: usize,
    ) -> Result<Vec<Certificate>, String> {
        self.deps
            .certificate_store
            .get_list(number_of_cert_to_fetch)
            .await
            .map_err(|e| format!("Querying certificate store should not fail {:?}", e))
    }
}

pub async fn initialize_dependencies(
    snapshot_uploader: Arc<DumbSnapshotUploader>,
    chain_observer: Arc<FakeObserver>,
    immutable_file_observer: Arc<DumbImmutableFileObserver>,
    digester: Arc<DumbImmutableDigester>,
    snapshotter: Arc<DumbSnapshotter>,
) -> (Arc<DependencyManager>, AggregatorConfig) {
    let config: Config = Config {
        cardano_cli_path: PathBuf::new(),
        cardano_node_socket_path: PathBuf::new(),
        network_magic: Some(42),
        network: "whatever".to_string(),
        protocol_parameters: fake_data::protocol_parameters(),
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
        protocol_parameters_store_directory: PathBuf::new(),
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
    let stake_store = Arc::new(StakeStore::new(Box::new(MemoryAdapter::new(None).unwrap())));
    let single_signature_store = Arc::new(SingleSignatureStore::new(Box::new(
        MemoryAdapter::new(None).unwrap(),
    )));
    let protocol_parameters_store = Arc::new(ProtocolParametersStore::new(Box::new(
        MemoryAdapter::new(None).unwrap(),
    )));
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
    let dependency_manager = DependencyManager {
        config,
        snapshot_store,
        snapshot_uploader: snapshot_uploader.clone(),
        multi_signer,
        certificate_pending_store: certificate_pending_store.clone(),
        certificate_store: certificate_store.clone(),
        verification_key_store: verification_key_store.clone(),
        stake_store: stake_store.clone(),
        single_signature_store: single_signature_store.clone(),
        protocol_parameters_store: protocol_parameters_store.clone(),
        chain_observer: chain_observer.clone(),
        beacon_provider,
        immutable_file_observer: immutable_file_observer.clone(),
        digester: digester.clone(),
        snapshotter: snapshotter.clone(),
    };

    let config = AggregatorConfig::new(
        dependency_manager.config.run_interval,
        CardanoNetwork::TestNet(42),
        dependency_manager.config.db_directory.as_path(),
    );

    (Arc::new(dependency_manager), config)
}

use std::{collections::HashMap, sync::Arc};

use config::ConfigError;
use mithril_common::{
    api_version::APIVersionProvider,
    certificate_chain::{CertificateVerifier, MithrilCertificateVerifier},
    chain_observer::{CardanoCliChainObserver, CardanoCliRunner, ChainObserver, FakeObserver},
    digesters::{
        CardanoImmutableDigester, DumbImmutableFileObserver, ImmutableDigester,
        ImmutableFileObserver, ImmutableFileSystemObserver,
    },
    entities::{
        Beacon, Certificate, CertificatePending, Epoch, PartyId, ProtocolParameters, Signer,
        SingleSignatures,
    },
    store::{
        adapter::{MemoryAdapter, SQLiteAdapter, StoreAdapter},
        StakeStorer,
    },
    BeaconProvider, BeaconProviderImpl, StdError,
};
use slog::Logger;
use sqlite::Connection;
use tokio::sync::{Mutex, RwLock};

use crate::{
    configuration::{ExecutionEnvironment, LIST_SNAPSHOTS_MAX_ITEMS},
    database::provider::StakePoolStore,
    event_store::{EventMessage, TransmitterService},
    stake_distribution_service::StakeDistributionService,
    tools::GcpFileUploader,
    CertificatePendingStore, CertificateStore, Configuration, DumbSnapshotter, GzipSnapshotter,
    LocalSnapshotStore, LocalSnapshotUploader, MultiSigner, MultiSignerImpl,
    ProtocolParametersStore, RemoteSnapshotUploader, SignerRegisterer,
    SignerRegistrationRoundOpener, SingleSignatureStore, SnapshotStore, SnapshotUploader,
    SnapshotUploaderType, Snapshotter, VerificationKeyStore,
};

type Result<T> = std::result::Result<T, DependenciesBuilderError>;

/// Error that can occure during dependencies initialization process.
pub enum DependenciesBuilderError {
    /// Unrecoverable system initialization failure
    Initialization {
        message: String,
        error: Option<StdError>,
    },

    /// Configuration parameter missing for initialization.
    MissingConfiguration(String),
}

impl From<StdError> for DependenciesBuilderError {
    fn from(value: StdError) -> Self {
        DependenciesBuilderError::Initialization {
            message: "subsystem error".to_string(),
            error: Some(value),
        }
    }
}

impl From<ConfigError> for DependenciesBuilderError {
    fn from(value: ConfigError) -> Self {
        Self::MissingConfiguration(format!("{value}"))
    }
}
/// Dependencies container builder
pub struct DependenciesBuilder {
    /// Configuration parameters
    pub configuration: Configuration,

    /// SQLite database connection
    pub sqlite_connection: Option<Arc<Mutex<Connection>>>,

    /// Stake Store used by the StakeDistributionService
    /// It shall be a private dependency.
    pub stake_store: Option<Arc<dyn StakeStorer>>,

    /// Snapshot store.
    pub snapshot_store: Option<Arc<dyn SnapshotStore>>,

    /// Snapshot uploader service.
    pub snapshot_uploader: Option<Arc<dyn SnapshotUploader>>,

    /// Multisigner service.
    pub multi_signer: Option<Arc<RwLock<dyn MultiSigner>>>,

    /// Certificate pending store.
    pub certificate_pending_store: Option<Arc<CertificatePendingStore>>,

    /// Certificate store.
    pub certificate_store: Option<Arc<CertificateStore>>,

    /// Verification key store.
    pub verification_key_store: Option<Arc<VerificationKeyStore>>,

    /// Signer single signature store.
    pub single_signature_store: Option<Arc<SingleSignatureStore>>,

    /// Protocol parameter store.
    pub protocol_parameters_store: Option<Arc<ProtocolParametersStore>>,

    /// Cardano CLI Runner for the [ChainObserver]
    pub cardano_cli_runner: Option<Box<CardanoCliRunner>>,

    /// Chain observer service.
    pub chain_observer: Option<Arc<dyn ChainObserver>>,

    /// Beacon provider service.
    pub beacon_provider: Option<Arc<dyn BeaconProvider>>,

    /// Immutable file digester service.
    pub immutable_digester: Option<Arc<dyn ImmutableDigester>>,

    /// Immutable file observer service.
    pub immutable_file_observer: Option<Arc<dyn ImmutableFileObserver>>,

    /// Digester service.
    pub digester: Option<Arc<dyn ImmutableDigester>>,

    /// Snapshotter service.
    pub snapshotter: Option<Arc<dyn Snapshotter>>,

    /// Certificate verifier service.
    pub certificate_verifier: Option<Arc<dyn CertificateVerifier>>,

    /// Genesis signature verifier service.
    pub genesis_verifier: Option<Arc<mithril_common::crypto_helper::ProtocolGenesisVerifier>>,

    /// Signer registerer service
    pub signer_registerer: Option<Arc<dyn SignerRegisterer>>,

    /// Signer registration round opener service
    pub signer_registration_round_opener: Option<Arc<dyn SignerRegistrationRoundOpener>>,

    /// Era checker service
    pub era_checker: Option<Arc<mithril_common::era::EraChecker>>,

    /// Era reader service
    pub era_reader: Option<Arc<mithril_common::era::EraReader>>,

    /// Event Transmitter Service
    pub event_transmitter: Option<Arc<TransmitterService<EventMessage>>>,

    /// API Version provider
    pub api_version_provider: Option<Arc<APIVersionProvider>>,

    /// Stake Distribution Service
    pub stake_distribution_service: Option<Arc<dyn StakeDistributionService>>,
}

impl DependenciesBuilder {
    /// Create a new clean dependency builder
    pub fn new(configuration: Configuration) -> Self {
        Self {
            configuration,
            sqlite_connection: None,
            stake_store: None,
            snapshot_store: None,
            snapshot_uploader: None,
            multi_signer: None,
            certificate_pending_store: None,
            certificate_store: None,
            verification_key_store: None,
            single_signature_store: None,
            protocol_parameters_store: None,
            cardano_cli_runner: None,
            chain_observer: None,
            beacon_provider: None,
            immutable_digester: None,
            immutable_file_observer: None,
            digester: None,
            snapshotter: None,
            certificate_verifier: None,
            genesis_verifier: None,
            signer_registerer: None,
            signer_registration_round_opener: None,
            era_checker: None,
            era_reader: None,
            event_transmitter: None,
            api_version_provider: None,
            stake_distribution_service: None,
        }
    }

    fn build_sqlite_connection(&self) -> Result<Arc<Mutex<Connection>>> {
        let connection = match self.configuration.environment {
            ExecutionEnvironment::Production => {
                Connection::open(self.configuration.get_sqlite_file())
            }
            _ => Connection::open(":memory:"),
        };

        connection
            .map(|conn| Arc::new(Mutex::new(conn)))
            .map_err(|e| DependenciesBuilderError::Initialization {
                message: "Could not initialize SQLite driver.".to_string(),
                error: Some(Box::new(e)),
            })
    }

    /// Get SQLite connection
    pub fn get_sqlite_connection(&mut self) -> Result<Arc<Mutex<Connection>>> {
        if self.sqlite_connection.is_none() {
            self.sqlite_connection = Some(self.build_sqlite_connection()?);
        }

        Ok(self.sqlite_connection.as_ref().cloned().unwrap())
    }

    fn build_stake_store(&mut self) -> Result<Arc<dyn StakeStorer>> {
        let stake_pool_store = Arc::new(StakePoolStore::new(self.get_sqlite_connection()?));

        Ok(stake_pool_store)
    }

    pub fn get_stake_store(&mut self) -> Result<Arc<dyn StakeStorer>> {
        if self.stake_store.is_none() {
            self.stake_store = Some(self.build_stake_store()?);
        }

        Ok(self.stake_store.as_ref().cloned().unwrap())
    }

    fn build_snapshot_store(&mut self) -> Result<Arc<dyn SnapshotStore>> {
        let adapter: Box<
            dyn StoreAdapter<Key = String, Record = mithril_common::entities::Snapshot>,
        > = match self.configuration.environment {
            ExecutionEnvironment::Production => {
                let adapter = SQLiteAdapter::new("snapshot", self.get_sqlite_connection()?)
                    .map_err(|e| DependenciesBuilderError::Initialization {
                        message: "Cannot create SQLite adapter for Snapshot Store.".to_string(),
                        error: Some(e.into()),
                    })?;

                Box::new(adapter)
            }
            _ => {
                let adapter = MemoryAdapter::new(None).map_err(|e| {
                    DependenciesBuilderError::Initialization {
                        message: "Cannot create Memory adapter for Snapshot Store.".to_string(),
                        error: Some(e.into()),
                    }
                })?;
                Box::new(adapter)
            }
        };

        Ok(Arc::new(LocalSnapshotStore::new(
            adapter,
            LIST_SNAPSHOTS_MAX_ITEMS,
        )))
    }

    pub fn get_snapshot_store(&mut self) -> Result<Arc<dyn SnapshotStore>> {
        if self.snapshot_store.is_none() {
            self.snapshot_store = Some(self.build_snapshot_store()?);
        }

        Ok(self.snapshot_store.as_ref().cloned().unwrap())
    }

    fn build_snapshot_uploader(&mut self) -> Result<Arc<dyn SnapshotUploader>> {
        match self.configuration.snapshot_uploader_type {
            SnapshotUploaderType::Gcp => {
                let bucket = self
                    .configuration
                    .snapshot_bucket_name
                    .to_owned()
                    .ok_or_else(|| {
                        DependenciesBuilderError::MissingConfiguration(
                            "snapshot_bucket_name".to_string(),
                        )
                    })?;

                Ok(Arc::new(RemoteSnapshotUploader::new(
                    Box::new(GcpFileUploader::new(bucket.clone())),
                    bucket,
                )))
            }
            SnapshotUploaderType::Local => Ok(Arc::new(LocalSnapshotUploader::new(
                self.configuration.get_server_url(),
                &self.configuration.snapshot_directory,
            ))),
        }
    }

    pub fn get_snapshot_uploader(&mut self) -> Result<Arc<dyn SnapshotUploader>> {
        if self.snapshot_uploader.is_none() {
            self.snapshot_uploader = Some(self.build_snapshot_uploader()?);
        }

        Ok(self.snapshot_uploader.as_ref().cloned().unwrap())
    }

    fn build_multi_signer(&mut self) -> Result<Arc<RwLock<dyn MultiSigner>>> {
        let multi_signer = MultiSignerImpl::new(
            self.get_verification_key_store()?,
            self.get_stake_store()?,
            self.get_single_signature_store()?,
            self.get_protocol_parameters_store()?,
        );

        Ok(Arc::new(RwLock::new(multi_signer)))
    }

    /// Get a configured multi signer
    pub fn get_multi_signer(&mut self) -> Result<Arc<RwLock<dyn MultiSigner>>> {
        if self.multi_signer.is_none() {
            self.multi_signer = Some(self.build_multi_signer()?);
        }

        Ok(self.multi_signer.as_ref().cloned().unwrap())
    }

    fn build_certificate_pending_store(&mut self) -> Result<Arc<CertificatePendingStore>> {
        let adapter: Box<dyn StoreAdapter<Key = String, Record = CertificatePending>> =
            match self.configuration.environment {
                ExecutionEnvironment::Production => {
                    let adapter =
                        SQLiteAdapter::new("pending_certificate", self.get_sqlite_connection()?)
                            .map_err(|e| DependenciesBuilderError::Initialization {
                                message:
                                    "Cannot create SQLite adapter for PendingCertificate Store."
                                        .to_string(),
                                error: Some(e.into()),
                            })?;

                    Box::new(adapter)
                }
                _ => {
                    let adapter = MemoryAdapter::new(None).map_err(|e| {
                        DependenciesBuilderError::Initialization {
                            message: "Cannot create Memory adapter for PendingCertificate Store."
                                .to_string(),
                            error: Some(e.into()),
                        }
                    })?;
                    Box::new(adapter)
                }
            };

        Ok(Arc::new(CertificatePendingStore::new(adapter)))
    }

    /// Get a configured [CertificatePendingStore].
    pub fn get_certificate_pending_store(&mut self) -> Result<Arc<CertificatePendingStore>> {
        if self.certificate_pending_store.is_none() {
            self.certificate_pending_store = Some(self.build_certificate_pending_store()?);
        }

        Ok(self.certificate_pending_store.as_ref().cloned().unwrap())
    }

    fn build_certificate_store(&mut self) -> Result<Arc<CertificateStore>> {
        let adapter: Box<dyn StoreAdapter<Key = String, Record = Certificate>> = match self
            .configuration
            .environment
        {
            ExecutionEnvironment::Production => {
                let adapter = SQLiteAdapter::new("certificate", self.get_sqlite_connection()?)
                    .map_err(|e| DependenciesBuilderError::Initialization {
                        message: "Cannot create SQLite adapter for Certificate Store.".to_string(),
                        error: Some(e.into()),
                    })?;

                Box::new(adapter)
            }
            _ => {
                let adapter = MemoryAdapter::new(None).map_err(|e| {
                    DependenciesBuilderError::Initialization {
                        message: "Cannot create Memory adapter for Certificate Store.".to_string(),
                        error: Some(e.into()),
                    }
                })?;
                Box::new(adapter)
            }
        };

        Ok(Arc::new(CertificateStore::new(adapter)))
    }

    /// Get a configured [CertificateStore].
    pub fn get_certificate_store(&mut self) -> Result<Arc<CertificateStore>> {
        if self.certificate_store.is_none() {
            self.certificate_store = Some(self.build_certificate_store()?);
        }

        Ok(self.certificate_store.as_ref().cloned().unwrap())
    }

    fn build_verification_key_store(&mut self) -> Result<Arc<VerificationKeyStore>> {
        let adapter: Box<dyn StoreAdapter<Key = Epoch, Record = HashMap<PartyId, Signer>>> =
            match self.configuration.environment {
                ExecutionEnvironment::Production => {
                    let adapter = SQLiteAdapter::new("certificate", self.get_sqlite_connection()?)
                        .map_err(|e| DependenciesBuilderError::Initialization {
                            message: "Cannot create SQLite adapter for VerificationKeyStore."
                                .to_string(),
                            error: Some(e.into()),
                        })?;

                    Box::new(adapter)
                }
                _ => {
                    let adapter = MemoryAdapter::new(None).map_err(|e| {
                        DependenciesBuilderError::Initialization {
                            message: "Cannot create Memory adapter for VerificationKeyStore."
                                .to_string(),
                            error: Some(e.into()),
                        }
                    })?;
                    Box::new(adapter)
                }
            };

        Ok(Arc::new(VerificationKeyStore::new(
            adapter,
            self.configuration.store_retention_limit,
        )))
    }

    /// Get a configured [VerificationKeyStore].
    pub fn get_verification_key_store(&mut self) -> Result<Arc<VerificationKeyStore>> {
        if self.verification_key_store.is_none() {
            self.verification_key_store = Some(self.build_verification_key_store()?);
        }

        Ok(self.verification_key_store.as_ref().cloned().unwrap())
    }

    fn build_single_signature_store(&mut self) -> Result<Arc<SingleSignatureStore>> {
        let adapter: Box<
            dyn StoreAdapter<Key = Beacon, Record = HashMap<PartyId, SingleSignatures>>,
        > = match self.configuration.environment {
            ExecutionEnvironment::Production => {
                let adapter = SQLiteAdapter::new("single_signature", self.get_sqlite_connection()?)
                    .map_err(|e| DependenciesBuilderError::Initialization {
                        message: "Cannot create SQLite adapter for SingleSignatureStore."
                            .to_string(),
                        error: Some(e.into()),
                    })?;

                Box::new(adapter)
            }
            _ => {
                let adapter = MemoryAdapter::new(None).map_err(|e| {
                    DependenciesBuilderError::Initialization {
                        message: "Cannot create Memory adapter for SingleSignatureStore."
                            .to_string(),
                        error: Some(e.into()),
                    }
                })?;
                Box::new(adapter)
            }
        };

        Ok(Arc::new(SingleSignatureStore::new(
            adapter,
            self.configuration.store_retention_limit,
        )))
    }

    /// Get a configured [SingleSignatureStore].
    pub fn get_single_signature_store(&mut self) -> Result<Arc<SingleSignatureStore>> {
        if self.single_signature_store.is_none() {
            self.single_signature_store = Some(self.build_single_signature_store()?);
        }

        Ok(self.single_signature_store.as_ref().cloned().unwrap())
    }

    fn build_protocol_parameters_store(&mut self) -> Result<Arc<ProtocolParametersStore>> {
        let adapter: Box<dyn StoreAdapter<Key = Epoch, Record = ProtocolParameters>> =
            match self.configuration.environment {
                ExecutionEnvironment::Production => {
                    let adapter =
                        SQLiteAdapter::new("protocol_parameters", self.get_sqlite_connection()?)
                            .map_err(|e| DependenciesBuilderError::Initialization {
                                message:
                                    "Cannot create SQLite adapter for ProtocolParametersStore."
                                        .to_string(),
                                error: Some(e.into()),
                            })?;

                    Box::new(adapter)
                }
                _ => {
                    let adapter = MemoryAdapter::new(None).map_err(|e| {
                        DependenciesBuilderError::Initialization {
                            message: "Cannot create Memory adapter for ProtocolParametersStore."
                                .to_string(),
                            error: Some(e.into()),
                        }
                    })?;
                    Box::new(adapter)
                }
            };

        Ok(Arc::new(ProtocolParametersStore::new(
            adapter,
            self.configuration.store_retention_limit,
        )))
    }

    /// Get a configured [ProtocolParametersStore].
    pub fn get_protocol_parameters_store(&mut self) -> Result<Arc<ProtocolParametersStore>> {
        if self.protocol_parameters_store.is_none() {
            self.protocol_parameters_store = Some(self.build_protocol_parameters_store()?);
        }

        Ok(self.protocol_parameters_store.as_ref().cloned().unwrap())
    }

    fn build_chain_observer(&mut self) -> Result<Arc<dyn ChainObserver>> {
        let chain_observer: Arc<dyn ChainObserver> = match self.configuration.environment {
            ExecutionEnvironment::Production => {
                Arc::new(CardanoCliChainObserver::new(self.get_cardano_cli_runner()?))
            }
            _ => Arc::new(FakeObserver::default()),
        };

        Ok(chain_observer)
    }

    pub fn get_chain_observer(&mut self) -> Result<Arc<dyn ChainObserver>> {
        if self.chain_observer.is_none() {
            self.chain_observer = Some(self.build_chain_observer()?);
        }

        Ok(self.chain_observer.as_ref().cloned().unwrap())
    }

    fn build_cardano_cli_runner(&mut self) -> Result<Box<CardanoCliRunner>> {
        let cli_runner = CardanoCliRunner::new(
            self.configuration.cardano_cli_path.clone(),
            self.configuration.cardano_node_socket_path.clone(),
            self.configuration.get_network()?,
        );

        Ok(Box::new(cli_runner))
    }

    fn get_cardano_cli_runner(&mut self) -> Result<Box<CardanoCliRunner>> {
        if self.cardano_cli_runner.is_none() {
            self.cardano_cli_runner = Some(self.build_cardano_cli_runner()?);
        }

        Ok(self.cardano_cli_runner.as_ref().cloned().unwrap())
    }

    fn build_beacon_provider(&mut self) -> Result<Arc<dyn BeaconProvider>> {
        let beacon_provider = BeaconProviderImpl::new(
            self.get_chain_observer()?,
            self.get_immutable_file_observer()?,
            self.configuration.get_network()?,
        );

        Ok(Arc::new(beacon_provider))
    }

    /// Return a [BeaconProvider] instance.
    pub fn get_beacon_provider(&mut self) -> Result<Arc<dyn BeaconProvider>> {
        if self.beacon_provider.is_none() {
            self.beacon_provider = Some(self.build_beacon_provider()?);
        }

        Ok(self.beacon_provider.as_ref().cloned().unwrap())
    }

    fn build_immutable_file_observer(&mut self) -> Result<Arc<dyn ImmutableFileObserver>> {
        let immutable_file_observer: Arc<dyn ImmutableFileObserver> =
            match self.configuration.environment {
                ExecutionEnvironment::Production => Arc::new(ImmutableFileSystemObserver::new(
                    &self.configuration.db_directory,
                )),
                _ => Arc::new(DumbImmutableFileObserver::new()),
            };

        Ok(immutable_file_observer)
    }

    /// Return a [ImmutableFileObserver] instance.
    fn get_immutable_file_observer(&mut self) -> Result<Arc<dyn ImmutableFileObserver>> {
        if self.immutable_file_observer.is_none() {
            self.immutable_file_observer = Some(self.build_immutable_file_observer()?);
        }

        Ok(self.immutable_file_observer.as_ref().cloned().unwrap())
    }

    fn create_logger(&self) -> Result<Logger> {
        Ok(slog_scope::logger())
    }

    /// This method does not cache the logger since it is managed internally by
    /// its own crate.
    pub fn get_logger(&self) -> Result<Logger> {
        self.create_logger()
    }

    // TODO: cache management which should not be ASYNC
    fn build_immutable_digester(&mut self) -> Result<Arc<dyn ImmutableDigester>> {
        let digester = CardanoImmutableDigester::new(
            self.configuration.db_directory.clone(),
            None, // ← Cache shall be initialized here
            self.get_logger()?,
        );

        Ok(Arc::new(digester))
    }

    /// Immutable digester.
    /// **WARNING**: It is incomplete for now, the caching is no more supported,
    /// the dependency must be reworked.
    fn get_immutable_digester(&mut self) -> Result<Arc<dyn ImmutableDigester>> {
        if self.immutable_digester.is_none() {
            self.immutable_digester = Some(self.build_immutable_digester()?);
        }

        Ok(self.immutable_digester.as_ref().cloned().unwrap())
    }

    fn build_snapshotter(&mut self) -> Result<Arc<dyn Snapshotter>> {
        let snapshotter: Arc<dyn Snapshotter> = match self.configuration.environment {
            ExecutionEnvironment::Production => {
                let ongoing_snapshot_directory = self
                    .configuration
                    .snapshot_directory
                    .join("pending_snapshot");

                // **TODO** this code should be in the snapshotter constructor.
                if !ongoing_snapshot_directory.exists() {
                    std::fs::create_dir(&ongoing_snapshot_directory).map_err(|e| {
                        DependenciesBuilderError::Initialization {
                            message: format!(
                                "Cannot create snapshotter directory '{}'.",
                                ongoing_snapshot_directory.display()
                            ),
                            error: Some(e.into()),
                        }
                    })?;
                }

                Arc::new(GzipSnapshotter::new(
                    self.configuration.db_directory.clone(),
                    ongoing_snapshot_directory,
                ))
            }
            _ => Arc::new(DumbSnapshotter::new()),
        };

        Ok(snapshotter)
    }

    /// [Snapshotter] service.
    pub fn get_snapshotter(&mut self) -> Result<Arc<dyn Snapshotter>> {
        if self.snapshotter.is_none() {
            self.snapshotter = Some(self.build_snapshotter()?);
        }

        Ok(self.snapshotter.as_ref().cloned().unwrap())
    }

    fn build_certificate_verifier(&mut self) -> Result<Arc<dyn CertificateVerifier>> {
        let verifier = Arc::new(MithrilCertificateVerifier::new(self.get_logger()?));

        Ok(verifier)
    }

    /// [CertificateVerifier] service.
    pub fn get_certificate_verifier(&mut self) -> Result<Arc<dyn CertificateVerifier>> {
        if self.certificate_verifier.is_none() {
            self.certificate_verifier = Some(self.build_certificate_verifier()?);
        }

        Ok(self.certificate_verifier.as_ref().cloned().unwrap())
    }
}

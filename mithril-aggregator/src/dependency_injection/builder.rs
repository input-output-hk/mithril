use std::sync::Arc;

use mithril_common::{
    api_version::APIVersionProvider,
    certificate_chain::{CertificateVerifier, MithrilCertificateVerifier},
    chain_observer::{CardanoCliChainObserver, CardanoCliRunner, ChainObserver, FakeObserver},
    crypto_helper::{key_decode_hex, ProtocolGenesisSigner, ProtocolGenesisVerifier},
    database::{ApplicationNodeType, DatabaseVersionChecker},
    digesters::{
        cache::{ImmutableFileDigestCacheProvider, JsonImmutableFileDigestCacheProviderBuilder},
        CardanoImmutableDigester, DumbImmutableFileObserver, ImmutableDigester,
        ImmutableFileObserver, ImmutableFileSystemObserver,
    },
    entities::{CertificatePending, Epoch},
    era::{
        adapters::{EraReaderAdapterBuilder, EraReaderDummyAdapter},
        EraChecker, EraMarker, EraReader, EraReaderAdapter, SupportedEra,
    },
    signable_builder::{
        CardanoImmutableFilesFullSignableBuilder, MithrilStakeDistributionSignableBuilder,
    },
    signable_builder::{MithrilSignableBuilderService, SignableBuilderService},
    store::adapter::{MemoryAdapter, SQLiteAdapter, StoreAdapter},
    BeaconProvider, BeaconProviderImpl,
};
use slog::Logger;
use slog_scope::debug;
use sqlite::Connection;
use tokio::{
    sync::{
        mpsc::{UnboundedReceiver, UnboundedSender},
        Mutex, RwLock,
    },
    time::Duration,
};
use warp::Filter;

use crate::{
    artifact_builder::{
        CardanoImmutableFilesFullArtifactBuilder, MithrilStakeDistributionArtifactBuilder,
    },
    certifier_service::{CertifierService, MithrilCertifierService},
    configuration::ExecutionEnvironment,
    database::{
        provider::SignerRegistrationStore,
        provider::{
            CertificateRepository, CertificateStoreAdapter, EpochSettingStore,
            OpenMessageRepository, SignedEntityStoreAdapter, SignedEntityStorer, SignerStore,
            SingleSignatureRepository, StakePoolStore,
        },
    },
    event_store::{EventMessage, EventStore, TransmitterService},
    http_server::routes::router,
    services::{
        MithrilSignedEntityService, MithrilStakeDistributionService, MithrilTickerService,
        SignedEntityService, StakeDistributionService, TickerService,
    },
    signer_registerer::SignerRecorder,
    tools::{GcpFileUploader, GenesisToolsDependency},
    AggregatorConfig, AggregatorRunner, AggregatorRuntime, CertificatePendingStore,
    CertificateStore, Configuration, DependencyContainer, DumbSnapshotUploader, DumbSnapshotter,
    GzipSnapshotter, LocalSnapshotUploader, MithrilSignerRegisterer, MultiSigner, MultiSignerImpl,
    ProtocolParametersStore, ProtocolParametersStorer, RemoteSnapshotUploader, SnapshotUploader,
    SnapshotUploaderType, Snapshotter, VerificationKeyStorer,
};

use super::{DependenciesBuilderError, Result};

const SQLITE_FILE: &str = "aggregator.sqlite3";

/// ## Dependencies container builder
///
/// This is meant to create SHARED DEPENDENCIES, ie: dependencies instances that
/// must be shared amongst several Tokio tasks. For example, database
/// repositories are NOT shared dependencies and therefor can be created ad hoc
/// whereas the database connection is a shared dependency.
///
/// Each shared dependency must implement a `build` and a `get` function. The
/// build function creates the dependency, the get function creates the
/// dependency at first call then return a clone of the Arc containing the
/// dependency for all further calls.
pub struct DependenciesBuilder {
    /// Configuration parameters
    pub configuration: Configuration,

    /// SQLite database connection
    pub sqlite_connection: Option<Arc<Mutex<Connection>>>,

    /// Stake Store used by the StakeDistributionService
    /// It shall be a private dependency.
    pub stake_store: Option<Arc<StakePoolStore>>,

    /// Snapshot uploader service.
    pub snapshot_uploader: Option<Arc<dyn SnapshotUploader>>,

    /// Multisigner service.
    pub multi_signer: Option<Arc<RwLock<dyn MultiSigner>>>,

    /// Certificate pending store.
    pub certificate_pending_store: Option<Arc<CertificatePendingStore>>,

    /// Certificate store.
    pub certificate_store: Option<Arc<CertificateStore>>,

    /// Verification key store.
    pub verification_key_store: Option<Arc<dyn VerificationKeyStorer>>,

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

    /// Immutable cache provider service.
    pub immutable_cache_provider: Option<Arc<dyn ImmutableFileDigestCacheProvider>>,

    /// Digester service.
    pub digester: Option<Arc<dyn ImmutableDigester>>,

    /// Snapshotter service.
    pub snapshotter: Option<Arc<dyn Snapshotter>>,

    /// Certificate verifier service.
    pub certificate_verifier: Option<Arc<dyn CertificateVerifier>>,

    /// Genesis signature verifier service.
    pub genesis_verifier: Option<Arc<ProtocolGenesisVerifier>>,

    /// Signer registerer service
    pub mithril_registerer: Option<Arc<MithrilSignerRegisterer>>,

    /// Era checker service
    pub era_checker: Option<Arc<EraChecker>>,

    /// Adapter for [EraReader]
    pub era_reader_adapter: Option<Arc<dyn EraReaderAdapter>>,

    /// Era reader service
    pub era_reader: Option<Arc<EraReader>>,

    /// Event Transmitter Service
    pub event_transmitter: Option<Arc<TransmitterService<EventMessage>>>,

    /// Event transmitter Channel Sender endpoint
    pub event_transmitter_channel: (
        Option<UnboundedReceiver<EventMessage>>,
        Option<UnboundedSender<EventMessage>>,
    ),

    /// API Version provider
    pub api_version_provider: Option<Arc<APIVersionProvider>>,

    /// Stake Distribution Service
    pub stake_distribution_service: Option<Arc<dyn StakeDistributionService>>,

    /// Ticker Service (TODO: remove BeaconProvider)
    pub ticker_service: Option<Arc<dyn TickerService>>,

    /// Signer Recorder
    pub signer_recorder: Option<Arc<dyn SignerRecorder>>,

    /// Signable Builder Service
    pub signable_builder_service: Option<Arc<dyn SignableBuilderService>>,

    /// Signed Entity Service
    pub signed_entity_service: Option<Arc<dyn SignedEntityService>>,

    /// Certifier service
    pub certifier_service: Option<Arc<dyn CertifierService>>,

    /// Signed Entity storer
    pub signed_entity_storer: Option<Arc<dyn SignedEntityStorer>>,
}

impl DependenciesBuilder {
    /// Create a new clean dependency builder
    pub fn new(configuration: Configuration) -> Self {
        Self {
            configuration,
            sqlite_connection: None,
            stake_store: None,
            snapshot_uploader: None,
            multi_signer: None,
            certificate_pending_store: None,
            certificate_store: None,
            verification_key_store: None,
            protocol_parameters_store: None,
            cardano_cli_runner: None,
            chain_observer: None,
            beacon_provider: None,
            immutable_digester: None,
            immutable_file_observer: None,
            immutable_cache_provider: None,
            digester: None,
            snapshotter: None,
            certificate_verifier: None,
            genesis_verifier: None,
            mithril_registerer: None,
            era_reader_adapter: None,
            era_checker: None,
            era_reader: None,
            event_transmitter: None,
            event_transmitter_channel: (None, None),
            api_version_provider: None,
            stake_distribution_service: None,
            ticker_service: None,
            signer_recorder: None,
            signable_builder_service: None,
            signed_entity_service: None,
            certifier_service: None,
            signed_entity_storer: None,
        }
    }

    async fn build_sqlite_connection(&self) -> Result<Arc<Mutex<Connection>>> {
        let path = match self.configuration.environment {
            ExecutionEnvironment::Production => {
                self.configuration.get_sqlite_dir().join(SQLITE_FILE)
            }
            _ => self.configuration.data_stores_directory.clone(),
        };
        let connection = Connection::open(&path)
            .map(|c| Arc::new(Mutex::new(c)))
            .map_err(|e| DependenciesBuilderError::Initialization {
                message: format!(
                    "SQLite initialization: could not open connection with string '{}'.",
                    path.display()
                ),
                error: Some(Box::new(e)),
            })?;
        // Check database migrations
        let mut db_checker = DatabaseVersionChecker::new(
            self.get_logger().await?,
            ApplicationNodeType::Aggregator,
            connection.clone(),
        );

        for migration in crate::database::migration::get_migrations() {
            db_checker.add_migration(migration);
        }

        // configure session
        connection
            .lock()
            .await
            .execute("pragma foreign_keys=true")
            .map_err(|e| DependenciesBuilderError::Initialization {
                message: "SQLite initialization: could not enable FOREIGN KEY support.".to_string(),
                error: Some(e.into()),
            })?;

        db_checker.apply().await?;

        Ok(connection)
    }

    /// Get SQLite connection
    pub async fn get_sqlite_connection(&mut self) -> Result<Arc<Mutex<Connection>>> {
        if self.sqlite_connection.is_none() {
            self.sqlite_connection = Some(self.build_sqlite_connection().await?);
        }

        Ok(self.sqlite_connection.as_ref().cloned().unwrap())
    }

    async fn build_stake_store(&mut self) -> Result<Arc<StakePoolStore>> {
        let stake_pool_store = Arc::new(StakePoolStore::new(self.get_sqlite_connection().await?));

        Ok(stake_pool_store)
    }

    /// Return a [StakePoolStore]
    pub async fn get_stake_store(&mut self) -> Result<Arc<StakePoolStore>> {
        if self.stake_store.is_none() {
            self.stake_store = Some(self.build_stake_store().await?);
        }

        Ok(self.stake_store.as_ref().cloned().unwrap())
    }

    async fn build_snapshot_uploader(&mut self) -> Result<Arc<dyn SnapshotUploader>> {
        if self.configuration.environment == ExecutionEnvironment::Production {
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
        } else {
            Ok(Arc::new(DumbSnapshotUploader::new()))
        }
    }

    /// Get a [SnapshotUploader]
    pub async fn get_snapshot_uploader(&mut self) -> Result<Arc<dyn SnapshotUploader>> {
        if self.snapshot_uploader.is_none() {
            self.snapshot_uploader = Some(self.build_snapshot_uploader().await?);
        }

        Ok(self.snapshot_uploader.as_ref().cloned().unwrap())
    }

    async fn build_multi_signer(&mut self) -> Result<Arc<RwLock<dyn MultiSigner>>> {
        let multi_signer = MultiSignerImpl::new(
            self.get_verification_key_store().await?,
            self.get_stake_store().await?,
            self.get_protocol_parameters_store().await?,
        );

        Ok(Arc::new(RwLock::new(multi_signer)))
    }

    /// Get a configured multi signer
    pub async fn get_multi_signer(&mut self) -> Result<Arc<RwLock<dyn MultiSigner>>> {
        if self.multi_signer.is_none() {
            self.multi_signer = Some(self.build_multi_signer().await?);
        }

        Ok(self.multi_signer.as_ref().cloned().unwrap())
    }

    async fn build_certificate_pending_store(&mut self) -> Result<Arc<CertificatePendingStore>> {
        let adapter: Box<dyn StoreAdapter<Key = String, Record = CertificatePending>> = match self
            .configuration
            .environment
        {
            ExecutionEnvironment::Production => {
                let adapter =
                    SQLiteAdapter::new("pending_certificate", self.get_sqlite_connection().await?)
                        .map_err(|e| DependenciesBuilderError::Initialization {
                            message: "Cannot create SQLite adapter for PendingCertificate Store."
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
    pub async fn get_certificate_pending_store(&mut self) -> Result<Arc<CertificatePendingStore>> {
        if self.certificate_pending_store.is_none() {
            self.certificate_pending_store = Some(self.build_certificate_pending_store().await?);
        }

        Ok(self.certificate_pending_store.as_ref().cloned().unwrap())
    }

    async fn build_certificate_store(&mut self) -> Result<Arc<CertificateStore>> {
        Ok(Arc::new(CertificateStore::new(Box::new(
            CertificateStoreAdapter::new(self.get_sqlite_connection().await?),
        ))))
    }

    /// Get a configured [CertificateStore].
    pub async fn get_certificate_store(&mut self) -> Result<Arc<CertificateStore>> {
        if self.certificate_store.is_none() {
            self.certificate_store = Some(self.build_certificate_store().await?);
        }

        Ok(self.certificate_store.as_ref().cloned().unwrap())
    }

    async fn build_verification_key_store(&mut self) -> Result<Arc<dyn VerificationKeyStorer>> {
        Ok(Arc::new(SignerRegistrationStore::new(
            self.get_sqlite_connection().await?,
        )))
    }

    /// Get a configured [VerificationKeyStorer].
    pub async fn get_verification_key_store(&mut self) -> Result<Arc<dyn VerificationKeyStorer>> {
        if self.verification_key_store.is_none() {
            self.verification_key_store = Some(self.build_verification_key_store().await?);
        }

        Ok(self.verification_key_store.as_ref().cloned().unwrap())
    }

    async fn build_protocol_parameters_store(&mut self) -> Result<Arc<ProtocolParametersStore>> {
        Ok(Arc::new(ProtocolParametersStore::new(
            Box::new(EpochSettingStore::new(self.get_sqlite_connection().await?)),
            self.configuration.store_retention_limit,
        )))
    }

    /// Get a configured [ProtocolParametersStore].
    pub async fn get_protocol_parameters_store(&mut self) -> Result<Arc<ProtocolParametersStore>> {
        if self.protocol_parameters_store.is_none() {
            self.protocol_parameters_store = Some(self.build_protocol_parameters_store().await?);
        }

        Ok(self.protocol_parameters_store.as_ref().cloned().unwrap())
    }

    async fn build_chain_observer(&mut self) -> Result<Arc<dyn ChainObserver>> {
        let chain_observer: Arc<dyn ChainObserver> = match self.configuration.environment {
            ExecutionEnvironment::Production => Arc::new(CardanoCliChainObserver::new(
                self.get_cardano_cli_runner().await?,
            )),
            _ => Arc::new(FakeObserver::default()),
        };

        Ok(chain_observer)
    }

    /// Return a [ChainObserver]
    pub async fn get_chain_observer(&mut self) -> Result<Arc<dyn ChainObserver>> {
        if self.chain_observer.is_none() {
            self.chain_observer = Some(self.build_chain_observer().await?);
        }

        Ok(self.chain_observer.as_ref().cloned().unwrap())
    }

    async fn build_cardano_cli_runner(&mut self) -> Result<Box<CardanoCliRunner>> {
        let cli_runner = CardanoCliRunner::new(
            self.configuration.cardano_cli_path.clone(),
            self.configuration.cardano_node_socket_path.clone(),
            self.configuration.get_network()?,
        );

        Ok(Box::new(cli_runner))
    }

    /// Return a [CardanoCliRunner]
    pub async fn get_cardano_cli_runner(&mut self) -> Result<Box<CardanoCliRunner>> {
        if self.cardano_cli_runner.is_none() {
            self.cardano_cli_runner = Some(self.build_cardano_cli_runner().await?);
        }

        Ok(self.cardano_cli_runner.as_ref().cloned().unwrap())
    }

    async fn build_beacon_provider(&mut self) -> Result<Arc<dyn BeaconProvider>> {
        let beacon_provider = BeaconProviderImpl::new(
            self.get_chain_observer().await?,
            self.get_immutable_file_observer().await?,
            self.configuration.get_network()?,
        );

        Ok(Arc::new(beacon_provider))
    }

    /// Return a [BeaconProvider] instance.
    pub async fn get_beacon_provider(&mut self) -> Result<Arc<dyn BeaconProvider>> {
        if self.beacon_provider.is_none() {
            self.beacon_provider = Some(self.build_beacon_provider().await?);
        }

        Ok(self.beacon_provider.as_ref().cloned().unwrap())
    }

    async fn build_immutable_file_observer(&mut self) -> Result<Arc<dyn ImmutableFileObserver>> {
        let immutable_file_observer: Arc<dyn ImmutableFileObserver> =
            match self.configuration.environment {
                ExecutionEnvironment::Production => Arc::new(ImmutableFileSystemObserver::new(
                    &self.configuration.db_directory,
                )),
                _ => Arc::new(DumbImmutableFileObserver::default()),
            };

        Ok(immutable_file_observer)
    }

    /// Return a [ImmutableFileObserver] instance.
    pub async fn get_immutable_file_observer(&mut self) -> Result<Arc<dyn ImmutableFileObserver>> {
        if self.immutable_file_observer.is_none() {
            self.immutable_file_observer = Some(self.build_immutable_file_observer().await?);
        }

        Ok(self.immutable_file_observer.as_ref().cloned().unwrap())
    }

    async fn build_immutable_cache_provider(
        &mut self,
    ) -> Result<Arc<dyn ImmutableFileDigestCacheProvider>> {
        let cache_provider = JsonImmutableFileDigestCacheProviderBuilder::new(
            &self.configuration.data_stores_directory,
            &format!("immutables_digests_{}.json", self.configuration.network),
        )
        .should_reset_digests_cache(self.configuration.reset_digests_cache)
        .build()
        .await?;

        Ok(Arc::new(cache_provider))
    }

    /// Get an [ImmutableFileDigestCacheProvider]
    pub async fn get_immutable_cache_provider(
        &mut self,
    ) -> Result<Arc<dyn ImmutableFileDigestCacheProvider>> {
        if self.immutable_cache_provider.is_none() {
            self.immutable_cache_provider = Some(self.build_immutable_cache_provider().await?);
        }

        Ok(self.immutable_cache_provider.as_ref().cloned().unwrap())
    }

    async fn create_logger(&self) -> Result<Logger> {
        Ok(slog_scope::logger())
    }

    /// This method does not cache the logger since it is managed internally by
    /// its own crate.
    pub async fn get_logger(&self) -> Result<Logger> {
        self.create_logger().await
    }

    async fn build_immutable_digester(&mut self) -> Result<Arc<dyn ImmutableDigester>> {
        let immutable_digester_cache = match self.configuration.environment {
            ExecutionEnvironment::Production => Some(self.get_immutable_cache_provider().await?),
            _ => None,
        };
        let digester =
            CardanoImmutableDigester::new(immutable_digester_cache, self.get_logger().await?);

        Ok(Arc::new(digester))
    }

    /// Immutable digester.
    pub async fn get_immutable_digester(&mut self) -> Result<Arc<dyn ImmutableDigester>> {
        if self.immutable_digester.is_none() {
            self.immutable_digester = Some(self.build_immutable_digester().await?);
        }

        Ok(self.immutable_digester.as_ref().cloned().unwrap())
    }

    async fn build_snapshotter(&mut self) -> Result<Arc<dyn Snapshotter>> {
        let snapshotter: Arc<dyn Snapshotter> = match self.configuration.environment {
            ExecutionEnvironment::Production => {
                let ongoing_snapshot_directory = self
                    .configuration
                    .snapshot_directory
                    .join("pending_snapshot");

                Arc::new(GzipSnapshotter::new(
                    self.configuration.db_directory.clone(),
                    ongoing_snapshot_directory,
                )?)
            }
            _ => Arc::new(DumbSnapshotter::new()),
        };

        Ok(snapshotter)
    }

    /// [Snapshotter] service.
    pub async fn get_snapshotter(&mut self) -> Result<Arc<dyn Snapshotter>> {
        if self.snapshotter.is_none() {
            self.snapshotter = Some(self.build_snapshotter().await?);
        }

        Ok(self.snapshotter.as_ref().cloned().unwrap())
    }

    async fn build_certificate_verifier(&mut self) -> Result<Arc<dyn CertificateVerifier>> {
        let verifier = Arc::new(MithrilCertificateVerifier::new(self.get_logger().await?));

        Ok(verifier)
    }

    /// [CertificateVerifier] service.
    pub async fn get_certificate_verifier(&mut self) -> Result<Arc<dyn CertificateVerifier>> {
        if self.certificate_verifier.is_none() {
            self.certificate_verifier = Some(self.build_certificate_verifier().await?);
        }

        Ok(self.certificate_verifier.as_ref().cloned().unwrap())
    }

    async fn build_genesis_verifier(&mut self) -> Result<Arc<ProtocolGenesisVerifier>> {
        let genesis_verifier: ProtocolGenesisVerifier = match self.configuration.environment {
            ExecutionEnvironment::Production => ProtocolGenesisVerifier::from_verification_key(
                key_decode_hex(&self.configuration.genesis_verification_key).map_err(|e| {
                    DependenciesBuilderError::Initialization {
                        message: format!(
                            "Could not decode hex key to build genesis verifier: '{}' Error: {e}.",
                            self.configuration.genesis_verification_key
                        ),
                        error: None,
                    }
                })?,
            ),
            _ => ProtocolGenesisSigner::create_deterministic_genesis_signer()
                .create_genesis_verifier(),
        };

        Ok(Arc::new(genesis_verifier))
    }

    /// Return a [ProtocolGenesisVerifier]
    pub async fn get_genesis_verifier(&mut self) -> Result<Arc<ProtocolGenesisVerifier>> {
        if self.genesis_verifier.is_none() {
            self.genesis_verifier = Some(self.build_genesis_verifier().await?);
        }

        Ok(self.genesis_verifier.as_ref().cloned().unwrap())
    }

    async fn build_mithril_registerer(&mut self) -> Result<Arc<MithrilSignerRegisterer>> {
        let registerer = MithrilSignerRegisterer::new(
            self.get_chain_observer().await?,
            self.get_verification_key_store().await?,
            self.get_signer_recorder().await?,
            self.configuration.store_retention_limit.map(|l| l as u64),
        );

        Ok(Arc::new(registerer))
    }

    /// [MithrilSignerRegisterer] service
    pub async fn get_mithril_registerer(&mut self) -> Result<Arc<MithrilSignerRegisterer>> {
        if self.mithril_registerer.is_none() {
            self.mithril_registerer = Some(self.build_mithril_registerer().await?);
        }

        Ok(self.mithril_registerer.as_ref().cloned().unwrap())
    }

    async fn build_era_reader(&mut self) -> Result<Arc<EraReader>> {
        let era_adapter: Arc<dyn EraReaderAdapter> = match self.configuration.environment {
            ExecutionEnvironment::Production => EraReaderAdapterBuilder::new(
                &self.configuration.era_reader_adapter_type,
                &self.configuration.era_reader_adapter_params,
            )
            .build(self.get_chain_observer().await?)
            .map_err(|e| DependenciesBuilderError::Initialization {
                message: "Could not build EraReader as dependency.".to_string(),
                error: Some(Box::new(e)),
            })?,
            _ => Arc::new(EraReaderDummyAdapter::from_markers(vec![EraMarker::new(
                &SupportedEra::dummy().to_string(),
                Some(Epoch(0)),
            )])),
        };

        Ok(Arc::new(EraReader::new(era_adapter)))
    }

    /// [EraReader] service
    pub async fn get_era_reader(&mut self) -> Result<Arc<EraReader>> {
        if self.era_reader.is_none() {
            self.era_reader = Some(self.build_era_reader().await?);
        }

        Ok(self.era_reader.as_ref().cloned().unwrap())
    }

    async fn build_era_checker(&mut self) -> Result<Arc<EraChecker>> {
        let current_epoch = self
            .get_beacon_provider()
            .await?
            .get_current_beacon()
            .await
            .map_err(|e| DependenciesBuilderError::Initialization {
                message: "Error while building EraChecker".to_string(),
                error: Some(e.into()),
            })?
            .epoch;
        let era_epoch_token = self
            .get_era_reader()
            .await?
            .read_era_epoch_token(current_epoch)
            .await
            .map_err(|e| DependenciesBuilderError::Initialization {
                message: "Error while building EraChecker".to_string(),
                error: Some(e.into()),
            })?;
        let era_checker = Arc::new(EraChecker::new(
            era_epoch_token.get_current_supported_era().map_err(|e| {
                DependenciesBuilderError::Initialization {
                    message: "Error while building EraChecker".to_string(),
                    error: Some(e.into()),
                }
            })?,
            era_epoch_token.get_current_epoch(),
        ));

        Ok(era_checker)
    }

    /// [EraReader] service
    pub async fn get_era_checker(&mut self) -> Result<Arc<EraChecker>> {
        if self.era_checker.is_none() {
            self.era_checker = Some(self.build_era_checker().await?);
        }

        Ok(self.era_checker.as_ref().cloned().unwrap())
    }

    async fn build_event_transmitter_channel(
        &mut self,
    ) -> Result<(
        UnboundedReceiver<EventMessage>,
        UnboundedSender<EventMessage>,
    )> {
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<EventMessage>();

        Ok((rx, tx))
    }

    /// Return the EventMessage channel sender.
    pub async fn get_event_transmitter_sender(&mut self) -> Result<UnboundedSender<EventMessage>> {
        if let (_, None) = self.event_transmitter_channel {
            let (rx, tx) = self.build_event_transmitter_channel().await?;
            self.event_transmitter_channel = (Some(rx), Some(tx));
        }

        Ok(self
            .event_transmitter_channel
            .1
            .as_ref()
            .cloned()
            .expect("Transmitter<EventMessage> should be set."))
    }

    /// Return the channel receiver setup for the [EventStore]. Since this
    /// receiver is not clonable, it must be called only once.
    pub async fn get_event_transmitter_receiver(
        &mut self,
    ) -> Result<UnboundedReceiver<EventMessage>> {
        if let (_, None) = self.event_transmitter_channel {
            let (rx, tx) = self.build_event_transmitter_channel().await?;
            self.event_transmitter_channel = (Some(rx), Some(tx));
        }
        let mut receiver: Option<UnboundedReceiver<EventMessage>> = None;
        std::mem::swap(&mut self.event_transmitter_channel.0, &mut receiver);

        receiver.ok_or_else(|| {
            DependenciesBuilderError::InconsistentState(
                "Receiver<EventMessage> has already been given and is not clonable.".to_string(),
            )
        })
    }

    async fn build_event_transmitter(&mut self) -> Result<Arc<TransmitterService<EventMessage>>> {
        let sender = self.get_event_transmitter_sender().await?;
        let event_transmitter = Arc::new(TransmitterService::new(sender));

        Ok(event_transmitter)
    }

    /// [TransmitterService] service
    pub async fn get_event_transmitter(&mut self) -> Result<Arc<TransmitterService<EventMessage>>> {
        if self.event_transmitter.is_none() {
            self.event_transmitter = Some(self.build_event_transmitter().await?);
        }

        Ok(self.event_transmitter.as_ref().cloned().unwrap())
    }

    async fn build_api_version_provider(&mut self) -> Result<Arc<APIVersionProvider>> {
        let api_version_provider = Arc::new(APIVersionProvider::new(self.get_era_checker().await?));

        Ok(api_version_provider)
    }

    /// [APIVersionProvider] service
    pub async fn get_api_version_provider(&mut self) -> Result<Arc<APIVersionProvider>> {
        if self.api_version_provider.is_none() {
            self.api_version_provider = Some(self.build_api_version_provider().await?);
        }

        Ok(self.api_version_provider.as_ref().cloned().unwrap())
    }

    async fn build_stake_distribution_service(
        &mut self,
    ) -> Result<Arc<dyn StakeDistributionService>> {
        let stake_distribution_service = Arc::new(MithrilStakeDistributionService::new(
            self.get_stake_store().await?,
            self.get_chain_observer().await?,
        ));

        Ok(stake_distribution_service)
    }

    /// [StakeDistributionService] service
    pub async fn get_stake_distribution_service(
        &mut self,
    ) -> Result<Arc<dyn StakeDistributionService>> {
        if self.stake_distribution_service.is_none() {
            self.stake_distribution_service = Some(self.build_stake_distribution_service().await?);
        }

        Ok(self.stake_distribution_service.as_ref().cloned().unwrap())
    }

    async fn build_signer_recorder(&mut self) -> Result<Arc<dyn SignerRecorder>> {
        let signer_recorder = Arc::new(SignerStore::new(self.get_sqlite_connection().await?));

        Ok(signer_recorder)
    }

    /// [SignerRecorder] service
    pub async fn get_signer_recorder(&mut self) -> Result<Arc<dyn SignerRecorder>> {
        if self.signer_recorder.is_none() {
            self.signer_recorder = Some(self.build_signer_recorder().await?);
        }

        Ok(self.signer_recorder.as_ref().cloned().unwrap())
    }

    async fn build_signable_builder_service(&mut self) -> Result<Arc<dyn SignableBuilderService>> {
        let mithril_stake_distribution_builder =
            Arc::new(MithrilStakeDistributionSignableBuilder::default());
        let immutable_signable_builder = Arc::new(CardanoImmutableFilesFullSignableBuilder::new(
            self.get_immutable_digester().await?,
            &self.configuration.db_directory,
            self.get_logger().await?,
        ));
        let signable_builder_service = Arc::new(MithrilSignableBuilderService::new(
            mithril_stake_distribution_builder,
            immutable_signable_builder,
        ));

        Ok(signable_builder_service)
    }

    /// [SignableBuilderService] service
    pub async fn get_signable_builder_service(
        &mut self,
    ) -> Result<Arc<dyn SignableBuilderService>> {
        if self.signable_builder_service.is_none() {
            self.signable_builder_service = Some(self.build_signable_builder_service().await?);
        }

        Ok(self.signable_builder_service.as_ref().cloned().unwrap())
    }

    async fn build_signed_entity_service(&mut self) -> Result<Arc<dyn SignedEntityService>> {
        let signed_entity_storer = self.build_signed_entity_storer().await?;
        let multi_signer = self.get_multi_signer().await?;
        let mithril_stake_distribution_artifact_builder =
            Arc::new(MithrilStakeDistributionArtifactBuilder::new(multi_signer));
        let snapshotter = self.build_snapshotter().await?;
        let snapshot_uploader = self.build_snapshot_uploader().await?;
        let cardano_immutable_files_full_artifact_builder = Arc::new(
            CardanoImmutableFilesFullArtifactBuilder::new(snapshotter, snapshot_uploader),
        );
        let signed_entity_service = Arc::new(MithrilSignedEntityService::new(
            signed_entity_storer,
            mithril_stake_distribution_artifact_builder,
            cardano_immutable_files_full_artifact_builder,
        ));

        Ok(signed_entity_service)
    }

    /// [SignedEntityService] service
    pub async fn get_signed_entity_service(&mut self) -> Result<Arc<dyn SignedEntityService>> {
        if self.signed_entity_service.is_none() {
            self.signed_entity_service = Some(self.build_signed_entity_service().await?);
        }

        Ok(self.signed_entity_service.as_ref().cloned().unwrap())
    }

    async fn build_signed_entity_storer(&mut self) -> Result<Arc<dyn SignedEntityStorer>> {
        let signed_entity_storer = Arc::new(SignedEntityStoreAdapter::new(
            self.get_sqlite_connection().await?,
        ));

        Ok(signed_entity_storer)
    }

    /// [SignedEntityStorer] service
    pub async fn get_signed_entity_storer(&mut self) -> Result<Arc<dyn SignedEntityStorer>> {
        if self.signed_entity_storer.is_none() {
            self.signed_entity_storer = Some(self.build_signed_entity_storer().await?);
        }

        Ok(self.signed_entity_storer.as_ref().cloned().unwrap())
    }

    /// Return an unconfigured [DependencyManager]
    pub async fn build_dependency_container(&mut self) -> Result<DependencyContainer> {
        let dependency_manager = DependencyContainer {
            config: self.configuration.clone(),
            sqlite_connection: self.get_sqlite_connection().await?,
            stake_store: self.get_stake_store().await?,
            snapshot_uploader: self.get_snapshot_uploader().await?,
            multi_signer: self.get_multi_signer().await?,
            certificate_pending_store: self.get_certificate_pending_store().await?,
            certificate_store: self.get_certificate_store().await?,
            verification_key_store: self.get_verification_key_store().await?,
            protocol_parameters_store: self.get_protocol_parameters_store().await?,
            chain_observer: self.get_chain_observer().await?,
            beacon_provider: self.get_beacon_provider().await?,
            immutable_file_observer: self.get_immutable_file_observer().await?,
            digester: self.get_immutable_digester().await?,
            snapshotter: self.get_snapshotter().await?,
            certificate_verifier: self.get_certificate_verifier().await?,
            genesis_verifier: self.get_genesis_verifier().await?,
            signer_registerer: self.get_mithril_registerer().await?,
            signer_registration_round_opener: self.get_mithril_registerer().await?,
            era_checker: self.get_era_checker().await?,
            era_reader: self.get_era_reader().await?,
            event_transmitter: self.get_event_transmitter().await?,
            api_version_provider: self.get_api_version_provider().await?,
            stake_distribution_service: self.get_stake_distribution_service().await?,
            signer_recorder: self.get_signer_recorder().await?,
            signable_builder_service: self.get_signable_builder_service().await?,
            signed_entity_service: self.get_signed_entity_service().await?,
            certifier_service: self.get_certifier_service().await?,
            ticker_service: self.get_ticker_service().await?,
            signed_entity_storer: self.get_signed_entity_storer().await?,
        };

        Ok(dependency_manager)
    }

    /// Create dependencies for the [EventStore] task.
    pub async fn create_event_store(&mut self) -> Result<EventStore> {
        let event_store = EventStore::new(self.get_event_transmitter_receiver().await?);

        Ok(event_store)
    }

    /// Create the AggregatorRunner
    pub async fn create_aggregator_runner(&mut self) -> Result<AggregatorRuntime> {
        // initialize ProtocolParameters store if needed
        {
            let current_epoch = self
                .get_chain_observer()
                .await?
                .get_current_epoch()
                .await
                .map_err(|e| DependenciesBuilderError::Initialization {
                    message: "cannot create aggregator runner".to_string(),
                    error: Some(e.into()),
                })?
                .ok_or(DependenciesBuilderError::Initialization {
                    message: "cannot build aggregator runner: impossible to retrieve current epoch"
                        .to_string(),
                    error: None,
                })?;
            let (work_epoch, epoch_to_sign, next_epoch_to_sign) = match current_epoch {
                Epoch(0) => (Epoch(0), Epoch(1), Epoch(2)),
                epoch => (
                    epoch.offset_to_signer_retrieval_epoch().unwrap(),
                    epoch.offset_to_next_signer_retrieval_epoch(),
                    epoch.offset_to_next_signer_retrieval_epoch().next(),
                ),
            };
            let protocol_parameters_store = self.get_protocol_parameters_store().await?;

            if protocol_parameters_store
                .get_protocol_parameters(work_epoch)
                .await
                .map_err(|e| DependenciesBuilderError::Initialization {
                    message: "can not create aggregator runner".to_string(),
                    error: Some(e.into()),
                })?
                .is_none()
            {
                debug!("First launch, will use the configured protocol parameters for the current and next epoch certificate");

                for epoch in [work_epoch, epoch_to_sign, next_epoch_to_sign] {
                    protocol_parameters_store
                        .save_protocol_parameters(
                            epoch,
                            self.configuration.protocol_parameters.clone(),
                        )
                        .await
                        .map_err(|e| DependenciesBuilderError::Initialization {
                            message: "can not create aggregator runner".to_string(),
                            error: Some(e.into()),
                        })?;
                }
            }
        }
        let dependency_container = Arc::new(self.build_dependency_container().await?);

        let config = AggregatorConfig::new(
            self.configuration.run_interval,
            self.configuration.get_network()?,
            &self.configuration.db_directory.clone(),
        );
        let runtime = AggregatorRuntime::new(
            Duration::from_millis(config.interval),
            None,
            Arc::new(AggregatorRunner::new(dependency_container)),
        )
        .await
        .map_err(|e| DependenciesBuilderError::Initialization {
            message: "Cannot initialize Aggregator runtime.".to_string(),
            error: Some(e.into()),
        })?;

        Ok(runtime)
    }

    /// Create the HTTP route instance
    pub async fn create_http_routes(
        &mut self,
    ) -> Result<impl Filter<Extract = (impl warp::Reply,), Error = warp::Rejection> + Clone> {
        let dependency_container = Arc::new(self.build_dependency_container().await?);

        Ok(router::routes(dependency_container))
    }

    /// Create dependencies for genesis commands
    pub async fn create_genesis_container(&mut self) -> Result<GenesisToolsDependency> {
        let dependencies = GenesisToolsDependency {
            beacon_provider: self.get_beacon_provider().await?,
            certificate_store: self.get_certificate_store().await?,
            certificate_verifier: self.get_certificate_verifier().await?,
            genesis_verifier: self.get_genesis_verifier().await?,
            protocol_parameters_store: self.get_protocol_parameters_store().await?,
            multi_signer: self.get_multi_signer().await?,
        };

        Ok(dependencies)
    }

    /// Create [TickerService] instance.
    pub async fn build_ticker_service(&mut self) -> Result<Arc<dyn TickerService>> {
        let network = self.configuration.get_network()?;
        let chain_observer = self.get_chain_observer().await?;
        let immutable_observer = self.get_immutable_file_observer().await?;

        Ok(Arc::new(MithrilTickerService::new(
            chain_observer,
            immutable_observer,
            network,
        )))
    }

    /// [StakeDistributionService] service
    pub async fn get_ticker_service(&mut self) -> Result<Arc<dyn TickerService>> {
        if self.ticker_service.is_none() {
            self.ticker_service = Some(self.build_ticker_service().await?);
        }

        Ok(self.ticker_service.as_ref().cloned().unwrap())
    }

    /// Create [CertifierService] service
    pub async fn build_certifier_service(&mut self) -> Result<Arc<dyn CertifierService>> {
        let open_message_repository = Arc::new(OpenMessageRepository::new(
            self.get_sqlite_connection().await?,
        ));
        let single_signature_repository = Arc::new(SingleSignatureRepository::new(
            self.get_sqlite_connection().await?,
        ));
        let certificate_repository = Arc::new(CertificateRepository::new(
            self.get_sqlite_connection().await?,
        ));
        let certificate_verifier = self.get_certificate_verifier().await?;
        let genesis_verifier = self.get_genesis_verifier().await?;
        let multi_signer = self.get_multi_signer().await?;
        let logger = self.get_logger().await?;

        Ok(Arc::new(MithrilCertifierService::new(
            open_message_repository,
            single_signature_repository,
            certificate_repository,
            certificate_verifier,
            genesis_verifier,
            multi_signer,
            logger,
        )))
    }

    /// [CertifierService] service
    pub async fn get_certifier_service(&mut self) -> Result<Arc<dyn CertifierService>> {
        if self.certifier_service.is_none() {
            self.certifier_service = Some(self.build_certifier_service().await?);
        }

        Ok(self.certifier_service.as_ref().cloned().unwrap())
    }

    /// Remove the dependencies builder from memory to release Arc.
    pub fn vanish(self) {}
}

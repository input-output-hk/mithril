use anyhow::Context;
use semver::Version;
use slog::Logger;
use std::sync::Arc;
use tokio::{
    sync::{
        mpsc::{UnboundedReceiver, UnboundedSender},
        Mutex, RwLock,
    },
    time::Duration,
};
use warp::Filter;

use mithril_common::{
    api_version::APIVersionProvider,
    cardano_block_scanner::{BlockScanner, CardanoBlockScanner},
    cardano_transactions_preloader::CardanoTransactionsPreloader,
    certificate_chain::{CertificateVerifier, MithrilCertificateVerifier},
    chain_observer::{CardanoCliRunner, ChainObserver, ChainObserverBuilder, FakeObserver},
    chain_reader::{ChainBlockReader, PallasChainReader},
    crypto_helper::{
        ProtocolGenesisSigner, ProtocolGenesisVerificationKey, ProtocolGenesisVerifier,
    },
    digesters::{
        cache::{ImmutableFileDigestCacheProvider, JsonImmutableFileDigestCacheProviderBuilder},
        CardanoImmutableDigester, DumbImmutableFileObserver, ImmutableDigester,
        ImmutableFileObserver, ImmutableFileSystemObserver,
    },
    entities::{CertificatePending, CompressionAlgorithm, Epoch, SignedEntityConfig},
    era::{
        adapters::{EraReaderAdapterBuilder, EraReaderDummyAdapter},
        EraChecker, EraMarker, EraReader, EraReaderAdapter, SupportedEra,
    },
    signable_builder::{
        CardanoImmutableFilesFullSignableBuilder, CardanoTransactionsSignableBuilder,
        MithrilSignableBuilderService, MithrilStakeDistributionSignableBuilder,
        SignableBuilderService, TransactionsImporter,
    },
    signed_entity_type_lock::SignedEntityTypeLock,
    MithrilTickerService, TickerService,
};
use mithril_persistence::{
    database::{repository::CardanoTransactionRepository, ApplicationNodeType, SqlMigration},
    sqlite::{ConnectionBuilder, ConnectionOptions, SqliteConnection, SqliteConnectionPool},
    store::adapter::{MemoryAdapter, SQLiteAdapter, StoreAdapter},
};

use crate::{
    artifact_builder::{
        CardanoImmutableFilesFullArtifactBuilder, CardanoTransactionsArtifactBuilder,
        MithrilStakeDistributionArtifactBuilder,
    },
    configuration::ExecutionEnvironment,
    database::repository::{
        CertificateRepository, EpochSettingStore, OpenMessageRepository, SignedEntityStore,
        SignedEntityStorer, SignerRegistrationStore, SignerStore, SingleSignatureRepository,
        StakePoolStore,
    },
    event_store::{EventMessage, EventStore, TransmitterService},
    http_server::routes::router,
    services::{
        CardanoTransactionsImporter, CertifierService, MessageService, MithrilCertifierService,
        MithrilEpochService, MithrilMessageService, MithrilProverService,
        MithrilSignedEntityService, MithrilStakeDistributionService, ProverService,
        SignedEntityService, StakeDistributionService,
    },
    tools::{CExplorerSignerRetriever, GcpFileUploader, GenesisToolsDependency, SignersImporter},
    AggregatorConfig, AggregatorRunner, AggregatorRuntime, CertificatePendingStore,
    CompressedArchiveSnapshotter, Configuration, DependencyContainer, DumbSnapshotUploader,
    DumbSnapshotter, LocalSnapshotUploader, MithrilSignerRegisterer, MultiSigner, MultiSignerImpl,
    ProtocolParametersStorer, RemoteSnapshotUploader, SnapshotUploader, SnapshotUploaderType,
    Snapshotter, SnapshotterCompressionAlgorithm, VerificationKeyStorer,
};

use super::{DependenciesBuilderError, EpochServiceWrapper, Result};

const SQLITE_FILE: &str = "aggregator.sqlite3";
const SQLITE_FILE_CARDANO_TRANSACTION: &str = "cardano-transaction.sqlite3";

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

    /// Signed entity configuration
    pub signed_entity_config: Option<SignedEntityConfig>,

    /// SQLite database connection
    pub sqlite_connection: Option<Arc<SqliteConnection>>,

    /// Cardano transactions SQLite database connection pool
    pub sqlite_connection_cardano_transaction_pool: Option<Arc<SqliteConnectionPool>>,

    /// Stake Store used by the StakeDistributionService
    /// It shall be a private dependency.
    pub stake_store: Option<Arc<StakePoolStore>>,

    /// Snapshot uploader service.
    pub snapshot_uploader: Option<Arc<dyn SnapshotUploader>>,

    /// Multisigner service.
    pub multi_signer: Option<Arc<RwLock<dyn MultiSigner>>>,

    /// Certificate pending store.
    pub certificate_pending_store: Option<Arc<CertificatePendingStore>>,

    /// Certificate repository.
    pub certificate_repository: Option<Arc<CertificateRepository>>,

    /// Open message repository.
    pub open_message_repository: Option<Arc<OpenMessageRepository>>,

    /// Verification key store.
    pub verification_key_store: Option<Arc<dyn VerificationKeyStorer>>,

    /// Protocol parameter store.
    pub protocol_parameters_store: Option<Arc<dyn ProtocolParametersStorer>>,

    /// Cardano CLI Runner for the [ChainObserver]
    pub cardano_cli_runner: Option<Box<CardanoCliRunner>>,

    /// Chain observer service.
    pub chain_observer: Option<Arc<dyn ChainObserver>>,

    /// Chain block reader
    pub chain_block_reader: Option<Arc<Mutex<dyn ChainBlockReader>>>,

    /// Cardano transactions repository.
    pub transaction_repository: Option<Arc<CardanoTransactionRepository>>,

    /// Cardano block scanner.
    pub block_scanner: Option<Arc<dyn BlockScanner>>,

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

    /// Ticker Service
    pub ticker_service: Option<Arc<dyn TickerService>>,

    /// Signer Store
    pub signer_store: Option<Arc<SignerStore>>,

    /// Signable Builder Service
    pub signable_builder_service: Option<Arc<dyn SignableBuilderService>>,

    /// Signed Entity Service
    pub signed_entity_service: Option<Arc<dyn SignedEntityService>>,

    /// Certifier service
    pub certifier_service: Option<Arc<dyn CertifierService>>,

    /// Epoch service.
    pub epoch_service: Option<EpochServiceWrapper>,

    /// Signed Entity storer
    pub signed_entity_storer: Option<Arc<dyn SignedEntityStorer>>,

    /// HTTP Message service
    pub message_service: Option<Arc<dyn MessageService>>,

    /// Prover service
    pub prover_service: Option<Arc<dyn ProverService>>,

    /// Signed Entity Type Lock
    pub signed_entity_type_lock: Option<Arc<SignedEntityTypeLock>>,

    /// Transactions Importer
    pub transactions_importer: Option<Arc<dyn TransactionsImporter>>,
}

impl DependenciesBuilder {
    /// Create a new clean dependency builder
    pub fn new(configuration: Configuration) -> Self {
        Self {
            configuration,
            signed_entity_config: None,
            sqlite_connection: None,
            sqlite_connection_cardano_transaction_pool: None,
            stake_store: None,
            snapshot_uploader: None,
            multi_signer: None,
            certificate_pending_store: None,
            certificate_repository: None,
            open_message_repository: None,
            verification_key_store: None,
            protocol_parameters_store: None,
            cardano_cli_runner: None,
            chain_observer: None,
            chain_block_reader: None,
            block_scanner: None,
            transaction_repository: None,
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
            signer_store: None,
            signable_builder_service: None,
            signed_entity_service: None,
            certifier_service: None,
            epoch_service: None,
            signed_entity_storer: None,
            message_service: None,
            prover_service: None,
            signed_entity_type_lock: None,
            transactions_importer: None,
        }
    }

    /// Get the signed entity configuration
    pub fn get_signed_entity_config(&mut self) -> Result<SignedEntityConfig> {
        if self.signed_entity_config.is_none() {
            self.signed_entity_config = Some(self.configuration.compute_signed_entity_config()?);
        }

        Ok(self.signed_entity_config.clone().unwrap())
    }

    fn build_sqlite_connection(
        &self,
        sqlite_file_name: &str,
        migrations: Vec<SqlMigration>,
        do_vacuum_database: bool,
    ) -> Result<SqliteConnection> {
        let logger = self.get_logger()?;
        let connection_builder = match self.configuration.environment {
            ExecutionEnvironment::Production => ConnectionBuilder::open_file(
                &self.configuration.get_sqlite_dir().join(sqlite_file_name),
            ),
            _ if self.configuration.data_stores_directory.to_string_lossy() == ":memory:" => {
                ConnectionBuilder::open_memory()
            }
            _ => ConnectionBuilder::open_file(
                &self
                    .configuration
                    .data_stores_directory
                    .join(sqlite_file_name),
            ),
        };

        let connection_options = {
            let mut options = vec![
                ConnectionOptions::EnableForeignKeys,
                ConnectionOptions::EnableWriteAheadLog,
            ];
            if do_vacuum_database {
                options.push(ConnectionOptions::Vacuum);
            }

            options
        };

        let connection = connection_builder
            .with_node_type(ApplicationNodeType::Aggregator)
            .with_options(&connection_options)
            .with_logger(logger.clone())
            .with_migrations(migrations)
            .build()
            .map_err(|e| DependenciesBuilderError::Initialization {
                message: "SQLite initialization: failed to build connection.".to_string(),
                error: Some(e),
            })?;

        Ok(connection)
    }

    async fn drop_sqlite_connections(&self) {
        if let Some(connection) = &self.sqlite_connection {
            let _ = connection.execute("pragma analysis_limit=400; pragma optimize;");
        }

        if let Some(pool) = &self.sqlite_connection_cardano_transaction_pool {
            if let Ok(connection) = pool.connection() {
                let _ = connection.execute("pragma analysis_limit=400; pragma optimize;");
            }
        }
    }

    /// Get SQLite connection
    pub async fn get_sqlite_connection(&mut self) -> Result<Arc<SqliteConnection>> {
        if self.sqlite_connection.is_none() {
            self.sqlite_connection = Some(Arc::new(self.build_sqlite_connection(
                SQLITE_FILE,
                crate::database::migration::get_migrations(),
                true,
            )?));
        }

        Ok(self.sqlite_connection.as_ref().cloned().unwrap())
    }

    async fn build_sqlite_connection_cardano_transaction_pool(
        &mut self,
    ) -> Result<Arc<SqliteConnectionPool>> {
        let connection_pool_size = self
            .configuration
            .cardano_transactions_database_connection_pool_size;
        // little hack to apply migrations to the cardano transaction database
        // todo: add capacity to create a connection pool to the `ConnectionBuilder`
        let _connection = self.build_sqlite_connection(
            SQLITE_FILE_CARDANO_TRANSACTION,
            mithril_persistence::database::cardano_transaction_migration::get_migrations(),
            // Don't vacuum the Cardano transactions database as it can be very large
            false,
        )?;

        let connection_pool = Arc::new(SqliteConnectionPool::build(connection_pool_size, || {
            self.build_sqlite_connection(SQLITE_FILE_CARDANO_TRANSACTION, vec![], false)
                .with_context(|| {
                    "Dependencies Builder can not build SQLite connection for Cardano transactions"
                })
        })?);

        Ok(connection_pool)
    }

    /// Get SQLite connection pool for the cardano transactions store
    pub async fn get_sqlite_connection_cardano_transaction_pool(
        &mut self,
    ) -> Result<Arc<SqliteConnectionPool>> {
        if self.sqlite_connection_cardano_transaction_pool.is_none() {
            self.sqlite_connection_cardano_transaction_pool = Some(
                self.build_sqlite_connection_cardano_transaction_pool()
                    .await?,
            );
        }

        Ok(self
            .sqlite_connection_cardano_transaction_pool
            .as_ref()
            .cloned()
            .unwrap())
    }

    async fn build_stake_store(&mut self) -> Result<Arc<StakePoolStore>> {
        let stake_pool_store = Arc::new(StakePoolStore::new(
            self.get_sqlite_connection().await?,
            self.configuration.safe_epoch_retention_limit(),
        ));

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
                        self.configuration.snapshot_use_cdn_domain,
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
        let multi_signer = MultiSignerImpl::new(self.get_epoch_service().await?);

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

    async fn build_certificate_repository(&mut self) -> Result<Arc<CertificateRepository>> {
        Ok(Arc::new(CertificateRepository::new(
            self.get_sqlite_connection().await?,
        )))
    }

    /// Get a configured [CertificateRepository].
    pub async fn get_certificate_repository(&mut self) -> Result<Arc<CertificateRepository>> {
        if self.certificate_repository.is_none() {
            self.certificate_repository = Some(self.build_certificate_repository().await?);
        }

        Ok(self.certificate_repository.as_ref().cloned().unwrap())
    }

    async fn build_open_message_repository(&mut self) -> Result<Arc<OpenMessageRepository>> {
        Ok(Arc::new(OpenMessageRepository::new(
            self.get_sqlite_connection().await?,
        )))
    }

    /// Get a configured [OpenMessageRepository].
    pub async fn get_open_message_repository(&mut self) -> Result<Arc<OpenMessageRepository>> {
        if self.open_message_repository.is_none() {
            self.open_message_repository = Some(self.build_open_message_repository().await?);
        }

        Ok(self.open_message_repository.as_ref().cloned().unwrap())
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

    async fn build_protocol_parameters_store(
        &mut self,
    ) -> Result<Arc<dyn ProtocolParametersStorer>> {
        let protocol_parameters_store = EpochSettingStore::new(
            self.get_sqlite_connection().await?,
            self.configuration.safe_epoch_retention_limit(),
        );
        let current_epoch = self
            .get_chain_observer()
            .await?
            .get_current_epoch()
            .await
            .map_err(|e| DependenciesBuilderError::Initialization {
                message: "cannot create aggregator runner: failed to retrieve current epoch."
                    .to_string(),
                error: Some(e.into()),
            })?
            .ok_or(DependenciesBuilderError::Initialization {
                message: "cannot build aggregator runner: no epoch returned.".to_string(),
                error: None,
            })?;

        protocol_parameters_store
            .handle_discrepancies_at_startup(current_epoch, &self.configuration.protocol_parameters)
            .await
            .map_err(|e| DependenciesBuilderError::Initialization {
                message: "can not create aggregator runner".to_string(),
                error: Some(e),
            })?;

        Ok(Arc::new(protocol_parameters_store))
    }

    /// Get a configured [ProtocolParametersStorer].
    pub async fn get_protocol_parameters_store(
        &mut self,
    ) -> Result<Arc<dyn ProtocolParametersStorer>> {
        if self.protocol_parameters_store.is_none() {
            self.protocol_parameters_store = Some(self.build_protocol_parameters_store().await?);
        }

        Ok(self.protocol_parameters_store.as_ref().cloned().unwrap())
    }

    async fn build_chain_observer(&mut self) -> Result<Arc<dyn ChainObserver>> {
        let chain_observer: Arc<dyn ChainObserver> = match self.configuration.environment {
            ExecutionEnvironment::Production => {
                let cardano_cli_runner = &self.get_cardano_cli_runner().await?;
                let chain_observer_type = &self.configuration.chain_observer_type;
                let cardano_node_socket_path = &self.configuration.cardano_node_socket_path;
                let cardano_network = &self
                    .configuration
                    .get_network()
                    .with_context(|| "Dependencies Builder can not get Cardano network while building the chain observer")?;
                let chain_observer_builder = ChainObserverBuilder::new(
                    chain_observer_type,
                    cardano_node_socket_path,
                    cardano_network,
                    Some(cardano_cli_runner),
                );

                chain_observer_builder
                    .build()
                    .with_context(|| "Dependencies Builder can not build chain observer")?
            }
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
            self.configuration.get_network().with_context(|| {
                "Dependencies Builder can not get Cardano network while building cardano cli runner"
            })?,
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
        .with_logger(self.get_logger()?)
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

    fn create_logger(&self) -> Result<Logger> {
        Ok(slog_scope::logger())
    }

    /// This method does not cache the logger since it is managed internally by
    /// its own crate.
    pub fn get_logger(&self) -> Result<Logger> {
        self.create_logger()
    }

    async fn build_transaction_repository(&mut self) -> Result<Arc<CardanoTransactionRepository>> {
        let transaction_store = CardanoTransactionRepository::new(
            self.get_sqlite_connection_cardano_transaction_pool()
                .await?,
        );

        Ok(Arc::new(transaction_store))
    }

    /// Transaction repository.
    pub async fn get_transaction_repository(
        &mut self,
    ) -> Result<Arc<CardanoTransactionRepository>> {
        if self.transaction_repository.is_none() {
            self.transaction_repository = Some(self.build_transaction_repository().await?);
        }

        Ok(self.transaction_repository.as_ref().cloned().unwrap())
    }

    async fn build_chain_block_reader(&mut self) -> Result<Arc<Mutex<dyn ChainBlockReader>>> {
        let chain_block_reader = PallasChainReader::new(
            &self.configuration.cardano_node_socket_path,
            self.configuration.get_network()?,
        );

        Ok(Arc::new(Mutex::new(chain_block_reader)))
    }

    /// Chain reader
    pub async fn get_chain_block_reader(&mut self) -> Result<Arc<Mutex<dyn ChainBlockReader>>> {
        if self.chain_block_reader.is_none() {
            self.chain_block_reader = Some(self.build_chain_block_reader().await?);
        }

        Ok(self.chain_block_reader.as_ref().cloned().unwrap())
    }

    async fn build_block_scanner(&mut self) -> Result<Arc<dyn BlockScanner>> {
        let block_scanner = CardanoBlockScanner::new(
            self.get_chain_block_reader().await?,
            self.configuration
                .cardano_transactions_block_streamer_max_roll_forwards_per_poll,
            self.get_logger()?,
        );

        Ok(Arc::new(block_scanner))
    }

    /// Block scanner
    pub async fn get_block_scanner(&mut self) -> Result<Arc<dyn BlockScanner>> {
        if self.block_scanner.is_none() {
            self.block_scanner = Some(self.build_block_scanner().await?);
        }

        Ok(self.block_scanner.as_ref().cloned().unwrap())
    }

    async fn build_immutable_digester(&mut self) -> Result<Arc<dyn ImmutableDigester>> {
        let immutable_digester_cache = match self.configuration.environment {
            ExecutionEnvironment::Production => Some(self.get_immutable_cache_provider().await?),
            _ => None,
        };
        let digester = CardanoImmutableDigester::new(immutable_digester_cache, self.get_logger()?);

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

                let algorithm = match self.configuration.snapshot_compression_algorithm {
                    CompressionAlgorithm::Gzip => SnapshotterCompressionAlgorithm::Gzip,
                    CompressionAlgorithm::Zstandard => self
                        .configuration
                        .zstandard_parameters
                        .unwrap_or_default()
                        .into(),
                };

                Arc::new(CompressedArchiveSnapshotter::new(
                    self.configuration.db_directory.clone(),
                    ongoing_snapshot_directory,
                    algorithm,
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
        let verifier = Arc::new(MithrilCertificateVerifier::new(
            self.get_logger()?,
            self.get_certificate_repository().await?,
        ));

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
                ProtocolGenesisVerificationKey::from_json_hex(
                    &self.configuration.genesis_verification_key,
                )
                .map_err(|e| DependenciesBuilderError::Initialization {
                    message: format!(
                        "Could not decode hex key to build genesis verifier: '{}'",
                        self.configuration.genesis_verification_key
                    ),
                    error: Some(e),
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
            self.get_signer_store().await?,
            self.configuration.safe_epoch_retention_limit(),
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
                error: Some(e.into()),
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
            .get_ticker_service()
            .await?
            .get_current_epoch()
            .await
            .map_err(|e| DependenciesBuilderError::Initialization {
                message: "Error while building EraChecker".to_string(),
                error: Some(e),
            })?;
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
                    error: Some(e),
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

    async fn build_signer_store(&mut self) -> Result<Arc<SignerStore>> {
        let signer_store = Arc::new(SignerStore::new(self.get_sqlite_connection().await?));

        Ok(signer_store)
    }

    /// [SignerStore] service
    pub async fn get_signer_store(&mut self) -> Result<Arc<SignerStore>> {
        match self.signer_store.as_ref().cloned() {
            None => {
                let store = self.build_signer_store().await?;
                self.signer_store = Some(store.clone());
                Ok(store)
            }
            Some(store) => Ok(store),
        }
    }

    async fn build_signable_builder_service(&mut self) -> Result<Arc<dyn SignableBuilderService>> {
        let mithril_stake_distribution_builder =
            Arc::new(MithrilStakeDistributionSignableBuilder::default());
        let immutable_signable_builder = Arc::new(CardanoImmutableFilesFullSignableBuilder::new(
            self.get_immutable_digester().await?,
            &self.configuration.db_directory,
            self.get_logger()?,
        ));
        let transactions_importer = self.get_transactions_importer().await?;
        let block_range_root_retriever = self.get_transaction_repository().await?;
        let cardano_transactions_builder = Arc::new(CardanoTransactionsSignableBuilder::new(
            transactions_importer,
            block_range_root_retriever,
            self.get_logger()?,
        ));
        let signable_builder_service = Arc::new(MithrilSignableBuilderService::new(
            mithril_stake_distribution_builder,
            immutable_signable_builder,
            cardano_transactions_builder,
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
        let epoch_service = self.get_epoch_service().await?;
        let mithril_stake_distribution_artifact_builder =
            Arc::new(MithrilStakeDistributionArtifactBuilder::new(epoch_service));
        let snapshotter = self.build_snapshotter().await?;
        let snapshot_uploader = self.build_snapshot_uploader().await?;
        let cardano_node_version = Version::parse(&self.configuration.cardano_node_version)
            .map_err(|e| DependenciesBuilderError::Initialization { message: format!("Could not parse configuration setting 'cardano_node_version' value '{}' as Semver.", self.configuration.cardano_node_version), error: Some(e.into()) })?;
        let cardano_immutable_files_full_artifact_builder =
            Arc::new(CardanoImmutableFilesFullArtifactBuilder::new(
                &cardano_node_version,
                snapshotter,
                snapshot_uploader,
                self.configuration.snapshot_compression_algorithm,
            ));
        let prover_service = self.get_prover_service().await?;
        let cardano_transactions_artifact_builder = Arc::new(
            CardanoTransactionsArtifactBuilder::new(prover_service.clone()),
        );
        let signed_entity_service = Arc::new(MithrilSignedEntityService::new(
            signed_entity_storer,
            mithril_stake_distribution_artifact_builder,
            cardano_immutable_files_full_artifact_builder,
            cardano_transactions_artifact_builder,
        ));

        // Compute the cache pool for prover service
        // This is done here to avoid circular dependencies between the prover service and the signed entity service
        // TODO: Make this part of a warmup phase of the aggregator?
        if let Some(signed_entity) = signed_entity_service
            .get_last_cardano_transaction_snapshot()
            .await?
        {
            prover_service
                .compute_cache(signed_entity.artifact.block_number)
                .await?;
        }

        Ok(signed_entity_service)
    }

    /// [SignedEntityService] service
    pub async fn get_signed_entity_service(&mut self) -> Result<Arc<dyn SignedEntityService>> {
        if self.signed_entity_service.is_none() {
            self.signed_entity_service = Some(self.build_signed_entity_service().await?);
        }

        Ok(self.signed_entity_service.as_ref().cloned().unwrap())
    }

    async fn build_epoch_service(&mut self) -> Result<EpochServiceWrapper> {
        let verification_key_store = self.get_verification_key_store().await?;
        let protocol_parameters_store = self.get_protocol_parameters_store().await?;

        let epoch_service = Arc::new(RwLock::new(MithrilEpochService::new(
            self.configuration.protocol_parameters.clone(),
            protocol_parameters_store,
            verification_key_store,
        )));

        Ok(epoch_service)
    }

    /// [EpochService][crate::services::EpochService] service
    pub async fn get_epoch_service(&mut self) -> Result<EpochServiceWrapper> {
        if self.epoch_service.is_none() {
            self.epoch_service = Some(self.build_epoch_service().await?);
        }

        Ok(self.epoch_service.as_ref().cloned().unwrap())
    }

    async fn build_signed_entity_storer(&mut self) -> Result<Arc<dyn SignedEntityStorer>> {
        let signed_entity_storer =
            Arc::new(SignedEntityStore::new(self.get_sqlite_connection().await?));

        Ok(signed_entity_storer)
    }

    /// [SignedEntityStorer] service
    pub async fn get_signed_entity_storer(&mut self) -> Result<Arc<dyn SignedEntityStorer>> {
        if self.signed_entity_storer.is_none() {
            self.signed_entity_storer = Some(self.build_signed_entity_storer().await?);
        }

        Ok(self.signed_entity_storer.as_ref().cloned().unwrap())
    }

    async fn build_signed_entity_lock(&mut self) -> Result<Arc<SignedEntityTypeLock>> {
        let signed_entity_type_lock = Arc::new(SignedEntityTypeLock::default());
        Ok(signed_entity_type_lock)
    }

    async fn get_signed_entity_lock(&mut self) -> Result<Arc<SignedEntityTypeLock>> {
        if self.signed_entity_type_lock.is_none() {
            self.signed_entity_type_lock = Some(self.build_signed_entity_lock().await?);
        }

        Ok(self.signed_entity_type_lock.as_ref().cloned().unwrap())
    }

    async fn build_transactions_importer(&mut self) -> Result<Arc<dyn TransactionsImporter>> {
        let transactions_importer = Arc::new(CardanoTransactionsImporter::new(
            self.get_block_scanner().await?,
            self.get_transaction_repository().await?,
            &self.configuration.db_directory,
            self.get_logger()?,
        ));

        Ok(transactions_importer)
    }

    async fn get_transactions_importer(&mut self) -> Result<Arc<dyn TransactionsImporter>> {
        if self.transactions_importer.is_none() {
            self.transactions_importer = Some(self.build_transactions_importer().await?);
        }

        Ok(self.transactions_importer.as_ref().cloned().unwrap())
    }

    /// Return an unconfigured [DependencyContainer]
    pub async fn build_dependency_container(&mut self) -> Result<DependencyContainer> {
        let dependency_manager = DependencyContainer {
            config: self.configuration.clone(),
            signed_entity_config: self.get_signed_entity_config()?,
            sqlite_connection: self.get_sqlite_connection().await?,
            sqlite_connection_cardano_transaction_pool: self
                .get_sqlite_connection_cardano_transaction_pool()
                .await?,
            stake_store: self.get_stake_store().await?,
            snapshot_uploader: self.get_snapshot_uploader().await?,
            multi_signer: self.get_multi_signer().await?,
            certificate_pending_store: self.get_certificate_pending_store().await?,
            certificate_repository: self.get_certificate_repository().await?,
            open_message_repository: self.get_open_message_repository().await?,
            verification_key_store: self.get_verification_key_store().await?,
            protocol_parameters_store: self.get_protocol_parameters_store().await?,
            chain_observer: self.get_chain_observer().await?,
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
            signer_recorder: self.get_signer_store().await?,
            signable_builder_service: self.get_signable_builder_service().await?,
            signed_entity_service: self.get_signed_entity_service().await?,
            certifier_service: self.get_certifier_service().await?,
            epoch_service: self.get_epoch_service().await?,
            ticker_service: self.get_ticker_service().await?,
            signed_entity_storer: self.get_signed_entity_storer().await?,
            signer_getter: self.get_signer_store().await?,
            message_service: self.get_message_service().await?,
            block_scanner: self.get_block_scanner().await?,
            transaction_store: self.get_transaction_repository().await?,
            prover_service: self.get_prover_service().await?,
            signed_entity_type_lock: self.get_signed_entity_lock().await?,
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
        let dependency_container = Arc::new(self.build_dependency_container().await?);

        let config = AggregatorConfig::new(
            Duration::from_millis(self.configuration.run_interval),
            self.get_signed_entity_config()?,
        );
        let runtime = AggregatorRuntime::new(
            config,
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

    /// Create a [CardanoTransactionsPreloader] instance.
    pub async fn create_cardano_transactions_preloader(
        &mut self,
    ) -> Result<Arc<CardanoTransactionsPreloader>> {
        let cardano_transactions_preloader = CardanoTransactionsPreloader::new(
            self.get_signed_entity_lock().await?,
            self.get_transactions_importer().await?,
            self.configuration
                .cardano_transactions_signing_config
                .security_parameter,
            self.get_chain_observer().await?,
            self.get_logger()?,
        );

        Ok(Arc::new(cardano_transactions_preloader))
    }

    /// Create dependencies for genesis commands
    pub async fn create_genesis_container(&mut self) -> Result<GenesisToolsDependency> {
        let network = self.configuration.get_network().with_context(|| {
            "Dependencies Builder can not get Cardano network while building genesis container"
        })?;

        let dependencies = GenesisToolsDependency {
            network,
            ticker_service: self.get_ticker_service().await?,
            certificate_repository: self.get_certificate_repository().await?,
            certificate_verifier: self.get_certificate_verifier().await?,
            genesis_verifier: self.get_genesis_verifier().await?,
            protocol_parameters_store: self.get_protocol_parameters_store().await?,
            verification_key_store: self.get_verification_key_store().await?,
        };

        Ok(dependencies)
    }

    /// Create a [SignersImporter] instance.
    pub async fn create_signer_importer(
        &mut self,
        cexplorer_pools_url: &str,
    ) -> Result<SignersImporter> {
        let retriever =
            CExplorerSignerRetriever::new(cexplorer_pools_url, Some(Duration::from_secs(30)))?;
        let persister = self.get_signer_store().await?;

        Ok(SignersImporter::new(Arc::new(retriever), persister))
    }

    /// Create [TickerService] instance.
    pub async fn build_ticker_service(&mut self) -> Result<Arc<dyn TickerService>> {
        let chain_observer = self.get_chain_observer().await?;
        let immutable_observer = self.get_immutable_file_observer().await?;

        Ok(Arc::new(MithrilTickerService::new(
            chain_observer,
            immutable_observer,
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
        let cardano_network = self.configuration.get_network().with_context(|| {
            "Dependencies Builder can not get Cardano network while building the chain observer"
        })?;
        let open_message_repository = self.get_open_message_repository().await?;
        let single_signature_repository = Arc::new(SingleSignatureRepository::new(
            self.get_sqlite_connection().await?,
        ));
        let certificate_repository = self.get_certificate_repository().await?;
        let certificate_verifier = self.get_certificate_verifier().await?;
        let genesis_verifier = self.get_genesis_verifier().await?;
        let multi_signer = self.get_multi_signer().await?;
        let ticker_service = self.get_ticker_service().await?;
        let epoch_service = self.get_epoch_service().await?;
        let logger = self.get_logger()?;

        Ok(Arc::new(MithrilCertifierService::new(
            cardano_network,
            open_message_repository,
            single_signature_repository,
            certificate_repository,
            certificate_verifier,
            genesis_verifier,
            multi_signer,
            ticker_service,
            epoch_service,
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

    /// build HTTP message service
    pub async fn build_message_service(&mut self) -> Result<Arc<dyn MessageService>> {
        let certificate_repository = Arc::new(CertificateRepository::new(
            self.get_sqlite_connection().await?,
        ));
        let signed_entity_storer = self.get_signed_entity_storer().await?;
        let service = MithrilMessageService::new(certificate_repository, signed_entity_storer);

        Ok(Arc::new(service))
    }

    /// [MessageService] service
    pub async fn get_message_service(&mut self) -> Result<Arc<dyn MessageService>> {
        if self.message_service.is_none() {
            self.message_service = Some(self.build_message_service().await?);
        }

        Ok(self.message_service.as_ref().cloned().unwrap())
    }

    /// Build Prover service
    pub async fn build_prover_service(&mut self) -> Result<Arc<dyn ProverService>> {
        let mk_map_pool_size = self
            .configuration
            .cardano_transactions_prover_cache_pool_size;
        let transaction_retriever = self.get_transaction_repository().await?;
        let block_range_root_retriever = self.get_transaction_repository().await?;
        let logger = self.get_logger()?;
        let prover_service = MithrilProverService::new(
            transaction_retriever,
            block_range_root_retriever,
            mk_map_pool_size,
            logger,
        );

        Ok(Arc::new(prover_service))
    }

    /// [ProverService] service
    pub async fn get_prover_service(&mut self) -> Result<Arc<dyn ProverService>> {
        if self.prover_service.is_none() {
            self.prover_service = Some(self.build_prover_service().await?);
        }

        Ok(self.prover_service.as_ref().cloned().unwrap())
    }

    /// Remove the dependencies builder from memory to release Arc instances.
    pub async fn vanish(self) {
        self.drop_sqlite_connections().await;
    }
}

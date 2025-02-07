mod enablers;
mod protocol;
mod support;

use anyhow::Context;
use slog::{debug, Logger};
use std::{collections::BTreeSet, path::PathBuf, sync::Arc};
use tokio::{
    sync::{
        mpsc::{UnboundedReceiver, UnboundedSender},
        Mutex,
    },
    time::Duration,
};
use warp::Filter;

use mithril_common::{
    api_version::APIVersionProvider,
    cardano_block_scanner::BlockScanner,
    certificate_chain::CertificateVerifier,
    chain_observer::{CardanoCliRunner, ChainObserver},
    chain_reader::ChainBlockReader,
    crypto_helper::ProtocolGenesisVerifier,
    digesters::{
        cache::ImmutableFileDigestCacheProvider, ImmutableDigester, ImmutableFileObserver,
    },
    entities::{Epoch, SignedEntityTypeDiscriminants},
    era::{
        adapters::{EraReaderAdapterBuilder, EraReaderDummyAdapter},
        EraChecker, EraMarker, EraReader, EraReaderAdapter, SupportedEra,
    },
    signable_builder::{SignableBuilderService, SignableSeedBuilder, TransactionsImporter},
    signed_entity_type_lock::SignedEntityTypeLock,
    TickerService,
};
use mithril_persistence::{
    database::{repository::CardanoTransactionRepository, ApplicationNodeType, SqlMigration},
    sqlite::{ConnectionBuilder, ConnectionOptions, SqliteConnection, SqliteConnectionPool},
};

use super::{DependenciesBuilderError, EpochServiceWrapper, Result};
use crate::{
    configuration::ExecutionEnvironment,
    database::repository::{
        CertificatePendingRepository, CertificateRepository, EpochSettingsStore,
        ImmutableFileDigestRepository, OpenMessageRepository, SignedEntityStore,
        SignedEntityStorer, SignerRegistrationStore, SignerStore, StakePoolStore,
    },
    entities::AggregatorEpochSettings,
    event_store::{EventMessage, EventStore, TransmitterService},
    file_uploaders::FileUploader,
    http_server::routes::router::{self, RouterConfig, RouterState},
    services::{
        AggregatorUpkeepService, CertifierService, MessageService, ProverService,
        SignedEntityService, Snapshotter, StakeDistributionService, UpkeepService, UsageReporter,
    },
    store::CertificatePendingStorer,
    tools::{CExplorerSignerRetriever, GenesisToolsDependency, SignersImporter},
    AggregatorConfig, AggregatorRunner, AggregatorRuntime, Configuration, DependencyContainer,
    EpochSettingsStorer, ImmutableFileDigestMapper, MetricsService, MithrilSignerRegisterer,
    MultiSigner, SingleSignatureAuthenticator, VerificationKeyStorer,
};

const SQLITE_FILE: &str = "aggregator.sqlite3";
const SQLITE_FILE_CARDANO_TRANSACTION: &str = "cardano-transaction.sqlite3";
const SQLITE_MONITORING_FILE: &str = "monitoring.sqlite3";
const CARDANO_DB_ARTIFACTS_DIR: &str = "cardano-database";
const SNAPSHOT_ARTIFACTS_DIR: &str = "cardano-immutable-files-full";

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

    /// Application root logger
    pub root_logger: Logger,

    /// SQLite database connection
    pub sqlite_connection: Option<Arc<SqliteConnection>>,

    /// Event store SQLite database connection
    pub sqlite_connection_event_store: Option<Arc<SqliteConnection>>,

    /// Cardano transactions SQLite database connection pool
    pub sqlite_connection_cardano_transaction_pool: Option<Arc<SqliteConnectionPool>>,

    /// Stake Store used by the StakeDistributionService
    /// It shall be a private dependency.
    pub stake_store: Option<Arc<StakePoolStore>>,

    /// Snapshot uploader service.
    pub snapshot_uploader: Option<Arc<dyn FileUploader>>,

    /// Multisigner service.
    pub multi_signer: Option<Arc<dyn MultiSigner>>,

    /// Certificate pending store.
    pub certificate_pending_store: Option<Arc<dyn CertificatePendingStorer>>,

    /// Certificate repository.
    pub certificate_repository: Option<Arc<CertificateRepository>>,

    /// Open message repository.
    pub open_message_repository: Option<Arc<OpenMessageRepository>>,

    /// Verification key store.
    pub verification_key_store: Option<Arc<dyn VerificationKeyStorer>>,

    /// Epoch settings store.
    pub epoch_settings_store: Option<Arc<EpochSettingsStore>>,

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

    /// Immutable file digest mapper service.
    pub immutable_file_digest_mapper: Option<Arc<dyn ImmutableFileDigestMapper>>,

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

    /// Signable Seed Builder
    pub signable_seed_builder: Option<Arc<dyn SignableSeedBuilder>>,

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

    /// Upkeep service
    pub upkeep_service: Option<Arc<dyn UpkeepService>>,

    /// Single signer authenticator
    pub single_signer_authenticator: Option<Arc<SingleSignatureAuthenticator>>,

    /// Metrics service
    pub metrics_service: Option<Arc<MetricsService>>,
}

impl DependenciesBuilder {
    /// Create a new clean dependency builder
    pub fn new(root_logger: Logger, configuration: Configuration) -> Self {
        Self {
            configuration,
            root_logger,
            sqlite_connection: None,
            sqlite_connection_event_store: None,
            sqlite_connection_cardano_transaction_pool: None,
            stake_store: None,
            snapshot_uploader: None,
            multi_signer: None,
            certificate_pending_store: None,
            certificate_repository: None,
            open_message_repository: None,
            verification_key_store: None,
            epoch_settings_store: None,
            cardano_cli_runner: None,
            chain_observer: None,
            chain_block_reader: None,
            block_scanner: None,
            transaction_repository: None,
            immutable_digester: None,
            immutable_file_observer: None,
            immutable_cache_provider: None,
            immutable_file_digest_mapper: None,
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
            signable_seed_builder: None,
            signable_builder_service: None,
            signed_entity_service: None,
            certifier_service: None,
            epoch_service: None,
            signed_entity_storer: None,
            message_service: None,
            prover_service: None,
            signed_entity_type_lock: None,
            transactions_importer: None,
            upkeep_service: None,
            single_signer_authenticator: None,
            metrics_service: None,
        }
    }

    /// Get the allowed signed entity types discriminants
    fn get_allowed_signed_entity_types_discriminants(
        &self,
    ) -> Result<BTreeSet<SignedEntityTypeDiscriminants>> {
        let allowed_discriminants = self
            .configuration
            .compute_allowed_signed_entity_types_discriminants()?;

        Ok(allowed_discriminants)
    }

    fn build_sqlite_connection(
        &self,
        sqlite_file_name: &str,
        migrations: Vec<SqlMigration>,
    ) -> Result<SqliteConnection> {
        let logger = self.root_logger();
        let connection_builder = match self.configuration.environment {
            ExecutionEnvironment::Test
                if self.configuration.data_stores_directory.to_string_lossy() == ":memory:" =>
            {
                ConnectionBuilder::open_memory()
            }
            _ => ConnectionBuilder::open_file(
                &self.configuration.get_sqlite_dir().join(sqlite_file_name),
            ),
        };

        let connection = connection_builder
            .with_node_type(ApplicationNodeType::Aggregator)
            .with_options(&[
                ConnectionOptions::EnableForeignKeys,
                ConnectionOptions::EnableWriteAheadLog,
            ])
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
            )?));
        }

        Ok(self.sqlite_connection.as_ref().cloned().unwrap())
    }
    /// Get EventStore SQLite connection
    pub async fn get_event_store_sqlite_connection(&mut self) -> Result<Arc<SqliteConnection>> {
        if self.sqlite_connection_event_store.is_none() {
            self.sqlite_connection_event_store = Some(Arc::new(self.build_sqlite_connection(
                SQLITE_MONITORING_FILE,
                crate::event_store::database::migration::get_migrations(),
            )?));
        }

        Ok(self
            .sqlite_connection_event_store
            .as_ref()
            .cloned()
            .unwrap())
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
        )?;

        let connection_pool = Arc::new(SqliteConnectionPool::build(connection_pool_size, || {
            self.build_sqlite_connection(SQLITE_FILE_CARDANO_TRANSACTION, vec![])
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

    async fn build_certificate_pending_storer(
        &mut self,
    ) -> Result<Arc<dyn CertificatePendingStorer>> {
        Ok(Arc::new(CertificatePendingRepository::new(
            self.get_sqlite_connection().await?,
        )))
    }

    /// Get a configured [CertificatePendingStorer].
    pub async fn get_certificate_pending_storer(
        &mut self,
    ) -> Result<Arc<dyn CertificatePendingStorer>> {
        if self.certificate_pending_store.is_none() {
            self.certificate_pending_store = Some(self.build_certificate_pending_storer().await?);
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

    async fn build_epoch_settings_store(&mut self) -> Result<Arc<EpochSettingsStore>> {
        let logger = self.root_logger();
        let epoch_settings_store = EpochSettingsStore::new(
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

        {
            // Temporary fix, should be removed
            // Replace empty JSON values '{}' injected with Migration #28
            let cardano_signing_config = self
                .configuration
                .cardano_transactions_signing_config
                .clone();
            #[allow(deprecated)]
            epoch_settings_store
                .replace_cardano_signing_config_empty_values(cardano_signing_config)?;
        }

        let epoch_settings_configuration = self.get_epoch_settings_configuration()?;
        debug!(
            logger,
            "Handle discrepancies at startup of epoch settings store, will record epoch settings from the configuration for epoch {current_epoch}";
            "epoch_settings_configuration" => ?epoch_settings_configuration,
        );
        epoch_settings_store
            .handle_discrepancies_at_startup(current_epoch, &epoch_settings_configuration)
            .await
            .map_err(|e| DependenciesBuilderError::Initialization {
                message: "can not create aggregator runner".to_string(),
                error: Some(e),
            })?;

        Ok(Arc::new(epoch_settings_store))
    }

    /// Get a configured [EpochSettingsStorer].
    pub async fn get_epoch_settings_store(&mut self) -> Result<Arc<EpochSettingsStore>> {
        if self.epoch_settings_store.is_none() {
            self.epoch_settings_store = Some(self.build_epoch_settings_store().await?);
        }

        Ok(self.epoch_settings_store.as_ref().cloned().unwrap())
    }

    async fn build_immutable_cache_provider(
        &mut self,
    ) -> Result<Arc<dyn ImmutableFileDigestCacheProvider>> {
        let cache_provider =
            ImmutableFileDigestRepository::new(self.get_sqlite_connection().await?);
        if self.configuration.reset_digests_cache {
            cache_provider
                .reset()
                .await
                .with_context(|| "Failure occurred when resetting immutable file digest cache")?;
        }

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

    /// Return a copy of the root logger.
    pub fn root_logger(&self) -> Logger {
        self.root_logger.clone()
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

    async fn build_immutable_file_digest_mapper(
        &mut self,
    ) -> Result<Arc<dyn ImmutableFileDigestMapper>> {
        let mapper = ImmutableFileDigestRepository::new(self.get_sqlite_connection().await?);

        Ok(Arc::new(mapper))
    }

    /// Immutable digest mapper.
    pub async fn get_immutable_file_digest_mapper(
        &mut self,
    ) -> Result<Arc<dyn ImmutableFileDigestMapper>> {
        if self.immutable_file_digest_mapper.is_none() {
            self.immutable_file_digest_mapper =
                Some(self.build_immutable_file_digest_mapper().await?);
        }

        Ok(self.immutable_file_digest_mapper.as_ref().cloned().unwrap())
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
        let event_transmitter = Arc::new(TransmitterService::new(sender, self.root_logger()));

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

    fn get_cardano_db_artifacts_dir(&self) -> Result<PathBuf> {
        let cardano_db_artifacts_dir = self
            .configuration
            .get_snapshot_dir()?
            .join(CARDANO_DB_ARTIFACTS_DIR);

        if !cardano_db_artifacts_dir.exists() {
            std::fs::create_dir(&cardano_db_artifacts_dir).map_err(|e| {
                DependenciesBuilderError::Initialization {
                    message: format!("Cannot create '{cardano_db_artifacts_dir:?}' directory."),
                    error: Some(e.into()),
                }
            })?;
        }

        Ok(cardano_db_artifacts_dir)
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

    async fn build_upkeep_service(&mut self) -> Result<Arc<dyn UpkeepService>> {
        let stake_pool_pruning_task = self.get_stake_store().await?;
        let epoch_settings_pruning_task = self.get_epoch_settings_store().await?;
        let mithril_registerer_pruning_task = self.get_mithril_registerer().await?;

        let upkeep_service = Arc::new(AggregatorUpkeepService::new(
            self.get_sqlite_connection().await?,
            self.get_sqlite_connection_cardano_transaction_pool()
                .await?,
            self.get_event_store_sqlite_connection().await?,
            self.get_signed_entity_lock().await?,
            vec![
                stake_pool_pruning_task,
                epoch_settings_pruning_task,
                mithril_registerer_pruning_task,
            ],
            self.root_logger(),
        ));

        Ok(upkeep_service)
    }

    async fn get_upkeep_service(&mut self) -> Result<Arc<dyn UpkeepService>> {
        if self.upkeep_service.is_none() {
            self.upkeep_service = Some(self.build_upkeep_service().await?);
        }

        Ok(self.upkeep_service.as_ref().cloned().unwrap())
    }

    fn get_epoch_settings_configuration(&mut self) -> Result<AggregatorEpochSettings> {
        let epoch_settings = AggregatorEpochSettings {
            protocol_parameters: self.configuration.protocol_parameters.clone(),
            cardano_transactions_signing_config: self
                .configuration
                .cardano_transactions_signing_config
                .clone(),
        };
        Ok(epoch_settings)
    }

    /// Create a [MetricsService] instance.
    async fn build_metrics_service(&self) -> Result<Arc<MetricsService>> {
        let metrics_service = MetricsService::new(self.root_logger())?;

        Ok(Arc::new(metrics_service))
    }

    /// [MetricsService] service
    pub async fn get_metrics_service(&mut self) -> Result<Arc<MetricsService>> {
        if self.metrics_service.is_none() {
            self.metrics_service = Some(self.build_metrics_service().await?);
        }

        Ok(self.metrics_service.as_ref().cloned().unwrap())
    }

    /// Create a [UsageReporter] instance.
    pub async fn create_usage_reporter(&mut self) -> Result<UsageReporter> {
        let usage_reporter = UsageReporter::new(
            self.get_event_transmitter().await?,
            self.get_metrics_service().await?,
            self.root_logger(),
        );

        Ok(usage_reporter)
    }

    /// Return an unconfigured [DependencyContainer]
    pub async fn build_dependency_container(&mut self) -> Result<DependencyContainer> {
        #[allow(deprecated)]
        let dependency_manager = DependencyContainer {
            config: self.configuration.clone(),
            root_logger: self.root_logger(),
            sqlite_connection: self.get_sqlite_connection().await?,
            sqlite_connection_cardano_transaction_pool: self
                .get_sqlite_connection_cardano_transaction_pool()
                .await?,
            stake_store: self.get_stake_store().await?,
            snapshot_uploader: self.get_snapshot_uploader().await?,
            multi_signer: self.get_multi_signer().await?,
            certificate_pending_store: self.get_certificate_pending_storer().await?,
            certificate_repository: self.get_certificate_repository().await?,
            open_message_repository: self.get_open_message_repository().await?,
            verification_key_store: self.get_verification_key_store().await?,
            epoch_settings_storer: self.get_epoch_settings_store().await?,
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
            upkeep_service: self.get_upkeep_service().await?,
            single_signer_authenticator: self.get_single_signature_authenticator().await?,
            metrics_service: self.get_metrics_service().await?,
        };

        Ok(dependency_manager)
    }

    /// Create dependencies for the [EventStore] task.
    pub async fn create_event_store(&mut self) -> Result<EventStore> {
        let event_store = EventStore::new(
            self.get_event_transmitter_receiver().await?,
            self.get_event_store_sqlite_connection().await?,
            self.root_logger(),
        );

        Ok(event_store)
    }

    /// Create the AggregatorRunner
    pub async fn create_aggregator_runner(&mut self) -> Result<AggregatorRuntime> {
        let dependency_container = Arc::new(self.build_dependency_container().await?);

        let config = AggregatorConfig::new(Duration::from_millis(self.configuration.run_interval));
        let runtime = AggregatorRuntime::new(
            config,
            None,
            Arc::new(AggregatorRunner::new(dependency_container)),
            self.root_logger(),
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
        let snapshot_dir = self.configuration.get_snapshot_dir()?;
        let router_state = RouterState::new(
            dependency_container.clone(),
            RouterConfig {
                network: self.configuration.get_network()?,
                server_url: self.configuration.get_server_url()?,
                allowed_discriminants: self.get_allowed_signed_entity_types_discriminants()?,
                cardano_transactions_prover_max_hashes_allowed_by_request: self
                    .configuration
                    .cardano_transactions_prover_max_hashes_allowed_by_request,
                cardano_transactions_signing_config: self
                    .configuration
                    .cardano_transactions_signing_config
                    .clone(),
                cardano_db_artifacts_directory: self.get_cardano_db_artifacts_dir()?,
                snapshot_directory: snapshot_dir.join(SNAPSHOT_ARTIFACTS_DIR),
                cardano_node_version: self.configuration.cardano_node_version.clone(),
                allow_http_serve_directory: self.configuration.allow_http_serve_directory(),
            },
        );

        Ok(router::routes(Arc::new(router_state)))
    }

    /// Create dependencies for genesis commands
    pub async fn create_genesis_container(&mut self) -> Result<GenesisToolsDependency> {
        let network = self.configuration.get_network().with_context(|| {
            "Dependencies Builder can not get Cardano network while building genesis container"
        })?;

        // Disable store pruning for genesis commands
        self.configuration.store_retention_limit = None;

        let dependencies = GenesisToolsDependency {
            network,
            ticker_service: self.get_ticker_service().await?,
            certificate_repository: self.get_certificate_repository().await?,
            certificate_verifier: self.get_certificate_verifier().await?,
            genesis_verifier: self.get_genesis_verifier().await?,
            epoch_settings_storer: self.get_epoch_settings_store().await?,
            verification_key_store: self.get_verification_key_store().await?,
        };

        Ok(dependencies)
    }

    /// Create a [SignersImporter] instance.
    pub async fn create_signer_importer(
        &mut self,
        cexplorer_pools_url: &str,
    ) -> Result<SignersImporter> {
        let retriever = CExplorerSignerRetriever::new(
            cexplorer_pools_url,
            Some(Duration::from_secs(30)),
            self.root_logger(),
        )?;
        let persister = self.get_signer_store().await?;

        Ok(SignersImporter::new(
            Arc::new(retriever),
            persister,
            self.root_logger(),
        ))
    }

    /// Remove the dependencies builder from memory to release Arc instances.
    pub async fn vanish(self) {
        self.drop_sqlite_connections().await;
    }
}

#[cfg(test)]
impl DependenciesBuilder {
    pub(crate) fn new_with_stdout_logger(configuration: Configuration) -> Self {
        Self::new(crate::test_tools::TestLogger::stdout(), configuration)
    }
}

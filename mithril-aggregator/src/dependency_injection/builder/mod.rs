mod enablers;
mod protocol;
mod support;

use anyhow::Context;
use slog::Logger;
use std::{path::PathBuf, sync::Arc};
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
    era::{EraChecker, EraReader, EraReaderAdapter},
    signable_builder::{SignableBuilderService, SignableSeedBuilder, TransactionsImporter},
    TickerService,
};
use mithril_persistence::{
    database::repository::CardanoTransactionRepository,
    sqlite::{SqliteConnection, SqliteConnectionPool},
};
use mithril_signed_entity_lock::SignedEntityTypeLock;

use super::{DependenciesBuilderError, EpochServiceWrapper, Result};
use crate::{
    database::repository::{
        CertificateRepository, EpochSettingsStore, OpenMessageRepository, SignedEntityStorer,
        SignerStore, StakePoolStore,
    },
    event_store::{EventMessage, TransmitterService},
    file_uploaders::FileUploader,
    http_server::routes::router::{self, RouterConfig, RouterState},
    services::{
        CertifierService, MessageService, ProverService, SignedEntityService, Snapshotter,
        StakeDistributionService, UpkeepService,
    },
    store::CertificatePendingStorer,
    tools::GenesisToolsDependency,
    AggregatorConfig, AggregatorRunner, AggregatorRuntime, Configuration, DependencyContainer,
    ImmutableFileDigestMapper, MetricsService, MithrilSignerRegisterer, MultiSigner,
    SingleSignatureAuthenticator, VerificationKeyStorer,
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
                allowed_discriminants: self
                    .configuration
                    .compute_allowed_signed_entity_types_discriminants()?,
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

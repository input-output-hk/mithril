use anyhow::{anyhow, Context};
use async_trait::async_trait;
use std::{fs, sync::Arc, time::Duration};

use mithril_common::{
    api_version::APIVersionProvider,
    cardano_block_scanner::CardanoBlockScanner,
    chain_observer::{CardanoCliRunner, ChainObserver, ChainObserverBuilder, ChainObserverType},
    crypto_helper::{OpCert, ProtocolPartyId, SerDeShelleyFileFormat},
    digesters::{
        cache::{ImmutableFileDigestCacheProvider, JsonImmutableFileDigestCacheProviderBuilder},
        CardanoImmutableDigester, ImmutableDigester, ImmutableFileObserver,
        ImmutableFileSystemObserver,
    },
    era::{EraChecker, EraReader},
    signable_builder::{
        CardanoImmutableFilesFullSignableBuilder, CardanoTransactionsSignableBuilder,
        MithrilSignableBuilderService, MithrilStakeDistributionSignableBuilder,
        SignableBuilderService,
    },
    StdResult, TimePointProvider, TimePointProviderImpl,
};
use mithril_persistence::{
    database::{repository::CardanoTransactionRepository, ApplicationNodeType, SqlMigration},
    sqlite::{ConnectionBuilder, SqliteConnection},
    store::{adapter::SQLiteAdapter, StakeStore},
};

use crate::{
    aggregator_client::AggregatorClient, metrics::MetricsService, single_signer::SingleSigner,
    AggregatorHTTPClient, CardanoTransactionsImporter, Configuration, MithrilSingleSigner,
    ProtocolInitializerStore, ProtocolInitializerStorer, TransactionsImporterWithPruner,
    HTTP_REQUEST_TIMEOUT_DURATION, SQLITE_FILE, SQLITE_FILE_CARDANO_TRANSACTION,
};

type StakeStoreService = Arc<StakeStore>;
type CertificateHandlerService = Arc<dyn AggregatorClient>;
type ChainObserverService = Arc<dyn ChainObserver>;
type DigesterService = Arc<dyn ImmutableDigester>;
type SingleSignerService = Arc<dyn SingleSigner>;
type TimePointProviderService = Arc<dyn TimePointProvider>;
type ProtocolInitializerStoreService = Arc<dyn ProtocolInitializerStorer>;

/// The ServiceBuilder is intended to manage Services instance creation.
/// The goal of this is to put all this code out of the way of business code.
#[async_trait]
pub trait ServiceBuilder {
    /// Create a SignerService instance.
    async fn build(&self) -> StdResult<SignerServices>;
}

/// Create a SignerService instance for Production environment.
pub struct ProductionServiceBuilder<'a> {
    config: &'a Configuration,
    chain_observer_builder: fn(&Configuration) -> StdResult<ChainObserverService>,
    immutable_file_observer_builder:
        fn(&Configuration) -> StdResult<Arc<dyn ImmutableFileObserver>>,
}

impl<'a> ProductionServiceBuilder<'a> {
    /// Create a new production service builder.
    pub fn new(config: &'a Configuration) -> Self {
        let chain_observer_builder: fn(&Configuration) -> StdResult<ChainObserverService> =
            |config: &Configuration| {
                let chain_observer_type = ChainObserverType::Pallas;
                let cardano_cli_path = &config.cardano_cli_path;
                let cardano_node_socket_path = &config.cardano_node_socket_path;
                let cardano_network = &config.get_network().with_context(|| {
                    "Production Service Builder can not get Cardano network while building the chain observer"
                })?;
                let cardano_cli_runner = &CardanoCliRunner::new(
                    cardano_cli_path.to_owned(),
                    cardano_node_socket_path.to_owned(),
                    cardano_network.to_owned(),
                );

                let chain_observer_builder = ChainObserverBuilder::new(
                    &chain_observer_type,
                    cardano_node_socket_path,
                    cardano_network,
                    Some(cardano_cli_runner),
                );

                chain_observer_builder
                    .build()
                    .with_context(|| "Dependencies Builder can not build chain observer")
            };

        let immutable_file_observer_builder: fn(
            &Configuration,
        )
            -> StdResult<Arc<dyn ImmutableFileObserver>> = |config: &Configuration| {
            Ok(Arc::new(ImmutableFileSystemObserver::new(
                &config.db_directory,
            )))
        };

        Self {
            config,
            chain_observer_builder,
            immutable_file_observer_builder,
        }
    }

    /// Override immutable file observer builder.
    pub fn override_immutable_file_observer_builder(
        &mut self,
        builder: fn(&Configuration) -> StdResult<Arc<dyn ImmutableFileObserver>>,
    ) -> &mut Self {
        self.immutable_file_observer_builder = builder;

        self
    }

    /// Override default chain observer builder.
    pub fn override_chain_observer_builder(
        &mut self,
        builder: fn(&Configuration) -> StdResult<ChainObserverService>,
    ) -> &mut Self {
        self.chain_observer_builder = builder;

        self
    }

    /// Compute protocol party id
    fn compute_protocol_party_id(&self) -> StdResult<ProtocolPartyId> {
        match &self.config.operational_certificate_path {
            Some(operational_certificate_path) => {
                let opcert: OpCert = OpCert::from_file(operational_certificate_path)
                    .with_context(|| "Could not decode operational certificate")?;
                Ok(opcert
                    .compute_protocol_party_id()
                    .with_context(|| "Could not compute party_id from operational certificate")?)
            }
            _ => Ok(self
                .config
                .party_id
                .to_owned()
                .ok_or(anyhow!("A party_id should at least be provided"))?),
        }
    }

    async fn build_digester_cache_provider(
        &self,
    ) -> StdResult<Option<Arc<dyn ImmutableFileDigestCacheProvider>>> {
        if self.config.disable_digests_cache {
            return Ok(None);
        }

        let cache_provider = JsonImmutableFileDigestCacheProviderBuilder::new(
            &self.config.data_stores_directory,
            &format!("immutables_digests_{}.json", self.config.network),
        )
        .should_reset_digests_cache(self.config.reset_digests_cache)
        .with_logger(slog_scope::logger())
        .build()
        .await?;

        Ok(Some(Arc::new(cache_provider)))
    }

    /// Build a SQLite connection.
    pub async fn build_sqlite_connection(
        &self,
        sqlite_file_name: &str,
        migrations: Vec<SqlMigration>,
    ) -> StdResult<Arc<SqliteConnection>> {
        let sqlite_db_path = self.config.get_sqlite_file(sqlite_file_name)?;
        let connection = ConnectionBuilder::open_file(&sqlite_db_path)
            .with_node_type(ApplicationNodeType::Signer)
            .with_migrations(migrations)
            .with_logger(slog_scope::logger())
            .build()
            .with_context(|| "Database connection initialisation error")?;

        Ok(Arc::new(connection))
    }
}

#[async_trait]
impl<'a> ServiceBuilder for ProductionServiceBuilder<'a> {
    /// Build a Services for the Production environment.
    async fn build(&self) -> StdResult<SignerServices> {
        if !self.config.data_stores_directory.exists() {
            fs::create_dir_all(self.config.data_stores_directory.clone()).with_context(|| {
                format!(
                    "Could not create data stores directory: `{}`",
                    self.config.data_stores_directory.display()
                )
            })?;
        }

        let sqlite_connection = self
            .build_sqlite_connection(SQLITE_FILE, crate::database::migration::get_migrations())
            .await?;
        let transaction_sqlite_connection = self
            .build_sqlite_connection(
                SQLITE_FILE_CARDANO_TRANSACTION,
                mithril_persistence::database::cardano_transaction_migration::get_migrations(),
            )
            .await?;

        let protocol_initializer_store = Arc::new(ProtocolInitializerStore::new(
            Box::new(SQLiteAdapter::new(
                "protocol_initializer",
                sqlite_connection.clone(),
            )?),
            self.config.store_retention_limit,
        ));
        let single_signer = Arc::new(MithrilSingleSigner::new(self.compute_protocol_party_id()?));
        let digester = Arc::new(CardanoImmutableDigester::new(
            self.build_digester_cache_provider().await?,
            slog_scope::logger(),
        ));
        let stake_store = Arc::new(StakeStore::new(
            Box::new(SQLiteAdapter::new("stake", sqlite_connection)?),
            self.config.store_retention_limit,
        ));
        let chain_observer = {
            let builder = self.chain_observer_builder;
            builder(self.config)?
        };
        let time_point_provider = {
            let builder = self.immutable_file_observer_builder;
            Arc::new(TimePointProviderImpl::new(
                chain_observer.clone(),
                builder(self.config)?,
            ))
        };

        let era_reader = Arc::new(EraReader::new(
            self.config
                .build_era_reader_adapter(chain_observer.clone())?,
        ));
        let era_epoch_token = era_reader
            .read_era_epoch_token(time_point_provider.get_current_time_point().await?.epoch)
            .await?;
        let era_checker = Arc::new(EraChecker::new(
            era_epoch_token.get_current_supported_era()?,
            era_epoch_token.get_current_epoch(),
        ));

        let api_version_provider = Arc::new(APIVersionProvider::new(era_checker.clone()));
        let certificate_handler = Arc::new(AggregatorHTTPClient::new(
            self.config.aggregator_endpoint.clone(),
            self.config.relay_endpoint.clone(),
            api_version_provider.clone(),
            Some(Duration::from_millis(HTTP_REQUEST_TIMEOUT_DURATION)),
        ));

        let cardano_immutable_snapshot_builder =
            Arc::new(CardanoImmutableFilesFullSignableBuilder::new(
                digester.clone(),
                &self.config.db_directory,
                slog_scope::logger(),
            ));
        let mithril_stake_distribution_signable_builder =
            Arc::new(MithrilStakeDistributionSignableBuilder::default());
        let block_scanner = Arc::new(CardanoBlockScanner::new(
            slog_scope::logger(),
            self.config
                .get_network()?
                .compute_allow_unparsable_block(self.config.allow_unparsable_block)?,
        ));
        let transaction_store = Arc::new(CardanoTransactionRepository::new(
            transaction_sqlite_connection,
        ));
        let transactions_importer = Arc::new(CardanoTransactionsImporter::new(
            block_scanner,
            transaction_store.clone(),
            &self.config.db_directory,
            // Rescan the last immutable when importing transactions, it may have been partially imported
            Some(1),
            slog_scope::logger(),
        ));
        // Wrap the transaction importer with decorator to prune the transactions after import
        let transactions_importer = Arc::new(TransactionsImporterWithPruner::new(
            self.config
                .enable_transaction_pruning
                .then_some(self.config.network_security_parameter),
            transaction_store.clone(),
            transactions_importer,
        ));
        let block_range_root_retriever = transaction_store.clone();
        let cardano_transactions_builder = Arc::new(CardanoTransactionsSignableBuilder::new(
            transactions_importer,
            block_range_root_retriever,
            slog_scope::logger(),
        ));
        let signable_builder_service = Arc::new(MithrilSignableBuilderService::new(
            mithril_stake_distribution_signable_builder,
            cardano_immutable_snapshot_builder,
            cardano_transactions_builder,
        ));
        let metrics_service = Arc::new(MetricsService::new().unwrap());

        let services = SignerServices {
            time_point_provider,
            certificate_handler,
            chain_observer,
            digester,
            single_signer,
            stake_store,
            protocol_initializer_store,
            era_checker,
            era_reader,
            api_version_provider,
            signable_builder_service,
            metrics_service,
        };

        Ok(services)
    }
}

/// This structure groups all the services required by the state machine.
pub struct SignerServices {
    /// Time point provider service
    pub time_point_provider: TimePointProviderService,

    /// Stake store service
    pub stake_store: StakeStoreService,

    /// Certificate handler service
    pub certificate_handler: CertificateHandlerService,

    /// Chain Observer service
    pub chain_observer: ChainObserverService,

    /// Digester service
    pub digester: DigesterService,

    /// SingleSigner service
    pub single_signer: SingleSignerService,

    /// ProtocolInitializer store
    pub protocol_initializer_store: ProtocolInitializerStoreService,

    /// Era checker service
    pub era_checker: Arc<EraChecker>,

    /// Era reader service
    pub era_reader: Arc<EraReader>,

    /// API version provider
    pub api_version_provider: Arc<APIVersionProvider>,

    /// Signable Builder Service
    pub signable_builder_service: Arc<dyn SignableBuilderService>,

    /// Metrics service
    pub metrics_service: Arc<MetricsService>,
}

#[cfg(test)]
mod tests {
    use mithril_common::{
        chain_observer::FakeObserver,
        digesters::DumbImmutableFileObserver,
        entities::{Epoch, TimePoint},
        test_utils::TempDir,
    };

    use super::*;

    use std::path::PathBuf;

    fn get_test_dir(test_name: &str) -> PathBuf {
        TempDir::create("signer_service", test_name)
    }

    #[tokio::test]
    async fn test_auto_create_stores_directory() {
        let stores_dir = get_test_dir("test_auto_create_stores_directory").join("stores");
        let config = Configuration {
            data_stores_directory: stores_dir.clone(),
            ..Configuration::new_sample("party-123456")
        };

        assert!(!stores_dir.exists());
        let chain_observer_builder: fn(&Configuration) -> StdResult<ChainObserverService> =
            |_config| {
                Ok(Arc::new(FakeObserver::new(Some(TimePoint {
                    epoch: Epoch(1),
                    immutable_file_number: 1,
                }))))
            };
        let immutable_file_observer_builder: fn(
            &Configuration,
        )
            -> StdResult<Arc<dyn ImmutableFileObserver>> =
            |_config: &Configuration| Ok(Arc::new(DumbImmutableFileObserver::default()));

        let mut service_builder = ProductionServiceBuilder::new(&config);
        service_builder
            .override_chain_observer_builder(chain_observer_builder)
            .override_immutable_file_observer_builder(immutable_file_observer_builder)
            .build()
            .await
            .expect("service builder build should not fail");
        assert!(stores_dir.exists());
    }
}

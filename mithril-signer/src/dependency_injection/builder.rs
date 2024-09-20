use std::fs;
use std::sync::Arc;
use std::time::Duration;

use anyhow::{anyhow, Context};

use tokio::sync::{Mutex, RwLock};

use mithril_common::api_version::APIVersionProvider;
use mithril_common::cardano_block_scanner::CardanoBlockScanner;
use mithril_common::cardano_transactions_preloader::CardanoTransactionsPreloader;
use mithril_common::chain_observer::{
    CardanoCliRunner, ChainObserver, ChainObserverBuilder, ChainObserverType,
};
use mithril_common::chain_reader::PallasChainReader;
use mithril_common::crypto_helper::{OpCert, ProtocolPartyId, SerDeShelleyFileFormat};
use mithril_common::digesters::cache::{
    ImmutableFileDigestCacheProvider, JsonImmutableFileDigestCacheProviderBuilder,
};
use mithril_common::digesters::{
    CardanoImmutableDigester, ImmutableFileObserver, ImmutableFileSystemObserver,
};
use mithril_common::era::{EraChecker, EraReader};
use mithril_common::signable_builder::{
    CardanoImmutableFilesFullSignableBuilder, CardanoStakeDistributionSignableBuilder,
    CardanoTransactionsSignableBuilder, MithrilSignableBuilderService,
    MithrilStakeDistributionSignableBuilder,
};
use mithril_common::signed_entity_type_lock::SignedEntityTypeLock;
use mithril_common::{MithrilTickerService, StdResult, TickerService};

use mithril_persistence::database::repository::CardanoTransactionRepository;
use mithril_persistence::database::{ApplicationNodeType, SqlMigration};
use mithril_persistence::sqlite::{ConnectionBuilder, SqliteConnection, SqliteConnectionPool};
use mithril_persistence::store::adapter::SQLiteAdapter;
use mithril_persistence::store::StakeStore;

use crate::dependency_injection::SignerDependencyContainer;
use crate::services::{
    AggregatorHTTPClient, CardanoTransactionsImporter,
    CardanoTransactionsPreloaderActivationSigner, MithrilEpochService, MithrilSingleSigner,
    SignableSeedBuilderService, SignerUpkeepService, TransactionsImporterByChunk,
    TransactionsImporterWithPruner, TransactionsImporterWithVacuum,
};
use crate::store::{MKTreeStoreSqlite, ProtocolInitializerStore};
use crate::{
    Configuration, MetricsService, HTTP_REQUEST_TIMEOUT_DURATION, SQLITE_FILE,
    SQLITE_FILE_CARDANO_TRANSACTION,
};

/// The `DependenciesBuilder` is intended to manage Services instance creation.
///
/// The goal of this is to put all this code out of the way of business code.
pub struct DependenciesBuilder<'a> {
    config: &'a Configuration,
    chain_observer_builder: fn(&Configuration) -> StdResult<Arc<dyn ChainObserver>>,
    immutable_file_observer_builder:
        fn(&Configuration) -> StdResult<Arc<dyn ImmutableFileObserver>>,
}

impl<'a> DependenciesBuilder<'a> {
    /// Create a new `DependenciesBuilder`.
    pub fn new(config: &'a Configuration) -> Self {
        let chain_observer_builder: fn(&Configuration) -> StdResult<Arc<dyn ChainObserver>> =
            |config: &Configuration| {
                let chain_observer_type = ChainObserverType::Pallas;
                let cardano_cli_path = &config.cardano_cli_path;
                let cardano_node_socket_path = &config.cardano_node_socket_path;
                let cardano_network = &config.get_network().with_context(|| {
                    "Dependencies Builder can not get Cardano network while building the chain observer"
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
        builder: fn(&Configuration) -> StdResult<Arc<dyn ChainObserver>>,
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
    ) -> StdResult<SqliteConnection> {
        let sqlite_db_path = self.config.get_sqlite_file(sqlite_file_name)?;
        let logger = slog_scope::logger();
        let connection = ConnectionBuilder::open_file(&sqlite_db_path)
            .with_node_type(ApplicationNodeType::Signer)
            .with_migrations(migrations)
            .with_logger(logger.clone())
            .build()
            .with_context(|| "Database connection initialisation error")?;

        Ok(connection)
    }

    /// Build dependencies for the Production environment.
    pub async fn build(&self) -> StdResult<SignerDependencyContainer> {
        if !self.config.data_stores_directory.exists() {
            fs::create_dir_all(self.config.data_stores_directory.clone()).with_context(|| {
                format!(
                    "Could not create data stores directory: `{}`",
                    self.config.data_stores_directory.display()
                )
            })?;
        }

        let network = self.config.get_network()?;
        let sqlite_connection = Arc::new(
            self.build_sqlite_connection(SQLITE_FILE, crate::database::migration::get_migrations())
                .await?,
        );
        let transaction_sqlite_connection = self
            .build_sqlite_connection(
                SQLITE_FILE_CARDANO_TRANSACTION,
                mithril_persistence::database::cardano_transaction_migration::get_migrations(),
            )
            .await?;
        let sqlite_connection_cardano_transaction_pool = Arc::new(
            SqliteConnectionPool::build_from_connection(transaction_sqlite_connection),
        );

        let signed_entity_type_lock = Arc::new(SignedEntityTypeLock::default());
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
            Box::new(SQLiteAdapter::new("stake", sqlite_connection.clone())?),
            self.config.store_retention_limit,
        ));
        let chain_observer = {
            let builder = self.chain_observer_builder;
            builder(self.config)?
        };
        let ticker_service = {
            let builder = self.immutable_file_observer_builder;
            Arc::new(MithrilTickerService::new(
                chain_observer.clone(),
                builder(self.config)?,
            ))
        };

        let era_reader = Arc::new(EraReader::new(
            self.config
                .build_era_reader_adapter(chain_observer.clone())?,
        ));
        let era_epoch_token = era_reader
            .read_era_epoch_token(ticker_service.get_current_epoch().await?)
            .await?;
        let era_checker = Arc::new(EraChecker::new(
            era_epoch_token.get_current_supported_era()?,
            era_epoch_token.get_current_epoch(),
        ));

        let api_version_provider = Arc::new(APIVersionProvider::new(era_checker.clone()));
        let aggregator_client = Arc::new(AggregatorHTTPClient::new(
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
        let transaction_store = Arc::new(CardanoTransactionRepository::new(
            sqlite_connection_cardano_transaction_pool.clone(),
        ));
        let chain_block_reader = PallasChainReader::new(
            &self.config.cardano_node_socket_path,
            network,
            slog_scope::logger(),
        );
        let block_scanner = Arc::new(CardanoBlockScanner::new(
            Arc::new(Mutex::new(chain_block_reader)),
            self.config
                .cardano_transactions_block_streamer_max_roll_forwards_per_poll,
            slog_scope::logger(),
        ));
        let transactions_importer = Arc::new(CardanoTransactionsImporter::new(
            block_scanner,
            transaction_store.clone(),
            slog_scope::logger(),
        ));
        // Wrap the transaction importer with decorator to prune the transactions after import
        let transactions_importer = Arc::new(TransactionsImporterWithPruner::new(
            self.config
                .enable_transaction_pruning
                .then_some(self.config.network_security_parameter),
            transaction_store.clone(),
            transactions_importer,
            slog_scope::logger(),
        ));
        // Wrap the transaction importer with decorator to chunk its workload, so it prunes
        // transactions after each chunk, reducing the storage footprint
        let state_machine_transactions_importer = Arc::new(TransactionsImporterByChunk::new(
            transaction_store.clone(),
            transactions_importer.clone(),
            self.config.transactions_import_block_chunk_size,
            slog_scope::logger(),
        ));
        // For the preloader, we want to vacuum the database after each chunk, to reclaim disk space
        // earlier than with just auto_vacuum (that execute only after the end of all import).
        let preloader_transactions_importer = Arc::new(TransactionsImporterByChunk::new(
            transaction_store.clone(),
            Arc::new(TransactionsImporterWithVacuum::new(
                sqlite_connection_cardano_transaction_pool.clone(),
                transactions_importer.clone(),
                slog_scope::logger(),
            )),
            self.config.transactions_import_block_chunk_size,
            slog_scope::logger(),
        ));
        let block_range_root_retriever = transaction_store.clone();
        let cardano_transactions_builder = Arc::new(CardanoTransactionsSignableBuilder::<
            MKTreeStoreSqlite,
        >::new(
            state_machine_transactions_importer,
            block_range_root_retriever,
            slog_scope::logger(),
        ));
        let cardano_stake_distribution_signable_builder = Arc::new(
            CardanoStakeDistributionSignableBuilder::new(stake_store.clone()),
        );
        let epoch_service = Arc::new(RwLock::new(MithrilEpochService::new(stake_store.clone())));
        let signable_seed_builder_service = Arc::new(SignableSeedBuilderService::new(
            epoch_service.clone(),
            single_signer.clone(),
            protocol_initializer_store.clone(),
        ));
        let signable_builder_service = Arc::new(MithrilSignableBuilderService::new(
            signable_seed_builder_service,
            mithril_stake_distribution_signable_builder,
            cardano_immutable_snapshot_builder,
            cardano_transactions_builder,
            cardano_stake_distribution_signable_builder,
        ));
        let metrics_service = Arc::new(MetricsService::new()?);
        let preloader_activation =
            CardanoTransactionsPreloaderActivationSigner::new(aggregator_client.clone());
        let cardano_transactions_preloader = Arc::new(CardanoTransactionsPreloader::new(
            signed_entity_type_lock.clone(),
            preloader_transactions_importer,
            self.config.preload_security_parameter,
            chain_observer.clone(),
            slog_scope::logger(),
            Arc::new(preloader_activation),
        ));
        let upkeep_service = Arc::new(SignerUpkeepService::new(
            sqlite_connection.clone(),
            sqlite_connection_cardano_transaction_pool,
            signed_entity_type_lock.clone(),
            slog_scope::logger(),
        ));

        let services = SignerDependencyContainer {
            ticker_service,
            certificate_handler: aggregator_client,
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
            signed_entity_type_lock,
            cardano_transactions_preloader,
            upkeep_service,
            epoch_service,
        };

        Ok(services)
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use mithril_common::{
        chain_observer::FakeObserver, digesters::DumbImmutableFileObserver, entities::TimePoint,
        test_utils::TempDir,
    };

    use super::*;

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
        let chain_observer_builder: fn(&Configuration) -> StdResult<Arc<dyn ChainObserver>> =
            |_config| Ok(Arc::new(FakeObserver::new(Some(TimePoint::dummy()))));
        let immutable_file_observer_builder: fn(
            &Configuration,
        )
            -> StdResult<Arc<dyn ImmutableFileObserver>> =
            |_config: &Configuration| Ok(Arc::new(DumbImmutableFileObserver::default()));

        let mut dependencies_builder = DependenciesBuilder::new(&config);
        dependencies_builder
            .override_chain_observer_builder(chain_observer_builder)
            .override_immutable_file_observer_builder(immutable_file_observer_builder)
            .build()
            .await
            .expect("service builder build should not fail");
        assert!(stores_dir.exists());
    }
}

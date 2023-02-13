use clap::{Parser, Subcommand};
use config::{builder::DefaultState, ConfigBuilder, Map, Source, Value, ValueKind};
use slog::Level;
use slog_scope::debug;
use std::{error::Error, fs, path::PathBuf, sync::Arc};
use tokio::{sync::RwLock, time::Duration};

use mithril_common::{
    certificate_chain::MithrilCertificateVerifier,
    chain_observer::{CardanoCliRunner, ChainObserver},
    crypto_helper::{key_decode_hex, ProtocolGenesisSigner, ProtocolGenesisVerifier},
    database::{ApplicationNodeType, DatabaseVersionChecker},
    digesters::{
        cache::{ImmutableFileDigestCacheProvider, JsonImmutableFileDigestCacheProviderBuilder},
        CardanoImmutableDigester, ImmutableFileSystemObserver,
    },
    entities::{Epoch, HexEncodedGenesisSecretKey},
    era::{EraChecker, EraReader},
    store::{adapter::SQLiteAdapter, StakeStore},
    BeaconProvider, BeaconProviderImpl,
};

use crate::{
    tools::{GenesisTools, GenesisToolsDependency},
    AggregatorConfig, AggregatorRunner, AggregatorRuntime, CertificatePendingStore,
    CertificateStore, Configuration, DefaultConfiguration, DependencyManager, GenesisConfiguration,
    GzipSnapshotter, MithrilSignerRegisterer, MultiSignerImpl, ProtocolParametersStore,
    ProtocolParametersStorer, Server, SingleSignatureStore, VerificationKeyStore,
};

fn setup_genesis_dependencies(
    config: &GenesisConfiguration,
) -> Result<GenesisToolsDependency, Box<dyn std::error::Error>> {
    let sqlite_db_path = Some(config.get_sqlite_file());
    check_database_migration(config.get_sqlite_file())?;

    let chain_observer = Arc::new(
        mithril_common::chain_observer::CardanoCliChainObserver::new(Box::new(
            CardanoCliRunner::new(
                config.cardano_cli_path.clone(),
                config.cardano_node_socket_path.clone(),
                config.get_network()?,
            ),
        )),
    );
    let immutable_file_observer = Arc::new(ImmutableFileSystemObserver::new(&config.db_directory));
    let beacon_provider = Arc::new(BeaconProviderImpl::new(
        chain_observer,
        immutable_file_observer,
        config.get_network()?,
    ));
    let certificate_store = Arc::new(CertificateStore::new(Box::new(SQLiteAdapter::new(
        "certificate",
        sqlite_db_path.clone(),
    )?)));
    let certificate_verifier = Arc::new(MithrilCertificateVerifier::new(slog_scope::logger()));
    let genesis_verification_key = key_decode_hex(&config.genesis_verification_key)?;
    let genesis_verifier = Arc::new(ProtocolGenesisVerifier::from_verification_key(
        genesis_verification_key,
    ));
    let protocol_parameters_store = Arc::new(ProtocolParametersStore::new(
        Box::new(SQLiteAdapter::new(
            "protocol_parameters",
            sqlite_db_path.clone(),
        )?),
        config.store_retention_limit,
    ));
    let verification_key_store = Arc::new(VerificationKeyStore::new(
        Box::new(SQLiteAdapter::new(
            "verification_key",
            sqlite_db_path.clone(),
        )?),
        config.store_retention_limit,
    ));
    let stake_store = Arc::new(StakeStore::new(
        Box::new(SQLiteAdapter::new("stake", sqlite_db_path.clone())?),
        config.store_retention_limit,
    ));
    let single_signature_store = Arc::new(SingleSignatureStore::new(
        Box::new(SQLiteAdapter::new("single_signature", sqlite_db_path)?),
        config.store_retention_limit,
    ));
    let multi_signer = Arc::new(RwLock::new(MultiSignerImpl::new(
        verification_key_store,
        stake_store,
        single_signature_store,
        protocol_parameters_store.clone(),
    )));
    let dependencies = GenesisToolsDependency {
        beacon_provider,
        certificate_store,
        certificate_verifier,
        genesis_verifier,
        protocol_parameters_store,
        multi_signer,
    };

    Ok(dependencies)
}

async fn do_first_launch_initialization_if_needed(
    chain_observer: Arc<dyn ChainObserver>,
    protocol_parameters_store: Arc<ProtocolParametersStore>,
    config: &Configuration,
) -> Result<(), Box<dyn Error>> {
    let (work_epoch, epoch_to_sign, next_epoch_to_sign) = match chain_observer
        .get_current_epoch()
        .await?
        .ok_or("Can't retrieve current epoch")?
    {
        Epoch(0) => (Epoch(0), Epoch(1), Epoch(2)),
        epoch => (
            epoch.offset_to_signer_retrieval_epoch()?,
            epoch.offset_to_next_signer_retrieval_epoch(),
            epoch.offset_to_next_signer_retrieval_epoch().next(),
        ),
    };

    if protocol_parameters_store
        .get_protocol_parameters(work_epoch)
        .await?
        .is_none()
    {
        debug!("First launch, will use the configured protocol parameters for the current and next epoch certificate");

        for epoch in [work_epoch, epoch_to_sign, next_epoch_to_sign] {
            protocol_parameters_store
                .save_protocol_parameters(epoch, config.protocol_parameters.clone())
                .await?;
        }
    }

    Ok(())
}

/// Database version checker.
/// This is the place where migrations are to be registered.
fn check_database_migration(sql_file_path: PathBuf) -> Result<(), Box<dyn Error>> {
    DatabaseVersionChecker::new(
        slog_scope::logger(),
        ApplicationNodeType::Aggregator,
        sql_file_path,
    )
    .apply()
}

/// Mithril Aggregator Node
#[derive(Parser, Debug, Clone)]
#[command(version)]
pub struct MainOpts {
    /// application main command
    #[clap(subcommand)]
    pub command: MainCommand,

    /// Run Mode
    #[clap(short, long, default_value = "dev")]
    pub run_mode: String,

    /// Verbosity level
    #[clap(short, long, action = clap::ArgAction::Count)]
    pub verbose: u8,

    /// Directory of the Cardano node files
    #[clap(long)]
    pub db_directory: Option<PathBuf>,

    /// Directory where configuration file is located
    #[clap(long, default_value = "./config")]
    pub config_directory: PathBuf,
}

impl Source for MainOpts {
    fn clone_into_box(&self) -> Box<dyn Source + Send + Sync> {
        Box::new(self.clone())
    }

    fn collect(&self) -> Result<Map<String, Value>, config::ConfigError> {
        let mut result = Map::new();
        let namespace = "clap arguments".to_string();

        if let Some(db_directory) = self.db_directory.clone() {
            result.insert(
                "db_directory".to_string(),
                Value::new(
                    Some(&namespace),
                    ValueKind::from(format!("{}", db_directory.to_string_lossy())),
                ),
            );
        }

        Ok(result)
    }
}

impl MainOpts {
    /// execute command
    pub async fn execute(&self) -> Result<(), Box<dyn Error>> {
        let config_file_path = self
            .config_directory
            .join(format!("{}.json", self.run_mode));
        let config_builder = config::Config::builder()
            .add_source(DefaultConfiguration::default())
            .add_source(
                config::File::with_name(&config_file_path.to_string_lossy()).required(false),
            )
            .add_source(config::Environment::default().separator("__"))
            .add_source(self.clone());
        debug!("Started"; "run_mode" => &self.run_mode);

        self.command.execute(config_builder).await
    }

    /// get log level from parameters
    pub fn log_level(&self) -> Level {
        match self.verbose {
            0 => Level::Warning,
            1 => Level::Info,
            2 => Level::Debug,
            _ => Level::Trace,
        }
    }
}

/// Main command selecter
#[derive(Debug, Clone, Subcommand)]
pub enum MainCommand {
    Genesis(GenesisCommand),
    Serve(ServeCommand),
}

impl MainCommand {
    pub async fn execute(
        &self,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), Box<dyn Error>> {
        match self {
            Self::Genesis(cmd) => cmd.execute(config_builder).await,
            Self::Serve(cmd) => cmd.execute(config_builder).await,
        }
    }
}

/// Server runtime mode
#[derive(Parser, Debug, Clone)]
pub struct ServeCommand {
    /// Server listening IP
    #[clap(long)]
    pub server_ip: Option<String>,

    /// Server TCP port
    #[clap(long)]
    pub server_port: Option<u16>,

    /// Directory to store snapshot
    /// Defaults to work folder
    #[clap(long)]
    pub snapshot_directory: Option<PathBuf>,

    /// Disable immutables digests cache.
    #[clap(long)]
    disable_digests_cache: bool,

    /// If set the existing immutables digests cache will be reset.
    ///
    /// Will be ignored if set in conjunction with `--disable-digests-cache`.
    #[clap(long)]
    reset_digests_cache: bool,
}

impl Source for ServeCommand {
    fn clone_into_box(&self) -> Box<dyn Source + Send + Sync> {
        Box::new(self.clone())
    }

    fn collect(&self) -> Result<Map<String, Value>, config::ConfigError> {
        let mut result = Map::new();
        let namespace = "clap arguments".to_string();

        if let Some(server_ip) = self.server_ip.clone() {
            result.insert(
                "server_ip".to_string(),
                Value::new(Some(&namespace), ValueKind::from(server_ip)),
            );
        }
        if let Some(server_port) = self.server_port {
            result.insert(
                "server_port".to_string(),
                Value::new(Some(&namespace), ValueKind::from(server_port)),
            );
        }
        if let Some(snapshot_directory) = self.snapshot_directory.clone() {
            result.insert(
                "snapshot_directory".to_string(),
                Value::new(
                    Some(&namespace),
                    ValueKind::from(format!("{}", snapshot_directory.to_string_lossy())),
                ),
            );
        }

        Ok(result)
    }
}

impl ServeCommand {
    pub async fn execute(
        &self,
        mut config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), Box<dyn Error>> {
        config_builder = config_builder.add_source(self.clone());
        let config: Configuration = config_builder
            .build()
            .map_err(|e| format!("configuration build error: {e}"))?
            .try_deserialize()
            .map_err(|e| format!("configuration deserialize error: {e}"))?;
        debug!("SERVE command"; "config" => format!("{config:?}"));
        let sqlite_db_path = Some(config.get_sqlite_file());
        check_database_migration(config.get_sqlite_file())?;

        // Init dependencies
        let snapshot_store = config.build_snapshot_store()?;
        let snapshot_uploader = config.build_snapshot_uploader()?;

        let certificate_pending_store = Arc::new(CertificatePendingStore::new(Box::new(
            SQLiteAdapter::new("pending_certificate", sqlite_db_path.clone())?,
        )));
        let certificate_store = Arc::new(CertificateStore::new(Box::new(SQLiteAdapter::new(
            "certificate",
            sqlite_db_path.clone(),
        )?)));
        let verification_key_store = Arc::new(VerificationKeyStore::new(
            Box::new(SQLiteAdapter::new(
                "verification_key",
                sqlite_db_path.clone(),
            )?),
            config.store_retention_limit,
        ));
        let stake_store = Arc::new(StakeStore::new(
            Box::new(SQLiteAdapter::new("stake", sqlite_db_path.clone())?),
            config.store_retention_limit,
        ));
        let single_signature_store = Arc::new(SingleSignatureStore::new(
            Box::new(SQLiteAdapter::new(
                "single_signature",
                sqlite_db_path.clone(),
            )?),
            config.store_retention_limit,
        ));
        let protocol_parameters_store = Arc::new(ProtocolParametersStore::new(
            Box::new(SQLiteAdapter::new("protocol_parameters", sqlite_db_path)?),
            config.store_retention_limit,
        ));
        let chain_observer = Arc::new(
            mithril_common::chain_observer::CardanoCliChainObserver::new(Box::new(
                CardanoCliRunner::new(
                    config.cardano_cli_path.clone(),
                    config.cardano_node_socket_path.clone(),
                    config.get_network()?,
                ),
            )),
        );
        let immutable_file_observer =
            Arc::new(ImmutableFileSystemObserver::new(&config.db_directory));
        let beacon_provider = Arc::new(BeaconProviderImpl::new(
            chain_observer.clone(),
            immutable_file_observer.clone(),
            config.get_network()?,
        ));
        let digester = Arc::new(CardanoImmutableDigester::new(
            config.db_directory.clone(),
            self.build_digester_cache_provider(&config).await?,
            slog_scope::logger(),
        ));
        let multi_signer = Arc::new(RwLock::new(MultiSignerImpl::new(
            verification_key_store.clone(),
            stake_store.clone(),
            single_signature_store.clone(),
            protocol_parameters_store.clone(),
        )));
        let certificate_verifier = Arc::new(MithrilCertificateVerifier::new(slog_scope::logger()));
        let genesis_verification_key = key_decode_hex(&config.genesis_verification_key)?;
        let genesis_verifier = Arc::new(ProtocolGenesisVerifier::from_verification_key(
            genesis_verification_key,
        ));
        let signer_registerer = Arc::new(MithrilSignerRegisterer::new(
            chain_observer.clone(),
            verification_key_store.clone(),
        ));

        // Snapshotter - Ensure its ongoing snapshot directory exist
        let ongoing_snapshot_directory = config.snapshot_directory.join("pending_snapshot");
        if !ongoing_snapshot_directory.exists() {
            fs::create_dir(&ongoing_snapshot_directory)
                .expect("Pending snapshot directory creation failure");
        }
        let snapshotter = Arc::new(GzipSnapshotter::new(
            config.db_directory.clone(),
            ongoing_snapshot_directory,
        ));

        let era_reader = Arc::new(EraReader::new(
            config.build_era_reader_adapter(chain_observer.clone())?,
        ));
        let era_epoch_token = era_reader
            .read_era_epoch_token(beacon_provider.get_current_beacon().await?.epoch)
            .await?;
        let era_checker = Arc::new(EraChecker::new(
            era_epoch_token.get_current_supported_era()?,
            era_epoch_token.get_current_epoch(),
        ));

        // Init dependency manager
        let dependency_manager = DependencyManager {
            config: config.clone(),
            snapshot_store: snapshot_store.clone(),
            snapshot_uploader: snapshot_uploader.clone(),
            multi_signer: multi_signer.clone(),
            certificate_pending_store: certificate_pending_store.clone(),
            certificate_store: certificate_store.clone(),
            verification_key_store: verification_key_store.clone(),
            stake_store: stake_store.clone(),
            single_signature_store: single_signature_store.clone(),
            protocol_parameters_store: protocol_parameters_store.clone(),
            chain_observer: chain_observer.clone(),
            beacon_provider: beacon_provider.clone(),
            immutable_file_observer,
            digester,
            snapshotter,
            certificate_verifier,
            genesis_verifier,
            signer_registerer: signer_registerer.clone(),
            signer_registration_round_opener: signer_registerer.clone(),
            era_checker: era_checker.clone(),
            era_reader: era_reader.clone(),
        };
        let dependency_manager = Arc::new(dependency_manager);

        do_first_launch_initialization_if_needed(
            dependency_manager.chain_observer.clone(),
            dependency_manager.protocol_parameters_store.clone(),
            &config,
        )
        .await?;

        // Start snapshot uploader
        let network = config.get_network()?;
        let runtime_dependencies = dependency_manager.clone();
        let handle = tokio::spawn(async move {
            let config =
                AggregatorConfig::new(config.run_interval, network, &config.db_directory.clone());
            let mut runtime = AggregatorRuntime::new(
                Duration::from_millis(config.interval),
                None,
                Arc::new(AggregatorRunner::new(config, runtime_dependencies.clone())),
            )
            .await
            .unwrap();
            runtime.run().await
        });

        // Start REST server
        println!("Starting server...");
        println!("Press Ctrl+C to stop...");
        let shutdown_signal = async {
            tokio::signal::ctrl_c()
                .await
                .expect("failed to install CTRL+C signal handler");
        };
        let http_server = Server::new(
            config.server_ip.clone(),
            config.server_port,
            dependency_manager.clone(),
        );
        http_server.start(shutdown_signal).await;

        handle.abort();

        println!("Exiting...");
        Ok(())
    }

    async fn build_digester_cache_provider(
        &self,
        config: &Configuration,
    ) -> Result<Option<Arc<dyn ImmutableFileDigestCacheProvider>>, Box<dyn Error>> {
        if self.disable_digests_cache {
            return Ok(None);
        }
        let cache_provider = JsonImmutableFileDigestCacheProviderBuilder::new(
            &config.data_stores_directory,
            &format!("immutables_digests_{}.json", config.network),
        )
        .should_reset_digests_cache(self.reset_digests_cache)
        .build()
        .await?;

        Ok(Some(Arc::new(cache_provider)))
    }
}

/// Genesis tools
#[derive(Parser, Debug, Clone)]
pub struct GenesisCommand {
    /// commands
    #[clap(subcommand)]
    pub genesis_subcommand: GenesisSubCommand,
}

impl GenesisCommand {
    pub async fn execute(
        &self,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), Box<dyn Error>> {
        self.genesis_subcommand.execute(config_builder).await
    }
}

/// Genesis tools commands.
#[derive(Debug, Clone, Subcommand)]
pub enum GenesisSubCommand {
    /// Genesis certificate export command.
    Export(ExportGenesisSubCommand),

    /// Genesis certificate import command.
    Import(ImportGenesisSubCommand),

    /// Genesis certificate bootstrap command.
    Bootstrap(BootstrapGenesisSubCommand),
}

impl GenesisSubCommand {
    pub async fn execute(
        &self,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), Box<dyn Error>> {
        match self {
            Self::Bootstrap(cmd) => cmd.execute(config_builder).await,
            Self::Export(cmd) => cmd.execute(config_builder).await,
            Self::Import(cmd) => cmd.execute(config_builder).await,
        }
    }
}

/// Genesis certificate export command
#[derive(Parser, Debug, Clone)]
pub struct ExportGenesisSubCommand {
    /// Target Path
    #[clap(long)]
    target_path: PathBuf,
}

impl ExportGenesisSubCommand {
    pub async fn execute(
        &self,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), Box<dyn Error>> {
        let config: GenesisConfiguration = config_builder
            .build()
            .map_err(|e| format!("configuration build error: {e}"))?
            .try_deserialize()
            .map_err(|e| format!("configuration deserialize error: {e}"))?;
        debug!("EXPORT GENESIS command"; "config" => format!("{config:?}"));
        println!(
            "Genesis export payload to sign to {}",
            self.target_path.display()
        );
        let dependencies = setup_genesis_dependencies(&config)?;

        let genesis_tools = GenesisTools::from_dependencies(dependencies).await?;
        genesis_tools.export_payload_to_sign(&self.target_path)
    }
}

#[derive(Parser, Debug, Clone)]
pub struct ImportGenesisSubCommand {
    /// Signed Payload Path
    #[clap(long)]
    signed_payload_path: PathBuf,
}

impl ImportGenesisSubCommand {
    pub async fn execute(
        &self,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), Box<dyn Error>> {
        let config: GenesisConfiguration = config_builder
            .build()
            .map_err(|e| format!("configuration build error: {e}"))?
            .try_deserialize()
            .map_err(|e| format!("configuration deserialize error: {e}"))?;
        debug!("IMPORT GENESIS command"; "config" => format!("{config:?}"));
        println!(
            "Genesis import signed payload from {}",
            self.signed_payload_path.to_string_lossy()
        );
        let dependencies = setup_genesis_dependencies(&config)?;

        let genesis_tools = GenesisTools::from_dependencies(dependencies).await?;
        genesis_tools
            .import_payload_signature(&self.signed_payload_path)
            .await
    }
}

#[derive(Parser, Debug, Clone)]
pub struct BootstrapGenesisSubCommand {
    /// Genesis Secret Key (test only)
    #[clap(long, env = "GENESIS_SECRET_KEY")]
    genesis_secret_key: HexEncodedGenesisSecretKey,
}

impl BootstrapGenesisSubCommand {
    pub async fn execute(
        &self,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), Box<dyn Error>> {
        let config: GenesisConfiguration = config_builder
            .build()
            .map_err(|e| format!("configuration build error: {e}"))?
            .try_deserialize()
            .map_err(|e| format!("configuration deserialize error: {e}"))?;
        debug!("BOOTSTRAP GENESIS command"; "config" => format!("{config:?}"));
        println!("Genesis bootstrap for test only!");
        let dependencies = setup_genesis_dependencies(&config)?;

        let genesis_tools = GenesisTools::from_dependencies(dependencies).await?;
        let genesis_secret_key = key_decode_hex(&self.genesis_secret_key)?;
        let genesis_signer = ProtocolGenesisSigner::from_secret_key(genesis_secret_key);
        genesis_tools
            .bootstrap_test_genesis_certificate(genesis_signer)
            .await
    }
}

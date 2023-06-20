use clap::{Parser, Subcommand};
use config::{builder::DefaultState, ConfigBuilder, Map, Source, Value, ValueKind};
use slog::Level;
use slog_scope::{crit, debug, info};
use std::{error::Error, net::IpAddr, path::PathBuf};
use tokio::{sync::oneshot, task::JoinSet};

use mithril_common::{
    crypto_helper::{key_decode_hex, EraMarkersSigner, ProtocolGenesisSigner},
    entities::{Epoch, HexEncodedEraMarkersSecretKey, HexEncodedGenesisSecretKey},
};

use crate::{
    dependency_injection::DependenciesBuilder,
    tools::{EraTools, GenesisTools},
    Configuration, DefaultConfiguration,
};

const SQLITE_MONITORING_FILE: &str = "monitoring.sqlite3";

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
        debug!("Started"; "run_mode" => &self.run_mode, "node_version" => env!("CARGO_PKG_VERSION"));

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
    Era(EraCommand),
    Serve(ServeCommand),
}

impl MainCommand {
    pub async fn execute(
        &self,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), Box<dyn Error>> {
        match self {
            Self::Genesis(cmd) => cmd.execute(config_builder).await,
            Self::Era(cmd) => cmd.execute(config_builder).await,
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
        let mut dependencies_builder = DependenciesBuilder::new(config.clone());

        // start servers
        println!("Starting server...");
        println!("Press Ctrl+C to stop");

        // start the monitoring thread
        let mut event_store = dependencies_builder.create_event_store().await?;
        let event_store_config = config.clone();
        let event_store_thread = tokio::spawn(async move {
            event_store
                .run(Some(
                    event_store_config
                        .get_sqlite_dir()
                        .join(SQLITE_MONITORING_FILE),
                ))
                .await
                .unwrap()
        });

        // start the aggregator runtime
        let mut runtime = dependencies_builder.create_aggregator_runner().await?;
        let mut join_set = JoinSet::new();
        join_set.spawn(async move { runtime.run().await.map_err(|e| e.to_string()) });

        // start the HTTP server
        let (shutdown_tx, shutdown_rx) = oneshot::channel();
        let routes = dependencies_builder.create_http_routes().await?;
        join_set.spawn(async move {
            let (_, server) = warp::serve(routes).bind_with_graceful_shutdown(
                (
                    config.server_ip.clone().parse::<IpAddr>().unwrap(),
                    config.server_port,
                ),
                async {
                    shutdown_rx.await.ok();
                },
            );
            server.await;

            Ok(())
        });
        join_set.spawn(async { tokio::signal::ctrl_c().await.map_err(|e| e.to_string()) });
        dependencies_builder.vanish();

        if let Err(e) = join_set.join_next().await.unwrap()? {
            crit!("A critical error occurred: {e}");
        }

        // stop servers
        join_set.shutdown().await;
        let _ = shutdown_tx.send(());

        info!("Event store is finishing...");
        event_store_thread.await.unwrap();
        println!("Services stopped, exiting.");

        Ok(())
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
        let config: Configuration = config_builder
            .build()
            .map_err(|e| format!("configuration build error: {e}"))?
            .try_deserialize()
            .map_err(|e| format!("configuration deserialize error: {e}"))?;
        debug!("EXPORT GENESIS command"; "config" => format!("{config:?}"));
        println!(
            "Genesis export payload to sign to {}",
            self.target_path.display()
        );
        let mut dependencies_builder = DependenciesBuilder::new(config.clone());
        let dependencies = dependencies_builder.create_genesis_container().await?;

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
        let config: Configuration = config_builder
            .build()
            .map_err(|e| format!("configuration build error: {e}"))?
            .try_deserialize()
            .map_err(|e| format!("configuration deserialize error: {e}"))?;
        debug!("IMPORT GENESIS command"; "config" => format!("{config:?}"));
        println!(
            "Genesis import signed payload from {}",
            self.signed_payload_path.to_string_lossy()
        );
        let mut dependencies_builder = DependenciesBuilder::new(config.clone());
        let dependencies = dependencies_builder.create_genesis_container().await?;

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
        let config: Configuration = config_builder
            .build()
            .map_err(|e| format!("configuration build error: {e}"))?
            .try_deserialize()
            .map_err(|e| format!("configuration deserialize error: {e}"))?;
        debug!("BOOTSTRAP GENESIS command"; "config" => format!("{config:?}"));
        println!("Genesis bootstrap for test only!");
        let mut dependencies_builder = DependenciesBuilder::new(config.clone());
        let dependencies = dependencies_builder.create_genesis_container().await?;

        let genesis_tools = GenesisTools::from_dependencies(dependencies).await?;
        let genesis_secret_key = key_decode_hex(&self.genesis_secret_key)?;
        let genesis_signer = ProtocolGenesisSigner::from_secret_key(genesis_secret_key);
        genesis_tools
            .bootstrap_test_genesis_certificate(genesis_signer)
            .await
    }
}

/// Era tools
#[derive(Parser, Debug, Clone)]
pub struct EraCommand {
    /// commands
    #[clap(subcommand)]
    pub era_subcommand: EraSubCommand,
}

impl EraCommand {
    pub async fn execute(
        &self,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), Box<dyn Error>> {
        self.era_subcommand.execute(config_builder).await
    }
}

/// Era tools commands.
#[derive(Debug, Clone, Subcommand)]
pub enum EraSubCommand {
    /// Era list command.
    List(ListEraSubCommand),

    /// Era tx datum generate command.
    GenerateTxDatum(GenerateTxDatumEraSubCommand),
}

impl EraSubCommand {
    pub async fn execute(
        &self,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), Box<dyn Error>> {
        match self {
            Self::List(cmd) => cmd.execute(config_builder).await,
            Self::GenerateTxDatum(cmd) => cmd.execute(config_builder).await,
        }
    }
}

/// Era list command
#[derive(Parser, Debug, Clone)]
pub struct ListEraSubCommand {
    /// Enable JSON output.
    #[clap(long)]
    json: bool,
}

impl ListEraSubCommand {
    pub async fn execute(
        &self,
        _config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), Box<dyn Error>> {
        debug!("LIST ERA command");
        let era_tools = EraTools::new();
        let eras = era_tools.get_supported_eras_list()?;

        if self.json {
            println!("{}", serde_json::to_string(&eras)?);
        } else {
            println!("Supported Eras:");
            println!("{eras:#?}");
        }

        Ok(())
    }
}

/// Era tx datum generate command
#[derive(Parser, Debug, Clone)]
pub struct GenerateTxDatumEraSubCommand {
    /// Current Era epoch
    #[clap(long, env = "CURRENT_ERA_EPOCH")]
    current_era_epoch: u64,

    /// Next Era epoch start, if exists
    #[clap(long, env = "NEXT_ERA_EPOCH")]
    next_era_epoch: Option<u64>,

    /// Era Markers Secret Key
    #[clap(long, env = "ERA_MARKERS_SECRET_KEY")]
    era_markers_secret_key: HexEncodedEraMarkersSecretKey,
}

impl GenerateTxDatumEraSubCommand {
    pub async fn execute(
        &self,
        _config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), Box<dyn Error>> {
        debug!("GENERATETXDATUM ERA command");
        let era_tools = EraTools::new();

        let era_markers_secret_key = key_decode_hex(&self.era_markers_secret_key)?;
        let era_markers_signer = EraMarkersSigner::from_secret_key(era_markers_secret_key);
        print!(
            "{}",
            era_tools.generate_tx_datum(
                Epoch(self.current_era_epoch),
                self.next_era_epoch.map(Epoch),
                &era_markers_signer
            )?
        );

        Ok(())
    }
}

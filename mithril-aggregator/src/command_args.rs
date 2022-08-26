use clap::{Parser, Subcommand};
use config::{Map, Source, Value, ValueKind};
use mithril_common::{chain_observer::ChainObserver, entities::Epoch};
use slog::{Drain, Level, Logger};
use slog_scope::debug;
use std::error::Error;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::time::Duration;

use crate::{
    tools::GenesisTools, AggregatorConfig, AggregatorRunner, AggregatorRuntime, Configuration,
    DependencyManager, ProtocolParametersStore, ProtocolParametersStorer, Server,
};

/// Node args
#[derive(Parser, Debug, Clone)]
pub struct Args {
    /// Available commands
    #[clap(subcommand)]
    pub command: Commands,

    /// Run Mode
    #[clap(short, long, default_value = "dev")]
    pub run_mode: String,

    /// Verbosity level
    #[clap(short, long, parse(from_occurrences))]
    pub verbose: usize,

    /// Server listening IP
    #[clap(long, default_value = "0.0.0.0")]
    pub server_ip: String,

    /// Server listening port
    #[clap(long, default_value_t = 8080)]
    pub server_port: u16,

    /// Directory to snapshot
    #[clap(long, default_value = "/db")]
    pub db_directory: PathBuf,

    /// Directory to store snapshot
    /// Defaults to work folder
    #[clap(long, default_value = ".")]
    pub snapshot_directory: PathBuf,
}

/// CLI command list
#[derive(Subcommand, Debug, Clone)]
pub enum Commands {
    /// Aggregator runs in Serve mode
    #[clap(arg_required_else_help = false)]
    Serve {},

    /// Aggregator runs in Genesis tools mode
    #[clap(arg_required_else_help = true)]
    Genesis {
        /// Available commands
        #[clap(subcommand)]
        genesis_command: GenesisCommands,
    },
}

/// CLI Genesis command list
#[derive(Subcommand, Debug, Clone)]
pub enum GenesisCommands {
    /// Export payload to sign with genesis secret key
    #[clap(arg_required_else_help = false)]
    Export {
        /// Target Path
        #[clap(long, default_value = "./mithril-genesis-payload.txt")]
        target_path: PathBuf,
    },

    /// Import payload signed with genesis secret key and create & import a genesis certificate
    #[clap(arg_required_else_help = false)]
    Import {
        /// Signed Payload Path
        #[clap(long, default_value = "./mithril-genesis-signed-payload.txt")]
        signed_payload_path: PathBuf,
    },

    /// Bootstrap a genesis certificate
    /// Test only usage
    #[clap(arg_required_else_help = false)]
    Bootstrap {},
}

impl Args {
    fn log_level(&self) -> Level {
        match self.verbose {
            0 => Level::Warning,
            1 => Level::Info,
            2 => Level::Debug,
            _ => Level::Trace,
        }
    }

    /// Build a logger from args.
    pub fn build_logger(&self) -> Logger {
        let drain = slog_bunyan::new(std::io::stdout())
            .set_pretty(false)
            .build()
            .fuse();
        let drain = slog::LevelFilter::new(drain, self.log_level()).fuse();
        let drain = slog_async::Async::new(drain).build().fuse();

        Logger::root(Arc::new(drain), slog::o!())
    }

    /// Execute a command from args.
    pub async fn execute_command(
        &self,
        dependency_manager: Arc<DependencyManager>,
        config: Configuration,
    ) -> Result<(), Box<dyn Error>> {
        // Execute commands
        match self.command.clone() {
            Commands::Serve {} => self.serve(dependency_manager, config).await?,
            Commands::Genesis { genesis_command } => match genesis_command {
                GenesisCommands::Export { target_path } => {
                    println!(
                        "Genesis export payload to sign to {}",
                        target_path.to_string_lossy()
                    );

                    let genesis_tools = GenesisTools::from_dependencies(dependency_manager).await?;
                    genesis_tools.export_payload_to_sign(&target_path)?;
                }
                GenesisCommands::Import {
                    signed_payload_path,
                } => {
                    println!(
                        "Genesis import signed payload from {}",
                        signed_payload_path.to_string_lossy()
                    );

                    let genesis_tools = GenesisTools::from_dependencies(dependency_manager).await?;
                    genesis_tools
                        .import_payload_signature(&signed_payload_path)
                        .await?;
                }
                GenesisCommands::Bootstrap {} => {
                    println!("Genesis bootstrap for test only");

                    let genesis_tools = GenesisTools::from_dependencies(dependency_manager).await?;
                    genesis_tools.bootstrap_test_genesis_certificate().await?;
                }
            },
        }

        Ok(())
    }

    async fn serve(
        &self,
        dependency_manager: Arc<DependencyManager>,
        config: Configuration,
    ) -> Result<(), Box<dyn Error>> {
        // todo: Genesis ?
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
            self.server_ip.clone(),
            self.server_port,
            dependency_manager.clone(),
        );
        http_server.start(shutdown_signal).await;

        handle.abort();

        println!("Exiting...");
        Ok(())
    }
}

impl Source for Args {
    fn clone_into_box(&self) -> Box<dyn Source + Send + Sync> {
        Box::new((*self).clone())
    }

    fn collect(&self) -> Result<Map<String, Value>, config::ConfigError> {
        let mut result = Map::new();
        let uri = "clap arguments".to_string();

        let server_url = format!("http://{}:{}/", &self.server_ip, &self.server_port);
        result.insert(
            "server_url".to_string(),
            Value::new(Some(&uri), ValueKind::from(server_url)),
        );
        result.insert(
            "db_directory".to_string(),
            Value::new(
                Some(&uri),
                ValueKind::from(self.db_directory.to_str().unwrap().to_string()),
            ),
        );
        result.insert(
            "snapshot_directory".to_string(),
            Value::new(
                Some(&uri),
                ValueKind::from(self.snapshot_directory.to_str().unwrap().to_string()),
            ),
        );

        Ok(result)
    }
}

async fn do_first_launch_initialization_if_needed(
    chain_observer: Arc<dyn ChainObserver>,
    protocol_parameters_store: Arc<ProtocolParametersStore>,
    config: &Configuration,
) -> Result<(), Box<dyn Error>> {
    // TODO: Remove that when we hande genesis certificate
    let (work_epoch, epoch_to_sign) = match chain_observer
        .get_current_epoch()
        .await?
        .ok_or("Can't retrieve current epoch")?
    {
        Epoch(0) => (Epoch(0), Epoch(1)),
        epoch => (
            epoch.offset_to_signer_retrieval_epoch()?,
            epoch.offset_to_next_signer_retrieval_epoch()?,
        ),
    };

    if protocol_parameters_store
        .get_protocol_parameters(work_epoch)
        .await?
        .is_none()
    {
        debug!("First launch, will use the configured protocol parameters for the current and next epoch certificate");

        for epoch in [work_epoch, epoch_to_sign] {
            protocol_parameters_store
                .save_protocol_parameters(epoch, config.protocol_parameters.clone())
                .await?;
        }
    }

    Ok(())
}

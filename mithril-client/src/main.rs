#![doc = include_str!("../README.md")]

use clap::{Parser, Subcommand};
use config::builder::DefaultState;
use config::{ConfigBuilder, Map, Source, Value, ValueKind};
use slog::{Drain, Level, Logger};
use slog_scope::debug;
use std::error::Error;
use std::path::PathBuf;
use std::sync::Arc;

use mithril_client::commands::{DownloadCommand, ListCommand, RestoreCommand, ShowCommand};

/// CLI args
#[derive(Parser, Debug, Clone)]
#[clap(name = "mithril-client")]
#[clap(
    about = "This program download, check and restore certified blockchain snapshots.",
    long_about = None
)]
pub struct Args {
    /// Available commands
    #[clap(subcommand)]
    command: Commands,

    /// Run Mode.
    #[clap(long, env = "RUN_MODE", default_value = "dev")]
    run_mode: String,

    /// Verbosity level (-v=warning, -vv=debug, -vvv=very verbose).
    #[clap(short, long, parse(from_occurrences))]
    verbose: usize,

    /// Directory where configuration file is located.
    #[clap(long, default_value = "./config")]
    pub config_directory: PathBuf,

    /// Override configuration Aggregator endpoint URL.
    #[clap(long)]
    aggregator_endpoint: Option<String>,
}

impl Args {
    pub async fn execute(&self) -> Result<(), Box<dyn Error>> {
        debug!("Run Mode: {}", self.run_mode);
        let config: ConfigBuilder<DefaultState> = config::Config::builder()
            .add_source(
                config::File::with_name(&format!(
                    "{}/{}.json",
                    self.config_directory.display(),
                    self.run_mode
                ))
                .required(false),
            )
            .add_source(config::Environment::default())
            .add_source(self.clone());

        self.command.execute(config).await
    }

    fn log_level(&self) -> Level {
        match self.verbose {
            0 => Level::Warning,
            1 => Level::Info,
            2 => Level::Debug,
            _ => Level::Trace,
        }
    }

    fn build_logger(&self) -> Logger {
        let decorator = slog_term::TermDecorator::new().build();
        let drain = slog_term::CompactFormat::new(decorator).build().fuse();
        let drain = slog::LevelFilter::new(drain, self.log_level()).fuse();
        let drain = slog_async::Async::new(drain).build().fuse();

        Logger::root(Arc::new(drain), slog::o!())
    }
}

impl Source for Args {
    fn clone_into_box(&self) -> Box<dyn Source + Send + Sync> {
        Box::new(self.clone())
    }

    fn collect(&self) -> Result<Map<String, Value>, config::ConfigError> {
        let mut map = Map::new();
        let namespace = "clap arguments".to_string();

        if let Some(aggregator_endpoint) = self.aggregator_endpoint.clone() {
            map.insert(
                "aggregator_endpoint".to_string(),
                Value::new(Some(&namespace), ValueKind::from(aggregator_endpoint)),
            );
        }

        Ok(map)
    }
}

/// CLI command list
#[derive(Subcommand, Debug, Clone)]
enum Commands {
    /// List available snapshots
    #[clap(arg_required_else_help = false)]
    List(ListCommand),

    /// Show detailed informations about a snapshot
    #[clap(arg_required_else_help = false)]
    Show(ShowCommand),

    /// Download a snapshot
    #[clap(arg_required_else_help = true)]
    Download(DownloadCommand),

    /// Restore a snapshot
    #[clap(arg_required_else_help = true)]
    Restore(RestoreCommand),
}

impl Commands {
    pub async fn execute(
        &self,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), Box<dyn Error>> {
        match self {
            Self::List(cmd) => cmd.execute(config_builder).await,
            Self::Download(cmd) => cmd.execute(config_builder).await,
            Self::Show(cmd) => cmd.execute(config_builder).await,
            Self::Restore(cmd) => cmd.execute(config_builder).await,
        }
    }
}

#[tokio::main]
async fn main() -> Result<(), String> {
    // Load args
    let args = Args::parse();
    let _guard = slog_scope::set_global_logger(args.build_logger());
    let result = args.execute().await;

    result.map_err(|e| format!("An error occured: {:?}", e))
}

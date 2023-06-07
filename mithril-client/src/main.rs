#![doc = include_str!("../README.md")]

use clap::{Parser, Subcommand};
use config::builder::DefaultState;
use config::{ConfigBuilder, Map, Source, Value, ValueKind};
use mithril_common::StdError;
use slog::{Drain, Level, Logger};
use slog_scope::debug;
use std::path::PathBuf;
use std::sync::Arc;

use mithril_client::commands::snapshot::*;

/// CLI args
#[derive(Parser, Debug, Clone)]
#[clap(name = "mithril-client")]
#[clap(
    about = "This program shows, downloads and verifies certified blockchain artifacts.",
    long_about = None
)]
#[command(version)]
pub struct Args {
    /// Available commands
    #[clap(subcommand)]
    command: ArtifactCommands,

    /// Run Mode.
    #[clap(long, env = "RUN_MODE", default_value = "dev")]
    run_mode: String,

    /// Verbosity level (-v=warning, -vv=info, -vvv=debug).
    #[clap(short, long, action = clap::ArgAction::Count)]
    verbose: u8,

    /// Directory where configuration file is located.
    #[clap(long, default_value = "./config")]
    pub config_directory: PathBuf,

    /// Override configuration Aggregator endpoint URL.
    #[clap(long)]
    aggregator_endpoint: Option<String>,
}

impl Args {
    pub async fn execute(&self) -> Result<(), StdError> {
        debug!("Run Mode: {}", self.run_mode);
        let filename = format!("{}/{}.json", self.config_directory.display(), self.run_mode);
        debug!("Reading configuration file '{}'.", filename);
        let config: ConfigBuilder<DefaultState> = config::Config::builder()
            .add_source(config::File::with_name(&filename).required(false))
            .add_source(config::Environment::default())
            .add_source(self.clone())
            .set_default("download_dir", "")?;

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

#[derive(Subcommand, Debug, Clone)]
enum ArtifactCommands {
    #[clap(subcommand)]
    Snapshot(SnapshotCommands),

    #[clap(subcommand)]
    MithrilStakeDistribution(MithrilStakeDistributionCommands),
}

impl ArtifactCommands {
    pub async fn execute(
        &self,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), StdError> {
        match self {
            Self::Snapshot(cmd) => cmd.execute(config_builder).await,
            Self::MithrilStakeDistribution(cmd) => cmd.execute(config_builder).await,
        }
    }
}

#[tokio::main]
async fn main() -> Result<(), String> {
    // Load args
    let args = Args::parse();
    let _guard = slog_scope::set_global_logger(args.build_logger());

    args.execute()
        .await
        .map_err(|e| format!("An error occured: {e}"))
}

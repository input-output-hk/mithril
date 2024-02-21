mod aggregator;
mod passive;
mod relay;
mod signer;

pub use aggregator::AggregatorCommand;
pub use passive::PassiveCommand;
pub use relay::RelayCommands;
pub use signer::SignerCommand;

use clap::Parser;
use config::{builder::DefaultState, ConfigBuilder, Map, Source, Value};
use mithril_common::StdResult;
use slog::Level;
use slog_scope::debug;
use std::path::PathBuf;

/// Relay for Mithril Node
#[derive(Parser, Debug, Clone)]
#[clap(name = "mithril-relay")]
#[clap(
    about = "This program is a relay for Mithril nodes.",
    long_about = None
)]
#[command(version)]
pub struct Args {
    /// Available commands
    #[clap(subcommand)]
    command: RelayCommands,

    /// Run Mode.
    #[clap(long, env = "RUN_MODE", default_value = "dev")]
    run_mode: String,

    /// Verbosity level (-v=warning, -vv=info, -vvv=debug).
    #[clap(short, long, action = clap::ArgAction::Count)]
    verbose: u8,

    /// Directory where configuration file is located.
    #[clap(long, default_value = "./config")]
    pub config_directory: PathBuf,
}

impl Args {
    /// execute command
    pub async fn execute(&self) -> StdResult<()> {
        debug!("Run Mode: {}", self.run_mode);
        let filename = format!("{}/{}.json", self.config_directory.display(), self.run_mode);
        debug!("Reading configuration file '{}'.", filename);
        let config: ConfigBuilder<DefaultState> = config::Config::builder()
            .add_source(config::File::with_name(&filename).required(false))
            .add_source(self.clone());

        self.command.execute(config).await
    }

    /// get log level from parameters
    pub fn log_level(&self) -> Level {
        match self.verbose {
            0 => Level::Error,
            1 => Level::Warning,
            2 => Level::Info,
            3 => Level::Debug,
            _ => Level::Trace,
        }
    }
}

impl Source for Args {
    fn clone_into_box(&self) -> Box<dyn Source + Send + Sync> {
        Box::new(self.clone())
    }

    fn collect(&self) -> Result<Map<String, Value>, config::ConfigError> {
        Ok(Map::new())
    }
}

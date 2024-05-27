#![doc = include_str!("../README.md")]

use anyhow::{anyhow, Context};
use clap::{CommandFactory, Parser, Subcommand};
use config::{builder::DefaultState, ConfigBuilder, Map, Source, Value, ValueKind};
use slog::{Drain, Fuse, Level, Logger};
use slog_async::Async;
use slog_scope::debug;
use slog_term::Decorator;
use std::io::Write;
use std::sync::Arc;
use std::{fs::File, path::PathBuf};

use mithril_client::MithrilResult;
use mithril_doc::{Documenter, GenerateDocCommands, StructDoc};

use mithril_client_cli::commands::{
    cardano_db::CardanoDbCommands, cardano_transaction::CardanoTransactionCommands,
    mithril_stake_distribution::MithrilStakeDistributionCommands, DeprecatedCommand, Deprecation,
};
use mithril_client_cli::ClapError;

enum LogOutputType {
    StdErr,
    File(String),
}

impl LogOutputType {
    fn get_writer(&self) -> MithrilResult<Box<dyn Write + Send>> {
        let writer: Box<dyn Write + Send> = match self {
            LogOutputType::StdErr => Box::new(std::io::stderr()),
            LogOutputType::File(filepath) => Box::new(
                File::create(filepath)
                    .with_context(|| format!("Can not create output log file: {}", filepath))?,
            ),
        };

        Ok(writer)
    }
}

#[derive(Documenter, Parser, Debug, Clone)]
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
    #[example = "Parsed from the number of occurrences: `-v` for `Warning`, `-vv` for `Info`, `-vvv` for `Debug` and `-vvvv` for `Trace`"]
    verbose: u8,

    /// Directory where configuration file is located.
    #[clap(long, default_value = "./config")]
    pub config_directory: PathBuf,

    /// Override configuration Aggregator endpoint URL.
    #[clap(long, env = "AGGREGATOR_ENDPOINT")]
    #[example = "`https://aggregator.pre-release-preview.api.mithril.network/aggregator`"]
    aggregator_endpoint: Option<String>,

    /// Enable JSON output for logs displayed according to verbosity level
    #[clap(long)]
    log_format_json: bool,

    /// Redirect the logs to a file
    #[clap(long, alias("o"))]
    #[example = "`./mithril-client.log`"]
    log_output: Option<String>,

    /// Enable unstable commands (such as Cardano Transactions)
    #[clap(long)]
    unstable: bool,
}

impl Args {
    pub async fn execute(&self) -> MithrilResult<()> {
        debug!("Run Mode: {}", self.run_mode);
        let filename = format!("{}/{}.json", self.config_directory.display(), self.run_mode);
        debug!("Reading configuration file '{}'.", filename);
        let config: ConfigBuilder<DefaultState> = config::Config::builder()
            .add_source(config::File::with_name(&filename).required(false))
            .add_source(self.clone())
            .set_default("download_dir", "")?;

        self.command.execute(self.unstable, config).await
    }

    fn log_level(&self) -> Level {
        match self.verbose {
            0 => Level::Error,
            1 => Level::Warning,
            2 => Level::Info,
            3 => Level::Debug,
            _ => Level::Trace,
        }
    }

    fn get_log_output_type(&self) -> LogOutputType {
        if let Some(output_filepath) = &self.log_output {
            LogOutputType::File(output_filepath.to_string())
        } else {
            LogOutputType::StdErr
        }
    }

    fn wrap_drain<D: Decorator + Send + 'static>(&self, decorator: D) -> Fuse<Async> {
        let drain = slog_term::CompactFormat::new(decorator).build().fuse();
        let drain = slog::LevelFilter::new(drain, self.log_level()).fuse();

        slog_async::Async::new(drain).build().fuse()
    }

    fn build_logger(&self) -> MithrilResult<Logger> {
        let log_output_type = self.get_log_output_type();
        let writer = log_output_type.get_writer()?;

        let drain = if self.log_format_json {
            let drain = slog_bunyan::new(writer).set_pretty(false).build().fuse();
            let drain = slog::LevelFilter::new(drain, self.log_level()).fuse();

            slog_async::Async::new(drain).build().fuse()
        } else {
            match log_output_type {
                LogOutputType::StdErr => self.wrap_drain(slog_term::TermDecorator::new().build()),
                LogOutputType::File(_) => self.wrap_drain(slog_term::PlainDecorator::new(writer)),
            }
        };

        Ok(Logger::root(Arc::new(drain), slog::o!()))
    }

    fn parse_with_decorator(
        decorator: &dyn Fn(Result<Self, ClapError>) -> Result<Self, ClapError>,
    ) -> Self {
        let result = decorator(Self::try_parse());
        match result {
            Ok(s) => s,
            Err(e) => e.exit(),
        }
    }

    fn handle_deprecated_decorator(
        args_result: Result<Self, ClapError>,
        deprecated_commands: Vec<DeprecatedCommand>,
    ) -> Result<Self, ClapError> {
        let styles = Args::command().get_styles().clone();
        Deprecation::handle_deprecated_commands(args_result, styles, deprecated_commands)
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
    #[clap(subcommand, alias("cdb"))]
    CardanoDb(CardanoDbCommands),

    #[clap(subcommand, alias("msd"))]
    MithrilStakeDistribution(MithrilStakeDistributionCommands),

    #[clap(subcommand, alias("ctx"))]
    CardanoTransaction(CardanoTransactionCommands),

    #[clap(alias("doc"), hide(true))]
    GenerateDoc(GenerateDocCommands),
}

impl ArtifactCommands {
    pub async fn execute(
        &self,
        unstable_enabled: bool,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> MithrilResult<()> {
        match self {
            Self::CardanoDb(cmd) => cmd.execute(config_builder).await,
            Self::MithrilStakeDistribution(cmd) => cmd.execute(config_builder).await,
            Self::CardanoTransaction(ctx) => {
                if !unstable_enabled {
                    Err(anyhow::anyhow!(
                        "The \"cardano-transaction\" subcommand is only accepted using the \
                        --unstable flag.\n \
                    \n \
                    ie: \"mithril-client --unstable cardano-transaction list\""
                    ))
                } else {
                    ctx.execute(config_builder).await
                }
            }
            Self::GenerateDoc(cmd) => cmd
                .execute(&mut Args::command())
                .map_err(|message| anyhow!(message)),
        }
    }
}

#[tokio::main]
async fn main() -> MithrilResult<()> {
    // Load args
    let args = Args::parse_with_decorator(&|result: Result<Args, ClapError>| {
        Args::handle_deprecated_decorator(
            result,
            vec![DeprecatedCommand::new("snapshot", "cardano-db")],
        )
    });
    let _guard = slog_scope::set_global_logger(args.build_logger()?);

    #[cfg(feature = "bundle_openssl")]
    openssl_probe::init_ssl_cert_env_vars();

    args.execute().await
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn fail_if_cardano_tx_command_is_used_without_unstable_flag() {
        let args =
            Args::try_parse_from(["mithril-client", "cardano-transaction", "snapshot", "list"])
                .unwrap();

        args.execute()
            .await
            .expect_err("Should fail if unstable flag missing");
    }
}

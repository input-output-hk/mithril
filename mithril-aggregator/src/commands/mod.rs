mod database_command;
mod era_command;
mod genesis_command;
mod serve_command;
mod tools_command;

use anyhow::anyhow;
use clap::{CommandFactory, Parser, Subcommand};
use config::{builder::DefaultState, ConfigBuilder, Map, Source, Value};
use mithril_cli_helper::{register, register_parameter_opt};
use mithril_common::StdResult;
use mithril_doc::{Documenter, DocumenterDefault, StructDoc};
use slog::{debug, Level, Logger};
use std::path::PathBuf;

use crate::{Configuration, DefaultConfiguration};
use mithril_doc::GenerateDocCommands;

/// Main command selector
#[derive(Debug, Clone, Subcommand)]
pub enum MainCommand {
    Genesis(genesis_command::GenesisCommand),
    Era(era_command::EraCommand),
    Serve(serve_command::ServeCommand),
    Tools(tools_command::ToolsCommand),
    Database(database_command::DatabaseCommand),
    #[clap(alias("doc"), hide(true))]
    GenerateDoc(GenerateDocCommands),
}
/// Identifies the type of command
pub enum CommandType {
    /// Command that runs a server
    Server,

    /// Command that outputs some result after execution
    CommandLine,
}

impl MainCommand {
    pub async fn execute(
        &self,
        root_logger: Logger,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> StdResult<()> {
        match self {
            Self::Genesis(cmd) => cmd.execute(root_logger, config_builder).await,
            Self::Era(cmd) => cmd.execute(root_logger, config_builder).await,
            Self::Serve(cmd) => cmd.execute(root_logger, config_builder).await,
            Self::Tools(cmd) => cmd.execute(root_logger, config_builder).await,
            Self::Database(cmd) => cmd.execute(root_logger).await,
            Self::GenerateDoc(cmd) => {
                let config_infos = vec![Configuration::extract(), DefaultConfiguration::extract()];
                cmd.execute_with_configurations(&mut MainOpts::command(), &config_infos)
                    .map_err(|message| anyhow!(message))
            }
        }
    }

    pub fn command_type(&self) -> CommandType {
        match self {
            MainCommand::Serve(_) => CommandType::Server,
            MainCommand::Genesis(_) => CommandType::CommandLine,
            MainCommand::Era(_) => CommandType::CommandLine,
            MainCommand::Tools(_) => CommandType::CommandLine,
            MainCommand::Database(_) => CommandType::CommandLine,
            MainCommand::GenerateDoc(_) => CommandType::CommandLine,
        }
    }
}

/// Mithril Aggregator Node
#[derive(Documenter, Parser, Debug, Clone)]
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
    #[example = "Parsed from the number of occurrences: `-v` for `Warning`, `-vv` for `Info`, `-vvv` for `Debug` and `-vvvv` for `Trace`"]
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

        // TODO Is it normal to only have db_directory ?
        register_parameter_opt!(result, &namespace, self.db_directory, |v: PathBuf| format!(
            "{}",
            v.to_string_lossy()
        ));

        Ok(result)
    }
}

impl MainOpts {
    /// execute command
    pub async fn execute(&self, root_logger: Logger) -> StdResult<()> {
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
        debug!(root_logger, "Started"; "run_mode" => &self.run_mode, "node_version" => env!("CARGO_PKG_VERSION"));

        self.command.execute(root_logger, config_builder).await
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

#[cfg(test)]
mod tests {
    use config::ValueKind;

    use crate::commands::tools_command::{
        RecomputeCertificatesHashCommand, ToolsCommand, ToolsSubCommand::RecomputeCertificatesHash,
    };

    use super::*;

    // TODO : just here to check there is no regression with the old configuration.
    // We may remove it and probably all tests in this file when macros are finished
    impl MainOpts {
        fn collect_legacy(&self) -> Result<Map<String, Value>, config::ConfigError> {
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

    #[test]
    fn test_main_opts_collect() {
        let serve_command = MainOpts {
            db_directory: Some(PathBuf::from("/db_directory")),
            command: MainCommand::Tools(ToolsCommand {
                genesis_subcommand: RecomputeCertificatesHash(RecomputeCertificatesHashCommand {}),
            }),
            run_mode: "mode".to_string(),
            verbose: 1,
            config_directory: PathBuf::from(""),
        };

        let result = serve_command.collect().unwrap().clone();

        assert_eq!(serve_command.collect_legacy().unwrap(), result);
    }

    #[test]
    fn test_main_opts_collect_when_empty_values() {
        let serve_command = MainOpts {
            db_directory: None,
            command: MainCommand::Tools(ToolsCommand {
                genesis_subcommand: RecomputeCertificatesHash(RecomputeCertificatesHashCommand {}),
            }),
            run_mode: "mode".to_string(),
            verbose: 1,
            config_directory: PathBuf::from(""),
        };

        let result = serve_command.collect().unwrap().clone();

        assert_eq!(serve_command.collect_legacy().unwrap(), result);
    }
}

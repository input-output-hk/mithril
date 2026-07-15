use std::path::PathBuf;

use clap::{Parser, Subcommand};
use config::{ConfigBuilder, builder::DefaultState};
use mithril_common::StdResult;
use slog::Logger;

pub struct ProtocolConfigurationParametersConfiguration {}

#[derive(Parser, Debug, Clone)]
pub struct ProtocolConfigurationCommand {
    /// commands
    #[clap(subcommand)]
    pub protocol_configuration_sub_command: ProtocolConfigurationSubCommand,
}

impl ProtocolConfigurationCommand {
    pub async fn execute(
        &self,
        root_logger: Logger,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> StdResult<()> {
        self.protocol_configuration_sub_command
            .execute(root_logger, config_builder)
            .await
    }
}

#[derive(Debug, Clone, Subcommand)]
pub enum ProtocolConfigurationSubCommand {
    /// Protocol configuration export command.
    Export(ExportProtocolConfigurationSubCommand),

    /// Protocol configuration import command.
    Import(ImportProtocolConfigurationSubCommand),
}

impl ProtocolConfigurationSubCommand {
    pub async fn execute(
        &self,
        root_logger: Logger,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> StdResult<()> {
        match self {
            Self::Export(cmd) => cmd.execute(root_logger, config_builder).await,
            Self::Import(cmd) => cmd.execute(root_logger, config_builder).await,
        }
    }
}

/// Protocol configuration export command
#[derive(Parser, Debug, Clone)]
pub struct ExportProtocolConfigurationSubCommand {
    /// Target path
    #[clap(long)]
    target_path: PathBuf,
}

impl ExportProtocolConfigurationSubCommand {
    pub async fn execute(
        &self,
        root_logger: Logger,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> StdResult<()> {
        Ok(())
    }
}

/// Protocol configuration import command
#[derive(Parser, Debug, Clone)]
pub struct ImportProtocolConfigurationSubCommand {
    /// Import path
    #[clap(long, value_parser)]
    pub import_path: PathBuf,
}

impl ImportProtocolConfigurationSubCommand {
    pub async fn execute(
        &self,
        root_logger: Logger,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> StdResult<()> {
        Ok(())
    }
}

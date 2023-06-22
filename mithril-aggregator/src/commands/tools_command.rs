use clap::{Parser, Subcommand};
use config::{builder::DefaultState, ConfigBuilder};
use slog_scope::debug;
use std::error::Error;

use crate::{dependency_injection::DependenciesBuilder, Configuration};

/// Tools command
#[derive(Parser, Debug, Clone)]
pub struct ToolsCommand {
    /// commands
    #[clap(subcommand)]
    pub genesis_subcommand: ToolsSubCommand,
}

impl ToolsCommand {
    pub async fn execute(
        &self,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), Box<dyn Error>> {
        self.genesis_subcommand.execute(config_builder).await
    }
}

/// Tools subcommands.
#[derive(Debug, Clone, Subcommand)]
pub enum ToolsSubCommand {
    /// Recompute certificates hash command.
    RecomputeCertificatesHash(RecomputeCertificatesHashCommand),
}

impl ToolsSubCommand {
    pub async fn execute(
        &self,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), Box<dyn Error>> {
        match self {
            Self::RecomputeCertificatesHash(cmd) => cmd.execute(config_builder).await,
        }
    }
}

/// Recompute certificates hash command.
#[derive(Parser, Debug, Clone)]
pub struct RecomputeCertificatesHashCommand {}

impl RecomputeCertificatesHashCommand {
    pub async fn execute(
        &self,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> Result<(), Box<dyn Error>> {
        let config: Configuration = config_builder
            .build()
            .map_err(|e| format!("configuration build error: {e}"))?
            .try_deserialize()
            .map_err(|e| format!("configuration deserialize error: {e}"))?;
        debug!("RECOMPUTE CERTIFICATES HASH command"; "config" => format!("{config:?}"));
        println!("Recomputing all certificate hash",);
        let mut dependencies_builder = DependenciesBuilder::new(config.clone());
        let _dependencies = dependencies_builder.create_genesis_container().await?;

        todo!()
    }
}

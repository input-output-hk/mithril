use clap::{Parser, Subcommand};
use config::{builder::DefaultState, ConfigBuilder};
use slog_scope::debug;
use std::{error::Error, sync::Arc};

use crate::{
    database::provider::{CertificateRepository, SignedEntityStoreAdapter},
    dependency_injection::DependenciesBuilder,
    tools::CertificatesHashMigrator,
    Configuration,
};

/// List of tools to upkeep the aggregator
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
    /// Load all certificates in the database to recompute their hash and update all related
    /// entities.
    ///
    /// Since it will modify the aggregator sqlite database it's strongly recommended to backup it
    /// before running this command.
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
        let connection = dependencies_builder.get_sqlite_connection().await?;
        let migrator = CertificatesHashMigrator::new(
            CertificateRepository::new(connection.clone()),
            Arc::new(SignedEntityStoreAdapter::new(connection)),
        );

        migrator
            .migrate()
            .await
            .map_err(|err| format!("Certificate hash migrator error: {err}"))?;
        Ok(())
    }
}

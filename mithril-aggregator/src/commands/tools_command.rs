use anyhow::Context;
use clap::{Parser, Subcommand};
use config::{builder::DefaultState, ConfigBuilder};
use mithril_common::StdResult;
use mithril_persistence::sqlite::vacuum_database;
use slog_scope::debug;
use std::sync::Arc;

use crate::{
    database::repository::{CertificateRepository, SignedEntityStore},
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
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
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
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
        match self {
            Self::RecomputeCertificatesHash(cmd) => cmd.execute(config_builder).await,
        }
    }
}

/// Recompute certificates hash command.
#[derive(Parser, Debug, Clone)]
pub struct RecomputeCertificatesHashCommand {}

impl RecomputeCertificatesHashCommand {
    pub async fn execute(&self, config_builder: ConfigBuilder<DefaultState>) -> StdResult<()> {
        let config: Configuration = config_builder
            .build()
            .with_context(|| "configuration build error")?
            .try_deserialize()
            .with_context(|| "configuration deserialize error")?;
        debug!("RECOMPUTE CERTIFICATES HASH command"; "config" => format!("{config:?}"));
        println!("Recomputing all certificate hash",);
        let mut dependencies_builder = DependenciesBuilder::new(config.clone());
        let connection = dependencies_builder
            .get_sqlite_connection()
            .await
            .with_context(|| "Dependencies Builder can not get sqlite connection")?;
        let migrator = CertificatesHashMigrator::new(
            CertificateRepository::new(connection.clone()),
            Arc::new(SignedEntityStore::new(connection.clone())),
        );

        migrator
            .migrate()
            .await
            .with_context(|| "recompute-certificates-hash: database migration error")?;

        vacuum_database(&connection)
            .with_context(|| "recompute-certificates-hash: database vacuum error")?;

        Ok(())
    }
}

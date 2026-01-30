use std::path::PathBuf;

use anyhow::Context;
use clap::{Parser, Subcommand};
use slog::{Logger, debug};

use mithril_common::StdResult;

use crate::{
    Configuration, SQLITE_FILE, SQLITE_FILE_CARDANO_TRANSACTION,
    dependency_injection::DependenciesBuilder,
};

/// Database tools
#[derive(Parser, Debug, Clone)]
pub struct DatabaseCommand {
    /// commands
    #[clap(subcommand)]
    pub database_subcommand: DatabaseSubCommand,
}

impl DatabaseCommand {
    /// Execute the database command
    pub async fn execute(&self, root_logger: Logger) -> StdResult<()> {
        self.database_subcommand.execute(root_logger).await
    }
}

/// Database subcommands
#[derive(Debug, Clone, Subcommand)]
pub enum DatabaseSubCommand {
    /// Migrate databases located in the given stores directory
    Migrate(MigrateCommand),
}

impl DatabaseSubCommand {
    /// Execute the database subcommand
    pub async fn execute(&self, root_logger: Logger) -> StdResult<()> {
        match self {
            Self::Migrate(cmd) => cmd.execute(root_logger).await,
        }
    }
}

/// Migrate command
#[derive(Parser, Debug, Clone)]
pub struct MigrateCommand {
    /// Stores directory
    #[clap(long, env = "STORES_DIRECTORY")]
    stores_directory: PathBuf,
}

impl MigrateCommand {
    /// Execute the migrate command
    pub async fn execute(&self, root_logger: Logger) -> StdResult<()> {
        let config = Configuration {
            data_stores_directory: self.stores_directory.clone(),
            // Temporary solution to avoid the need to provide a full configuration
            ..Configuration::new_sample("0")
        };
        debug!(root_logger, "DATABASE MIGRATE command"; "config" => format!("{config:?}"));
        println!(
            "Migrating databases from stores directory: {}",
            self.stores_directory.to_string_lossy()
        );
        let services = DependenciesBuilder::new(&config, root_logger.clone());

        services
            .build_main_sqlite_connection(SQLITE_FILE)
            .await
            .with_context(|| "Dependencies Builder can not get sqlite connection")?;

        services
            .build_cardano_tx_sqlite_connection(SQLITE_FILE_CARDANO_TRANSACTION)
            .await
            .with_context(
                || "Dependencies Builder can not get cardano transaction pool sqlite connection",
            )?;

        Ok(())
    }
}

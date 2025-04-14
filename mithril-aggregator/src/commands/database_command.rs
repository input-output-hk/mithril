use std::{path::PathBuf, sync::Arc};

use anyhow::Context;
use clap::{Parser, Subcommand};
use slog::{debug, Logger};

use mithril_common::StdResult;

use crate::{dependency_injection::DependenciesBuilder, Configuration, ExecutionEnvironment};

/// Database tools
#[derive(Parser, Debug, Clone)]
pub struct DatabaseCommand {
    /// commands
    #[clap(subcommand)]
    pub database_subcommand: DatabaseSubCommand,
}

impl DatabaseCommand {
    pub async fn execute(&self, root_logger: Logger) -> StdResult<()> {
        self.database_subcommand.execute(root_logger).await
    }
}

#[derive(Debug, Clone, Subcommand)]
pub enum DatabaseSubCommand {
    /// Migrate databases located in the given stores directory
    Migrate(MigrateCommand),

    /// Vacuum the aggregator main database
    Vacuum(VacuumCommand),
}

impl DatabaseSubCommand {
    pub async fn execute(&self, root_logger: Logger) -> StdResult<()> {
        match self {
            Self::Migrate(cmd) => cmd.execute(root_logger).await,
            Self::Vacuum(cmd) => cmd.execute(root_logger).await,
        }
    }
}

#[derive(Parser, Debug, Clone)]
pub struct MigrateCommand {
    /// Stores directory
    #[clap(long, env = "STORES_DIRECTORY")]
    stores_directory: PathBuf,
}

impl MigrateCommand {
    pub async fn execute(&self, root_logger: Logger) -> StdResult<()> {
        let config = Configuration {
            environment: ExecutionEnvironment::Production,
            data_stores_directory: self.stores_directory.clone(),
            // Temporary solution to avoid the need to provide a full configuration
            ..Configuration::new_sample(std::env::temp_dir())
        };
        debug!(root_logger, "DATABASE MIGRATE command"; "config" => format!("{config:?}"));
        println!(
            "Migrating databases from stores directory: {}",
            self.stores_directory.to_string_lossy()
        );
        let mut dependencies_builder =
            DependenciesBuilder::new(root_logger.clone(), Arc::new(config));

        dependencies_builder
            .get_sqlite_connection()
            .await
            .with_context(|| "Dependencies Builder can not get sqlite connection")?;

        dependencies_builder
            .get_event_store_sqlite_connection()
            .await
            .with_context(|| "Dependencies Builder can not get event store sqlite connection")?;

        dependencies_builder
            .get_sqlite_connection_cardano_transaction_pool()
            .await
            .with_context(|| {
                "Dependencies Builder can not get cardano transaction pool sqlite connection"
            })?;

        Ok(())
    }
}

#[derive(Parser, Debug, Clone)]
pub struct VacuumCommand {
    /// Stores directory
    #[clap(long, env = "STORES_DIRECTORY")]
    stores_directory: PathBuf,
}

impl VacuumCommand {
    pub async fn execute(&self, root_logger: Logger) -> StdResult<()> {
        let config = Configuration {
            environment: ExecutionEnvironment::Production,
            data_stores_directory: self.stores_directory.clone(),
            // Temporary solution to avoid the need to provide a full configuration
            ..Configuration::new_sample(std::env::temp_dir())
        };
        debug!(root_logger, "DATABASE VACUUM command"; "config" => format!("{config:?}"));
        println!(
            "Vacuuming database from stores directory: {}",
            self.stores_directory.to_string_lossy()
        );
        let mut dependencies_builder =
            DependenciesBuilder::new(root_logger.clone(), Arc::new(config.clone()));

        dependencies_builder
            .get_upkeep_service()
            .await
            .with_context(|| "Dependencies Builder can not get upkeep service")?
            .vacuum()
            .await
            .with_context(|| "Upkeep service can not vacuum")?;

        Ok(())
    }
}

use std::{path::PathBuf, sync::Arc};

use anyhow::Context;
use clap::{Parser, Subcommand};
use config::{builder::DefaultState, ConfigBuilder, Map, Value};
use mithril_persistence::sqlite::{SqliteCleaner, SqliteCleaningTask, SqliteConnection};
use serde::{Deserialize, Serialize};
use slog::{debug, Logger};

use mithril_common::StdResult;
use mithril_doc::{Documenter, StructDoc};

use crate::{dependency_injection::DependenciesBuilder, ConfigurationSource, ExecutionEnvironment};

#[derive(Debug, Clone, Serialize, Deserialize, Documenter)]
pub struct DatabaseCommandConfiguration {
    #[example = "`./mithril-aggregator/stores`"]
    pub data_stores_directory: PathBuf,
}

impl ConfigurationSource for DatabaseCommandConfiguration {
    fn environment(&self) -> ExecutionEnvironment {
        ExecutionEnvironment::Production
    }

    fn data_stores_directory(&self) -> PathBuf {
        self.data_stores_directory.clone()
    }

    fn get_sqlite_dir(&self) -> PathBuf {
        let store_dir = &self.data_stores_directory;

        if !store_dir.exists() {
            std::fs::create_dir_all(store_dir).unwrap();
        }

        self.data_stores_directory.clone()
    }

    fn cardano_transactions_database_connection_pool_size(&self) -> usize {
        1
    }
}

/// Database tools
#[derive(Parser, Debug, Clone)]
pub struct DatabaseCommand {
    /// commands
    #[clap(subcommand)]
    pub database_subcommand: DatabaseSubCommand,
}

impl DatabaseCommand {
    pub async fn execute(
        &self,
        root_logger: Logger,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> StdResult<()> {
        self.database_subcommand
            .execute(root_logger, config_builder)
            .await
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
    pub async fn execute(
        &self,
        root_logger: Logger,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> StdResult<()> {
        match self {
            Self::Migrate(cmd) => cmd.execute(root_logger, config_builder).await,
            Self::Vacuum(cmd) => cmd.execute(root_logger, config_builder).await,
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
    pub async fn execute(
        &self,
        root_logger: Logger,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> StdResult<()> {
        let mut config: DatabaseCommandConfiguration = config_builder
            .build()
            .with_context(|| "configuration build error")?
            .try_deserialize()
            .with_context(|| "configuration deserialize error")?;
        config.data_stores_directory = self.stores_directory.clone();
        debug!(root_logger, "DATABASE MIGRATE command"; "config" => format!("{config:?}"));
        println!(
            "Migrating databases from stores directory: {}",
            self.stores_directory.to_string_lossy()
        );
        let mut dependencies_builder =
            DependenciesBuilder::new(root_logger.clone(), Arc::new(config));

        dependencies_builder
            .create_database_command_container()
            .await
            .with_context(|| {
                "Failed to run databases migrations while creating the database command dependencies container"
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
    async fn vacuum_database(
        db_connection: Arc<SqliteConnection>,
        logger: Logger,
    ) -> StdResult<()> {
        SqliteCleaner::new(&db_connection)
            .with_logger(logger)
            .with_tasks(&[SqliteCleaningTask::Vacuum])
            .run()?;

        Ok(())
    }

    pub async fn execute(
        &self,
        root_logger: Logger,
        config_builder: ConfigBuilder<DefaultState>,
    ) -> StdResult<()> {
        let mut config: DatabaseCommandConfiguration = config_builder
            .build()
            .with_context(|| "configuration build error")?
            .try_deserialize()
            .with_context(|| "configuration deserialize error")?;
        config.data_stores_directory = self.stores_directory.clone();
        debug!(root_logger, "DATABASE VACUUM command"; "config" => format!("{config:?}"));
        println!(
            "Vacuuming database from stores directory: {}",
            self.stores_directory.to_string_lossy()
        );
        let mut dependencies_builder =
            DependenciesBuilder::new(root_logger.clone(), Arc::new(config.clone()));

        let dependency_container = dependencies_builder
            .create_database_command_container()
            .await
            .with_context(|| "Failed to create the database command dependencies container")?;

        Self::vacuum_database(dependency_container.main_db_connection, root_logger.clone())
            .await
            .with_context(|| "Failed to vacuum the main database")?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use mithril_common::temp_dir;

    use crate::test_tools::TestLogger;

    use super::*;

    #[tokio::test]
    async fn create_container_does_not_panic() {
        let config = DatabaseCommandConfiguration {
            data_stores_directory: temp_dir!().join("stores"),
        };
        let mut dependencies_builder =
            DependenciesBuilder::new(TestLogger::stdout(), Arc::new(config));

        dependencies_builder
            .create_database_command_container()
            .await
            .expect("Expected container creation to succeed without panicking");
    }
}

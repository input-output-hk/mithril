use anyhow::Context;

use mithril_persistence::database::{ApplicationNodeType, SqlMigration};
use mithril_persistence::sqlite::{
    ConnectionBuilder, ConnectionOptions, SqliteConnection, SqliteConnectionPool,
};
use std::sync::Arc;

use crate::dependency_injection::builder::{
    SQLITE_FILE, SQLITE_FILE_CARDANO_TRANSACTION, SQLITE_MONITORING_FILE,
};
use crate::dependency_injection::{DependenciesBuilder, DependenciesBuilderError, Result};
use crate::ExecutionEnvironment;

impl DependenciesBuilder {
    fn build_sqlite_connection(
        &self,
        sqlite_file_name: &str,
        migrations: Vec<SqlMigration>,
    ) -> Result<SqliteConnection> {
        let logger = self.root_logger();
        let connection_builder = match self.configuration.environment {
            ExecutionEnvironment::Test
                if self.configuration.data_stores_directory.to_string_lossy() == ":memory:" =>
            {
                ConnectionBuilder::open_memory()
            }
            _ => ConnectionBuilder::open_file(
                &self.configuration.get_sqlite_dir().join(sqlite_file_name),
            ),
        };

        let connection = connection_builder
            .with_node_type(ApplicationNodeType::Aggregator)
            .with_options(&[
                ConnectionOptions::EnableForeignKeys,
                ConnectionOptions::EnableWriteAheadLog,
            ])
            .with_logger(logger.clone())
            .with_migrations(migrations)
            .build()
            .map_err(|e| DependenciesBuilderError::Initialization {
                message: "SQLite initialization: failed to build connection.".to_string(),
                error: Some(e),
            })?;

        Ok(connection)
    }

    /// Execute cleanup operations on SQLite connections
    pub async fn drop_sqlite_connections(&self) {
        if let Some(connection) = &self.sqlite_connection {
            let _ = connection.execute("pragma analysis_limit=400; pragma optimize;");
        }

        if let Some(pool) = &self.sqlite_connection_cardano_transaction_pool {
            if let Ok(connection) = pool.connection() {
                let _ = connection.execute("pragma analysis_limit=400; pragma optimize;");
            }
        }
    }

    /// Get SQLite connection
    pub async fn get_sqlite_connection(&mut self) -> Result<Arc<SqliteConnection>> {
        if self.sqlite_connection.is_none() {
            self.sqlite_connection = Some(Arc::new(self.build_sqlite_connection(
                SQLITE_FILE,
                crate::database::migration::get_migrations(),
            )?));
        }

        Ok(self.sqlite_connection.as_ref().cloned().unwrap())
    }
    /// Get EventStore SQLite connection
    pub async fn get_event_store_sqlite_connection(&mut self) -> Result<Arc<SqliteConnection>> {
        if self.sqlite_connection_event_store.is_none() {
            self.sqlite_connection_event_store = Some(Arc::new(self.build_sqlite_connection(
                SQLITE_MONITORING_FILE,
                crate::event_store::database::migration::get_migrations(),
            )?));
        }

        Ok(self
            .sqlite_connection_event_store
            .as_ref()
            .cloned()
            .unwrap())
    }

    async fn build_sqlite_connection_cardano_transaction_pool(
        &mut self,
    ) -> Result<Arc<SqliteConnectionPool>> {
        let connection_pool_size = self
            .configuration
            .cardano_transactions_database_connection_pool_size;
        // little hack to apply migrations to the cardano transaction database
        // todo: add capacity to create a connection pool to the `ConnectionBuilder`
        let _connection = self.build_sqlite_connection(
            SQLITE_FILE_CARDANO_TRANSACTION,
            mithril_persistence::database::cardano_transaction_migration::get_migrations(),
            // Don't vacuum the Cardano transactions database as it can be very large
        )?;

        let connection_pool = Arc::new(SqliteConnectionPool::build(connection_pool_size, || {
            self.build_sqlite_connection(SQLITE_FILE_CARDANO_TRANSACTION, vec![])
                .with_context(|| {
                    "Dependencies Builder can not build SQLite connection for Cardano transactions"
                })
        })?);

        Ok(connection_pool)
    }

    /// Get SQLite connection pool for the cardano transactions store
    pub async fn get_sqlite_connection_cardano_transaction_pool(
        &mut self,
    ) -> Result<Arc<SqliteConnectionPool>> {
        if self.sqlite_connection_cardano_transaction_pool.is_none() {
            self.sqlite_connection_cardano_transaction_pool = Some(
                self.build_sqlite_connection_cardano_transaction_pool()
                    .await?,
            );
        }

        Ok(self
            .sqlite_connection_cardano_transaction_pool
            .as_ref()
            .cloned()
            .unwrap())
    }
}

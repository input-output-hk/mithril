//! ## Upkeep Service
//!
//! This service is responsible for the upkeep of the application.
//!
//! It is in charge of the following tasks:
//! * free up space by executing vacuum and WAL checkpoint on the database

use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;
use slog::{info, Logger};

use mithril_common::signed_entity_type_lock::SignedEntityTypeLock;
use mithril_common::StdResult;
use mithril_persistence::sqlite::{
    SqliteCleaner, SqliteCleaningTask, SqliteConnection, SqliteConnectionPool,
};

/// Define the service responsible for the upkeep of the application.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait UpkeepService: Send + Sync {
    /// Run the upkeep service.
    async fn run(&self) -> StdResult<()>;
}

/// Implementation of the upkeep service for the aggregator.
///
/// To ensure that connections are cleaned up properly, it creates new connections itself
/// instead of relying on a connection pool or a shared connection.
pub struct AggregatorUpkeepService {
    main_db_connection: Arc<SqliteConnection>,
    cardano_tx_connection_pool: Arc<SqliteConnectionPool>,
    signed_entity_type_lock: Arc<SignedEntityTypeLock>,
    logger: Logger,
}

impl AggregatorUpkeepService {
    /// Create a new instance of the aggregator upkeep service.
    pub fn new(
        main_db_connection: Arc<SqliteConnection>,
        cardano_tx_connection_pool: Arc<SqliteConnectionPool>,
        signed_entity_type_lock: Arc<SignedEntityTypeLock>,
        logger: Logger,
    ) -> Self {
        Self {
            main_db_connection,
            cardano_tx_connection_pool,
            signed_entity_type_lock,
            logger,
        }
    }

    async fn upkeep_all_databases(&self) -> StdResult<()> {
        if self.signed_entity_type_lock.has_locked_entities().await {
            info!(
                self.logger,
                "UpkeepService::Some entities are locked - Skipping database upkeep"
            );
            return Ok(());
        }

        let main_db_connection = self.main_db_connection.clone();
        let cardano_tx_db_connection_pool = self.cardano_tx_connection_pool.clone();
        let db_upkeep_logger = self.logger.clone();

        // Run the database upkeep tasks in another thread to avoid blocking the tokio runtime
        let db_upkeep_thread = tokio::task::spawn_blocking(move || -> StdResult<()> {
            info!(db_upkeep_logger, "UpkeepService::Cleaning main database");
            SqliteCleaner::new(&main_db_connection)
                .with_logger(db_upkeep_logger.clone())
                .with_tasks(&[
                    SqliteCleaningTask::Vacuum,
                    SqliteCleaningTask::WalCheckpointTruncate,
                ])
                .run()?;

            info!(
                db_upkeep_logger,
                "UpkeepService::Cleaning cardano transactions database"
            );
            let cardano_tx_db_connection = cardano_tx_db_connection_pool.connection()?;
            SqliteCleaner::new(&cardano_tx_db_connection)
                .with_logger(db_upkeep_logger.clone())
                .with_tasks(&[SqliteCleaningTask::WalCheckpointTruncate])
                .run()?;

            Ok(())
        });

        db_upkeep_thread
            .await
            .with_context(|| "Database Upkeep thread crashed")?
    }
}

#[async_trait]
impl UpkeepService for AggregatorUpkeepService {
    async fn run(&self) -> StdResult<()> {
        info!(self.logger, "UpkeepService::start");

        self.upkeep_all_databases()
            .await
            .with_context(|| "Database upkeep failed")?;

        info!(self.logger, "UpkeepService::end");
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::SignedEntityTypeDiscriminants;
    use mithril_common::test_utils::TempDir;

    use crate::database::test_helper::{
        cardano_tx_db_connection, cardano_tx_db_file_connection, main_db_connection,
        main_db_file_connection,
    };
    use crate::test_tools::TestLogger;

    use super::*;

    #[tokio::test]
    async fn test_cleanup_database() {
        let (main_db_path, ctx_db_path, log_path) = {
            let db_dir = TempDir::create("aggregator_upkeep", "test_cleanup_database");
            (
                db_dir.join("main.db"),
                db_dir.join("cardano_tx.db"),
                db_dir.join("upkeep.log"),
            )
        };

        let main_db_connection = main_db_file_connection(&main_db_path).unwrap();
        let cardano_tx_connection = cardano_tx_db_file_connection(&ctx_db_path).unwrap();

        let service = AggregatorUpkeepService::new(
            Arc::new(main_db_connection),
            Arc::new(SqliteConnectionPool::build_from_connection(
                cardano_tx_connection,
            )),
            Arc::new(SignedEntityTypeLock::default()),
            TestLogger::file(&log_path),
        );

        // Separate block to ensure the log is flushed after run
        {
            service.run().await.expect("Upkeep service failed");
        }

        let logs = std::fs::read_to_string(&log_path).unwrap();

        assert_eq!(
            logs.matches(SqliteCleaningTask::Vacuum.log_message())
                .count(),
            1,
            "Should have run only once since only the main database has a `Vacuum` cleanup"
        );
        assert_eq!(
            logs.matches(SqliteCleaningTask::WalCheckpointTruncate.log_message())
                .count(),
            2,
            "Should have run twice since the two databases have a `WalCheckpointTruncate` cleanup"
        );
    }

    #[tokio::test]
    async fn test_doesnt_cleanup_db_if_any_entity_is_locked() {
        let log_path = TempDir::create(
            "aggregator_upkeep",
            "test_doesnt_cleanup_db_if_any_entity_is_locked",
        )
        .join("upkeep.log");

        let signed_entity_type_lock = Arc::new(SignedEntityTypeLock::default());
        let service = AggregatorUpkeepService::new(
            Arc::new(main_db_connection().unwrap()),
            Arc::new(SqliteConnectionPool::build(1, cardano_tx_db_connection).unwrap()),
            signed_entity_type_lock.clone(),
            TestLogger::file(&log_path),
        );

        // Separate block to ensure the log is flushed after run
        {
            signed_entity_type_lock
                .lock(SignedEntityTypeDiscriminants::CardanoTransactions)
                .await;
            service.run().await.expect("Upkeep service failed");
        }

        let logs = std::fs::read_to_string(&log_path).unwrap();

        assert_eq!(
            logs.matches(SqliteCleaningTask::Vacuum.log_message())
                .count(),
            0,
        );
        assert_eq!(
            logs.matches(SqliteCleaningTask::WalCheckpointTruncate.log_message())
                .count(),
            0,
        );
    }
}

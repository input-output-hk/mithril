//! ## Upkeep Service
//!
//! This service is responsible for the upkeep of the application.
//!
//! It is in charge of the following tasks:
//! * free up space by executing vacuum and WAL checkpoint on the database

use std::path::{Path, PathBuf};

use anyhow::Context;
use async_trait::async_trait;
use slog::{info, Logger};

use mithril_common::StdResult;
use mithril_persistence::sqlite::{vacuum_database, ConnectionBuilder, ConnectionOptions};

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
    main_db_path: PathBuf,
    cardano_tx_path: PathBuf,
    logger: Logger,
}

#[derive(Eq, PartialEq)]
enum DatabaseUpkeepTask {
    Vacuum,
    WalCheckpointTruncate,
}

impl AggregatorUpkeepService {
    /// Create a new instance of the aggregator upkeep service.
    pub fn new<P1: Into<PathBuf>, P2: Into<PathBuf>>(
        main_db_path: P1,
        cardano_tx_path: P2,
        logger: Logger,
    ) -> Self {
        Self {
            main_db_path: main_db_path.into(),
            cardano_tx_path: cardano_tx_path.into(),
            logger,
        }
    }

    fn upkeep_database(db_path: &Path, tasks: &[DatabaseUpkeepTask]) -> StdResult<()> {
        let connection = ConnectionBuilder::open_file(db_path)
            .with_options(&[ConnectionOptions::EnableWriteAheadLog])
            .build()?;

        if tasks.contains(&DatabaseUpkeepTask::Vacuum) {
            vacuum_database(&connection)?;
        }

        // Important: Vacuuming the database will not shrink it until a WAL checkpoint is run, so
        // it must be done after vacuuming.
        if tasks.contains(&DatabaseUpkeepTask::WalCheckpointTruncate) {
            connection.execute("PRAGMA wal_checkpoint(TRUNCATE);")?;
        }

        Ok(())
    }

    async fn upkeep_all_databases(&self) -> StdResult<()> {
        let main_db_path = self.main_db_path.clone();
        let cardano_tx_path = self.cardano_tx_path.clone();
        let db_upkeep_logger = self.logger.clone();

        // Run the database upkeep tasks in another thread to avoid blocking the tokio runtime
        let db_upkeep_thread = tokio::task::spawn_blocking(move || -> StdResult<()> {
            info!(
                db_upkeep_logger,
                "UpkeepService::Cleaning main database";
                "db_path" => main_db_path.display()
            );
            Self::upkeep_database(
                &main_db_path,
                &[
                    DatabaseUpkeepTask::Vacuum,
                    DatabaseUpkeepTask::WalCheckpointTruncate,
                ],
            )?;

            info!(
                db_upkeep_logger,
                "UpkeepService::Cleaning cardano transactions database";
                "db_path" => cardano_tx_path.display()
            );
            Self::upkeep_database(
                &cardano_tx_path,
                &[DatabaseUpkeepTask::WalCheckpointTruncate],
            )?;

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
    use std::ops::Range;

    use mithril_common::test_utils::TempDir;
    use mithril_persistence::sqlite::SqliteConnection;

    use crate::database::test_helper::{cardano_tx_db_file_connection, main_db_file_connection};
    use crate::test_tools::logger_for_tests;

    use super::*;

    fn add_test_table(connection: &SqliteConnection) {
        connection
            .execute("CREATE TABLE test (id INTEGER PRIMARY KEY, text TEXT);")
            .unwrap();
    }

    fn fill_test_table(connection: &SqliteConnection, ids: Range<u64>) {
        connection
            .execute(format!(
                "INSERT INTO test (id, text) VALUES {}",
                ids.map(|i| format!("({}, 'some text to fill the db')", i))
                    .collect::<Vec<String>>()
                    .join(", ")
            ))
            .unwrap();
    }

    fn delete_test_rows(connection: &SqliteConnection, ids: Range<u64>) {
        connection
            .execute(format!(
                "DELETE FROM test WHERE id >= {} and id < {}",
                ids.start, ids.end
            ))
            .unwrap();
    }

    /// Apply migrations, disable auto_vacuum and mangle the database to create some free pages
    /// for the vacuum to reclaim
    fn prepare_dbs(main_db_path: &Path, ctx_db_path: &Path) {
        for connection in &[
            main_db_file_connection(main_db_path).unwrap(),
            cardano_tx_db_file_connection(ctx_db_path).unwrap(),
        ] {
            // Disable Auto vacuum to allow the test to check if the vacuum was run
            connection
                .execute("pragma auto_vacuum = none; vacuum;")
                .unwrap();
            add_test_table(connection);
            fill_test_table(connection, 0..10_000);
            // Checkpoint before deletion so entries are transferred from the WAL file to the main db
            connection
                .execute("PRAGMA wal_checkpoint(PASSIVE)")
                .unwrap();
            delete_test_rows(connection, 0..5_000);
            // Checkpoint after deletion to create free pages in the main db
            connection
                .execute("PRAGMA wal_checkpoint(PASSIVE)")
                .unwrap();
        }
    }

    fn file_size(path: &Path) -> u64 {
        path.metadata()
            .unwrap_or_else(|_| panic!("Failed to read len of '{}'", path.display()))
            .len()
    }

    #[tokio::test]
    async fn test_cleanup_database() {
        let (main_db_path, main_db_wal_path, ctx_db_path, ctx_db_wal_path) = {
            let db_dir = TempDir::create("aggregator_upkeep", "test_cleanup_database");
            (
                db_dir.join("main.db"),
                db_dir.join("main.db-wal"),
                db_dir.join("cardano_tx.db"),
                db_dir.join("cardano_tx.db-wal"),
            )
        };
        prepare_dbs(&main_db_path, &ctx_db_path);

        let main_db_initial_size = file_size(&main_db_path);
        let ctx_db_initial_size = file_size(&ctx_db_path);

        let main_db_connection = main_db_file_connection(&main_db_path).unwrap();
        let cardano_tx_connection = cardano_tx_db_file_connection(&ctx_db_path).unwrap();
        // Make "neutral" changes to the db, this will fill the WAL files with some data
        // but won't change the db size after cleaning up.
        for connection in &[&main_db_connection, &cardano_tx_connection] {
            fill_test_table(connection, 10_000..15_000);
            delete_test_rows(connection, 10_000..15_000);
        }
        assert!(main_db_initial_size > 0);
        assert!(file_size(&main_db_wal_path) > 0);
        assert!(ctx_db_initial_size > 0);
        assert!(file_size(&ctx_db_wal_path) > 0);

        let service = AggregatorUpkeepService::new(&main_db_path, &ctx_db_path, logger_for_tests());

        service.run().await.expect("Upkeep service failed");

        let main_db_after_upkeep_size = file_size(&main_db_path);
        let ctx_db_after_upkeep_size = file_size(&ctx_db_path);

        assert!(
            main_db_initial_size > main_db_after_upkeep_size,
            "Main db size should have decreased (vacuum enabled)"
        );
        assert_eq!(
            file_size(&main_db_wal_path),
            0,
            "Main db wal file should have been truncated"
        );
        assert!(
            ctx_db_initial_size <= ctx_db_after_upkeep_size,
            "Cardano_tx db size should not have decreased (vacuum disabled)"
        );
        assert_eq!(
            file_size(&ctx_db_wal_path),
            0,
            "Cardano_tx db wal file should have been truncated"
        );
    }
}

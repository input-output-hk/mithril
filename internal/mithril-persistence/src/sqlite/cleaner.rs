use slog::{debug, Logger};

use mithril_common::logging::LoggerExtensions;
use mithril_common::StdResult;

use crate::sqlite::SqliteConnection;

/// Tasks that can be performed by the SqliteCleaner
#[derive(Eq, PartialEq, Copy, Clone)]
pub enum SqliteCleaningTask {
    /// Reconstruct the database file, repacking it into a minimal amount of disk space.
    ///
    /// see: <https://www.sqlite.org/lang_vacuum.html>
    ///
    /// ⚠ This operation can be very slow on large databases ⚠
    Vacuum,
    /// Run a checkpoint to transfer the data from the WAL file to the main db file and truncate
    /// it afterward.
    ///
    /// see: <https://www.sqlite.org/pragma.html#pragma_wal_checkpoint>
    WalCheckpointTruncate,
}

impl SqliteCleaningTask {
    /// Get the log message for the task.
    pub fn log_message(self: SqliteCleaningTask) -> &'static str {
        match self {
            SqliteCleaningTask::Vacuum => "Running `vacuum` on the SQLite database",
            SqliteCleaningTask::WalCheckpointTruncate => {
                "Running `wal_checkpoint(TRUNCATE)` on the SQLite database"
            }
        }
    }
}

/// The SqliteCleaner is responsible for cleaning up databases by performing tasks defined
/// in [SqliteCleaningTask].
pub struct SqliteCleaner<'a> {
    connection: &'a SqliteConnection,
    logger: Logger,
    tasks: Vec<SqliteCleaningTask>,
}

impl<'a> SqliteCleaner<'a> {
    /// Create a new instance of the `SqliteCleaner`.
    pub fn new(connection: &'a SqliteConnection) -> Self {
        Self {
            connection,
            logger: Logger::root(slog::Discard, slog::o!()),
            tasks: vec![],
        }
    }

    /// Set the logger to be used by the cleaner.
    pub fn with_logger(mut self, logger: Logger) -> Self {
        self.logger = logger.new_with_component_name::<Self>();
        self
    }

    /// Set the [SqliteCleaningTask] to be performed by the cleaner.
    pub fn with_tasks(mut self, tasks: &[SqliteCleaningTask]) -> Self {
        for option in tasks {
            self.tasks.push(*option);
        }
        self
    }

    /// Cleanup the database by performing the defined tasks.
    pub fn run(self) -> StdResult<()> {
        if self.tasks.contains(&SqliteCleaningTask::Vacuum) {
            debug!(self.logger, "{}", SqliteCleaningTask::Vacuum.log_message());
            self.connection.execute("vacuum")?;
        }

        // Important: If WAL is enabled Vacuuming the database will not shrink until a
        // checkpoint is run, so it must be done after vacuuming.
        // Note: running a checkpoint when the WAL is disabled is harmless.
        if self
            .tasks
            .contains(&SqliteCleaningTask::WalCheckpointTruncate)
        {
            debug!(
                self.logger,
                "{}",
                SqliteCleaningTask::WalCheckpointTruncate.log_message()
            );
            self.connection
                .execute("PRAGMA wal_checkpoint(TRUNCATE);")?;
        } else {
            self.connection.execute("PRAGMA wal_checkpoint(PASSIVE);")?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::ops::Range;
    use std::path::Path;

    use mithril_common::test_utils::TempDir;

    use crate::sqlite::{ConnectionBuilder, ConnectionOptions, SqliteConnection};

    use super::*;

    fn add_test_table(connection: &SqliteConnection) {
        connection
            .execute("CREATE TABLE IF NOT EXISTS test (id INTEGER PRIMARY KEY, text TEXT);")
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
    fn prepare_db_for_vacuum(connection: &SqliteConnection) {
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

    fn file_size(path: &Path) -> u64 {
        path.metadata()
            .unwrap_or_else(|_| panic!("Failed to read len of '{}'", path.display()))
            .len()
    }

    #[test]
    fn cleanup_empty_in_memory_db_should_not_crash() {
        let connection = ConnectionBuilder::open_memory().build().unwrap();

        SqliteCleaner::new(&connection)
            .with_tasks(&[SqliteCleaningTask::Vacuum])
            .run()
            .expect("Vacuum should not fail");
        SqliteCleaner::new(&connection)
            .with_tasks(&[SqliteCleaningTask::WalCheckpointTruncate])
            .run()
            .expect("WalCheckpointTruncate should not fail");
    }

    #[test]
    fn cleanup_empty_file_without_wal_db_should_not_crash() {
        let db_path = TempDir::create(
            "sqlite_cleaner",
            "cleanup_empty_file_without_wal_db_should_not_crash",
        )
        .join("test.db");
        let connection = ConnectionBuilder::open_file(&db_path).build().unwrap();

        SqliteCleaner::new(&connection)
            .with_tasks(&[SqliteCleaningTask::Vacuum])
            .run()
            .expect("Vacuum should not fail");
        SqliteCleaner::new(&connection)
            .with_tasks(&[SqliteCleaningTask::WalCheckpointTruncate])
            .run()
            .expect("WalCheckpointTruncate should not fail");
    }

    #[test]
    fn test_vacuum() {
        let db_dir = TempDir::create("sqlite_cleaner", "test_vacuum");
        let (db_path, db_wal_path) = (db_dir.join("test.db"), db_dir.join("test.db-wal"));
        let connection = ConnectionBuilder::open_file(&db_path)
            .with_options(&[ConnectionOptions::EnableWriteAheadLog])
            .build()
            .unwrap();
        prepare_db_for_vacuum(&connection);

        let db_initial_size = file_size(&db_path);
        assert!(db_initial_size > 0);

        SqliteCleaner::new(&connection)
            .with_tasks(&[SqliteCleaningTask::Vacuum])
            .run()
            .unwrap();

        let db_after_vacuum_size = file_size(&db_path);

        assert!(
            db_initial_size > db_after_vacuum_size,
            "db size should have decreased (vacuum enabled)"
        );
        assert!(
            file_size(&db_wal_path) > 0,
            "db wal file should not have been truncated (truncate disabled)"
        );
    }

    #[test]
    fn test_truncate_wal() {
        let db_dir = TempDir::create("sqlite_cleaner", "test_truncate_wal");
        let (db_path, db_wal_path) = (db_dir.join("test.db"), db_dir.join("test.db-wal"));
        let connection = ConnectionBuilder::open_file(&db_path)
            .with_options(&[ConnectionOptions::EnableWriteAheadLog])
            .build()
            .unwrap();

        // Make "neutral" changes to the db, this will fill the WAL files with some data
        // but won't change the db size after cleaning up.
        add_test_table(&connection);
        fill_test_table(&connection, 0..10_000);
        delete_test_rows(&connection, 0..10_000);

        assert!(file_size(&db_wal_path) > 0);

        SqliteCleaner::new(&connection)
            .with_tasks(&[SqliteCleaningTask::WalCheckpointTruncate])
            .run()
            .unwrap();

        assert_eq!(
            file_size(&db_wal_path),
            0,
            "db wal file should have been truncated"
        );
    }
}

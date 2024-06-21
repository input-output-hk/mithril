use std::sync::Arc;

use async_trait::async_trait;
use slog::{debug, Logger};

use mithril_common::entities::BlockNumber;
use mithril_common::signable_builder::TransactionsImporter;
use mithril_common::StdResult;
use mithril_persistence::sqlite::{vacuum_database, SqliteConnectionPool};

/// A decorator of [TransactionsImporter] that vacuums the database after running the import.
pub struct TransactionsImporterWithVacuum {
    connection_pool: Arc<SqliteConnectionPool>,
    wrapped_importer: Arc<dyn TransactionsImporter>,
    logger: Logger,
}

impl TransactionsImporterWithVacuum {
    /// Create a new instance of [TransactionsImporterWithVacuum].
    pub fn new(
        connection_pool: Arc<SqliteConnectionPool>,
        wrapped_importer: Arc<dyn TransactionsImporter>,
        logger: Logger,
    ) -> Self {
        Self {
            connection_pool,
            wrapped_importer,
            logger,
        }
    }
}

#[async_trait]
impl TransactionsImporter for TransactionsImporterWithVacuum {
    async fn import(&self, up_to_beacon: BlockNumber) -> StdResult<()> {
        self.wrapped_importer.import(up_to_beacon).await?;

        debug!(
            self.logger,
            "Transaction Import finished - Vacuuming database to reclaim disk space"
        );
        let connection = self.connection_pool.connection()?;
        vacuum_database(&connection)?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use mockall::mock;
    use sqlite::Connection;

    use mithril_common::test_utils::TempDir;
    use mithril_persistence::sqlite::SqliteConnection;

    use super::*;

    mock! {
        pub TransactionImporterImpl {}

        #[async_trait]
        impl TransactionsImporter for TransactionImporterImpl {
            async fn import(&self, up_to_beacon: BlockNumber) -> StdResult<()>;
        }
    }

    impl TransactionsImporterWithVacuum {
        pub fn new_with_mock<I>(
            connection_pool: Arc<SqliteConnectionPool>,
            importer_mock_config: I,
        ) -> Self
        where
            I: FnOnce(&mut MockTransactionImporterImpl),
        {
            let mut transaction_importer = MockTransactionImporterImpl::new();
            importer_mock_config(&mut transaction_importer);

            Self::new(
                connection_pool,
                Arc::new(transaction_importer),
                crate::test_tools::logger_for_tests(),
            )
        }
    }

    /// Create a table, insert a lot of data and drop the table to make the database size grow.
    fn mangle_db(connection: &SqliteConnection) {
        connection
            .execute("CREATE TABLE test (id INTEGER PRIMARY KEY, text TEXT);")
            .unwrap();
        connection
            .execute(format!(
                "INSERT INTO test (id, text) VALUES {}",
                (0..10_000)
                    .map(|i| format!("({}, 'some text to fill the db')", i))
                    .collect::<Vec<String>>()
                    .join(", ")
            ))
            .unwrap();
        connection.execute("DROP TABLE test").unwrap();
    }

    #[tokio::test]
    async fn test_database_size_shrink_after_import() {
        let db_path = TempDir::create("mithril-persistence", "test_vacuum").join("test.db");
        let connection = Connection::open_thread_safe(&db_path).unwrap();
        // make the database size grow
        mangle_db(&connection);

        let importer = TransactionsImporterWithVacuum::new_with_mock(
            Arc::new(SqliteConnectionPool::build_from_connection(connection)),
            |mock| {
                mock.expect_import().once().returning(|_| Ok(()));
            },
        );

        let initial_size = db_path.metadata().unwrap().len();

        importer.import(100).await.expect("Import should not fail");

        let after_import_size = db_path.metadata().unwrap().len();

        assert!(
            initial_size > after_import_size,
            "Database size did not shrink after import: \
            initial_size: {initial_size} -> after_import_size: {after_import_size}"
        );
    }
}

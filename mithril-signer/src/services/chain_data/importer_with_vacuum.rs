use std::sync::Arc;

use async_trait::async_trait;
use slog::{Logger, debug};

use mithril_cardano_node_chain::chain_importer::ChainDataImporter;
use mithril_common::StdResult;
use mithril_common::entities::BlockNumber;
use mithril_common::logging::LoggerExtensions;
use mithril_persistence::sqlite::{SqliteCleaner, SqliteCleaningTask, SqliteConnectionPool};

/// A decorator of [ChainDataImporter] that vacuums the database after running the import.
pub struct ChainDataImporterWithVacuum {
    connection_pool: Arc<SqliteConnectionPool>,
    wrapped_importer: Arc<dyn ChainDataImporter>,
    logger: Logger,
}

impl ChainDataImporterWithVacuum {
    /// Create a new instance of [ChainDataImporterWithVacuum].
    pub fn new(
        connection_pool: Arc<SqliteConnectionPool>,
        wrapped_importer: Arc<dyn ChainDataImporter>,
        logger: Logger,
    ) -> Self {
        Self {
            connection_pool,
            wrapped_importer,
            logger: logger.new_with_component_name::<Self>(),
        }
    }
}

#[async_trait]
impl ChainDataImporter for ChainDataImporterWithVacuum {
    async fn import(&self, up_to_beacon: BlockNumber) -> StdResult<()> {
        self.wrapped_importer.import(up_to_beacon).await?;

        debug!(
            self.logger,
            "Chain data Import finished - Vacuuming database to reclaim disk space"
        );
        let connection = self.connection_pool.connection()?;

        SqliteCleaner::new(&connection)
            .with_tasks(&[SqliteCleaningTask::Vacuum])
            .run()?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use mockall::mock;
    use sqlite::Connection;

    use mithril_common::test::TempDir;
    use mithril_persistence::sqlite::SqliteConnection;

    use crate::test::TestLogger;

    use super::*;

    mock! {
        pub ChainDataImporter {}

        #[async_trait]
        impl ChainDataImporter for ChainDataImporter {
            async fn import(&self, up_to_beacon: BlockNumber) -> StdResult<()>;
        }
    }

    impl ChainDataImporterWithVacuum {
        pub(crate) fn new_with_mock<I>(
            connection_pool: Arc<SqliteConnectionPool>,
            importer_mock_config: I,
        ) -> Self
        where
            I: FnOnce(&mut MockChainDataImporter),
        {
            let mut chain_data_importer = MockChainDataImporter::new();
            importer_mock_config(&mut chain_data_importer);

            Self::new(
                connection_pool,
                Arc::new(chain_data_importer),
                TestLogger::stdout(),
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
                    .map(|i| format!("({i}, 'some text to fill the db')"))
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

        let importer = ChainDataImporterWithVacuum::new_with_mock(
            Arc::new(SqliteConnectionPool::build_from_connection(connection)),
            |mock| {
                mock.expect_import().once().returning(|_| Ok(()));
            },
        );

        let initial_size = db_path.metadata().unwrap().len();

        importer
            .import(BlockNumber(100))
            .await
            .expect("Import should not fail");

        let after_import_size = db_path.metadata().unwrap().len();

        assert!(
            initial_size > after_import_size,
            "Database size did not shrink after import: \
            initial_size: {initial_size} -> after_import_size: {after_import_size}"
        );
    }
}

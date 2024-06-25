use std::ops::Not;
use std::path::{Path, PathBuf};

use anyhow::Context;
use slog::{info, Logger};
use sqlite::{Connection, ConnectionThreadSafe};

use mithril_common::StdResult;

use crate::database::{ApplicationNodeType, DatabaseVersionChecker, SqlMigration};
use crate::sqlite::vacuum_database;

/// Builder of SQLite connection
pub struct ConnectionBuilder {
    connection_path: PathBuf,
    sql_migrations: Vec<SqlMigration>,
    options: Vec<ConnectionOptions>,
    node_type: ApplicationNodeType,
    logger: Logger,
}

/// Options to apply to the connection
#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub enum ConnectionOptions {
    /// Enable Write Ahead Log journal mod (not available for in memory connection)
    EnableWriteAheadLog,

    /// Enable foreign key support
    EnableForeignKeys,

    /// Disable foreign key support after the migrations are run
    ///
    /// This option take priority over [ConnectionOptions::EnableForeignKeys] if both are enabled.
    ForceDisableForeignKeys,

    /// Run a VACUUM operation on the database after the connection is opened
    ///
    /// ⚠ This operation can be very slow on large databases ⚠
    Vacuum,
}

impl ConnectionBuilder {
    /// Builder of file SQLite connection
    pub fn open_file(path: &Path) -> Self {
        Self {
            connection_path: path.to_path_buf(),
            sql_migrations: vec![],
            options: vec![],
            node_type: ApplicationNodeType::Signer,
            logger: Logger::root(slog::Discard, slog::o!()),
        }
    }

    /// Builder of in memory SQLite connection
    pub fn open_memory() -> Self {
        Self::open_file(":memory:".as_ref())
    }

    /// Set migrations to apply at build time
    pub fn with_migrations(mut self, migrations: Vec<SqlMigration>) -> Self {
        self.sql_migrations = migrations;
        self
    }

    /// Set the [ConnectionOptions] to enabled on the connection.
    pub fn with_options(mut self, options: &[ConnectionOptions]) -> Self {
        for option in options {
            self.options.push(option.clone());
        }
        self
    }

    /// Set the logger to log to at build time
    pub fn with_logger(mut self, logger: Logger) -> Self {
        self.logger = logger;
        self
    }

    /// Set the node type (default: [ApplicationNodeType::Signer]).
    pub fn with_node_type(mut self, node_type: ApplicationNodeType) -> Self {
        self.node_type = node_type;
        self
    }

    /// Build a connection based on the builder configuration
    pub fn build(self) -> StdResult<ConnectionThreadSafe> {
        let connection =
            Connection::open_thread_safe(&self.connection_path).with_context(|| {
                format!(
                    "SQLite initialization: could not open connection with string '{}'.",
                    self.connection_path.display()
                )
            })?;

        if self.options.contains(&ConnectionOptions::Vacuum) {
            info!(
                self.logger,
                "Vacuuming SQLite database, this may take a while...";
                "database" => self.connection_path.display()
            );

            vacuum_database(&connection)
                .with_context(|| "SQLite initialization: database VACUUM error")?;

            info!(
                self.logger,
                "SQLite database vacuumed successfully";
                "database" => self.connection_path.display()
            );
        }

        if self
            .options
            .contains(&ConnectionOptions::EnableWriteAheadLog)
        {
            connection
                .execute("pragma journal_mode = wal; pragma synchronous = normal;")
                .with_context(|| "SQLite initialization: could not enable WAL.")?;
        }

        if self.options.contains(&ConnectionOptions::EnableForeignKeys) {
            connection
                .execute("pragma foreign_keys=true")
                .with_context(|| "SQLite initialization: could not enable FOREIGN KEY support.")?;
        }

        if self.sql_migrations.is_empty().not() {
            // Check database migrations
            let mut db_checker =
                DatabaseVersionChecker::new(self.logger, self.node_type, &connection);

            for migration in self.sql_migrations {
                db_checker.add_migration(migration);
            }

            db_checker
                .apply()
                .with_context(|| "Database migration error")?;
        }

        if self
            .options
            .contains(&ConnectionOptions::ForceDisableForeignKeys)
        {
            connection
                .execute("pragma foreign_keys=false")
                .with_context(|| "SQLite initialization: could not disable FOREIGN KEY support.")?;
        }

        Ok(connection)
    }
}

#[cfg(test)]
mod tests {
    use sqlite::Value;

    use mithril_common::test_utils::TempDir;

    use crate::sqlite::ConnectionOptions::ForceDisableForeignKeys;

    use super::*;

    // see: https://www.sqlite.org/pragma.html#pragma_journal_mode
    const DEFAULT_SQLITE_JOURNAL_MODE: &str = "delete";
    // see: https://www.sqlite.org/pragma.html#pragma_synchronous
    const NORMAL_SYNCHRONOUS_FLAG: i64 = 1;

    fn execute_single_cell_query(connection: &Connection, query: &str) -> Value {
        let mut statement = connection.prepare(query).unwrap();
        let mut row = statement.iter().next().unwrap().unwrap();
        row.take(0)
    }

    #[test]
    fn test_open_in_memory_without_foreign_key() {
        let connection = ConnectionBuilder::open_memory().build().unwrap();

        let journal_mode = execute_single_cell_query(&connection, "pragma journal_mode;");
        let foreign_keys = execute_single_cell_query(&connection, "pragma foreign_keys;");

        assert_eq!(Value::String("memory".to_string()), journal_mode);
        assert_eq!(Value::Integer(false.into()), foreign_keys);
    }

    #[test]
    fn test_open_with_foreign_key() {
        let connection = ConnectionBuilder::open_memory()
            .with_options(&[ConnectionOptions::EnableForeignKeys])
            .build()
            .unwrap();

        let journal_mode = execute_single_cell_query(&connection, "pragma journal_mode;");
        let foreign_keys = execute_single_cell_query(&connection, "pragma foreign_keys;");

        assert_eq!(Value::String("memory".to_string()), journal_mode);
        assert_eq!(Value::Integer(true.into()), foreign_keys);
    }

    #[test]
    fn test_open_file_without_wal_and_foreign_keys() {
        let dirpath = TempDir::create(
            "mithril_test_database",
            "test_open_file_without_wal_and_foreign_keys",
        );
        let filepath = dirpath.join("db.sqlite3");
        assert!(!filepath.exists());

        let connection = ConnectionBuilder::open_file(&filepath).build().unwrap();

        let journal_mode = execute_single_cell_query(&connection, "pragma journal_mode;");
        let foreign_keys = execute_single_cell_query(&connection, "pragma foreign_keys;");

        assert!(filepath.exists());
        assert_eq!(
            Value::String(DEFAULT_SQLITE_JOURNAL_MODE.to_string()),
            journal_mode
        );
        assert_eq!(Value::Integer(false.into()), foreign_keys);
    }

    #[test]
    fn test_open_file_with_wal_and_foreign_keys() {
        let dirpath = TempDir::create(
            "mithril_test_database",
            "test_open_file_with_wal_and_foreign_keys",
        );
        let filepath = dirpath.join("db.sqlite3");
        assert!(!filepath.exists());

        let connection = ConnectionBuilder::open_file(&filepath)
            .with_options(&[
                ConnectionOptions::EnableForeignKeys,
                ConnectionOptions::EnableWriteAheadLog,
            ])
            .build()
            .unwrap();

        let journal_mode = execute_single_cell_query(&connection, "pragma journal_mode;");
        let foreign_keys = execute_single_cell_query(&connection, "pragma foreign_keys;");

        assert!(filepath.exists());
        assert_eq!(Value::String("wal".to_string()), journal_mode);
        assert_eq!(Value::Integer(true.into()), foreign_keys);
    }

    #[test]
    fn enabling_wal_option_also_set_synchronous_flag_to_normal() {
        let dirpath = TempDir::create(
            "mithril_test_database",
            "enabling_wal_option_also_set_synchronous_flag_to_normal",
        );

        let connection = ConnectionBuilder::open_file(&dirpath.join("db.sqlite3"))
            .with_options(&[ConnectionOptions::EnableWriteAheadLog])
            .build()
            .unwrap();

        let synchronous_flag = execute_single_cell_query(&connection, "pragma synchronous;");

        assert_eq!(Value::Integer(NORMAL_SYNCHRONOUS_FLAG), synchronous_flag);
    }

    #[test]
    fn builder_apply_given_migrations() {
        let connection = ConnectionBuilder::open_memory()
            .with_migrations(vec![
                SqlMigration::new(1, "create table first(id integer);"),
                SqlMigration::new(2, "create table second(id integer);"),
            ])
            .build()
            .unwrap();

        let tables_list = execute_single_cell_query(
            &connection,
            // Note: exclude sqlite system tables and migration system `db_version` table
            "SELECT group_concat(name) FROM sqlite_schema \
            WHERE type = 'table' AND name NOT LIKE 'sqlite_%' AND name != 'db_version' \
            ORDER BY name;",
        );

        assert_eq!(Value::String("first,second".to_string()), tables_list);
    }

    #[test]
    fn can_disable_foreign_keys_even_if_a_migration_enable_them() {
        let connection = ConnectionBuilder::open_memory()
            .with_migrations(vec![SqlMigration::new(1, "pragma foreign_keys=true;")])
            .with_options(&[ForceDisableForeignKeys])
            .build()
            .unwrap();

        let foreign_keys = execute_single_cell_query(&connection, "pragma foreign_keys;");
        assert_eq!(Value::Integer(false.into()), foreign_keys);
    }
}

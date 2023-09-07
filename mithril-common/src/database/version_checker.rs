use anyhow::{anyhow, Context};
use chrono::Utc;
use slog::{debug, error, info, Logger};
use sqlite::Connection;
use std::{cmp::Ordering, collections::BTreeSet, ops::Deref, sync::Arc};
use tokio::sync::Mutex;

use super::{
    ApplicationNodeType, DatabaseVersion, DatabaseVersionProvider, DatabaseVersionUpdater,
    DbVersion,
};

use crate::StdResult;

/// Struct to perform application version check in the database.
pub struct DatabaseVersionChecker {
    /// Pathbuf to the SQLite3 file.
    connection: Arc<Mutex<Connection>>,

    /// Application type which vesion is verified.
    application_type: ApplicationNodeType,

    /// logger
    logger: Logger,

    /// known migrations
    migrations: BTreeSet<SqlMigration>,
}

impl DatabaseVersionChecker {
    /// constructor
    pub fn new(
        logger: Logger,
        application_type: ApplicationNodeType,
        connection: Arc<Mutex<Connection>>,
    ) -> Self {
        let migrations = BTreeSet::new();

        Self {
            connection,
            application_type,
            logger,
            migrations,
        }
    }

    /// Register a migration.
    pub fn add_migration(&mut self, migration: SqlMigration) -> &mut Self {
        let _ = self.migrations.insert(migration);

        self
    }

    /// Apply migrations
    pub async fn apply(&self) -> StdResult<()> {
        debug!(&self.logger, "check database version",);
        let lock = self.connection.lock().await;
        let connection = lock.deref();
        let provider = DatabaseVersionProvider::new(connection);
        provider
            .create_table_if_not_exists(&self.application_type)
            .with_context(|| "Can not create table 'db_version' while applying migrations")?;
        let updater = DatabaseVersionUpdater::new(connection);
        let db_version = provider
            .get_application_version(&self.application_type)?
            .with_context(|| "Can not get application version while applying migrations")
            .unwrap(); // At least a record exists.

        // the current database version is equal to the maximum migration
        // version present in this software.
        // If no migration registered then version = 0.
        let migration_version = self.migrations.iter().map(|m| m.version).max().unwrap_or(0);

        match migration_version.cmp(&db_version.version) {
            Ordering::Greater => {
                debug!(
                    &self.logger,
                    "Database needs upgrade from version '{}' to version '{}', applying new migrations…",
                    db_version.version, migration_version
                );
                self.apply_migrations(&db_version, &updater, connection)?;
                info!(
                    &self.logger,
                    "database upgraded to version '{}'", migration_version
                );
            }
            Ordering::Less => {
                error!(
                    &self.logger,
                    "Software version '{}' is older than database structure version '{}'.",
                    db_version.version,
                    migration_version,
                );

                Err(anyhow!("This software version is older than the database structure. Aborting launch to prevent possible data corruption."))?;
            }
            Ordering::Equal => {
                debug!(&self.logger, "database up to date");
            }
        };

        Ok(())
    }

    fn apply_migrations(
        &self,
        starting_version: &DatabaseVersion,
        updater: &DatabaseVersionUpdater,
        connection: &Connection,
    ) -> StdResult<()> {
        for migration in &self
            .migrations
            .iter()
            .filter(|&m| m.version > starting_version.version)
            .collect::<Vec<&SqlMigration>>()
        {
            connection.execute(&migration.alterations)?;
            let db_version = DatabaseVersion {
                version: migration.version,
                application_type: self.application_type.clone(),
                updated_at: Utc::now(),
            };
            let _ = updater.save(db_version).with_context(|| {
                format!(
                    "Can not save database version when applying migration: '{}'",
                    migration.version
                )
            })?;
        }

        Ok(())
    }
}

/// Represent a file containing SQL structure or data alterations.
#[derive(Debug)]
pub struct SqlMigration {
    /// The semver version this migration targets.
    pub version: DbVersion,

    /// SQL statements to alter the database.
    pub alterations: String,
}

impl SqlMigration {
    /// Create a new SQL migration instance.
    pub fn new(version: DbVersion, alteration: &str) -> Self {
        Self {
            version,
            alterations: alteration.to_string(),
        }
    }
}

impl PartialOrd for SqlMigration {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.version.partial_cmp(&other.version)
    }
}

impl Ord for SqlMigration {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl PartialEq for SqlMigration {
    fn eq(&self, other: &Self) -> bool {
        self.version.eq(&other.version)
    }
}

impl Eq for SqlMigration {}

#[cfg(test)]
mod tests {
    use crate::StdResult;
    use anyhow::Context;
    use std::path::PathBuf;

    use super::*;

    async fn check_database_version(connection: Arc<Mutex<Connection>>, db_version: DbVersion) {
        let lock = connection.lock().await;
        let provider = DatabaseVersionProvider::new(lock.deref());
        let version = provider
            .get_application_version(&ApplicationNodeType::Aggregator)
            .unwrap()
            .unwrap();

        assert_eq!(db_version, version.version);
    }

    fn create_sqlite_file(name: &str) -> StdResult<(PathBuf, Connection)> {
        let dirpath = std::env::temp_dir().join("mithril_test_database");
        std::fs::create_dir_all(&dirpath).unwrap();
        let filepath = dirpath.join(name);

        if filepath.exists() {
            std::fs::remove_file(filepath.as_path()).unwrap();
        }

        let connection =
            Connection::open(&filepath).with_context(|| "connection to sqlite file failure")?;

        Ok((filepath, connection))
    }

    async fn get_table_whatever_column_count(cnt_mutex: Arc<Mutex<Connection>>) -> i64 {
        let lock = cnt_mutex.lock().await;
        let connection = lock.deref();
        let sql = "select count(*) as column_count from pragma_table_info('whatever');";
        let column_count = connection
            .prepare(sql)
            .unwrap()
            .iter()
            .next()
            .unwrap()
            .unwrap()
            .read::<i64, _>(0);

        column_count
    }

    #[tokio::test]
    async fn test_upgrade_with_migration() {
        let (_filepath, connection) =
            create_sqlite_file("test_upgrade_with_migration.sqlite3").unwrap();
        let connection = Arc::new(Mutex::new(connection));
        let mut db_checker = DatabaseVersionChecker::new(
            slog_scope::logger(),
            ApplicationNodeType::Aggregator,
            connection.clone(),
        );

        db_checker.apply().await.unwrap();
        assert_eq!(0, get_table_whatever_column_count(connection.clone()).await);

        db_checker.apply().await.unwrap();
        assert_eq!(0, get_table_whatever_column_count(connection.clone()).await);

        let alterations = "create table whatever (thing_id integer); insert into whatever (thing_id) values (1), (2), (3), (4);";
        let migration = SqlMigration {
            version: 1,
            alterations: alterations.to_string(),
        };
        db_checker.add_migration(migration);
        db_checker.apply().await.unwrap();
        assert_eq!(1, get_table_whatever_column_count(connection.clone()).await);
        check_database_version(connection.clone(), 1).await;

        db_checker.apply().await.unwrap();
        assert_eq!(1, get_table_whatever_column_count(connection.clone()).await);
        check_database_version(connection.clone(), 1).await;

        let alterations = "alter table whatever add column thing_content text; update whatever set thing_content = 'some content'";
        let migration = SqlMigration {
            version: 2,
            alterations: alterations.to_string(),
        };
        db_checker.add_migration(migration);
        db_checker.apply().await.unwrap();
        assert_eq!(2, get_table_whatever_column_count(connection.clone()).await);
        check_database_version(connection.clone(), 2).await;

        // in the test below both migrations are declared in reversed order to
        // ensure they are played in the right order. The last one depends on
        // the 3rd.
        let alterations = "alter table whatever add column one_last_thing text; update whatever set one_last_thing = more_thing";
        let migration = SqlMigration {
            version: 4,
            alterations: alterations.to_string(),
        };
        db_checker.add_migration(migration);
        let alterations = "alter table whatever add column more_thing text; update whatever set more_thing = 'more thing'";
        let migration = SqlMigration {
            version: 3,
            alterations: alterations.to_string(),
        };
        db_checker.add_migration(migration);
        db_checker.apply().await.unwrap();
        assert_eq!(4, get_table_whatever_column_count(connection.clone()).await);
        check_database_version(connection, 4).await;
    }

    #[tokio::test]
    async fn starting_with_migration() {
        let (_filepath, connection) =
            create_sqlite_file("starting_with_migration.sqlite3").unwrap();
        let connection = Arc::new(Mutex::new(connection));
        let mut db_checker = DatabaseVersionChecker::new(
            slog_scope::logger(),
            ApplicationNodeType::Aggregator,
            connection.clone(),
        );

        let alterations = "create table whatever (thing_id integer); insert into whatever (thing_id) values (1), (2), (3), (4);";
        let migration = SqlMigration {
            version: 1,
            alterations: alterations.to_string(),
        };
        db_checker.add_migration(migration);
        db_checker.apply().await.unwrap();
        assert_eq!(1, get_table_whatever_column_count(connection.clone()).await);
        check_database_version(connection, 1).await;
    }

    #[tokio::test]
    /// This test case ensure that when multiple migrations are played and one fails:
    /// * previous migrations are ok and the database version is updated
    /// * further migrations are not played.
    async fn test_failing_migration() {
        let (_filepath, connection) = create_sqlite_file("test_failing_migration.sqlite3").unwrap();
        let connection = Arc::new(Mutex::new(connection));
        let mut db_checker = DatabaseVersionChecker::new(
            slog_scope::logger(),
            ApplicationNodeType::Aggregator,
            connection.clone(),
        );
        // Table whatever does not exist, this should fail with error.
        let alterations = "create table whatever (thing_id integer); insert into whatever (thing_id) values (1), (2), (3), (4);";
        let migration = SqlMigration {
            version: 1,
            alterations: alterations.to_string(),
        };
        db_checker.add_migration(migration);
        let alterations = "alter table wrong add column thing_content text; update whatever set thing_content = 'some content'";
        let migration = SqlMigration {
            version: 2,
            alterations: alterations.to_string(),
        };
        db_checker.add_migration(migration);
        let alterations = "alter table whatever add column thing_content text; update whatever set thing_content = 'some content'";
        let migration = SqlMigration {
            version: 3,
            alterations: alterations.to_string(),
        };
        db_checker.add_migration(migration);
        db_checker.apply().await.unwrap_err();
        check_database_version(connection, 1).await;
    }

    #[tokio::test]
    async fn test_fail_downgrading() {
        let (_filepath, connection) = create_sqlite_file("test_fail_downgrading.sqlite3").unwrap();
        let connection = Arc::new(Mutex::new(connection));
        let mut db_checker = DatabaseVersionChecker::new(
            slog_scope::logger(),
            ApplicationNodeType::Aggregator,
            connection.clone(),
        );
        let alterations = "create table whatever (thing_id integer); insert into whatever (thing_id) values (1), (2), (3), (4);";
        let migration = SqlMigration {
            version: 1,
            alterations: alterations.to_string(),
        };
        db_checker.add_migration(migration);
        db_checker.apply().await.unwrap();
        check_database_version(connection.clone(), 1).await;

        // re instantiate a new checker with no migration registered (version 0).
        let db_checker = DatabaseVersionChecker::new(
            slog_scope::logger(),
            ApplicationNodeType::Aggregator,
            connection.clone(),
        );
        assert!(
            db_checker.apply().await.is_err(),
            "using an old version with an up to date database should fail"
        );
        check_database_version(connection, 1).await;
    }
}

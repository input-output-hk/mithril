use chrono::Local;
use slog::{debug, error};
use slog::{info, Logger};
use sqlite::Connection;

use std::{cmp::Ordering, collections::BTreeSet, path::PathBuf};

use crate::StdError;

use super::{
    ApplicationNodeType, DatabaseVersion, DatabaseVersionProvider, DatabaseVersionUpdater,
    DbVersion,
};

/// Struct to perform application version check in the database.
#[derive(Debug)]
pub struct DatabaseVersionChecker {
    /// Pathbuf to the SQLite3 file.
    sqlite_file_path: PathBuf,

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
        sqlite_file_path: PathBuf,
    ) -> Self {
        let migrations = BTreeSet::new();

        Self {
            sqlite_file_path,
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

    /// Performs an actual version check in the database. This method creates a
    /// connection to the SQLite3 file and drops it at the end.
    pub fn apply(&self) -> Result<(), StdError> {
        debug!(
            &self.logger,
            "check database version, database file = '{}'",
            self.sqlite_file_path.display()
        );
        let connection = Connection::open(&self.sqlite_file_path)?;
        let provider = DatabaseVersionProvider::new(&connection);
        provider.create_table_if_not_exists(&self.application_type)?;
        let updater = DatabaseVersionUpdater::new(&connection);
        let db_version = provider
            .get_application_version(&self.application_type)?
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
                self.apply_migrations(&db_version, &updater, &connection)?;
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

                Err("This software version is older than the database structure. Aborting launch to prevent possible data corruption.")?;
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
    ) -> Result<(), StdError> {
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
                updated_at: Local::now().naive_local(),
            };
            let _ = updater.save(db_version)?;
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
    use super::*;

    fn check_database_version(filepath: &PathBuf, db_version: DbVersion) {
        let connection = Connection::open(filepath).unwrap();
        let provider = DatabaseVersionProvider::new(&connection);
        let version = provider
            .get_application_version(&ApplicationNodeType::Aggregator)
            .unwrap()
            .unwrap();

        assert_eq!(db_version, version.version);
    }

    fn create_sqlite_file(name: &str) -> PathBuf {
        let dirpath = std::env::temp_dir().join("mithril_test_database");
        std::fs::create_dir_all(&dirpath).unwrap();
        let filepath = dirpath.join(name);

        if filepath.exists() {
            std::fs::remove_file(filepath.as_path()).unwrap();
        }

        filepath
    }

    fn get_table_whatever_column_count(filepath: &PathBuf) -> i64 {
        let connection = Connection::open(filepath).unwrap();
        let sql = "select count(*) as column_count from pragma_table_info('whatever');";
        let column_count = connection
            .prepare(sql)
            .unwrap()
            .into_cursor()
            .bind(&[])
            .unwrap()
            .next()
            .unwrap()
            .unwrap()
            .get::<i64, _>(0);

        column_count
    }

    #[test]
    fn test_upgrade_with_migration() {
        let filepath = create_sqlite_file("test_upgrade_with_migration.sqlite3");
        let mut db_checker = DatabaseVersionChecker::new(
            slog_scope::logger(),
            ApplicationNodeType::Aggregator,
            filepath.clone(),
        );

        db_checker.apply().unwrap();
        assert_eq!(0, get_table_whatever_column_count(&filepath));

        db_checker.apply().unwrap();
        assert_eq!(0, get_table_whatever_column_count(&filepath));

        let alterations = "create table whatever (thing_id integer); insert into whatever (thing_id) values (1), (2), (3), (4);";
        let migration = SqlMigration {
            version: 1,
            alterations: alterations.to_string(),
        };
        db_checker.add_migration(migration);
        db_checker.apply().unwrap();
        assert_eq!(1, get_table_whatever_column_count(&filepath));
        check_database_version(&filepath, 1);

        db_checker.apply().unwrap();
        assert_eq!(1, get_table_whatever_column_count(&filepath));
        check_database_version(&filepath, 1);

        let alterations = "alter table whatever add column thing_content text; update whatever set thing_content = 'some content'";
        let migration = SqlMigration {
            version: 2,
            alterations: alterations.to_string(),
        };
        db_checker.add_migration(migration);
        db_checker.apply().unwrap();
        assert_eq!(2, get_table_whatever_column_count(&filepath));
        check_database_version(&filepath, 2);

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
        db_checker.apply().unwrap();
        assert_eq!(4, get_table_whatever_column_count(&filepath));
        check_database_version(&filepath, 4);
    }

    #[test]
    fn starting_with_migration() {
        let filepath = create_sqlite_file("starting_with_migration.sqlite3");
        let mut db_checker = DatabaseVersionChecker::new(
            slog_scope::logger(),
            ApplicationNodeType::Aggregator,
            filepath.clone(),
        );

        let alterations = "create table whatever (thing_id integer); insert into whatever (thing_id) values (1), (2), (3), (4);";
        let migration = SqlMigration {
            version: 1,
            alterations: alterations.to_string(),
        };
        db_checker.add_migration(migration);
        db_checker.apply().unwrap();
        assert_eq!(1, get_table_whatever_column_count(&filepath));
        check_database_version(&filepath, 1);
    }

    #[test]
    /// This test case ensure that when multiple migrations are played and one fails:
    /// * previous migrations are ok and the database version is updated
    /// * further migrations are not played.
    fn test_failing_migration() {
        let filepath = create_sqlite_file("test_failing_migration.sqlite3");
        let mut db_checker = DatabaseVersionChecker::new(
            slog_scope::logger(),
            ApplicationNodeType::Aggregator,
            filepath.clone(),
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
        db_checker.apply().unwrap_err();
        check_database_version(&filepath, 1);
    }

    #[test]
    fn test_fail_downgrading() {
        let filepath = create_sqlite_file("test_fail_downgrading.sqlite3");
        let mut db_checker = DatabaseVersionChecker::new(
            slog_scope::logger(),
            ApplicationNodeType::Aggregator,
            filepath.clone(),
        );
        let alterations = "create table whatever (thing_id integer); insert into whatever (thing_id) values (1), (2), (3), (4);";
        let migration = SqlMigration {
            version: 1,
            alterations: alterations.to_string(),
        };
        db_checker.add_migration(migration);
        db_checker.apply().unwrap();
        check_database_version(&filepath, 1);

        // re instanciate a new checker with no migration registered (version 0).
        let db_checker = DatabaseVersionChecker::new(
            slog_scope::logger(),
            ApplicationNodeType::Aggregator,
            filepath.clone(),
        );
        assert!(
            db_checker.apply().is_err(),
            "using an old version with an up to date database should fail"
        );
        check_database_version(&filepath, 1);
    }
}

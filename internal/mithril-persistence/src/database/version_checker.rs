use anyhow::{anyhow, Context};
use chrono::Utc;
use mithril_common::StdResult;
use slog::{debug, error, info, Logger};
use std::{cmp::Ordering, collections::BTreeSet};

use super::{
    ApplicationNodeType, DatabaseVersion, DbVersion, GetDatabaseVersionQuery,
    UpdateDatabaseVersionQuery,
};

use crate::sqlite::{ConnectionExtensions, SqliteConnection};

/// Struct to perform application version check in the database.
pub struct DatabaseVersionChecker<'conn> {
    /// Pathbuf to the SQLite3 file.
    connection: &'conn SqliteConnection,

    /// Application type which vesion is verified.
    application_type: ApplicationNodeType,

    /// logger
    logger: Logger,

    /// known migrations
    migrations: BTreeSet<SqlMigration>,
}

impl<'conn> DatabaseVersionChecker<'conn> {
    /// constructor
    pub fn new(
        logger: Logger,
        application_type: ApplicationNodeType,
        connection: &'conn SqliteConnection,
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
    pub fn apply(&self) -> StdResult<()> {
        debug!(&self.logger, "check database version",);
        self.create_table_if_not_exists(&self.application_type)
            .with_context(|| "Can not create table 'db_version' while applying migrations")?;
        let db_version = self
            .connection
            .fetch_first(GetDatabaseVersionQuery::get_application_version(
                &self.application_type,
            ))
            .with_context(|| "Can not get application version while applying migrations")?
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
                self.apply_migrations(&db_version, self.connection)?;
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
        connection: &SqliteConnection,
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
            let _ = connection
                .fetch_first(UpdateDatabaseVersionQuery::one(db_version))
                .with_context(|| {
                    format!(
                        "Can not save database version when applying migration: '{}'",
                        migration.version
                    )
                })?;
        }

        Ok(())
    }

    /// Method to create the table at the beginning of the migration procedure.
    /// This code is temporary and should not last.
    pub fn create_table_if_not_exists(
        &self,
        application_type: &ApplicationNodeType,
    ) -> StdResult<()> {
        let connection = self.connection;
        let table_exists = connection.query_single_cell::<_, i64>(
            "select exists(select name from sqlite_master where type='table' and name='db_version') as table_exists",
            &[],
        )? == 1;

        if !table_exists {
            let sql = format!("
create table db_version (application_type text not null primary key, version integer not null, updated_at text not null);
insert into db_version (application_type, version, updated_at) values ('{application_type}', 0, '{}');
", Utc::now().to_rfc3339());
            connection.execute(sql)?;
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
    pub fn new<T: Into<String>>(version: DbVersion, alteration: T) -> Self {
        Self {
            version,
            alterations: alteration.into(),
        }
    }
}

impl PartialOrd for SqlMigration {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for SqlMigration {
    fn cmp(&self, other: &Self) -> Ordering {
        self.version.cmp(&other.version)
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
    use anyhow::Context;
    use mithril_common::test_utils::TempDir;
    use mithril_common::StdResult;
    use sqlite::Connection;
    use std::path::PathBuf;

    use super::*;

    fn check_database_version(connection: &SqliteConnection, db_version: DbVersion) {
        let version = connection
            .fetch_first(GetDatabaseVersionQuery::get_application_version(
                &ApplicationNodeType::Aggregator,
            ))
            .unwrap()
            .unwrap();

        assert_eq!(db_version, version.version);
    }

    fn create_sqlite_file(test_name: &str) -> StdResult<(PathBuf, SqliteConnection)> {
        let dirpath = TempDir::create("mithril_test_database", test_name);
        let filepath = dirpath.join("db.sqlite3");

        let connection = Connection::open_thread_safe(&filepath)
            .with_context(|| "connection to sqlite file failure")?;

        Ok((filepath, connection))
    }

    fn get_table_whatever_column_count(connection: &SqliteConnection) -> i64 {
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

    #[test]
    fn test_upgrade_with_migration() {
        let (_filepath, connection) =
            create_sqlite_file("test_upgrade_with_migration.sqlite3").unwrap();
        let mut db_checker = DatabaseVersionChecker::new(
            slog_scope::logger(),
            ApplicationNodeType::Aggregator,
            &connection,
        );

        db_checker.apply().unwrap();
        assert_eq!(0, get_table_whatever_column_count(&connection));

        db_checker.apply().unwrap();
        assert_eq!(0, get_table_whatever_column_count(&connection));

        let alterations = "create table whatever (thing_id integer); insert into whatever (thing_id) values (1), (2), (3), (4);";
        let migration = SqlMigration {
            version: 1,
            alterations: alterations.to_string(),
        };
        db_checker.add_migration(migration);
        db_checker.apply().unwrap();
        assert_eq!(1, get_table_whatever_column_count(&connection));
        check_database_version(&connection, 1);

        db_checker.apply().unwrap();
        assert_eq!(1, get_table_whatever_column_count(&connection));
        check_database_version(&connection, 1);

        let alterations = "alter table whatever add column thing_content text; update whatever set thing_content = 'some content'";
        let migration = SqlMigration {
            version: 2,
            alterations: alterations.to_string(),
        };
        db_checker.add_migration(migration);
        db_checker.apply().unwrap();
        assert_eq!(2, get_table_whatever_column_count(&connection));
        check_database_version(&connection, 2);

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
        assert_eq!(4, get_table_whatever_column_count(&connection));
        check_database_version(&connection, 4);
    }

    #[test]
    fn starting_with_migration() {
        let (_filepath, connection) = create_sqlite_file("starting_with_migration").unwrap();
        let mut db_checker = DatabaseVersionChecker::new(
            slog_scope::logger(),
            ApplicationNodeType::Aggregator,
            &connection,
        );

        let alterations = "create table whatever (thing_id integer); insert into whatever (thing_id) values (1), (2), (3), (4);";
        let migration = SqlMigration {
            version: 1,
            alterations: alterations.to_string(),
        };
        db_checker.add_migration(migration);
        db_checker.apply().unwrap();
        assert_eq!(1, get_table_whatever_column_count(&connection));
        check_database_version(&connection, 1);
    }

    #[test]
    /// This test case ensure that when multiple migrations are played and one fails:
    /// * previous migrations are ok and the database version is updated
    /// * further migrations are not played.
    fn test_failing_migration() {
        let (_filepath, connection) = create_sqlite_file("test_failing_migration").unwrap();
        let mut db_checker = DatabaseVersionChecker::new(
            slog_scope::logger(),
            ApplicationNodeType::Aggregator,
            &connection,
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
        check_database_version(&connection, 1);
    }

    #[test]
    fn test_fail_downgrading() {
        let (_filepath, connection) = create_sqlite_file("test_fail_downgrading").unwrap();
        let mut db_checker = DatabaseVersionChecker::new(
            slog_scope::logger(),
            ApplicationNodeType::Aggregator,
            &connection,
        );
        let alterations = "create table whatever (thing_id integer); insert into whatever (thing_id) values (1), (2), (3), (4);";
        let migration = SqlMigration {
            version: 1,
            alterations: alterations.to_string(),
        };
        db_checker.add_migration(migration);
        db_checker.apply().unwrap();
        check_database_version(&connection, 1);

        // re instantiate a new checker with no migration registered (version 0).
        let db_checker = DatabaseVersionChecker::new(
            slog_scope::logger(),
            ApplicationNodeType::Aggregator,
            &connection,
        );
        assert!(
            db_checker.apply().is_err(),
            "using an old version with an up to date database should fail"
        );
        check_database_version(&connection, 1);
    }
}

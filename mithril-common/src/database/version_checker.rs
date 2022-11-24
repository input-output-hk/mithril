use chrono::Local;
use semver::Version;
use slog::{debug, error};
use slog::{info, Logger};
use sqlite::Connection;
use std::str::FromStr;
use std::{cmp::Ordering, collections::BTreeSet, error::Error, path::PathBuf};

use super::{
    ApplicationNodeType, DatabaseVersion, DatabaseVersionProvider, DatabaseVersionUpdater,
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
    pub fn check(&self, current_semver: &str) -> Result<(), Box<dyn Error>> {
        debug!(
            &self.logger,
            "check database version, database file = '{}'",
            self.sqlite_file_path.display()
        );
        let connection = Connection::open(&self.sqlite_file_path)?;
        let provider = DatabaseVersionProvider::new(&connection);
        provider.create_table_if_not_exists()?;
        let updater = DatabaseVersionUpdater::new(&connection);
        let version = provider
            .get_application_version(&self.application_type)?
            .unwrap_or_else(|| DatabaseVersion {
                semver: Version::from_str("0.0.0").unwrap(),
                application_type: self.application_type.clone(),
                updated_at: Local::now().naive_local(),
            });
        let current_version = DatabaseVersion {
            semver: Version::parse(current_semver)?,
            application_type: self.application_type.clone(),
            updated_at: Local::now().naive_local(),
        };

        match current_version.semver.cmp(&version.semver) {
            Ordering::Greater => {
                debug!(
                    &self.logger,
                    "Application '{}' is newer than last database migration '{}', checking for new migrations…",
                    current_version.semver, version.semver
                );
                if let Some(version) =
                    self.apply_migrations(&version, &current_version.semver, &updater, &connection)?
                {
                    info!(
                        &self.logger,
                        "database upgraded to version '{}'",
                        version.to_string()
                    );
                } else {
                    debug!(&self.logger, "no database upgrade needed");
                }
            }
            Ordering::Less => {
                error!(
                    &self.logger,
                    "Software version '{}' is older than database structure version '{}'.",
                    current_version.semver,
                    version.semver
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
        starting_db_version: &DatabaseVersion,
        app_version: &Version,
        updater: &DatabaseVersionUpdater,
        connection: &Connection,
    ) -> Result<Option<Version>, Box<dyn Error>> {
        let mut db_version = starting_db_version.clone();
        let mut last_updated_version: Option<Version> = None;

        for migration in &self.migrations {
            if migration.version > starting_db_version.semver && migration.version <= *app_version {
                connection.execute(&migration.alterations)?;
                db_version.semver = migration.version.clone();
                db_version = updater.save(db_version)?;
                last_updated_version = Some(db_version.semver.clone());
            }
        }

        Ok(last_updated_version)
    }
}

/// Represent a file containing SQL structure or data alterations.
#[derive(Debug)]
pub struct SqlMigration {
    /// The semver version this migration targets.
    pub version: Version,

    /// SQL statements to alter the database.
    pub alterations: String,
}

impl SqlMigration {
    /// Create a new SQL migration instance.
    pub fn new(version: &Version, alteration: &str) -> Self {
        Self {
            version: version.clone(),
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

    fn check_database_version(filepath: &PathBuf, semver: &str) {
        let connection = Connection::open(filepath).unwrap();
        let provider = DatabaseVersionProvider::new(&connection);
        let version = provider
            .get_application_version(&ApplicationNodeType::Aggregator)
            .unwrap()
            .unwrap_or_else(|| DatabaseVersion {
                application_type: ApplicationNodeType::Aggregator,
                semver: Version::from_str("0.0.0").unwrap(),
                updated_at: Local::now().naive_local(),
            });

        assert_eq!(semver, version.semver.to_string());
    }

    fn create_sqlite_file(name: &str) -> PathBuf {
        let filepath = std::env::temp_dir().join(name);

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
    fn test_database_version_checker() {
        let filepath = create_sqlite_file("test_1.sqlite3");
        let mut db_checker = DatabaseVersionChecker::new(
            slog_scope::logger(),
            ApplicationNodeType::Aggregator,
            filepath.clone(),
        );
        db_checker.add_migration(SqlMigration {
            version: Version::from_str("1.0.10").unwrap(),
            alterations: "select false".to_string(),
        });
        db_checker.check("1.0.0").unwrap();
        check_database_version(&filepath, "0.0.0");

        db_checker.check("1.0.0").unwrap();
        check_database_version(&filepath, "0.0.0");

        db_checker.check("1.1.0").unwrap();
        check_database_version(&filepath, "1.0.10");

        db_checker.check("1.0.12").unwrap();
        check_database_version(&filepath, "1.0.10");

        db_checker.check("1.0.9").unwrap_err();
    }

    #[test]
    fn test_upgrade_with_migration() {
        let filepath = create_sqlite_file("test_2.sqlite3");
        let mut db_checker = DatabaseVersionChecker::new(
            slog_scope::logger(),
            ApplicationNodeType::Aggregator,
            filepath.clone(),
        );

        // The order of the migrations is intentionnaly made in reverse to
        // ensure the version semver is the ordering criteria when applied.
        let alterations = "alter table whatever add column thing_content text; update whatever set thing_content = 'some content'";
        let migration = SqlMigration {
            version: Version::from_str("1.1.0").unwrap(),
            alterations: alterations.to_string(),
        };
        db_checker.add_migration(migration);

        let alterations = "create table whatever (thing_id integer); insert into whatever (thing_id) values (1), (2), (3), (4);";
        let migration = SqlMigration {
            version: Version::from_str("1.0.5").unwrap(),
            alterations: alterations.to_string(),
        };
        db_checker.add_migration(migration);

        db_checker.check("1.0.0").unwrap();
        assert_eq!(0, get_table_whatever_column_count(&filepath));

        db_checker.check("1.0.4").unwrap();
        assert_eq!(0, get_table_whatever_column_count(&filepath));

        db_checker.check("1.0.5").unwrap();
        assert_eq!(1, get_table_whatever_column_count(&filepath));
        check_database_version(&filepath, "1.0.5");

        db_checker.check("1.0.9").unwrap();
        assert_eq!(1, get_table_whatever_column_count(&filepath));
        check_database_version(&filepath, "1.0.5");

        db_checker.check("1.1.1").unwrap();
        assert_eq!(2, get_table_whatever_column_count(&filepath));
        check_database_version(&filepath, "1.1.0");
    }
}

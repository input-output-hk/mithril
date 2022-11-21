use chrono::Local;
use semver::Version;
use slog::Logger;
use slog::{debug, error, warn};
use sqlite::Connection;
use std::str::FromStr;
use std::{cmp::Ordering, collections::BTreeSet, error::Error, path::PathBuf};

use super::{ApplicationNodeType, ApplicationVersion, VersionProvider, VersionUpdaterProvider};

/// Struct to perform application version check in the database.
#[derive(Debug)]
pub struct ApplicationVersionChecker {
    /// Pathbuf to the SQLite3 file.
    sqlite_file_path: PathBuf,

    /// Application type which vesion is verified.
    application_type: ApplicationNodeType,

    /// logger
    logger: Logger,

    /// known migrations
    migrations: BTreeSet<SqlMigration>,
}

impl ApplicationVersionChecker {
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
            "check application version, database file = '{}'",
            self.sqlite_file_path.display()
        );
        let connection = Connection::open(&self.sqlite_file_path)?;
        let provider = VersionProvider::new(&connection);
        provider.create_table_if_not_exists()?;
        let updater = VersionUpdaterProvider::new(&connection);
        let version = provider
            .get_application_version(&self.application_type)?
            .unwrap_or_else(|| ApplicationVersion {
                semver: Version::from_str("0.0.0").unwrap(),
                application_type: self.application_type.clone(),
                updated_at: Local::now().naive_local(),
            });
        let current_version = ApplicationVersion {
            semver: Version::parse(current_semver)?,
            application_type: self.application_type.clone(),
            updated_at: Local::now().naive_local(),
        };

        match current_version.semver.cmp(&version.semver) {
            Ordering::Greater => {
                warn!(
                    &self.logger,
                    "Application version '{}' is out of date, new version is '{}'. Upgrading database…",
                    version.semver, current_version.semver
                );
                self.apply_migrations(&version, &current_version.semver, &updater, &connection)?;
                let current_version = updater.save(current_version)?;
                debug!(
                    &self.logger,
                    "database updated to version {}",
                    current_version.semver.to_string()
                );
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
        starting_app_version: &ApplicationVersion,
        target_version: &Version,
        updater: &VersionUpdaterProvider,
        connection: &Connection,
    ) -> Result<(), Box<dyn Error>> {
        let mut app_version = starting_app_version.clone();

        for migration in &self.migrations {
            if migration.version > starting_app_version.semver
                && migration.version <= *target_version
            {
                connection.execute(&migration.alterations)?;
                app_version.semver = migration.version.clone();
                app_version = updater.save(app_version)?;
            }
        }

        Ok(())
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
        let provider = VersionProvider::new(&connection);
        let version = provider
            .get_application_version(&ApplicationNodeType::Aggregator)
            .unwrap()
            .expect("there should be a version in the database");

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
    fn test_application_version_checker() {
        let filepath = create_sqlite_file("test_1.sqlite3");
        let app_checker = ApplicationVersionChecker::new(
            slog_scope::logger(),
            ApplicationNodeType::Aggregator,
            filepath.clone(),
        );
        app_checker.check("1.0.0").unwrap();
        check_database_version(&filepath, "1.0.0");
        app_checker.check("1.0.0").unwrap();
        check_database_version(&filepath, "1.0.0");
        app_checker.check("1.1.0").unwrap();
        check_database_version(&filepath, "1.1.0");
        app_checker.check("1.0.9").unwrap_err();
    }

    #[test]
    fn test_upgrade_with_migration() {
        let filepath = create_sqlite_file("test_2.sqlite3");
        let mut app_checker = ApplicationVersionChecker::new(
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
        app_checker.add_migration(migration);

        let alterations = "create table whatever (thing_id integer); insert into whatever (thing_id) values (1), (2), (3), (4);";
        let migration = SqlMigration {
            version: Version::from_str("1.0.5").unwrap(),
            alterations: alterations.to_string(),
        };
        app_checker.add_migration(migration);

        app_checker.check("1.0.0").unwrap();
        assert_eq!(0, get_table_whatever_column_count(&filepath));

        app_checker.check("1.0.4").unwrap();
        assert_eq!(0, get_table_whatever_column_count(&filepath));

        app_checker.check("1.0.5").unwrap();
        assert_eq!(1, get_table_whatever_column_count(&filepath));
        check_database_version(&filepath, "1.0.5");

        app_checker.check("1.0.9").unwrap();
        assert_eq!(1, get_table_whatever_column_count(&filepath));
        check_database_version(&filepath, "1.0.9");

        app_checker.check("1.1.1").unwrap();
        assert_eq!(2, get_table_whatever_column_count(&filepath));
        check_database_version(&filepath, "1.1.1");
    }
}

//! SQLite module.
//! This module provides a minimal yet useful Entity framework on top of SQLite
//! with ability to perform any SQL query possible and hydrate results in Rust
//! structs.
mod condition;
mod connection_builder;
mod connection_extensions;
mod cursor;
mod entity;
mod projection;
mod provider;
mod source_alias;

pub use condition::{GetAllCondition, WhereCondition};
pub use connection_builder::{ConnectionBuilder, ConnectionOptions};
pub use connection_extensions::ConnectionExtensions;
pub use cursor::EntityCursor;
pub use entity::{HydrationError, SqLiteEntity};
pub use projection::{Projection, ProjectionField};
pub use provider::{GetAllProvider, Provider};
pub use source_alias::SourceAlias;

use mithril_common::StdResult;
use sqlite::ConnectionThreadSafe;

/// Type of the connection used in Mithril
pub type SqliteConnection = ConnectionThreadSafe;

/// Do a [vacuum](https://www.sqlite.org/lang_vacuum.html) on the given connection, this will
/// reconstruct the database file, repacking it into a minimal amount of disk space.
pub async fn vacuum_database(connection: &SqliteConnection) -> StdResult<()> {
    connection.execute("vacuum")?;

    Ok(())
}

#[cfg(test)]
mod test {
    use crate::sqlite::vacuum_database;
    use sqlite::Connection;

    #[tokio::test]
    async fn calling_vacuum_on_an_empty_in_memory_db_should_not_fail() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();

        vacuum_database(&connection)
            .await
            .expect("Vacuum should not fail");
    }

    #[test]
    fn sqlite_version_should_be_3_42_or_more() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        let mut statement = connection.prepare("select sqlite_version()").unwrap();
        let cursor = statement.iter().next().unwrap().unwrap();
        let db_version = cursor.read::<&str, _>(0);
        let version = semver::Version::parse(db_version)
            .expect("Sqlite version should be parsable to semver");
        let requirement = semver::VersionReq::parse(">=3.42.0").unwrap();

        assert!(
            requirement.matches(&version),
            "Sqlite version {} is lower than 3.42.0",
            version
        )
    }
}

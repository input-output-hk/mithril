//! SQLite module.
//! This module provides a minimal yet useful Entity framework on top of SQLite
//! with ability to perform any SQL query possible and hydrate results in Rust
//! structs.
mod condition;
mod cursor;
mod entity;
mod projection;
mod provider;
mod source_alias;

pub use condition::WhereCondition;
pub use cursor::EntityCursor;
pub use entity::{HydrationError, SqLiteEntity};
pub use projection::{Projection, ProjectionField};
pub use provider::Provider;
pub use source_alias::SourceAlias;

use sqlite::Connection;
use std::sync::Arc;
use tokio::sync::Mutex;

use crate::StdResult;

/// Do a [vacuum](https://www.sqlite.org/lang_vacuum.html) on the given connection, this will
/// reconstruct the database file, repacking it into a minimal amount of disk space.
pub async fn vacuum_database(connection: Arc<Mutex<Connection>>) -> StdResult<()> {
    let connection = &connection.lock().await;
    connection.execute("vacuum")?;
    Ok(())
}

#[cfg(test)]
mod test {
    use crate::sqlite::vacuum_database;
    use sqlite::Connection;
    use std::sync::Arc;
    use tokio::sync::Mutex;

    #[tokio::test]
    async fn calling_vacuum_on_an_empty_in_memory_db_should_not_fail() {
        let connection = Arc::new(Mutex::new(Connection::open(":memory:").unwrap()));

        vacuum_database(connection)
            .await
            .expect("Vacuum should not fail");
    }
}

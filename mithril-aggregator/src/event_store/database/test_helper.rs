use std::path::Path;

use mithril_common::StdResult;
use mithril_persistence::sqlite::{ConnectionBuilder, ConnectionOptions, SqliteConnection};

/// File sqlite database without foreign key support with event store migrations applied and WAL activated
pub fn event_store_db_file_connection(db_path: &Path) -> StdResult<SqliteConnection> {
    let builder = ConnectionBuilder::open_file(db_path)
        .with_options(&[ConnectionOptions::EnableWriteAheadLog]);
    build_event_store_db_connection(builder)
}

/// In-memory sqlite database without foreign key support with migrations applied
pub fn event_store_db_connection() -> StdResult<SqliteConnection> {
    let builder = ConnectionBuilder::open_memory();
    build_event_store_db_connection(builder)
}

fn build_event_store_db_connection(
    connection_builder: ConnectionBuilder,
) -> StdResult<SqliteConnection> {
    let connection = connection_builder
        .with_options(&[ConnectionOptions::ForceDisableForeignKeys])
        .with_migrations(crate::event_store::database::migration::get_migrations())
        .build()?;
    Ok(connection)
}

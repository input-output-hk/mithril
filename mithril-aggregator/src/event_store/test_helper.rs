use mithril_common::StdResult;
use mithril_persistence::sqlite::{ConnectionBuilder, ConnectionOptions, SqliteConnection};

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
        .with_migrations(crate::event_store::migration::get_migrations())
        .build()?;
    Ok(connection)
}

use std::path::Path;

use mithril_common::StdResult;
use mithril_persistence::sqlite::{ConnectionBuilder, ConnectionOptions, SqliteConnection};

/// In-memory sqlite database without foreign key support with migrations applied
pub fn main_db_connection() -> StdResult<SqliteConnection> {
    let builder = ConnectionBuilder::open_memory();
    build_main_db_connection(builder)
}

/// File sqlite database without foreign key support with migrations applied and WAL activated
pub fn main_db_file_connection(db_path: &Path) -> StdResult<SqliteConnection> {
    let builder = ConnectionBuilder::open_file(db_path)
        .with_options(&[ConnectionOptions::EnableWriteAheadLog]);
    build_main_db_connection(builder)
}

fn build_main_db_connection(connection_builder: ConnectionBuilder) -> StdResult<SqliteConnection> {
    let connection = connection_builder
        .with_options(&[ConnectionOptions::ForceDisableForeignKeys])
        .with_migrations(crate::database::migration::get_migrations())
        .build()?;
    Ok(connection)
}

/// In-memory sqlite database without foreign key support with cardano db migrations applied
pub fn cardano_tx_db_connection() -> StdResult<SqliteConnection> {
    let builder = ConnectionBuilder::open_memory();
    build_cardano_tx_db_connection(builder)
}

/// File sqlite database without foreign key support with cardano db migrations applied and WAL activated
pub fn cardano_tx_db_file_connection(db_path: &Path) -> StdResult<SqliteConnection> {
    let builder = ConnectionBuilder::open_file(db_path)
        .with_options(&[ConnectionOptions::EnableWriteAheadLog]);
    build_cardano_tx_db_connection(builder)
}

fn build_cardano_tx_db_connection(
    connection_builder: ConnectionBuilder,
) -> StdResult<SqliteConnection> {
    let connection = connection_builder
        .with_options(&[ConnectionOptions::ForceDisableForeignKeys])
        .with_migrations(
            mithril_persistence::database::cardano_transaction_migration::get_migrations(),
        )
        .build()?;
    Ok(connection)
}

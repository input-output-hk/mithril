use std::path::Path;

use chrono::Utc;
use mithril_common::entities::Epoch;
use mithril_common::StdResult;
use mithril_persistence::sqlite::{
    ConnectionBuilder, ConnectionExtensions, ConnectionOptions, Query, SqliteConnection,
};
use sqlite::Value;

use crate::database::query::{InsertOrReplaceStakePoolQuery, InsertSignedBeaconRecordQuery};
use crate::database::record::SignedBeaconRecord;

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

pub fn insert_signed_beacons(connection: &SqliteConnection, records: Vec<SignedBeaconRecord>) {
    for record in records.iter() {
        connection
            .fetch_first(InsertSignedBeaconRecordQuery::one(record.clone()).unwrap())
            .unwrap();
    }
}

pub fn insert_stake_pool(
    connection: &SqliteConnection,
    epoch_to_insert_stake_pools: &[i64],
) -> StdResult<()> {
    let query = {
        // leverage the expanded parameter from this query which is unit
        // tested on its own above.
        let (sql_values, _) =
            InsertOrReplaceStakePoolQuery::many(vec![("pool_id".to_string(), Epoch(1), 1000)])
                .filters()
                .expand();

        format!("insert into stake_pool {sql_values}")
    };

    // Note: decreasing stakes for pool3 so we can test that the order has changed
    for (pool_id, epoch, stake) in epoch_to_insert_stake_pools.iter().flat_map(|epoch| {
        [
            ("pool1", *epoch, 1000 + (epoch - 1) * 40),
            ("pool2", *epoch, 1100 + (epoch - 1) * 45),
            ("pool3", *epoch, 1200 - (epoch - 1) * 50),
        ]
    }) {
        let mut statement = connection.prepare(&query)?;
        statement
            .bind::<&[(_, Value)]>(&[
                (1, pool_id.to_string().into()),
                (2, Value::Integer(epoch)),
                (3, Value::Integer(stake)),
                (4, Utc::now().to_rfc3339().into()),
            ])
            .unwrap();
        statement.next().unwrap();
    }

    Ok(())
}

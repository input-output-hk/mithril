use std::path::Path;
use std::sync::Arc;

use chrono::Utc;
use mithril_common::entities::Epoch;
use mithril_common::StdResult;
use mithril_persistence::sqlite::{
    ConnectionBuilder, ConnectionExtensions, ConnectionOptions, Query, SqliteConnection,
};
use serde::Serialize;
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

/// A simple struct that help to initialize database with the old adapter behavior for testing purposes.
pub struct FakeStoreAdapter {
    connection: Arc<SqliteConnection>,
    table: &'static str,
}

impl FakeStoreAdapter {
    pub fn new(connection: Arc<SqliteConnection>, table: &'static str) -> Self {
        Self { connection, table }
    }

    pub fn create_table(&self) {
        let sql = format!(
            "create table {} (key_hash text primary key, key json not null, value json not null)",
            self.table
        );
        self.connection.execute(sql).unwrap();
    }

    pub fn is_key_hash_exist(&self, key_hash: &str) -> bool {
        let sql = format!(
            "select exists(select 1 from {} where key_hash = ?1) as record_exists",
            self.table
        );
        let parameters = [Value::String(key_hash.to_string())];
        let result: i64 = self.connection.query_single_cell(sql, &parameters).unwrap();
        result == 1
    }

    pub fn store_record<K: Serialize, V: Serialize>(
        &self,
        key_hash: &str,
        key: &K,
        record: &V,
    ) -> StdResult<()> {
        let sql = format!(
            "insert into {} (key_hash, key, value) values (?1, ?2, ?3) on conflict (key_hash) do update set value = excluded.value",
            self.table
        );
        let mut statement = self.connection.prepare(sql)?;
        statement.bind((1, key_hash))?;
        statement.bind((2, serde_json::to_string(&key)?.as_str()))?;
        statement.bind((3, serde_json::to_string(record)?.as_str()))?;
        let _ = statement.next()?;

        Ok(())
    }
}

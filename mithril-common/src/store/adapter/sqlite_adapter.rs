use async_trait::async_trait;
use serde::{de::DeserializeOwned, Serialize};
use sha2::{Digest, Sha256};
use sqlite::Connection;
use tokio::sync::Mutex;

use std::{marker::PhantomData, path::PathBuf, sync::Arc};

use super::{AdapterError, StoreAdapter};

type Result<T> = std::result::Result<T, AdapterError>;

const TABLE_NAME: &str = "key_value_store";

/// Store adapter for SQLite3
struct SQLiteAdapter<K, V> {
    connection: Arc<Mutex<Connection>>,
    key: PhantomData<K>,
    value: PhantomData<V>,
}

impl<K, V> SQLiteAdapter<K, V>
where
    K: Serialize,
{
    /// Create a new SQLiteAdapter instance.
    pub fn new(file: PathBuf) -> Result<Self> {
        let connection =
            Connection::open(file).map_err(|e| AdapterError::InitializationError(e.into()))?;
        Self::check_table_exists(&connection)?;
        let connection = Arc::new(Mutex::new(connection));

        Ok(Self {
            connection,
            key: PhantomData,
            value: PhantomData,
        })
    }

    fn check_table_exists(connection: &Connection) -> Result<()> {
        let sql = format!(
            "select exists(select 1 from sqlite_master where type='table' and name='{}')",
            TABLE_NAME
        );
        let mut statement = connection
            .prepare(sql)
            .map_err(|e| AdapterError::OpeningStreamError(e.into()))?;
        statement
            .next()
            .map_err(|e| AdapterError::QueryError(e.into()))?;
        let table_exists = statement
            .read::<i64>(0)
            .map_err(|e| AdapterError::ParsingDataError(e.into()))?;

        if table_exists != 1 {
            Self::create_table(connection)?;
        }

        Ok(())
    }

    fn create_table(connection: &Connection) -> Result<()> {
        let sql = format!(
            "create table {} (key_hash text primary key, key text, value text, created_at text default CURRENT_TIMESTAMP)",
            TABLE_NAME
        );
        connection
            .execute(sql)
            .map_err(|e| AdapterError::QueryError(e.into()))?;

        Ok(())
    }

    fn get_hash_from_key(&self, key: &K) -> Result<String> {
        let mut hasher = Sha256::new();
        hasher.update(self.serialize_key(key)?);
        let checksum = hasher.finalize();

        Ok(hex::encode(checksum))
    }

    fn serialize_key(&self, key: &K) -> Result<String> {
        serde_json::to_string(&key).map_err(|e| {
            AdapterError::GeneralError(
                "SQLite adapter: Serde error while serializing store key".to_string(),
            )
        })
    }
}

#[async_trait]
impl<K, V> StoreAdapter for SQLiteAdapter<K, V>
where
    K: Send + Sync + Serialize,
    V: Send + Sync + Serialize,
{
    type Key = K;
    type Record = V;

    async fn store_record(&mut self, key: &Self::Key, record: &Self::Record) -> Result<()> {
        let connection = self.connection.lock().await;
        let sql = format!(
            "insert into {} (key_hash, key, value) values (?1, ?2, ?3)",
            TABLE_NAME
        );
        let value = serde_json::to_string(record).map_err(|e| {
            AdapterError::GeneralError(
                "SQLite adapter error: could not serialize value before insertion".to_string(),
            )
        })?;
        let mut statement = connection
            .prepare(sql)
            .map_err(|e| AdapterError::InitializationError(e.into()))?
            .bind::<&str>(1, self.get_hash_from_key(key)?.as_str())
            .map_err(|e| AdapterError::InitializationError(e.into()))?
            .bind::<&str>(2, self.serialize_key(key)?.as_str())
            .map_err(|e| AdapterError::InitializationError(e.into()))?
            .bind::<&str>(3, value.as_str())
            .map_err(|e| AdapterError::InitializationError(e.into()))?;
        let _ = statement
            .next()
            .map_err(|e| AdapterError::ParsingDataError(e.into()))?;

        Ok(())
    }

    async fn get_record(&self, key: &Self::Key) -> Result<Option<Self::Record>> {
        todo!()
    }

    async fn record_exists(&self, key: &Self::Key) -> Result<bool> {
        todo!()
    }

    async fn get_last_n_records(&self, how_many: usize) -> Result<Vec<(Self::Key, Self::Record)>> {
        todo!()
    }

    async fn remove(&mut self, key: &Self::Key) -> Result<Option<Self::Record>> {
        todo!()
    }

    async fn get_iter(&self) -> Result<Box<dyn Iterator<Item = Self::Record> + '_>> {
        todo!()
    }
}

/// SqlProjection allow structures to be stored and fetched from a SQL database.
trait SqlTableDescription {
    /// Return the table name for queries.
    fn get_table_name(&self) -> &String;

    /// Return the field name of the Key.
    fn get_key_field(&self) -> &String;

    /// Return the field name of the Record.
    fn get_record_field(&self) -> &String;

    /// Return the field name where the timestamp of record creation is.
    fn get_created_at_field(&self) -> &String;
}

#[cfg(test)]
mod tests {

    use std::{
        borrow::Borrow,
        fs::{create_dir_all, remove_file},
    };

    use super::*;

    fn get_file_path(test_name: &str) -> PathBuf {
        let dirpath = std::env::temp_dir().join("mithril_test");

        if !dirpath.exists() {
            create_dir_all(&dirpath).expect(&format!(
                "Expecting to be able to create the test directory '{}'.",
                dirpath.to_string_lossy()
            ));
        }

        dirpath.join(format!("{}.sqlite3", test_name))
    }

    fn init_db(test_name: &str) -> SQLiteAdapter<u64, String> {
        let filepath = get_file_path(test_name);

        if filepath.exists() {
            remove_file(&filepath).expect(&format!(
                "Expecting to be able to remove the database file '{}'.",
                filepath.to_string_lossy()
            ));
        }
        let adapter = SQLiteAdapter::new(filepath).unwrap();

        adapter
    }

    #[tokio::test]
    async fn test_store_record() {
        let test_name = "test_store_record";
        let mut adapter = init_db(test_name);
        adapter
            .store_record(&1, "one".to_string().borrow())
            .await
            .unwrap();
        let filepath = get_file_path(test_name);
        let connection = Connection::open(&filepath).expect(&format!(
            "Expecting to be able to open SQLite file '{}'.",
            filepath.to_string_lossy()
        ));
        let mut statement = connection
            .prepare(format!("select * from {}", TABLE_NAME))
            .unwrap()
            .into_cursor();
        let row = statement
            .try_next()
            .unwrap()
            .expect("Expecting at least one row in the result set.");

        assert_eq!(
            "1",
            row[1]
                .as_string()
                .expect("Expecting to have a field 1 (key).")
        );
        assert_eq!(
            "\"one\"".to_string(),
            row[2]
                .as_string()
                .expect("Expecting to have a field 2 (value).")
        );
    }

    #[tokio::test]
    async fn test_get_record() {
        let test_name = "test_get_record";
        let mut adapter = init_db(test_name);
        adapter
            .store_record(&1, "one".to_string().borrow())
            .await
            .unwrap();
        adapter
            .store_record(&2, "two".to_string().borrow())
            .await
            .unwrap();
        adapter
            .store_record(&3, "three".to_string().borrow())
            .await
            .unwrap();
        assert_eq!(
            Some("one".to_string()),
            adapter.get_record(&1).await.unwrap()
        );
        assert_eq!(
            Some("three".to_string()),
            adapter.get_record(&3).await.unwrap()
        );
        assert_eq!(
            Some("two".to_string()),
            adapter.get_record(&2).await.unwrap()
        );
        assert_eq!(None, adapter.get_record(&4).await.unwrap());
    }
}

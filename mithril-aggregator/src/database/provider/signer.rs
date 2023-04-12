use std::sync::Arc;

use async_trait::async_trait;
use chrono::Utc;
use sqlite::{Connection, Value};

use mithril_common::sqlite::{
    EntityCursor, HydrationError, Projection, Provider, SourceAlias, SqLiteEntity, WhereCondition,
};

use mithril_common::StdError;
use tokio::sync::Mutex;

/// Signer record is the representation of a stored signer.
#[derive(Debug, PartialEq, Clone)]
pub struct SignerRecord {
    /// Signer id.
    signer_id: String,

    /// Pool ticker of the signer.
    pool_ticker: Option<String>,

    /// Date and time when the signer was created.
    created_at: String,

    /// Date and time when the signer was updated.
    updated_at: String,
}

impl SqLiteEntity for SignerRecord {
    fn hydrate(row: sqlite::Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let signer_id = row.get::<String, _>(0);
        let pool_ticker = row.get::<Option<String>, _>(1);
        let created_at = row.get::<String, _>(2);
        let updated_at = row.get::<String, _>(3);

        let signer_record = Self {
            signer_id,
            pool_ticker,
            created_at,
            updated_at,
        };

        Ok(signer_record)
    }

    fn get_projection() -> Projection {
        let mut projection = Projection::default();
        projection.add_field("signer_id", "{:signer:}.signer_id", "text");
        projection.add_field("pool_ticker", "{:signer:}.pool_ticker", "text");
        projection.add_field("created_at", "{:signer:}.created_at", "text");
        projection.add_field("updated_at", "{:signer:}.updated_at", "text");

        projection
    }
}

/// Simple [SignerRecord] provider.
pub struct SignerRecordProvider<'client> {
    client: &'client Connection,
}

impl<'client> SignerRecordProvider<'client> {
    /// Create a new provider
    pub fn new(client: &'client Connection) -> Self {
        Self { client }
    }

    fn condition_by_signer_id(&self, signer_id: String) -> Result<WhereCondition, StdError> {
        Ok(WhereCondition::new(
            "signer_id = ?*",
            vec![Value::String(signer_id)],
        ))
    }

    /// Get SignerRecords for a given signer id.
    pub fn get_by_signer_id(
        &self,
        signer_id: String,
    ) -> Result<EntityCursor<SignerRecord>, StdError> {
        let filters = self.condition_by_signer_id(signer_id)?;
        let signer_record = self.find(filters)?;

        Ok(signer_record)
    }

    /// Get all SignerRecords.
    pub fn get_all(&self) -> Result<EntityCursor<SignerRecord>, StdError> {
        let filters = WhereCondition::default();
        let signer_record = self.find(filters)?;

        Ok(signer_record)
    }
}

impl<'client> Provider<'client> for SignerRecordProvider<'client> {
    type Entity = SignerRecord;

    fn get_connection(&'client self) -> &'client Connection {
        self.client
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:signer:}", "s")]);
        let projection = Self::Entity::get_projection().expand(aliases);
        format!("select {projection} from signer as s where {condition} order by ROWID desc")
    }
}

/// Query to insert the signer record
pub struct InsertSignerRecordProvider<'conn> {
    connection: &'conn Connection,
}

impl<'conn> InsertSignerRecordProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn Connection) -> Self {
        Self { connection }
    }

    fn get_insert_condition(&self, signer_record: SignerRecord) -> WhereCondition {
        WhereCondition::new(
            "(signer_id, pool_ticker, created_at, updated_at) values (?*, ?*, ?*, ?*)",
            vec![
                Value::String(signer_record.signer_id),
                signer_record
                    .pool_ticker
                    .map(Value::String)
                    .unwrap_or(Value::Null),
                Value::String(signer_record.created_at),
                Value::String(signer_record.updated_at),
            ],
        )
    }

    fn persist(&self, signer_record: SignerRecord) -> Result<SignerRecord, StdError> {
        let filters = self.get_insert_condition(signer_record.clone());

        let entity = self.find(filters)?.next().unwrap_or_else(|| {
            panic!("No entity returned by the persister, signer_record = {signer_record:?}")
        });

        Ok(entity)
    }
}

impl<'conn> Provider<'conn> for InsertSignerRecordProvider<'conn> {
    type Entity = SignerRecord;

    fn get_connection(&'conn self) -> &'conn Connection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection =
            Self::Entity::get_projection().expand(SourceAlias::new(&[("{:signer:}", "signer")]));

        format!("insert into signer {condition} on conflict (signer_id) do update set updated_at = excluded.updated_at returning {projection}")
    }
}

/// Query to update the signer record
pub struct UpdateSignerRecordProvider<'conn> {
    connection: &'conn Connection,
}

impl<'conn> UpdateSignerRecordProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn Connection) -> Self {
        Self { connection }
    }

    fn get_update_condition(&self, signer_record: SignerRecord) -> WhereCondition {
        WhereCondition::new(
            "(signer_id, pool_ticker, created_at, updated_at) values (?*, ?*, ?*, ?*)",
            vec![
                Value::String(signer_record.signer_id),
                signer_record
                    .pool_ticker
                    .map(Value::String)
                    .unwrap_or(Value::Null),
                Value::String(signer_record.created_at),
                Value::String(signer_record.updated_at),
            ],
        )
    }

    fn persist(&self, signer_record: SignerRecord) -> Result<SignerRecord, StdError> {
        let filters = self.get_update_condition(signer_record.clone());

        let entity = self.find(filters)?.next().unwrap_or_else(|| {
            panic!("No entity returned by the persister, signer_record = {signer_record:?}")
        });

        Ok(entity)
    }
}

impl<'conn> Provider<'conn> for UpdateSignerRecordProvider<'conn> {
    type Entity = SignerRecord;

    fn get_connection(&'conn self) -> &'conn Connection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection =
            Self::Entity::get_projection().expand(SourceAlias::new(&[("{:signer:}", "signer")]));

        format!("insert into signer {condition} on conflict(signer_id) do update set pool_ticker = excluded.pool_ticker, updated_at = excluded.updated_at returning {projection}")
    }
}

/// Signer recorder trait
#[async_trait]
pub trait SignerRecorder {
    /// Record signer_id
    async fn record_signer_id(&self, signer_id: String) -> Result<(), StdError>;

    /// Record pool ticker by id
    async fn record_signer_pool_ticker(
        &self,
        signer_id: String,
        pool_ticker: Option<String>,
    ) -> Result<(), StdError>;
}

/// Service to deal with signer (read & write).
pub struct SignerStore {
    connection: Arc<Mutex<Connection>>,
}

impl SignerStore {
    /// Create a new SignerStore service
    pub fn new(connection: Arc<Mutex<Connection>>) -> Self {
        Self { connection }
    }
}

#[async_trait]
impl SignerRecorder for SignerStore {
    async fn record_signer_id(&self, signer_id: String) -> Result<(), StdError> {
        let connection = &*self.connection.lock().await;
        let provider = InsertSignerRecordProvider::new(connection);
        let created_at = format!("{:?}", Utc::now());
        let updated_at = created_at.clone();
        let signer_record = SignerRecord {
            signer_id,
            pool_ticker: None,
            created_at,
            updated_at,
        };
        provider.persist(signer_record)?;

        Ok(())
    }

    async fn record_signer_pool_ticker(
        &self,
        signer_id: String,
        pool_ticker: Option<String>,
    ) -> Result<(), StdError> {
        let connection = &*self.connection.lock().await;
        let provider = UpdateSignerRecordProvider::new(connection);
        let created_at = format!("{:?}", Utc::now());
        let updated_at = created_at.clone();
        let signer_record = SignerRecord {
            signer_id,
            pool_ticker,
            created_at,
            updated_at,
        };
        provider.persist(signer_record)?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {

    use crate::database::migration::get_migrations;

    use super::*;

    fn fake_signer_records(total_records: usize) -> Vec<SignerRecord> {
        (0..total_records)
            .map(|idx| SignerRecord {
                signer_id: format!("signer-{idx}"),
                pool_ticker: Some(format!("pool-ticker-{idx}")),
                created_at: "2023-01-19T13:43:05.618857482Z".to_string(),
                updated_at: "2024-01-19T13:43:05.618857482Z".to_string(),
            })
            .collect()
    }

    pub fn setup_signer_db(
        connection: &Connection,
        signer_records: Vec<SignerRecord>,
    ) -> Result<(), StdError> {
        for migration in get_migrations() {
            connection.execute(&migration.alterations)?;
        }

        if signer_records.is_empty() {
            return Ok(());
        }

        let query = {
            // leverage the expanded parameter from this provider which is unit
            // tested on its own above.
            let update_provider = UpdateSignerRecordProvider::new(connection);
            let (sql_values, _) = update_provider
                .get_update_condition(signer_records.first().unwrap().to_owned())
                .expand();
            format!("insert into signer {sql_values}")
        };

        for signer_record in signer_records {
            let mut statement = connection.prepare(&query)?;

            statement.bind(1, signer_record.signer_id.as_str()).unwrap();
            statement
                .bind(
                    2,
                    &signer_record
                        .pool_ticker
                        .map(Value::String)
                        .unwrap_or(Value::Null),
                )
                .unwrap();
            statement
                .bind(3, signer_record.created_at.as_str())
                .unwrap();
            statement
                .bind(4, signer_record.updated_at.as_str())
                .unwrap();
            statement.next().unwrap();
        }

        Ok(())
    }

    #[test]
    fn projection() {
        let projection = SignerRecord::get_projection();
        let aliases = SourceAlias::new(&[("{:signer:}", "s")]);

        assert_eq!(
            "s.signer_id as signer_id, s.pool_ticker as pool_ticker, s.created_at as created_at, s.updated_at as updated_at"
                .to_string(),
            projection.expand(aliases)
        );
    }

    #[test]
    fn get_signer_record_by_signer_id() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = SignerRecordProvider::new(&connection);
        let condition = provider
            .condition_by_signer_id("signer-123".to_string())
            .unwrap();
        let (filter, values) = condition.expand();

        assert_eq!("signer_id = ?1".to_string(), filter);
        assert_eq!(vec![Value::String("signer-123".to_string())], values);
    }

    #[test]
    fn insert_signer_record() {
        let signer_record = fake_signer_records(1).first().unwrap().to_owned();
        let connection = Connection::open(":memory:").unwrap();
        let provider = InsertSignerRecordProvider::new(&connection);
        let condition = provider.get_insert_condition(signer_record.clone());
        let (values, params) = condition.expand();

        assert_eq!(
            "(signer_id, pool_ticker, created_at, updated_at) values (?1, ?2, ?3, ?4)".to_string(),
            values
        );
        assert_eq!(
            vec![
                Value::String(signer_record.signer_id),
                Value::String(signer_record.pool_ticker.unwrap()),
                Value::String(signer_record.created_at),
                Value::String(signer_record.updated_at),
            ],
            params
        );
    }

    #[test]
    fn update_signer_record() {
        let signer_record = fake_signer_records(1).first().unwrap().to_owned();
        let connection = Connection::open(":memory:").unwrap();
        let provider = UpdateSignerRecordProvider::new(&connection);
        let condition = provider.get_update_condition(signer_record.clone());
        let (values, params) = condition.expand();

        assert_eq!(
            "(signer_id, pool_ticker, created_at, updated_at) values (?1, ?2, ?3, ?4)".to_string(),
            values
        );
        assert_eq!(
            vec![
                Value::String(signer_record.signer_id),
                Value::String(signer_record.pool_ticker.unwrap()),
                Value::String(signer_record.created_at),
                Value::String(signer_record.updated_at),
            ],
            params
        );
    }

    #[test]
    fn test_get_signer_records() {
        let signer_records_fake = fake_signer_records(5);

        let connection = Connection::open(":memory:").unwrap();
        setup_signer_db(&connection, signer_records_fake.clone()).unwrap();

        let provider = SignerRecordProvider::new(&connection);

        let signer_records: Vec<SignerRecord> = provider
            .get_by_signer_id(signer_records_fake[0].signer_id.to_owned())
            .unwrap()
            .collect();
        let expected_signer_records: Vec<SignerRecord> = vec![signer_records_fake[0].to_owned()];
        assert_eq!(expected_signer_records, signer_records);

        let signer_records: Vec<SignerRecord> = provider
            .get_by_signer_id(signer_records_fake[2].signer_id.to_owned())
            .unwrap()
            .collect();
        let expected_signer_records: Vec<SignerRecord> = vec![signer_records_fake[2].to_owned()];
        assert_eq!(expected_signer_records, signer_records);

        let cursor = provider
            .get_by_signer_id("signer-id-not-registered".to_string())
            .unwrap();
        assert_eq!(0, cursor.count());

        let signer_records: Vec<SignerRecord> = provider.get_all().unwrap().collect();
        let expected_signer_records: Vec<SignerRecord> =
            signer_records_fake.into_iter().rev().collect();
        assert_eq!(expected_signer_records, signer_records);
    }

    #[test]
    fn test_insert_signer_record() {
        let signer_records_fake = fake_signer_records(5);

        let connection = Connection::open(":memory:").unwrap();
        setup_signer_db(&connection, Vec::new()).unwrap();

        let provider = InsertSignerRecordProvider::new(&connection);

        for signer_record in signer_records_fake.clone() {
            let signer_record_saved = provider.persist(signer_record.clone()).unwrap();
            assert_eq!(signer_record, signer_record_saved);
        }

        for signer_record in signer_records_fake {
            let mut signer_record_copy = signer_record.clone();
            signer_record_copy.updated_at = "2025-01-19T13:43:05.618857482Z".to_string();
            let signer_record_saved = provider.persist(signer_record_copy.clone()).unwrap();
            assert_eq!(signer_record_copy, signer_record_saved);
        }
    }

    #[test]
    fn test_update_signer_record() {
        let signer_records_fake = fake_signer_records(5);

        let connection = Connection::open(":memory:").unwrap();
        setup_signer_db(&connection, signer_records_fake.clone()).unwrap();

        let provider = UpdateSignerRecordProvider::new(&connection);

        for signer_record in signer_records_fake.clone() {
            let signer_record_saved = provider.persist(signer_record.clone()).unwrap();
            assert_eq!(signer_record, signer_record_saved);
        }

        for signer_record in signer_records_fake {
            let mut signer_record_copy = signer_record.clone();
            signer_record_copy.pool_ticker =
                Some(format!("new-pool-{}", signer_record_copy.signer_id));
            signer_record_copy.updated_at = "2025-01-19T13:43:05.618857482Z".to_string();
            let signer_record_saved = provider.persist(signer_record_copy.clone()).unwrap();
            assert_eq!(signer_record_copy, signer_record_saved);
        }
    }

    #[tokio::test]
    async fn test_signer_recorder() {
        let signer_records_fake = fake_signer_records(5);

        let connection = Connection::open(":memory:").unwrap();
        setup_signer_db(&connection, Vec::new()).unwrap();

        let connection = Arc::new(Mutex::new(connection));
        let store_recorder = SignerStore::new(connection.clone());

        for signer_record in signer_records_fake.clone() {
            store_recorder
                .record_signer_id(signer_record.signer_id.clone())
                .await
                .expect("record_signer_id should not fail");
            let connection = &*connection.lock().await;
            let provider = SignerRecordProvider::new(connection);
            let signer_records_stored: Vec<SignerRecord> = provider
                .get_by_signer_id(signer_record.signer_id)
                .unwrap()
                .collect::<Vec<_>>();
            assert_eq!(1, signer_records_stored.len());
        }

        for signer_record in signer_records_fake {
            let pool_ticker = Some(format!("new-pool-{}", signer_record.signer_id));
            store_recorder
                .record_signer_pool_ticker(signer_record.signer_id.clone(), pool_ticker.clone())
                .await
                .expect("record_signer_pool_ticker should not fail");
            let connection = &*connection.lock().await;
            let provider = SignerRecordProvider::new(connection);
            let signer_records_stored: Vec<SignerRecord> = provider
                .get_by_signer_id(signer_record.signer_id)
                .unwrap()
                .collect::<Vec<_>>();
            assert_eq!(pool_ticker, signer_records_stored[0].to_owned().pool_ticker);
        }
    }
}

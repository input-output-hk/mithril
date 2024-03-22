use async_trait::async_trait;
use chrono::{DateTime, Utc};
use sqlite::Value;
use std::collections::HashMap;
use std::iter::repeat;
use std::sync::Arc;

use mithril_common::StdResult;
use mithril_persistence::sqlite::{
    EntityCursor, HydrationError, Projection, Provider, SourceAlias, SqLiteEntity,
    SqliteConnection, WhereCondition,
};

use crate::signer_registerer::SignerRecorder;

#[cfg(test)]
use mockall::automock;

/// Signer record is the representation of a stored signer.
#[derive(Debug, PartialEq, Clone)]
pub struct SignerRecord {
    /// Signer id.
    pub signer_id: String,

    /// Pool ticker of the signer.
    pub pool_ticker: Option<String>,

    /// Date and time when the signer was created.
    pub created_at: DateTime<Utc>,

    /// Date and time when the signer was updated.
    pub updated_at: DateTime<Utc>,

    /// Date and time when the signer registered for the last time.
    pub last_registered_at: Option<DateTime<Utc>>,
}

impl SqLiteEntity for SignerRecord {
    fn hydrate(row: sqlite::Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let signer_id = row.read::<&str, _>(0).to_string();
        let pool_ticker = row.read::<Option<&str>, _>(1).map(|s| s.to_owned());
        let created_at = row.read::<&str, _>(2);
        let updated_at = row.read::<&str, _>(3);
        let registered_at = row.read::<Option<&str>, _>(4);

        let signer_record = Self {
            signer_id,
            pool_ticker,
            created_at: DateTime::parse_from_rfc3339(created_at)
                .map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{created_at}' to rfc3339 Datetime. Error: {e}"
                    ))
                })?
                .with_timezone(&Utc),
            updated_at: DateTime::parse_from_rfc3339(updated_at)
                .map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{updated_at}' to rfc3339 Datetime. Error: {e}"
                    ))
                })?
                .with_timezone(&Utc),
            last_registered_at: registered_at
                .map(|d| match DateTime::parse_from_rfc3339(d) {
                    Ok(date) => Ok(date.with_timezone(&Utc)),
                    Err(e) => Err(HydrationError::InvalidData(format!(
                        "Could not turn string '{d}' to rfc3339 Datetime. Error: {e}"
                    ))),
                })
                .transpose()?,
        };

        Ok(signer_record)
    }

    fn get_projection() -> Projection {
        let mut projection = Projection::default();
        projection.add_field("signer_id", "{:signer:}.signer_id", "text");
        projection.add_field("pool_ticker", "{:signer:}.pool_ticker", "text");
        projection.add_field("created_at", "{:signer:}.created_at", "text");
        projection.add_field("updated_at", "{:signer:}.updated_at", "text");
        projection.add_field(
            "last_registered_at",
            "{:signer:}.last_registered_at",
            "text",
        );

        projection
    }
}

/// Simple [SignerRecord] provider.
pub struct SignerRecordProvider<'client> {
    client: &'client SqliteConnection,
}

impl<'client> SignerRecordProvider<'client> {
    /// Create a new provider
    pub fn new(client: &'client SqliteConnection) -> Self {
        Self { client }
    }

    fn condition_by_signer_id(&self, signer_id: String) -> StdResult<WhereCondition> {
        Ok(WhereCondition::new(
            "signer_id = ?*",
            vec![Value::String(signer_id)],
        ))
    }

    /// Get SignerRecords for a given signer id.
    pub fn get_by_signer_id(&self, signer_id: String) -> StdResult<EntityCursor<SignerRecord>> {
        let filters = self.condition_by_signer_id(signer_id)?;
        let signer_record = self.find(filters)?;

        Ok(signer_record)
    }

    /// Get all SignerRecords.
    pub fn get_all(&self) -> StdResult<EntityCursor<SignerRecord>> {
        let filters = WhereCondition::default();
        let signer_record = self.find(filters)?;

        Ok(signer_record)
    }
}

impl<'client> Provider<'client> for SignerRecordProvider<'client> {
    type Entity = SignerRecord;

    fn get_connection(&'client self) -> &'client SqliteConnection {
        self.client
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:signer:}", "s")]);
        let projection = Self::Entity::get_projection().expand(aliases);
        format!("select {projection} from signer as s where {condition} order by ROWID desc")
    }
}

/// Query to insert the signer record
pub struct RegisterSignerRecordProvider<'conn> {
    connection: &'conn SqliteConnection,
}

impl<'conn> RegisterSignerRecordProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn SqliteConnection) -> Self {
        Self { connection }
    }

    fn get_register_condition(&self, signer_record: SignerRecord) -> WhereCondition {
        WhereCondition::new(
            "(signer_id, pool_ticker, created_at, updated_at, last_registered_at) values (?*, ?*, ?*, ?*, ?*)",
            vec![
                Value::String(signer_record.signer_id),
                signer_record
                    .pool_ticker
                    .map(Value::String)
                    .unwrap_or(Value::Null),
                Value::String(signer_record.created_at.to_rfc3339()),
                Value::String(signer_record.updated_at.to_rfc3339()),
                signer_record
                    .last_registered_at
                    .map(|d| Value::String(d.to_rfc3339()))
                    .unwrap_or(Value::Null),
            ],
        )
    }

    fn persist(&self, signer_record: SignerRecord) -> StdResult<SignerRecord> {
        let filters = self.get_register_condition(signer_record.clone());

        let entity = self.find(filters)?.next().unwrap_or_else(|| {
            panic!("No entity returned by the persister, signer_record = {signer_record:?}")
        });

        Ok(entity)
    }
}

impl<'conn> Provider<'conn> for RegisterSignerRecordProvider<'conn> {
    type Entity = SignerRecord;

    fn get_connection(&'conn self) -> &'conn SqliteConnection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection =
            Self::Entity::get_projection().expand(SourceAlias::new(&[("{:signer:}", "signer")]));

        format!(
            "insert into signer {condition} on conflict (signer_id) do update set \
            updated_at = excluded.updated_at, last_registered_at = excluded.last_registered_at returning {projection}"
        )
    }
}

/// Query to update the signer record
pub struct ImportSignerRecordProvider<'conn> {
    connection: &'conn SqliteConnection,
}

impl<'conn> ImportSignerRecordProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn SqliteConnection) -> Self {
        Self { connection }
    }

    fn get_import_condition(&self, signer_records: Vec<SignerRecord>) -> WhereCondition {
        let columns = "(signer_id, pool_ticker, created_at, updated_at, last_registered_at)";
        let values_columns: Vec<&str> = repeat("(?*, ?*, ?*, ?*, ?*)")
            .take(signer_records.len())
            .collect();
        let values = signer_records
            .into_iter()
            .flat_map(|signer_record| {
                vec![
                    Value::String(signer_record.signer_id),
                    signer_record
                        .pool_ticker
                        .map(Value::String)
                        .unwrap_or(Value::Null),
                    Value::String(signer_record.created_at.to_rfc3339()),
                    Value::String(signer_record.updated_at.to_rfc3339()),
                    signer_record
                        .last_registered_at
                        .map(|d| Value::String(d.to_rfc3339()))
                        .unwrap_or(Value::Null),
                ]
            })
            .collect();

        WhereCondition::new(
            format!("{columns} values {}", values_columns.join(", ")).as_str(),
            values,
        )
    }

    fn persist(&self, signer_record: SignerRecord) -> StdResult<SignerRecord> {
        let filters = self.get_import_condition(vec![signer_record.clone()]);

        let entity = self.find(filters)?.next().unwrap_or_else(|| {
            panic!("No entity returned by the persister, signer_record = {signer_record:?}")
        });

        Ok(entity)
    }

    fn persist_many(&self, signer_records: Vec<SignerRecord>) -> StdResult<Vec<SignerRecord>> {
        let filters = self.get_import_condition(signer_records);

        Ok(self.find(filters)?.collect())
    }
}

impl<'conn> Provider<'conn> for ImportSignerRecordProvider<'conn> {
    type Entity = SignerRecord;

    fn get_connection(&'conn self) -> &'conn SqliteConnection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection =
            Self::Entity::get_projection().expand(SourceAlias::new(&[("{:signer:}", "signer")]));

        format!(
            "insert into signer {condition} on conflict(signer_id) do update \
            set pool_ticker = excluded.pool_ticker, updated_at = excluded.updated_at returning {projection}"
        )
    }
}

/// Service to get [SignerRecord].
#[cfg_attr(test, automock)]
#[async_trait]
pub trait SignerGetter: Sync + Send {
    /// Return all stored records.
    async fn get_all(&self) -> StdResult<Vec<SignerRecord>>;
}

/// Service to deal with signer (read & write).
pub struct SignerStore {
    connection: Arc<SqliteConnection>,
}

impl SignerStore {
    /// Create a new SignerStore service
    pub fn new(connection: Arc<SqliteConnection>) -> Self {
        Self { connection }
    }

    /// Import a signer in the database, its last_registered_at date will be left empty
    pub async fn import_signer(
        &self,
        signer_id: String,
        pool_ticker: Option<String>,
    ) -> StdResult<()> {
        let provider = ImportSignerRecordProvider::new(&self.connection);
        let created_at = Utc::now();
        let updated_at = created_at;
        let signer_record = SignerRecord {
            signer_id,
            pool_ticker,
            created_at,
            updated_at,
            last_registered_at: None,
        };
        provider.persist(signer_record)?;

        Ok(())
    }

    /// Create many signers at once in the database, their last_registered_at date will be left empty
    pub async fn import_many_signers(
        &self,
        pool_ticker_by_id: HashMap<String, Option<String>>,
    ) -> StdResult<()> {
        let provider = ImportSignerRecordProvider::new(&self.connection);

        let created_at = Utc::now();
        let updated_at = created_at;
        let signer_records: Vec<_> = pool_ticker_by_id
            .into_iter()
            .map(|(signer_id, pool_ticker)| SignerRecord {
                signer_id,
                pool_ticker,
                created_at,
                updated_at,
                last_registered_at: None,
            })
            .collect();

        provider.persist_many(signer_records)?;

        Ok(())
    }
}

#[async_trait]
impl SignerRecorder for SignerStore {
    async fn record_signer_registration(&self, signer_id: String) -> StdResult<()> {
        let provider = RegisterSignerRecordProvider::new(&self.connection);
        let created_at = Utc::now();
        let updated_at = created_at;
        let registered_at = Some(created_at);
        let signer_record = SignerRecord {
            signer_id,
            pool_ticker: None,
            created_at,
            updated_at,
            last_registered_at: registered_at,
        };
        provider.persist(signer_record)?;

        Ok(())
    }
}

#[async_trait]
impl SignerGetter for SignerStore {
    async fn get_all(&self) -> StdResult<Vec<SignerRecord>> {
        let provider = SignerRecordProvider::new(&self.connection);
        let cursor = provider.get_all()?;

        Ok(cursor.collect())
    }
}

#[cfg(test)]
mod tests {
    use crate::database::provider::apply_all_migrations_to_db;
    use chrono::Duration;
    use mithril_common::StdResult;
    use sqlite::Connection;
    use std::collections::BTreeMap;

    use super::*;

    fn fake_signer_records(total_records: usize) -> Vec<SignerRecord> {
        (0..total_records)
            .map(|idx| SignerRecord {
                signer_id: format!("signer-{idx}"),
                pool_ticker: Some(format!("pool-ticker-{idx}")),
                created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                    .unwrap()
                    .with_timezone(&Utc),
                updated_at: DateTime::parse_from_rfc3339("2024-01-19T13:43:05.618857482Z")
                    .unwrap()
                    .with_timezone(&Utc),
                last_registered_at: Some(
                    DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                        .unwrap()
                        .with_timezone(&Utc),
                ),
            })
            .collect()
    }

    pub fn setup_signer_db(
        connection: &SqliteConnection,
        signer_records: Vec<SignerRecord>,
    ) -> StdResult<()> {
        apply_all_migrations_to_db(connection)?;

        if signer_records.is_empty() {
            return Ok(());
        }

        let query = {
            // leverage the expanded parameter from this provider which is unit
            // tested on its own above.
            let update_provider = ImportSignerRecordProvider::new(connection);
            let (sql_values, _) = update_provider
                .get_import_condition(vec![signer_records.first().unwrap().to_owned()])
                .expand();
            format!("insert into signer {sql_values}")
        };

        for signer_record in signer_records {
            let mut statement = connection.prepare(&query)?;
            statement
                .bind::<&[(_, Value)]>(&[
                    (1, signer_record.signer_id.into()),
                    (
                        2,
                        signer_record
                            .pool_ticker
                            .map(Value::String)
                            .unwrap_or(Value::Null),
                    ),
                    (3, signer_record.created_at.to_rfc3339().into()),
                    (4, signer_record.updated_at.to_rfc3339().into()),
                    (
                        5,
                        signer_record
                            .last_registered_at
                            .map(|d| Value::String(d.to_rfc3339()))
                            .unwrap_or(Value::Null),
                    ),
                ])
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
            "s.signer_id as signer_id, s.pool_ticker as pool_ticker, s.created_at as created_at, \
             s.updated_at as updated_at, s.last_registered_at as last_registered_at"
                .to_string(),
            projection.expand(aliases)
        );
    }

    #[test]
    fn get_signer_record_by_signer_id() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
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
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        let provider = RegisterSignerRecordProvider::new(&connection);
        let condition = provider.get_register_condition(signer_record.clone());
        let (values, params) = condition.expand();

        assert_eq!(
            "(signer_id, pool_ticker, created_at, updated_at, last_registered_at) values (?1, ?2, ?3, ?4, ?5)".to_string(),
            values
        );
        assert_eq!(
            vec![
                Value::String(signer_record.signer_id),
                Value::String(signer_record.pool_ticker.unwrap()),
                Value::String(signer_record.created_at.to_rfc3339()),
                Value::String(signer_record.updated_at.to_rfc3339()),
                Value::String(signer_record.last_registered_at.unwrap().to_rfc3339()),
            ],
            params
        );
    }

    #[test]
    fn update_signer_record() {
        let signer_records = fake_signer_records(2);
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        let provider = ImportSignerRecordProvider::new(&connection);
        let condition = provider.get_import_condition(signer_records.clone());
        let (values, params) = condition.expand();

        assert_eq!(
            "(signer_id, pool_ticker, created_at, updated_at, last_registered_at) values (?1, ?2, ?3, ?4, ?5), (?6, ?7, ?8, ?9, ?10)",
            &values
        );
        assert_eq!(
            vec![
                Value::String(signer_records[0].signer_id.to_owned()),
                Value::String(signer_records[0].pool_ticker.to_owned().unwrap()),
                Value::String(signer_records[0].created_at.to_rfc3339()),
                Value::String(signer_records[0].updated_at.to_rfc3339()),
                Value::String(signer_records[0].last_registered_at.unwrap().to_rfc3339()),
                Value::String(signer_records[1].signer_id.to_owned()),
                Value::String(signer_records[1].pool_ticker.to_owned().unwrap()),
                Value::String(signer_records[1].created_at.to_rfc3339()),
                Value::String(signer_records[1].updated_at.to_rfc3339()),
                Value::String(signer_records[1].last_registered_at.unwrap().to_rfc3339()),
            ],
            params
        );
    }

    #[test]
    fn test_get_signer_records() {
        let signer_records_fake = fake_signer_records(5);

        let connection = Connection::open_thread_safe(":memory:").unwrap();
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

        let connection = Connection::open_thread_safe(":memory:").unwrap();
        setup_signer_db(&connection, Vec::new()).unwrap();

        let provider = RegisterSignerRecordProvider::new(&connection);

        for signer_record in signer_records_fake.clone() {
            let signer_record_saved = provider.persist(signer_record.clone()).unwrap();
            assert_eq!(signer_record, signer_record_saved);
        }

        for mut signer_record in signer_records_fake {
            signer_record.updated_at += Duration::try_hours(1).unwrap();
            let signer_record_saved = provider.persist(signer_record.clone()).unwrap();
            assert_eq!(signer_record, signer_record_saved);
        }
    }

    #[test]
    fn test_update_signer_record() {
        let signer_records_fake = fake_signer_records(5);

        let connection = Connection::open_thread_safe(":memory:").unwrap();
        setup_signer_db(&connection, signer_records_fake.clone()).unwrap();

        let provider = ImportSignerRecordProvider::new(&connection);

        for signer_record in signer_records_fake.clone() {
            let signer_record_saved = provider.persist(signer_record.clone()).unwrap();
            assert_eq!(signer_record, signer_record_saved);
        }

        for mut signer_record in signer_records_fake {
            signer_record.pool_ticker = Some(format!("new-pool-{}", signer_record.signer_id));
            signer_record.updated_at += Duration::try_hours(1).unwrap();
            let signer_record_saved = provider.persist(signer_record.clone()).unwrap();
            assert_eq!(signer_record, signer_record_saved);
        }
    }

    #[test]
    fn test_update_many_signer_records() {
        let mut signer_records_fake = fake_signer_records(5);
        signer_records_fake.sort_by(|a, b| a.signer_id.cmp(&b.signer_id));

        let connection = Connection::open_thread_safe(":memory:").unwrap();
        setup_signer_db(&connection, signer_records_fake.clone()).unwrap();

        let provider = ImportSignerRecordProvider::new(&connection);
        let mut saved_records = provider.persist_many(signer_records_fake.clone()).unwrap();
        saved_records.sort_by(|a, b| a.signer_id.cmp(&b.signer_id));
        assert_eq!(signer_records_fake, saved_records);

        for signer_record in signer_records_fake.iter_mut() {
            signer_record.pool_ticker = Some(format!("new-pool-{}", signer_record.signer_id));
            signer_record.updated_at += Duration::try_hours(1).unwrap();
        }
        let mut saved_records = provider.persist_many(signer_records_fake.clone()).unwrap();
        saved_records.sort_by(|a, b| a.signer_id.cmp(&b.signer_id));
        assert_eq!(signer_records_fake, saved_records);
    }

    #[tokio::test]
    async fn test_get_all_signers() {
        let signer_records = fake_signer_records(5);
        let expected: Vec<_> = signer_records.iter().rev().cloned().collect();
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        setup_signer_db(&connection, signer_records).unwrap();

        let store = SignerStore::new(Arc::new(connection));

        let stored_signers = store
            .get_all()
            .await
            .expect("getting all signers should not fail");

        assert_eq!(expected, stored_signers);
    }

    #[tokio::test]
    async fn test_signer_recorder() {
        let signer_records_fake = fake_signer_records(5);

        let connection = Connection::open_thread_safe(":memory:").unwrap();
        setup_signer_db(&connection, Vec::new()).unwrap();

        let connection = Arc::new(connection);
        let store_recorder = SignerStore::new(connection.clone());

        for signer_record in signer_records_fake.clone() {
            store_recorder
                .record_signer_registration(signer_record.signer_id.clone())
                .await
                .expect("record_signer_registration should not fail");
            let provider = SignerRecordProvider::new(&connection);
            let signer_records_stored: Vec<SignerRecord> = provider
                .get_by_signer_id(signer_record.signer_id)
                .unwrap()
                .collect::<Vec<_>>();
            assert_eq!(1, signer_records_stored.len());
            assert!(
                signer_records_stored
                    .iter()
                    .all(|s| s.last_registered_at.is_some()),
                "registering a signer should set the registration date"
            )
        }
    }

    #[tokio::test]
    async fn test_store_import_signer() {
        let signer_records_fake = fake_signer_records(5);

        let connection = Connection::open_thread_safe(":memory:").unwrap();
        setup_signer_db(&connection, Vec::new()).unwrap();

        let connection = Arc::new(connection);
        let store = SignerStore::new(connection.clone());

        for signer_record in signer_records_fake {
            store
                .import_signer(
                    signer_record.signer_id.clone(),
                    signer_record.pool_ticker.clone(),
                )
                .await
                .expect("import_signer should not fail");
            let provider = SignerRecordProvider::new(&connection);
            let signer_records_stored: Vec<SignerRecord> = provider
                .get_by_signer_id(signer_record.signer_id)
                .unwrap()
                .collect::<Vec<_>>();
            assert_eq!(
                signer_record.pool_ticker,
                signer_records_stored[0].to_owned().pool_ticker
            );
            assert!(
                signer_records_stored
                    .iter()
                    .all(|s| s.last_registered_at.is_none()),
                "imported signer should not have a registration date"
            )
        }
    }

    #[tokio::test]
    async fn test_store_import_many_signers() {
        let signers_fake: BTreeMap<_, _> = fake_signer_records(5)
            .into_iter()
            .map(|r| (r.signer_id, r.pool_ticker))
            .collect();

        let connection = Connection::open_thread_safe(":memory:").unwrap();
        setup_signer_db(&connection, Vec::new()).unwrap();
        let store = SignerStore::new(Arc::new(connection));

        store
            .import_many_signers(signers_fake.clone().into_iter().collect())
            .await
            .expect("import_many_signers should not fail");

        let signer_records_stored = store.get_all().await.unwrap();
        let signers_stored = signer_records_stored
            .iter()
            .cloned()
            .map(|r| (r.signer_id, r.pool_ticker))
            .collect();
        assert_eq!(signers_fake, signers_stored);
        assert!(
            signer_records_stored
                .iter()
                .all(|s| s.last_registered_at.is_none()),
            "imported signer should not have a registration date"
        );
    }
}

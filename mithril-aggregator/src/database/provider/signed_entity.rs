use std::sync::Arc;

use sqlite::{Connection, Value};

use async_trait::async_trait;

use mithril_common::{
    entities::{SignedEntityType, Snapshot},
    sqlite::{
        EntityCursor, HydrationError, Projection, Provider, SourceAlias, SqLiteEntity,
        WhereCondition,
    },
    store::adapter::{AdapterError, StoreAdapter},
};

use mithril_common::StdError;
use tokio::sync::Mutex;

/// SignedEntity record is the representation of a stored signed_entity.
#[derive(Debug, PartialEq, Clone)]
pub struct SignedEntityRecord {
    /// Signed entity id.
    signed_entity_id: String,

    /// Signed entity type.
    signed_entity_type: SignedEntityType,

    /// Certificate id for this signed entity.
    certificate_id: String,

    /// Raw signed entity (in JSON format).
    pub entity: String,

    /// Date and time when the signed_entity was created
    created_at: String,
}

impl From<Snapshot> for SignedEntityRecord {
    fn from(other: Snapshot) -> Self {
        let entity = serde_json::to_string(&other).unwrap();
        SignedEntityRecord {
            signed_entity_id: other.digest,
            signed_entity_type: SignedEntityType::CardanoImmutableFilesFull(other.beacon),
            certificate_id: other.certificate_hash,
            entity,
            created_at: other.created_at,
        }
    }
}

impl From<SignedEntityRecord> for Snapshot {
    fn from(other: SignedEntityRecord) -> Snapshot {
        serde_json::from_str(&other.entity).unwrap()
    }
}

impl SqLiteEntity for SignedEntityRecord {
    fn hydrate(row: sqlite::Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let signed_entity_id = row.get::<String, _>(0);
        let signed_entity_type_id_int = row.get::<i64, _>(1);
        let certificate_id = row.get::<String, _>(2);
        let beacon_str = row.get::<String, _>(3);
        let entity_str = row.get::<String, _>(4);
        let created_at = row.get::<String, _>(5);

        let signed_entity_record = Self {
            signed_entity_id,
            signed_entity_type: SignedEntityType::hydrate(
                signed_entity_type_id_int.try_into().map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Could not cast i64 ({signed_entity_type_id_int}) to u64. Error: '{e}'"
                    ))
                })?,
                &beacon_str,
            )?,
            certificate_id,
            entity: entity_str,
            created_at,
        };

        Ok(signed_entity_record)
    }

    fn get_projection() -> Projection {
        Projection::from(&[
            (
                "signed_entity_id",
                "{:signed_entity:}.signed_entity_id",
                "text",
            ),
            (
                "signed_entity_type_id",
                "{:signed_entity:}.signed_entity_type_id",
                "integer",
            ),
            ("certificate_id", "{:signed_entity:}.certificate_id", "text"),
            ("beacon", "{:signed_entity:}.beacon", "text"),
            ("entity", "{:signed_entity:}.entity", "text"),
            ("created_at", "{:signed_entity:}.created_at", "text"),
        ])
    }
}

/// Simple [SignedEntityRecord] provider.
pub struct SignedEntityRecordProvider<'client> {
    client: &'client Connection,
}

impl<'client> SignedEntityRecordProvider<'client> {
    /// Create a new provider
    pub fn new(client: &'client Connection) -> Self {
        Self { client }
    }

    fn condition_by_signed_entity_id(
        &self,
        signed_entity_id: String,
    ) -> Result<WhereCondition, StdError> {
        Ok(WhereCondition::new(
            "signed_entity_id = ?*",
            vec![Value::String(signed_entity_id)],
        ))
    }

    fn condition_by_signed_entity_type(
        &self,
        signed_entity_type: SignedEntityType,
    ) -> Result<WhereCondition, StdError> {
        let signed_entity_type_id: i64 = signed_entity_type.index() as i64;

        Ok(WhereCondition::new(
            "signed_entity_type_id = ?*",
            vec![Value::Integer(signed_entity_type_id)],
        ))
    }

    /// Get SignedEntityRecords for a given signed_entity id.
    pub fn get_by_signed_entity_id(
        &self,
        signed_entity_id: String,
    ) -> Result<EntityCursor<SignedEntityRecord>, StdError> {
        let filters = self.condition_by_signed_entity_id(signed_entity_id)?;
        let signed_entity_record = self.find(filters)?;

        Ok(signed_entity_record)
    }

    /// Get SignedEntityRecords for a given signed entity type.
    pub fn get_by_signed_entity_type(
        &self,
        signed_entity_type: SignedEntityType,
    ) -> Result<EntityCursor<SignedEntityRecord>, StdError> {
        let filters = self.condition_by_signed_entity_type(signed_entity_type)?;
        let signed_entity_record = self.find(filters)?;

        Ok(signed_entity_record)
    }

    /// Get all SignedEntityRecords.
    pub fn get_all(&self) -> Result<EntityCursor<SignedEntityRecord>, StdError> {
        let filters = WhereCondition::default();
        let signed_entity_record = self.find(filters)?;

        Ok(signed_entity_record)
    }
}

impl<'client> Provider<'client> for SignedEntityRecordProvider<'client> {
    type Entity = SignedEntityRecord;

    fn get_connection(&'client self) -> &'client Connection {
        self.client
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:signed_entity:}", "se")]);
        let projection = Self::Entity::get_projection().expand(aliases);
        format!(
            "select {projection} from signed_entity as se where {condition} order by ROWID desc"
        )
    }
}

/// Query to insert the signed_entity record
pub struct InsertSignedEntityRecordProvider<'conn> {
    connection: &'conn Connection,
}

impl<'conn> InsertSignedEntityRecordProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn Connection) -> Self {
        Self { connection }
    }

    fn get_insert_condition(&self, signed_entity_record: SignedEntityRecord) -> WhereCondition {
        WhereCondition::new(
            "(signed_entity_id, signed_entity_type_id, certificate_id, beacon, entity, created_at) values (?*, ?*, ?*, ?*, ?*, ?*)",
            vec![
                Value::String(signed_entity_record.signed_entity_id),
                Value::Integer(signed_entity_record.signed_entity_type.index() as i64),
                Value::String(signed_entity_record.certificate_id),
                Value::String(signed_entity_record.signed_entity_type.get_json_beacon().unwrap()),
                Value::String(signed_entity_record.entity),
                Value::String(signed_entity_record.created_at),
            ],
        )
    }

    fn persist(
        &self,
        signed_entity_record: SignedEntityRecord,
    ) -> Result<SignedEntityRecord, StdError> {
        let filters = self.get_insert_condition(signed_entity_record.clone());

        let entity = self.find(filters)?.next().unwrap_or_else(|| {
            panic!(
                "No entity returned by the persister, signed_entity_record = {signed_entity_record:?}"
            )
        });

        Ok(entity)
    }
}

impl<'conn> Provider<'conn> for InsertSignedEntityRecordProvider<'conn> {
    type Entity = SignedEntityRecord;

    fn get_connection(&'conn self) -> &'conn Connection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection()
            .expand(SourceAlias::new(&[("{:signed_entity:}", "signed_entity")]));

        format!("insert into signed_entity {condition} returning {projection}")
    }
}

/// Service to deal with signed_entity (read & write).
pub struct SignedEntityStoreAdapter {
    connection: Arc<Mutex<Connection>>,
}

impl SignedEntityStoreAdapter {
    /// Create a new SignedEntityStoreAdapter service
    pub fn new(connection: Arc<Mutex<Connection>>) -> Self {
        Self { connection }
    }
}

#[async_trait]
impl StoreAdapter for SignedEntityStoreAdapter {
    type Key = String;
    type Record = Snapshot;

    async fn store_record(
        &mut self,
        _key: &Self::Key,
        record: &Self::Record,
    ) -> Result<(), AdapterError> {
        let connection = &*self.connection.lock().await;
        let provider = InsertSignedEntityRecordProvider::new(connection);
        let _signed_entity_record = provider
            .persist(record.to_owned().into())
            .map_err(|e| AdapterError::GeneralError(format!("{e}")))?;

        Ok(())
    }

    async fn get_record(&self, key: &Self::Key) -> Result<Option<Self::Record>, AdapterError> {
        let connection = &*self.connection.lock().await;
        let provider = SignedEntityRecordProvider::new(connection);
        let mut cursor = provider
            .get_by_signed_entity_id(key.to_string())
            .map_err(|e| AdapterError::GeneralError(format!("{e}")))?;
        let signed_entity = cursor
            .next()
            .map(|signed_entity_record| signed_entity_record.into());

        Ok(signed_entity)
    }

    async fn record_exists(&self, key: &Self::Key) -> Result<bool, AdapterError> {
        Ok(self.get_record(key).await?.is_some())
    }

    async fn get_last_n_records(
        &self,
        how_many: usize,
    ) -> Result<Vec<(Self::Key, Self::Record)>, AdapterError> {
        Ok(self
            .get_iter()
            .await?
            .take(how_many)
            .map(|se| (se.digest.to_owned(), se))
            .collect())
    }

    async fn remove(&mut self, _key: &Self::Key) -> Result<Option<Self::Record>, AdapterError> {
        unimplemented!()
    }

    async fn get_iter(&self) -> Result<Box<dyn Iterator<Item = Self::Record> + '_>, AdapterError> {
        let connection = &*self.connection.lock().await;
        let provider = SignedEntityRecordProvider::new(connection);
        let cursor = provider
            .get_all()
            .map_err(|e| AdapterError::GeneralError(format!("{e}")))?;
        let signed_entities: Vec<Snapshot> = cursor.map(|se| se.into()).collect();
        Ok(Box::new(signed_entities.into_iter()))
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::{entities::Beacon, test_utils::fake_data};

    use crate::database::migration::get_migrations;

    use super::*;

    pub fn fake_signed_entity_records(total_records: usize) -> Vec<SignedEntityRecord> {
        let snapshots = fake_data::snapshots(total_records as u64);
        (0..total_records)
            .map(|idx| {
                let snapshot = snapshots.get(idx).unwrap().to_owned();
                let entity = serde_json::to_string(&snapshot).unwrap();
                SignedEntityRecord {
                    signed_entity_id: snapshot.digest,
                    signed_entity_type: SignedEntityType::CardanoImmutableFilesFull(
                        snapshot.beacon,
                    ),
                    certificate_id: snapshot.certificate_hash,
                    entity,
                    created_at: snapshot.created_at,
                }
            })
            .collect()
    }

    pub fn setup_signed_entity_db(
        connection: &Connection,
        signed_entity_records: Vec<SignedEntityRecord>,
    ) -> Result<(), StdError> {
        for migration in get_migrations() {
            connection.execute(&migration.alterations)?;
        }

        if signed_entity_records.is_empty() {
            return Ok(());
        }

        let query = {
            // leverage the expanded parameter from this provider which is unit
            // tested on its own above.
            let insert_provider = InsertSignedEntityRecordProvider::new(connection);
            let (sql_values, _) = insert_provider
                .get_insert_condition(signed_entity_records.first().unwrap().to_owned())
                .expand();
            format!("insert into signed_entity {sql_values}")
        };

        for signed_entity_record in signed_entity_records {
            let mut statement = connection.prepare(&query)?;

            statement
                .bind(1, signed_entity_record.signed_entity_id.as_str())
                .unwrap();
            statement
                .bind(2, signed_entity_record.signed_entity_type.index() as i64)
                .unwrap();
            statement
                .bind(3, signed_entity_record.certificate_id.as_str())
                .unwrap();
            statement
                .bind(
                    4,
                    signed_entity_record
                        .signed_entity_type
                        .get_json_beacon()
                        .unwrap()
                        .as_str(),
                )
                .unwrap();
            statement
                .bind(5, signed_entity_record.entity.as_str())
                .unwrap();
            statement
                .bind(6, signed_entity_record.created_at.as_str())
                .unwrap();

            statement.next().unwrap();
        }

        Ok(())
    }

    #[test]
    fn test_convert_signed_entity() {
        let snapshots = fake_data::snapshots(1);
        let snapshot = snapshots.first().unwrap().to_owned();
        let snapshot_expected = snapshot.clone();

        let signed_entity: SignedEntityRecord = snapshot.into();
        let snapshot: Snapshot = signed_entity.into();
        assert_eq!(snapshot_expected, snapshot);
    }

    #[test]
    fn projection() {
        let projection = SignedEntityRecord::get_projection();
        let aliases = SourceAlias::new(&[("{:signed_entity:}", "se")]);

        assert_eq!(
            "se.signed_entity_id as signed_entity_id, se.signed_entity_type_id as signed_entity_type_id, se.certificate_id as certificate_id, se.beacon as beacon, se.entity as entity, se.created_at as created_at"
                .to_string(),
            projection.expand(aliases)
        );
    }

    #[test]
    fn get_signed_entity_record_by_signed_entity_type() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = SignedEntityRecordProvider::new(&connection);
        let condition = provider
            .condition_by_signed_entity_type(SignedEntityType::dummy())
            .unwrap();
        let (filter, values) = condition.expand();

        assert_eq!("signed_entity_type_id = ?1".to_string(), filter);
        assert_eq!(
            vec![Value::Integer(SignedEntityType::dummy().index() as i64)],
            values
        );
    }

    #[test]
    fn get_signed_entity_record_by_signed_entity_id() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = SignedEntityRecordProvider::new(&connection);
        let condition = provider
            .condition_by_signed_entity_id("signed-ent-123".to_string())
            .unwrap();
        let (filter, values) = condition.expand();

        assert_eq!("signed_entity_id = ?1".to_string(), filter);
        assert_eq!(vec![Value::String("signed-ent-123".to_string())], values);
    }

    #[test]
    fn insert_signed_entity_record() {
        let snapshots = fake_data::snapshots(1);
        let snapshot = snapshots.first().unwrap().to_owned();
        let signed_entity_record: SignedEntityRecord = snapshot.into();
        let connection = Connection::open(":memory:").unwrap();
        let provider = InsertSignedEntityRecordProvider::new(&connection);
        let condition = provider.get_insert_condition(signed_entity_record.clone());
        let (values, params) = condition.expand();

        assert_eq!(
            "(signed_entity_id, signed_entity_type_id, certificate_id, beacon, entity, created_at) values (?1, ?2, ?3, ?4, ?5, ?6)".to_string(),
            values
        );
        assert_eq!(
            vec![
                Value::String(signed_entity_record.signed_entity_id),
                Value::Integer(signed_entity_record.signed_entity_type.index() as i64),
                Value::String(signed_entity_record.certificate_id),
                Value::String(
                    signed_entity_record
                        .signed_entity_type
                        .get_json_beacon()
                        .unwrap()
                ),
                Value::String(signed_entity_record.entity),
                Value::String(signed_entity_record.created_at),
            ],
            params
        );
    }

    #[test]
    fn test_get_signed_entity_records() {
        let signed_entity_records = fake_signed_entity_records(5);

        let connection = Connection::open(":memory:").unwrap();
        setup_signed_entity_db(&connection, signed_entity_records.clone()).unwrap();

        let provider = SignedEntityRecordProvider::new(&connection);

        let first_signed_entity_type = signed_entity_records.first().unwrap().to_owned();
        let signed_entity_records: Vec<SignedEntityRecord> = provider
            .get_by_signed_entity_id(first_signed_entity_type.clone().signed_entity_id)
            .unwrap()
            .collect();
        assert_eq!(vec![first_signed_entity_type], signed_entity_records);

        let signed_entity_records: Vec<SignedEntityRecord> = provider
            .get_by_signed_entity_type(SignedEntityType::CardanoImmutableFilesFull(
                Beacon::default(),
            ))
            .unwrap()
            .collect();
        let expected_signed_entity_records: Vec<SignedEntityRecord> = signed_entity_records
            .iter()
            .filter_map(|se| {
                (se.signed_entity_type.index()
                    == SignedEntityType::CardanoImmutableFilesFull(Beacon::default()).index())
                .then_some(se.to_owned())
            })
            .collect();
        assert_eq!(expected_signed_entity_records, signed_entity_records);

        let signed_entity_records: Vec<SignedEntityRecord> = provider.get_all().unwrap().collect();
        let expected_signed_entity_records: Vec<SignedEntityRecord> =
            signed_entity_records.iter().map(|c| c.to_owned()).collect();
        assert_eq!(expected_signed_entity_records, signed_entity_records);
    }

    #[test]
    fn test_insert_signed_entity_record() {
        let signed_entity_records = fake_signed_entity_records(5);

        let connection = Connection::open(":memory:").unwrap();
        setup_signed_entity_db(&connection, Vec::new()).unwrap();

        let provider = InsertSignedEntityRecordProvider::new(&connection);

        for signed_entity_record in signed_entity_records {
            let signed_entity_record_saved =
                provider.persist(signed_entity_record.clone()).unwrap();
            assert_eq!(signed_entity_record, signed_entity_record_saved);
        }
    }

    #[tokio::test]
    async fn test_store_adapter() {
        let signed_entity_records = fake_signed_entity_records(5);

        let connection = Connection::open(":memory:").unwrap();
        setup_signed_entity_db(&connection, Vec::new()).unwrap();

        let mut signed_entity_store_adapter =
            SignedEntityStoreAdapter::new(Arc::new(Mutex::new(connection)));

        for signed_entity_record in &signed_entity_records {
            assert!(signed_entity_store_adapter
                .store_record(
                    &signed_entity_record.signed_entity_id,
                    &signed_entity_record.to_owned().into()
                )
                .await
                .is_ok());
        }

        for signed_entity_record in &signed_entity_records {
            assert!(signed_entity_store_adapter
                .record_exists(&signed_entity_record.signed_entity_id)
                .await
                .unwrap());
            assert_eq!(
                Some(signed_entity_record.to_owned().into()),
                signed_entity_store_adapter
                    .get_record(&signed_entity_record.signed_entity_id)
                    .await
                    .unwrap()
            );
        }

        assert_eq!(
            signed_entity_records,
            signed_entity_store_adapter
                .get_last_n_records(signed_entity_records.len())
                .await
                .unwrap()
                .into_iter()
                .map(|(_k, v)| v.into())
                .rev()
                .collect::<Vec<SignedEntityRecord>>()
        )
    }
}

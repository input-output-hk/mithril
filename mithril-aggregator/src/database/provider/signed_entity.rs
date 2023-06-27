use async_trait::async_trait;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use sqlite::{Connection, Value};
use std::sync::Arc;
use tokio::sync::Mutex;

use mithril_common::{
    entities::{SignedEntity, SignedEntityType, SignedEntityTypeDiscriminants, Snapshot},
    signable_builder::Artifact,
    sqlite::{
        EntityCursor, HydrationError, Projection, Provider, SourceAlias, SqLiteEntity,
        WhereCondition,
    },
    store::adapter::AdapterError,
    StdError, StdResult,
};

#[cfg(test)]
use mockall::automock;

/// SignedEntity record is the representation of a stored signed_entity.
#[derive(Debug, PartialEq, Clone)]
pub struct SignedEntityRecord {
    /// Signed entity id.
    pub signed_entity_id: String,

    /// Signed entity type.
    pub signed_entity_type: SignedEntityType,

    /// Certificate id for this signed entity.
    pub certificate_id: String,

    /// Raw artifact (in JSON format).
    pub artifact: String,

    /// Date and time when the signed_entity was created
    pub created_at: DateTime<Utc>,
}

impl From<Snapshot> for SignedEntityRecord {
    fn from(other: Snapshot) -> Self {
        let entity = serde_json::to_string(&other).unwrap();

        SignedEntityRecord {
            signed_entity_id: other.digest,
            signed_entity_type: SignedEntityType::CardanoImmutableFilesFull(other.beacon),
            certificate_id: other.certificate_hash,
            artifact: entity,
            created_at: other.created_at,
        }
    }
}

impl From<SignedEntityRecord> for Snapshot {
    fn from(other: SignedEntityRecord) -> Snapshot {
        serde_json::from_str(&other.artifact).unwrap()
    }
}

impl<T> TryFrom<SignedEntityRecord> for SignedEntity<T>
where
    for<'a> T: Artifact + Serialize + Deserialize<'a>,
{
    type Error = serde_json::error::Error;

    fn try_from(other: SignedEntityRecord) -> Result<SignedEntity<T>, Self::Error> {
        let signed_entity = SignedEntity {
            signed_entity_id: other.signed_entity_id,
            signed_entity_type: other.signed_entity_type,
            created_at: other.created_at,
            certificate_id: other.certificate_id,
            artifact: serde_json::from_str::<T>(&other.artifact)?,
        };

        Ok(signed_entity)
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
        // TODO: We need to check first that the cell can be read as a string first
        // (e.g. when beacon json is '{"network": "dev", "epoch": 1, "immutable_file_number": 2}').
        // If it fails, we fallback on readign the cell as an integer (e.g. when beacon json is '5').
        // Maybe there is a better way of doing this.
        let beacon_str = row
            .try_get::<String, _>(3)
            .unwrap_or_else(|_| (row.get::<i64, _>(3)).to_string());
        let artifact_str = row.get::<String, _>(4);
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
            artifact: artifact_str,
            created_at: DateTime::parse_from_rfc3339(&created_at)
                .map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{created_at}' to rfc3339 Datetime. Error: {e}"
                    ))
                })?
                .with_timezone(&Utc),
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
            ("artifact", "{:signed_entity:}.artifact", "text"),
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
        signed_entity_id: &str,
    ) -> Result<WhereCondition, StdError> {
        Ok(WhereCondition::new(
            "signed_entity_id = ?*",
            vec![Value::String(signed_entity_id.to_owned())],
        ))
    }

    fn condition_by_certificate_id(
        &self,
        certificate_id: &str,
    ) -> Result<WhereCondition, StdError> {
        Ok(WhereCondition::new(
            "certificate_id = ?*",
            vec![Value::String(certificate_id.to_owned())],
        ))
    }

    fn condition_by_signed_entity_type(
        &self,
        signed_entity_type: &SignedEntityTypeDiscriminants,
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
        signed_entity_id: &str,
    ) -> Result<EntityCursor<SignedEntityRecord>, StdError> {
        let filters = self.condition_by_signed_entity_id(signed_entity_id)?;
        let signed_entity_record = self.find(filters)?;

        Ok(signed_entity_record)
    }

    /// Get [record][SignedEntityRecord] for a given `certificate_id`.
    pub fn get_by_certificate_id(
        &self,
        certificate_id: &str,
    ) -> Result<EntityCursor<SignedEntityRecord>, StdError> {
        let filters = self.condition_by_certificate_id(certificate_id)?;
        let signed_entity_record = self.find(filters)?;

        Ok(signed_entity_record)
    }

    /// Get SignedEntityRecords for a given signed entity type.
    pub fn get_by_signed_entity_type(
        &self,
        signed_entity_type: &SignedEntityTypeDiscriminants,
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
            "(signed_entity_id, signed_entity_type_id, certificate_id, beacon, artifact, created_at) values (?*, ?*, ?*, ?*, ?*, ?*)",
            vec![
                Value::String(signed_entity_record.signed_entity_id),
                Value::Integer(signed_entity_record.signed_entity_type.index() as i64),
                Value::String(signed_entity_record.certificate_id),
                Value::String(signed_entity_record.signed_entity_type.get_json_beacon().unwrap()),
                Value::String(signed_entity_record.artifact),
                Value::String(signed_entity_record.created_at.to_rfc3339()),
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

struct UpdateSignedEntityProvider<'client> {
    connection: &'client Connection,
}

impl<'client> UpdateSignedEntityProvider<'client> {
    pub fn new(connection: &'client Connection) -> Self {
        Self { connection }
    }

    fn get_update_condition(
        &self,
        signed_entity_record: &SignedEntityRecord,
    ) -> StdResult<WhereCondition> {
        let expression =
            "signed_entity_type_id = ?*, certificate_id = ?*, beacon = ?*, artifact = ?*, \
created_at = ?* \
where signed_entity_id = ?*";
        let parameters = vec![
            Value::Integer(signed_entity_record.signed_entity_type.index() as i64),
            Value::String(signed_entity_record.certificate_id.to_owned()),
            Value::String(signed_entity_record.signed_entity_type.get_json_beacon()?),
            Value::String(signed_entity_record.artifact.to_owned()),
            Value::String(signed_entity_record.created_at.to_rfc3339()),
            Value::String(signed_entity_record.signed_entity_id.to_owned()),
        ];

        Ok(WhereCondition::new(expression, parameters))
    }

    fn persist(
        &self,
        signed_entity_record: &SignedEntityRecord,
    ) -> Result<SignedEntityRecord, StdError> {
        let filters = self.get_update_condition(signed_entity_record)?;
        let mut cursor = self.find(filters)?;

        cursor.next().ok_or_else(|| {
            panic!(
                "Updating a signed_entity should not return nothing, id = {:?}",
                signed_entity_record.signed_entity_id
            )
        })
    }
}

impl<'client> Provider<'client> for UpdateSignedEntityProvider<'client> {
    type Entity = SignedEntityRecord;

    fn get_connection(&'client self) -> &'client Connection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        let projection = Self::Entity::get_projection()
            .expand(SourceAlias::new(&[("{:signed_entity:}", "signed_entity")]));

        format!("update signed_entity set {condition} returning {projection}")
    }
}

/// Signed entity storer trait
#[cfg_attr(test, automock)]
#[async_trait]
pub trait SignedEntityStorer: Sync + Send {
    /// Store a signed entity
    async fn store_signed_entity(&self, signed_entity: &SignedEntityRecord) -> StdResult<()>;

    /// Get signed entity type
    async fn get_signed_entity(
        &self,
        signed_entity_id: &str,
    ) -> StdResult<Option<SignedEntityRecord>>;

    /// Get signed entity type by certificate id
    async fn get_signed_entity_by_certificate_id(
        &self,
        certificate_hash: &str,
    ) -> StdResult<Option<SignedEntityRecord>>;

    /// Get last signed entities by signed entity type
    async fn get_last_signed_entities_by_type(
        &self,
        signed_entity_type_id: &SignedEntityTypeDiscriminants,
        total: usize,
    ) -> StdResult<Vec<SignedEntityRecord>>;

    /// Perform an update for all the given signed entities.
    async fn update_signed_entities(
        &self,
        signed_entities: Vec<SignedEntityRecord>,
    ) -> StdResult<Vec<SignedEntityRecord>>;
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
impl SignedEntityStorer for SignedEntityStoreAdapter {
    async fn store_signed_entity(&self, signed_entity: &SignedEntityRecord) -> StdResult<()> {
        let connection = &*self.connection.lock().await;
        let provider = InsertSignedEntityRecordProvider::new(connection);
        let _signed_entity_record = provider.persist(signed_entity.to_owned())?;

        Ok(())
    }

    async fn get_signed_entity(
        &self,
        signed_entity_id: &str,
    ) -> StdResult<Option<SignedEntityRecord>> {
        let connection = &*self.connection.lock().await;
        let provider = SignedEntityRecordProvider::new(connection);
        let mut cursor = provider
            .get_by_signed_entity_id(signed_entity_id)
            .map_err(|e| AdapterError::GeneralError(format!("{e}")))?;
        let signed_entity = cursor.next();

        Ok(signed_entity)
    }

    async fn get_signed_entity_by_certificate_id(
        &self,
        certificate_id: &str,
    ) -> StdResult<Option<SignedEntityRecord>> {
        let connection = &*self.connection.lock().await;
        let provider = SignedEntityRecordProvider::new(connection);
        let mut cursor = provider
            .get_by_certificate_id(certificate_id)
            .map_err(|e| AdapterError::GeneralError(format!("{e}")))?;
        let signed_entity = cursor.next();

        Ok(signed_entity)
    }

    async fn get_last_signed_entities_by_type(
        &self,
        signed_entity_type_id: &SignedEntityTypeDiscriminants,
        total: usize,
    ) -> StdResult<Vec<SignedEntityRecord>> {
        let connection = &*self.connection.lock().await;
        let provider = SignedEntityRecordProvider::new(connection);
        let cursor = provider
            .get_by_signed_entity_type(signed_entity_type_id)
            .map_err(|e| AdapterError::GeneralError(format!("{e}")))?;
        let signed_entities: Vec<SignedEntityRecord> = cursor.take(total).collect();

        Ok(signed_entities)
    }

    async fn update_signed_entities(
        &self,
        signed_entities: Vec<SignedEntityRecord>,
    ) -> StdResult<Vec<SignedEntityRecord>> {
        let connection = &*self.connection.lock().await;
        let provider = UpdateSignedEntityProvider::new(connection);
        connection.execute("begin transaction")?;
        let mut updated_records = vec![];

        for record in signed_entities {
            updated_records.push(provider.persist(&record)?);
        }

        connection.execute("commit transaction")?;

        Ok(updated_records)
    }
}

#[cfg(test)]
mod tests {
    use crate::database::provider::{apply_all_migrations_to_db, disable_foreign_key_support};
    use mithril_common::{entities::Beacon, test_utils::fake_data};

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
                    artifact: entity,
                    created_at: snapshot.created_at,
                }
            })
            .collect()
    }

    pub fn setup_signed_entity_db(
        connection: &Connection,
        signed_entity_records: Vec<SignedEntityRecord>,
    ) -> Result<(), StdError> {
        apply_all_migrations_to_db(connection)?;
        disable_foreign_key_support(connection)?;

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
                .bind(5, signed_entity_record.artifact.as_str())
                .unwrap();
            statement
                .bind(6, signed_entity_record.created_at.to_rfc3339().as_str())
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
            "se.signed_entity_id as signed_entity_id, se.signed_entity_type_id as signed_entity_type_id, se.certificate_id as certificate_id, se.beacon as beacon, se.artifact as artifact, se.created_at as created_at"
                .to_string(),
            projection.expand(aliases)
        );
    }

    #[test]
    fn get_signed_entity_record_by_signed_entity_type() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = SignedEntityRecordProvider::new(&connection);
        let condition = provider
            .condition_by_signed_entity_type(
                &SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
            )
            .unwrap();
        let (filter, values) = condition.expand();

        assert_eq!("signed_entity_type_id = ?1".to_string(), filter);
        assert_eq!(vec![Value::Integer(2)], values);
    }

    #[test]
    fn get_signed_entity_record_by_signed_entity_id() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = SignedEntityRecordProvider::new(&connection);
        let condition = provider
            .condition_by_signed_entity_id("signed-ent-123")
            .unwrap();
        let (filter, values) = condition.expand();

        assert_eq!("signed_entity_id = ?1".to_string(), filter);
        assert_eq!(vec![Value::String("signed-ent-123".to_string())], values);
    }

    #[test]
    fn get_signed_entity_record_by_signed_certificate_id() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = SignedEntityRecordProvider::new(&connection);
        let condition = provider
            .condition_by_certificate_id("certificate_id")
            .unwrap();
        let (filter, values) = condition.expand();

        assert_eq!("certificate_id = ?1".to_string(), filter);
        assert_eq!(vec![Value::String("certificate_id".to_string())], values);
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
            "(signed_entity_id, signed_entity_type_id, certificate_id, beacon, artifact, created_at) values (?1, ?2, ?3, ?4, ?5, ?6)".to_string(),
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
                Value::String(signed_entity_record.artifact),
                Value::String(signed_entity_record.created_at.to_rfc3339()),
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
            .get_by_signed_entity_id(&first_signed_entity_type.signed_entity_id)
            .unwrap()
            .collect();
        assert_eq!(vec![first_signed_entity_type], signed_entity_records);

        let signed_entity_records: Vec<SignedEntityRecord> = provider
            .get_by_signed_entity_type(&SignedEntityTypeDiscriminants::CardanoImmutableFilesFull)
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

    #[tokio::test]
    async fn test_get_signed_entity_record_by_certificate_id() {
        let expected_record = fake_signed_entity_records(1).remove(0);
        let connection = Connection::open(":memory:").unwrap();
        setup_signed_entity_db(&connection, vec![expected_record.clone()]).unwrap();
        let store = SignedEntityStoreAdapter::new(Arc::new(Mutex::new(connection)));

        let record = store
            .get_signed_entity_by_certificate_id(&expected_record.certificate_id)
            .await
            .expect("querying signed entity record by certificate id should not fail");

        assert_eq!(Some(expected_record), record);
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

    #[test]
    fn update_provider_condition() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = UpdateSignedEntityProvider::new(&connection);
        let snapshots = fake_data::snapshots(1);
        let snapshot = snapshots.first().unwrap().to_owned();
        let signed_entity_record: SignedEntityRecord = snapshot.into();
        let (expr, params) = provider
            .get_update_condition(&signed_entity_record)
            .unwrap()
            .expand();

        assert_eq!(
            "signed_entity_type_id = ?1, certificate_id = ?2, beacon = ?3, artifact = ?4, created_at = ?5 where signed_entity_id = ?6"
                .to_string(),
            expr
        );
        assert_eq!(
            vec![
                Value::Integer(signed_entity_record.signed_entity_type.index() as i64),
                Value::String(signed_entity_record.certificate_id.to_owned()),
                Value::String(
                    signed_entity_record
                        .signed_entity_type
                        .get_json_beacon()
                        .unwrap()
                ),
                Value::String(signed_entity_record.artifact.to_owned()),
                Value::String(signed_entity_record.created_at.to_rfc3339()),
                Value::String(signed_entity_record.signed_entity_id),
            ],
            params
        );
    }

    #[tokio::test]
    async fn update_only_given_entities() {
        let mut signed_entity_records = fake_signed_entity_records(5);

        let connection = Connection::open(":memory:").unwrap();
        setup_signed_entity_db(&connection, signed_entity_records.clone()).unwrap();
        let store = SignedEntityStoreAdapter::new(Arc::new(Mutex::new(connection)));

        let records_to_update: Vec<SignedEntityRecord> = signed_entity_records
            .drain(2..)
            .map(|mut r| {
                r.certificate_id = format!("updated-{}", r.certificate_id);
                r
            })
            .collect();
        let expected_records: Vec<SignedEntityRecord> = signed_entity_records
            .into_iter()
            .chain(records_to_update.clone())
            .rev() // Records are returned from latest to oldest
            .collect();

        let updated_records = store
            .update_signed_entities(records_to_update.clone())
            .await
            .expect("updating signed entities should not fail");

        let stored_records = store
            .get_last_signed_entities_by_type(
                &SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
                usize::MAX,
            )
            .await
            .expect("getting signed entities should not fail");

        assert_eq!(records_to_update, updated_records);
        assert_eq!(expected_records, stored_records);
    }
}

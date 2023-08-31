use anyhow::Context;
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
    StdResult,
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
        let signed_entity_id = row.read::<&str, _>(0).to_string();
        let signed_entity_type_id_int = row.read::<i64, _>(1);
        let certificate_id = row.read::<&str, _>(2).to_string();
        // TODO: We need to check first that the cell can be read as a string first
        // (e.g. when beacon json is '{"network": "dev", "epoch": 1, "immutable_file_number": 2}').
        // If it fails, we fallback on readign the cell as an integer (e.g. when beacon json is '5').
        // Maybe there is a better way of doing this.
        let beacon_str = match row.try_read::<&str, _>(3) {
            Ok(value) => value.to_string(),
            Err(_) => (row.read::<i64, _>(3)).to_string(),
        };
        let artifact_str = row.read::<&str, _>(4).to_string();
        let created_at = row.read::<&str, _>(5);

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
            created_at: DateTime::parse_from_rfc3339(created_at)
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

    fn condition_by_signed_entity_id(&self, signed_entity_id: &str) -> StdResult<WhereCondition> {
        Ok(WhereCondition::new(
            "signed_entity_id = ?*",
            vec![Value::String(signed_entity_id.to_owned())],
        ))
    }

    fn condition_by_certificate_id(&self, certificate_id: &str) -> StdResult<WhereCondition> {
        Ok(WhereCondition::new(
            "certificate_id = ?*",
            vec![Value::String(certificate_id.to_owned())],
        ))
    }

    fn condition_by_certificates_ids(&self, certificates_ids: &[&str]) -> WhereCondition {
        let ids_values = certificates_ids
            .iter()
            .map(|id| Value::String(id.to_string()))
            .collect();

        WhereCondition::where_in("certificate_id", ids_values)
    }

    fn condition_by_signed_entity_type(
        &self,
        signed_entity_type: &SignedEntityTypeDiscriminants,
    ) -> StdResult<WhereCondition> {
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
    ) -> StdResult<EntityCursor<SignedEntityRecord>> {
        let filters = self.condition_by_signed_entity_id(signed_entity_id)?;
        let signed_entity_record = self.find(filters)?;

        Ok(signed_entity_record)
    }

    /// Get [record][SignedEntityRecord] for a given `certificate_id`.
    pub fn get_by_certificate_id(
        &self,
        certificate_id: &str,
    ) -> StdResult<EntityCursor<SignedEntityRecord>> {
        let filters = self.condition_by_certificate_id(certificate_id)?;
        let signed_entity_record = self.find(filters)?;

        Ok(signed_entity_record)
    }

    /// Get [records][SignedEntityRecord] for a list of given `certificates_ids`.
    pub fn get_by_certificates_ids(
        &self,
        certificates_ids: &[&str],
    ) -> StdResult<EntityCursor<SignedEntityRecord>> {
        let filters = self.condition_by_certificates_ids(certificates_ids);
        let signed_entity_record = self.find(filters)?;

        Ok(signed_entity_record)
    }

    /// Get SignedEntityRecords for a given signed entity type.
    pub fn get_by_signed_entity_type(
        &self,
        signed_entity_type: &SignedEntityTypeDiscriminants,
    ) -> StdResult<EntityCursor<SignedEntityRecord>> {
        let filters = self.condition_by_signed_entity_type(signed_entity_type)?;
        let signed_entity_record = self.find(filters)?;

        Ok(signed_entity_record)
    }

    /// Get all SignedEntityRecords.
    pub fn get_all(&self) -> StdResult<EntityCursor<SignedEntityRecord>> {
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

    fn persist(&self, signed_entity_record: SignedEntityRecord) -> StdResult<SignedEntityRecord> {
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

    fn persist(&self, signed_entity_record: &SignedEntityRecord) -> StdResult<SignedEntityRecord> {
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

    /// Get signed entities type by certificates ids
    async fn get_signed_entity_by_certificates_ids<'a>(
        &self,
        certificates_ids: &[&'a str],
    ) -> StdResult<Vec<SignedEntityRecord>>;

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
            .with_context(|| format!("get signed entity by id failure, id: {signed_entity_id}"))
            .map_err(AdapterError::GeneralError)?;
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
            .with_context(|| {
                format!(
                    "get signed entity by certificate id failure, certificate_id: {certificate_id}"
                )
            })
            .map_err(AdapterError::GeneralError)?;
        let signed_entity = cursor.next();

        Ok(signed_entity)
    }

    async fn get_signed_entity_by_certificates_ids<'a>(
        &self,
        certificates_ids: &[&'a str],
    ) -> StdResult<Vec<SignedEntityRecord>> {
        let connection = &*self.connection.lock().await;
        let provider = SignedEntityRecordProvider::new(connection);
        let cursor = provider.get_by_certificates_ids(certificates_ids)?;

        Ok(cursor.collect())
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
            .with_context(|| {
                format!("get last signed entity by type failure, type: {signed_entity_type_id:?}")
            })
            .map_err(AdapterError::GeneralError)?;
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
    use mithril_common::entities::MithrilStakeDistribution;
    use mithril_common::{entities::Beacon, test_utils::fake_data};

    use super::*;

    impl SignedEntityRecord {
        fn from_snapshot(
            snapshot: Snapshot,
            certificate_id: String,
            created_at: DateTime<Utc>,
        ) -> Self {
            let entity = serde_json::to_string(&snapshot).unwrap();

            SignedEntityRecord {
                signed_entity_id: snapshot.digest,
                signed_entity_type: SignedEntityType::CardanoImmutableFilesFull(snapshot.beacon),
                certificate_id,
                artifact: entity,
                created_at,
            }
        }
    }

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
                    certificate_id: format!("certificate-{idx}"),
                    artifact: entity,
                    created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                        .unwrap()
                        .with_timezone(&Utc),
                }
            })
            .collect()
    }

    pub fn setup_signed_entity_db(
        connection: &Connection,
        signed_entity_records: Vec<SignedEntityRecord>,
    ) -> StdResult<()> {
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
                .bind::<&[(_, Value)]>(&[
                    (1, signed_entity_record.signed_entity_id.into()),
                    (
                        2,
                        i64::try_from(signed_entity_record.signed_entity_type.index())
                            .unwrap()
                            .into(),
                    ),
                    (3, signed_entity_record.certificate_id.into()),
                    (
                        4,
                        signed_entity_record
                            .signed_entity_type
                            .get_json_beacon()
                            .unwrap()
                            .into(),
                    ),
                    (5, signed_entity_record.artifact.into()),
                    (6, signed_entity_record.created_at.to_rfc3339().into()),
                ])
                .unwrap();

            statement.next().unwrap();
        }

        Ok(())
    }

    fn insert_golden_signed_entities(connection: &Connection) {
        connection
            .execute(r#"
            -- Cardano immutable file full
            insert into signed_entity values(
                'bfcd77e372a25e13353bb77697d0d08785ba98b703e22640a317c5054dc05fb1',
                2,
                '258edf0f1238c60985d0229869a6d4c4c635c118915b4d524d2686515be99946',
                '{"network":"preview","epoch":142,"immutable_file_number":2847}',
                '2023-05-09T13:11:15Z',
                '{
                    "digest":"bfcd77e372a25e13353bb77697d0d08785ba98b703e22640a317c5054dc05fb1",
                    "beacon":{"network":"preview","epoch":142,"immutable_file_number":2847},
                    "size":1689696245,
                    "locations":["https://storage.googleapis.com/mithril-testing-preview-cs/preview-e142-i2847.bfcd77e372a25e13353bb77697d0d08785ba98b703e22640a317c5054dc05fb1.tar.gz"],
                    "compression_algorithm":"gunzip"
                }'
            );

            -- Mithril stake distribution
            insert into signed_entity
            values(
                '2da62e3ffee5e284ffd1e29ee52ee5547c5ff5ef34bee0a49dc54ea5e375f77e',
                0,
                'ad2d3705693dfaae8baac099b6976a5cc3e0f708245d0fa79d149a3fcbc79f00',
                203,
                '2023-05-16T02:17:16.203859116Z',
                '{
                    "type":"MithrilStakeDistribution",
                    "epoch":203,
                    "signers_with_stake":[{
                        "party_id":"pool1r0tln8nct3mpyvehgy6uu3cdlmjnmtr2fxjcqnfl6v0qg0we42e",
                        "verification_key":"7b22766b223a5b3138352c3132342c3231382c31362c3133312c3137382c3136302c37312c35382c3235312c31382c36382c37372c3135342c35382c3131352c3133392c3139392c38392c3230382c3139312c3235332c3138362c3232302c3133372c3135382c34312c3230332c382c3136352c3232362c3139342c3133382c3135322c35382c3131352c35342c3136322c3230332c32322c32332c3232382c3139342c34382c3137322c3139342c3130352c36382c302c3138302c3131332c3230312c3130392c3234372c39362c39342c3232372c3135372c36322c3139352c3134382c33352c3230352c3133372c3132312c3135322c3130302c3138342c3136372c3230362c3133322c34352c3133382c3131312c38392c3138322c3230352c3138372c3135382c32322c32332c3231382c36342c3137332c35392c3134312c3133332c3138302c3131392c36302c3134392c3134382c38332c3234312c3230312c33375d2c22706f70223a5b3137352c3135382c3130322c34352c3133322c38352c33312c3130342c36352c3230342c38352c312c34332c3137382c3138372c3233382c3135372c32372c39312c3230332c35342c37332c36322c35352c3131322c3131352c302c32312c3130302c3230382c37392c3135382c3233332c3132372c38332c3234352c3134362c3231382c3131382c3139332c38322c3139352c3137302c32312c36382c3231342c3138352c38372c3136382c3135372c3230322c3136382c3132392c3233332c38372c3230332c37372c36332c3232312c31362c3130392c33302c3235312c35312c38342c36392c3233372c39382c3133372c36302c39312c37362c38302c3232322c302c3130342c3231332c3132352c31332c3135312c3133312c3130312c3230312c33322c3138392c3137362c3139392c3131342c3234302c31352c33312c3136302c31332c3136352c32372c3134335d7d",
                        "verification_key_signature":"7b227369676d61223a7b227369676d61223a7b227369676d61223a7b227369676d61223a7b227369676d61223a7b227369676d61223a5b36322c33302c32362c3131302c3134382c3137342c36302c32392c3139302c34362c37342c3135392c3137312c3134362c31342c3231362c37352c32302c38352c3134342c32332c3134352c3132352c39382c36362c3132342c3139332c32352c3233352c3234372c3130342c382c352c3133342c302c3134302c3131352c33362c31342c33382c31322c34392c39372c33392c3232312c3234352c39322c362c35302c3134362c3135372c3136362c32382c38352c33312c33352c3232392c3233312c34332c3230322c34372c3235352c3138322c375d2c226c68735f706b223a5b3234322c3132322c34322c39332c39352c31322c36352c3131332c31342c35322c3135352c3133372c3130312c3137382c3232362c3133332c31372c39302c3138332c3132312c3136362c35322c342c39312c3135332c3232332c32352c3133372c37332c3137332c3235332c3233315d2c227268735f706b223a5b322c3231312c3230372c3234392c3234322c362c3131322c362c3235322c31322c3135362c3139332c38362c3133362c3138352c36342c3132342c35302c3230392c38382c3138322c3133352c32392c3138372c3133302c3138392c34312c3134302c34322c33342c3135392c3234365d7d2c226c68735f706b223a5b37352c3232312c3235302c3235322c3135382c3134362c35362c34312c39382c3137362c3139382c3231392c33352c3130392c3136332c36312c3139362c3139342c3137382c3130392c3132382c3131352c3130302c3135322c33392c3231392c35382c34392c3235302c33312c3138342c395d2c227268735f706b223a5b3135372c3232312c38372c3139342c3235322c3234382c3132372c33312c3136362c3235322c3233342c3232362c33362c3139352c3230312c33312c34372c3232302c3233372c3137342c3130372c3134342c33382c3234372c3135352c3135382c34372c3139302c3235322c3134302c3235342c3131375d7d2c226c68735f706b223a5b34322c37372c37392c36302c3137312c3234372c31392c3230332c3232302c36332c3231352c3135372c3132392c3230382c3135382c35352c3131302c3232312c3139372c3233322c38372c33312c31312c3235342c3133352c32372c3234352c3137352c3135342c3231382c3232312c3138345d2c227268735f706b223a5b36382c3230332c38362c31352c37352c33352c3232332c342c3130392c3234392c3231372c3132352c34372c3231372c3130342c36352c3131332c3234312c3235332c3138332c3138362c36362c37352c3135302c3233342c3138362c3137332c3233302c3130332c3139342c3135322c3132375d7d2c226c68735f706b223a5b3133312c38372c3135382c3233352c34312c3233372c33332c34362c3235342c38302c3235322c3132392c37332c3234382c3135332c33382c3138332c33342c3231362c3135362c3131302c36392c37322c39302c32382c36382c3131342c33352c33352c3134332c3234312c3231305d2c227268735f706b223a5b3232382c3135352c3133332c34312c3137392c39342c3233362c3133392c3231362c3136302c3130382c3137362c3134362c3232352c3134302c3231352c35392c3130372c32302c3133372c3139372c33392c3135332c3132362c3233372c3135382c3132332c392c3133322c3139342c3132312c3232355d7d2c226c68735f706b223a5b3232322c39362c35332c34312c32342c38382c3136342c39382c3133312c33342c3132362c3133392c32382c37342c34362c3137332c35302c3133362c39372c3137312c3130312c3136322c33312c3137352c32332c3130352c3231352c36332c37362c3132342c31322c3131365d2c227268735f706b223a5b3133382c31382c31302c352c3231382c3134372c35332c35322c33362c3234342c33362c3131302c31302c33382c3134382c3132332c3235302c3131352c36342c36372c3137332c3130352c3137392c3235342c3130352c332c3132372c32302c31322c3230352c37372c3230365d7d2c226c68735f706b223a5b3132342c382c37312c3135302c34352c3130362c3232322c3234372c3130302c3137342c34352c3135322c3136312c3130382c3135382c32372c3234342c35362c3131352c3233322c3136332c3234342c38372c3138332c3232372c3235302c3232372c3234382c3137352c3136332c3230392c37345d2c227268735f706b223a5b3134302c33372c3131392c36332c39302c3132302c3131332c3135372c3130352c34362c31342c33332c3230372c3131322c3131332c3235342c38342c37332c3131302c33392c392c3230372c3133312c342c3232352c39302c3135312c32302c31352c36342c39372c39385d7d",
                        "operational_certificate":"5b5b5b3138332c33342c3231362c34362c3232372c3235312c37342c3130312c31352c3233332c3234392c34322c312c37372c37322c3234382c3137392c32312c3137332c3131332c3131382c3139382c36322c3133352c34352c38382c3138372c3233332c34302c37322c31362c36365d2c312c3132332c5b31362c3136392c3134312c3138332c32322c3137342c3131312c33322c36342c35322c3234392c36382c3230322c33352c3130362c332c38362c3230352c37382c3230302c3138362c39342c3139372c3232382c37392c3137352c32392c31342c3132382c36332c35392c3139382c36322c3233302c34362c34312c38342c39382c3131392c3134352c32392c3132312c33352c3139372c3132382c3137322c302c3135342c392c31332c32362c3138332c3138362c3138362c33312c3234392c3133322c3232392c3235332c3134332c3130322c3235342c3231322c315d5d2c5b3234312c32372c31332c34342c3131342c37382c3138392c3234392c3135302c3135302c35332c3134342c3233362c3135312c38382c3134302c3132382c3136322c36302c3232382c38382c3131312c392c3134342c3233322c38332c39342c3231302c3135362c3136382c33352c3234325d5d",
                        "kes_period":12,
                        "stake":9497629046
                    }],
                    "hash":"2da62e3ffee5e284ffd1e29ee52ee5547c5ff5ef34bee0a49dc54ea5e375f77e",
                    "protocol_parameters":{"k":2422,"m":20973,"phi_f":0.2}}'
            );

            "#,
            )
            .unwrap();
    }

    #[test]
    fn test_golden_master() {
        let connection = Connection::open(":memory:").unwrap();
        setup_signed_entity_db(&connection, vec![]).unwrap();
        insert_golden_signed_entities(&connection);

        let provider = SignedEntityRecordProvider::new(&connection);
        let cardano_immutable_files_fulls: Vec<SignedEntity<Snapshot>> = provider
            .get_by_signed_entity_type(&SignedEntityTypeDiscriminants::CardanoImmutableFilesFull)
            .expect("Getting Golden snapshot signed entities should not fail")
            .map(|r| r.try_into().unwrap())
            .collect();
        let mithril_stake_distributions: Vec<SignedEntity<MithrilStakeDistribution>> = provider
            .get_by_signed_entity_type(&SignedEntityTypeDiscriminants::MithrilStakeDistribution)
            .expect("Getting Golden mithril stake distribution signed entities should not fail")
            .map(|r| r.try_into().unwrap())
            .collect();

        assert_eq!(cardano_immutable_files_fulls.len(), 1);
        assert_eq!(mithril_stake_distributions.len(), 1);
    }

    #[test]
    fn test_convert_signed_entity() {
        let snapshots = fake_data::snapshots(1);
        let snapshot = snapshots.first().unwrap().to_owned();
        let snapshot_expected = snapshot.clone();

        let signed_entity: SignedEntityRecord = SignedEntityRecord::from_snapshot(
            snapshot,
            "certificate-1".to_string(),
            DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                .unwrap()
                .with_timezone(&Utc),
        );
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
    fn get_signed_entity_record_by_signed_certificates_ids() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = SignedEntityRecordProvider::new(&connection);
        let condition = provider.condition_by_certificates_ids(&["a", "b", "c"]);
        let (condition, params) = condition.expand();

        assert_eq!("certificate_id in (?1, ?2, ?3)".to_string(), condition);
        assert_eq!(
            vec![
                Value::String("a".to_string()),
                Value::String("b".to_string()),
                Value::String("c".to_string()),
            ],
            params
        );
    }

    #[test]
    fn insert_signed_entity_record() {
        let snapshots = fake_data::snapshots(1);
        let snapshot = snapshots.first().unwrap().to_owned();
        let signed_entity_record: SignedEntityRecord = SignedEntityRecord::from_snapshot(
            snapshot,
            "certificate-1".to_string(),
            DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                .unwrap()
                .with_timezone(&Utc),
        );
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

    #[tokio::test]
    async fn test_get_signed_entity_record_by_certificates_ids() {
        let expected_records = fake_signed_entity_records(3);
        let connection = Connection::open(":memory:").unwrap();
        setup_signed_entity_db(&connection, expected_records.clone()).unwrap();
        let store = SignedEntityStoreAdapter::new(Arc::new(Mutex::new(connection)));
        let certificates_ids: Vec<&str> = expected_records
            .iter()
            .map(|r| r.certificate_id.as_str())
            .collect();

        let queried_records = store
            .get_signed_entity_by_certificates_ids(&certificates_ids)
            .await
            .expect("querying signed entity record by certificates ids should not fail");

        assert_eq!(
            // Records are inserted older to earlier and queried the other way round
            expected_records.into_iter().rev().collect::<Vec<_>>(),
            queried_records
        );
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
        let signed_entity_record: SignedEntityRecord = SignedEntityRecord::from_snapshot(
            snapshot,
            "certificate-1".to_string(),
            DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                .unwrap()
                .with_timezone(&Utc),
        );
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

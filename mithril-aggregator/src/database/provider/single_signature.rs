use chrono::{DateTime, Utc};
use sqlite::{Connection, Value};
use std::sync::Arc;
use tokio::sync::Mutex;
use uuid::Uuid;

use mithril_common::{
    entities::{Epoch, HexEncodedSingleSignature, LotteryIndex, SingleSignatures},
    sqlite::{
        EntityCursor, HydrationError, Projection, Provider, SourceAlias, SqLiteEntity,
        WhereCondition,
    },
    StdError, StdResult,
};

use super::OpenMessageRecord;

/// SingleSignature record is the representation of a stored single_signature.
#[derive(Debug, PartialEq, Clone)]
pub struct SingleSignatureRecord {
    /// Open message id.
    pub open_message_id: Uuid,

    /// Signer id.
    pub signer_id: String,

    /// Registration epoch setting id
    pub registration_epoch_setting_id: Epoch,

    /// Lottery indexes
    pub lottery_indexes: Vec<LotteryIndex>,

    /// The STM single signature of the message
    pub signature: HexEncodedSingleSignature,

    /// Date and time when the single_signature was created
    pub created_at: DateTime<Utc>,
}

impl SingleSignatureRecord {
    fn try_from_single_signatures(
        other: &SingleSignatures,
        open_message_id: &Uuid,
        registration_epoch_setting_id: Epoch,
    ) -> StdResult<Self> {
        let record = SingleSignatureRecord {
            open_message_id: open_message_id.to_owned(),
            signer_id: other.party_id.to_owned(),
            registration_epoch_setting_id,
            lottery_indexes: other.won_indexes.to_owned(),
            signature: other.signature.to_json_hex()?,
            created_at: Utc::now(),
        };

        Ok(record)
    }
}

impl TryFrom<SingleSignatureRecord> for SingleSignatures {
    type Error = StdError;

    fn try_from(value: SingleSignatureRecord) -> Result<Self, Self::Error> {
        let signatures = SingleSignatures {
            party_id: value.signer_id,
            won_indexes: value.lottery_indexes,
            signature: value.signature.try_into()?,
        };

        Ok(signatures)
    }
}

impl SqLiteEntity for SingleSignatureRecord {
    fn hydrate(row: sqlite::Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let open_message_id = row.read::<&str, _>(0);
        let open_message_id = Uuid::parse_str(open_message_id).map_err(|e| {
            HydrationError::InvalidData(format!(
                "Invalid UUID in single_signature.open_message_id: '{open_message_id}'. Error: {e}"
            ))
        })?;
        let signer_id = row.read::<&str, _>(1).to_string();
        let registration_epoch_setting_id_int = row.read::<i64, _>(2);
        let lottery_indexes_str = row.read::<&str, _>(3);
        let signature = row.read::<&str, _>(4).to_string();
        let created_at = row.read::<&str, _>(5);

        let single_signature_record = Self {
            open_message_id,
            signer_id,
            registration_epoch_setting_id: Epoch(
                registration_epoch_setting_id_int.try_into().map_err(|e| {
                    HydrationError::InvalidData(format!(
                    "Could not cast i64 ({registration_epoch_setting_id_int}) to u64. Error: '{e}'"
                ))
                })?,
            ),
            lottery_indexes: serde_json::from_str(lottery_indexes_str).map_err(|e| {
                HydrationError::InvalidData(format!(
                    "Could not turn string '{lottery_indexes_str}' to Vec<LotteryIndex>. Error: {e}"
                ))
            })?,
            signature,
            created_at: DateTime::parse_from_rfc3339(created_at)
                .map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{created_at}' to rfc3339 Datetime. Error: {e}"
                    ))
                })?
                .with_timezone(&Utc),
        };

        Ok(single_signature_record)
    }

    fn get_projection() -> Projection {
        let mut projection = Projection::default();
        projection.add_field(
            "open_message_id",
            "{:single_signature:}.open_message_id",
            "text",
        );
        projection.add_field("signer_id", "{:single_signature:}.signer_id", "text");
        projection.add_field(
            "registration_epoch_setting_id",
            "{:single_signature:}.registration_epoch_setting_id",
            "integer",
        );
        projection.add_field(
            "lottery_indexes",
            "{:single_signature:}.lottery_indexes",
            "text",
        );
        projection.add_field("signature", "{:single_signature:}.signature", "text");
        projection.add_field("created_at", "{:single_signature:}.created_at", "text");

        projection
    }
}

/// Simple [SingleSignatureRecord] provider.
pub struct SingleSignatureRecordProvider<'client> {
    client: &'client Connection,
}

impl<'client> SingleSignatureRecordProvider<'client> {
    /// Create a new provider
    pub fn new(client: &'client Connection) -> Self {
        Self { client }
    }

    fn condition_by_open_message_id(&self, open_message_id: &Uuid) -> StdResult<WhereCondition> {
        Ok(WhereCondition::new(
            "open_message_id = ?*",
            vec![Value::String(open_message_id.to_string())],
        ))
    }

    #[allow(dead_code)] // todo: Should we keep this ?
    fn condition_by_signer_id(&self, signer_id: String) -> StdResult<WhereCondition> {
        Ok(WhereCondition::new(
            "signer_id = ?*",
            vec![Value::String(signer_id)],
        ))
    }

    #[allow(dead_code)] // todo: Should we keep this ?
    fn condition_by_registration_epoch(
        &self,
        registration_epoch: &Epoch,
    ) -> StdResult<WhereCondition> {
        let epoch: i64 = registration_epoch.try_into()?;

        Ok(WhereCondition::new(
            "registration_epoch_setting_id = ?*",
            vec![Value::Integer(epoch)],
        ))
    }

    /// Get SingleSignatureRecords for a given Open Message id.
    pub fn get_by_open_message_id(
        &self,
        open_message_id: &Uuid,
    ) -> StdResult<EntityCursor<SingleSignatureRecord>> {
        let filters = self.condition_by_open_message_id(open_message_id)?;
        let single_signature_record = self.find(filters)?;

        Ok(single_signature_record)
    }

    /// Get all SingleSignatureRecords.
    pub fn get_all(&self) -> StdResult<EntityCursor<SingleSignatureRecord>> {
        let filters = WhereCondition::default();
        let single_signature_record = self.find(filters)?;

        Ok(single_signature_record)
    }
}

impl<'client> Provider<'client> for SingleSignatureRecordProvider<'client> {
    type Entity = SingleSignatureRecord;

    fn get_connection(&'client self) -> &'client Connection {
        self.client
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:single_signature:}", "ssig")]);
        let projection = Self::Entity::get_projection().expand(aliases);
        format!("select {projection} from single_signature as ssig where {condition} order by ROWID desc")
    }
}

/// Query to update the single_signature record
pub struct UpdateSingleSignatureRecordProvider<'conn> {
    connection: &'conn Connection,
}

impl<'conn> UpdateSingleSignatureRecordProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn Connection) -> Self {
        Self { connection }
    }

    pub(crate) fn get_update_condition(
        &self,
        single_signature_record: &SingleSignatureRecord,
    ) -> WhereCondition {
        WhereCondition::new(
            "(open_message_id, signer_id, registration_epoch_setting_id, lottery_indexes, signature, created_at) values (?*, ?*, ?*, ?*, ?*, ?*)",
            vec![
                Value::String(single_signature_record.open_message_id.to_string()),
                Value::String(single_signature_record.signer_id.to_owned()),
                Value::Integer(
                    single_signature_record.registration_epoch_setting_id.try_into().unwrap(),
                ),
                Value::String(serde_json::to_string(&single_signature_record.lottery_indexes).unwrap()),
                Value::String(single_signature_record.signature.to_owned()),
                Value::String(single_signature_record.created_at.to_rfc3339()),
            ],
        )
    }

    fn persist(
        &self,
        single_signature_record: SingleSignatureRecord,
    ) -> StdResult<SingleSignatureRecord> {
        let filters = self.get_update_condition(&single_signature_record);

        let entity = self.find(filters)?.next().unwrap_or_else(|| {
            panic!(
                "No entity returned by the persister, single_signature_record = {single_signature_record:?}"
            )
        });

        Ok(entity)
    }
}

impl<'conn> Provider<'conn> for UpdateSingleSignatureRecordProvider<'conn> {
    type Entity = SingleSignatureRecord;

    fn get_connection(&'conn self) -> &'conn Connection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection().expand(SourceAlias::new(&[(
            "{:single_signature:}",
            "single_signature",
        )]));

        format!("insert or replace into single_signature {condition} returning {projection}")
    }
}

/// Service to deal with single_signature (read & write).
pub struct SingleSignatureRepository {
    connection: Arc<Mutex<Connection>>,
}

impl SingleSignatureRepository {
    /// Create a new SingleSignatureStoreAdapter service
    pub fn new(connection: Arc<Mutex<Connection>>) -> Self {
        Self { connection }
    }

    /// Create a new Single Signature in database
    pub async fn create_single_signature(
        &self,
        single_signature: &SingleSignatures,
        open_message: &OpenMessageRecord,
    ) -> StdResult<SingleSignatureRecord> {
        let connection = self.connection.lock().await;
        let single_signature = SingleSignatureRecord::try_from_single_signatures(
            single_signature,
            &open_message.open_message_id,
            open_message.epoch.offset_to_signer_retrieval_epoch()?,
        )?;
        let provider = UpdateSingleSignatureRecordProvider::new(&connection);

        provider.persist(single_signature)
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::test_utils::fake_data;

    use crate::database::provider::{
        apply_all_migrations_to_db, disable_foreign_key_support, insert_single_signatures_in_db,
        setup_single_signature_records,
    };

    use super::*;

    #[test]
    fn test_convert_single_signatures() {
        let single_signature = fake_data::single_signatures(vec![1, 3, 4, 6, 7, 9]);
        let open_message_id = Uuid::parse_str("193d1442-e89b-43cf-9519-04d8db9a12ff").unwrap();
        let single_signature_record = SingleSignatureRecord::try_from_single_signatures(
            &single_signature,
            &open_message_id,
            Epoch(1),
        )
        .unwrap();
        let single_signature_returned = single_signature_record.try_into().unwrap();

        assert_eq!(single_signature, single_signature_returned);
    }

    #[test]
    fn projection() {
        let projection = SingleSignatureRecord::get_projection();
        let aliases = SourceAlias::new(&[("{:single_signature:}", "ssig")]);

        assert_eq!(
            "ssig.open_message_id as open_message_id, ssig.signer_id as signer_id, ssig.registration_epoch_setting_id as registration_epoch_setting_id, ssig.lottery_indexes as lottery_indexes, ssig.signature as signature, ssig.created_at as created_at"
                .to_string(),
            projection.expand(aliases)
        );
    }

    #[test]
    fn get_single_signature_record_by_epoch() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = SingleSignatureRecordProvider::new(&connection);
        let open_message_id_test = Uuid::parse_str("193d1442-e89b-43cf-9519-04d8db9a12ff").unwrap();
        let condition = provider
            .condition_by_open_message_id(&open_message_id_test)
            .unwrap();
        let (filter, values) = condition.expand();

        assert_eq!("open_message_id = ?1".to_string(), filter);
        assert_eq!(
            vec![Value::String(open_message_id_test.to_string())],
            values
        );
    }

    #[test]
    fn get_single_signature_record_by_signer_id() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = SingleSignatureRecordProvider::new(&connection);
        let condition = provider
            .condition_by_signer_id("signer-123".to_string())
            .unwrap();
        let (filter, values) = condition.expand();

        assert_eq!("signer_id = ?1".to_string(), filter);
        assert_eq!(vec![Value::String("signer-123".to_string())], values);
    }

    #[test]
    fn get_single_signature_record_by_registration_epoch() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = SingleSignatureRecordProvider::new(&connection);
        let condition = provider
            .condition_by_registration_epoch(&Epoch(17))
            .unwrap();
        let (filter, values) = condition.expand();

        assert_eq!("registration_epoch_setting_id = ?1".to_string(), filter);
        assert_eq!(vec![Value::Integer(17)], values);
    }

    #[test]
    fn update_single_signature_record() {
        let single_signature = fake_data::single_signatures(vec![1, 3, 4, 6, 7, 9]);
        let single_signature_record = SingleSignatureRecord::try_from_single_signatures(
            &single_signature,
            &Uuid::parse_str("193d1442-e89b-43cf-9519-04d8db9a12ff").unwrap(),
            Epoch(1),
        )
        .unwrap();
        let connection = Connection::open(":memory:").unwrap();
        let provider = UpdateSingleSignatureRecordProvider::new(&connection);
        let condition = provider.get_update_condition(&single_signature_record);
        let (values, params) = condition.expand();

        assert_eq!(
            "(open_message_id, signer_id, registration_epoch_setting_id, lottery_indexes, signature, created_at) values (?1, ?2, ?3, ?4, ?5, ?6)".to_string(),
            values
        );
        assert_eq!(
            vec![
                Value::String(single_signature_record.open_message_id.to_string()),
                Value::String(single_signature_record.signer_id),
                Value::Integer(*single_signature_record.registration_epoch_setting_id as i64),
                Value::String(
                    serde_json::to_string(&single_signature_record.lottery_indexes).unwrap()
                ),
                Value::String(single_signature_record.signature),
                Value::String(single_signature_record.created_at.to_rfc3339()),
            ],
            params
        );
    }

    #[tokio::test]
    async fn test_get_single_signature_records() {
        let single_signature_records_src = setup_single_signature_records(2, 3, 4);

        let connection = Connection::open(":memory:").unwrap();
        apply_all_migrations_to_db(&connection).unwrap();
        disable_foreign_key_support(&connection).unwrap();
        insert_single_signatures_in_db(&connection, single_signature_records_src.clone()).unwrap();

        let provider = SingleSignatureRecordProvider::new(&connection);

        let open_message_id_test = single_signature_records_src[0].open_message_id.to_owned();
        let single_signature_records: Vec<SingleSignatureRecord> = provider
            .get_by_open_message_id(&open_message_id_test)
            .unwrap()
            .collect();
        let expected_single_signature_records: Vec<SingleSignatureRecord> =
            single_signature_records
                .iter()
                .filter_map(|ssig| {
                    if ssig.open_message_id == open_message_id_test {
                        Some(ssig.to_owned())
                    } else {
                        None
                    }
                })
                .collect();
        assert!(!single_signature_records.is_empty());
        assert_eq!(expected_single_signature_records, single_signature_records);

        let open_message_id_test = single_signature_records
            .last()
            .unwrap()
            .open_message_id
            .to_owned();
        let single_signature_records: Vec<SingleSignatureRecord> = provider
            .get_by_open_message_id(&open_message_id_test)
            .unwrap()
            .collect();
        let expected_single_signature_records: Vec<SingleSignatureRecord> =
            single_signature_records
                .iter()
                .filter_map(|ssig| {
                    if ssig.open_message_id == open_message_id_test {
                        Some(ssig.to_owned())
                    } else {
                        None
                    }
                })
                .collect();
        assert!(!single_signature_records.is_empty());
        assert_eq!(expected_single_signature_records, single_signature_records);

        let open_message_id_test = Uuid::parse_str("193d1442-e89b-43cf-9519-04d8db9a12ff").unwrap();
        let single_signature_records: Vec<SingleSignatureRecord> = provider
            .get_by_open_message_id(&open_message_id_test)
            .unwrap()
            .collect();
        assert!(single_signature_records.is_empty());

        let single_signature_records_returned: Vec<SingleSignatureRecord> = provider
            .get_all()
            .unwrap()
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
            .collect();
        assert_eq!(
            single_signature_records_src,
            single_signature_records_returned
        );
    }

    #[test]
    fn test_update_single_signature_record() {
        let single_signature_records = setup_single_signature_records(2, 3, 4);
        let single_signature_records_copy = single_signature_records.clone();

        let connection = Connection::open(":memory:").unwrap();
        apply_all_migrations_to_db(&connection).unwrap();
        disable_foreign_key_support(&connection).unwrap();

        let provider = UpdateSingleSignatureRecordProvider::new(&connection);

        for single_signature_record in single_signature_records {
            let single_signature_record_saved =
                provider.persist(single_signature_record.clone()).unwrap();
            assert_eq!(single_signature_record, single_signature_record_saved);
        }

        for single_signature_record in single_signature_records_copy {
            let single_signature_record_saved =
                provider.persist(single_signature_record.clone()).unwrap();
            assert_eq!(single_signature_record, single_signature_record_saved);
        }
    }
}

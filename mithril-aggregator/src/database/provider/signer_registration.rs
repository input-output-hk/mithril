use async_trait::async_trait;
use chrono::{DateTime, Utc};
use sqlite::{Connection, Value};
use std::{
    collections::{BTreeMap, HashMap},
    sync::Arc,
};
use tokio::sync::Mutex;

use mithril_common::{
    crypto_helper::KESPeriod,
    entities::{
        Epoch, HexEncodedOpCert, HexEncodedVerificationKey, HexEncodedVerificationKeySignature,
        PartyId, SignerWithStake, Stake,
    },
    sqlite::{
        EntityCursor, HydrationError, Projection, Provider, SourceAlias, SqLiteEntity,
        WhereCondition,
    },
    store::adapter::{AdapterError, StoreAdapter},
    StdError,
};

/// SignerRegistration record is the representation of a stored signer_registration.
#[derive(Debug, PartialEq, Clone)]
pub struct SignerRegistrationRecord {
    /// Signer id.
    signer_id: String,

    /// Epoch of creation of the signer_registration.
    epoch_setting_id: Epoch,

    /// Verification key of the signer
    verification_key: HexEncodedVerificationKey,

    /// Signature of the verification key of the signer
    verification_key_signature: Option<HexEncodedVerificationKeySignature>,

    /// Operational certificate of the stake pool operator associated to the signer
    operational_certificate: Option<HexEncodedOpCert>,

    /// The kes period used to compute the verification key signature
    kes_period: Option<KESPeriod>,

    /// The stake associated to the signer
    stake: Option<Stake>,

    /// Date and time when the signer_registration was created
    created_at: DateTime<Utc>,
}

impl SignerRegistrationRecord {
    fn from_signer_with_stake(other: SignerWithStake, epoch: Epoch) -> Self {
        SignerRegistrationRecord {
            signer_id: other.party_id,
            epoch_setting_id: epoch,
            verification_key: other.verification_key,
            verification_key_signature: other.verification_key_signature,
            operational_certificate: other.operational_certificate,
            kes_period: other.kes_period,
            stake: Some(other.stake),
            created_at: Utc::now(),
        }
    }
}

impl From<SignerRegistrationRecord> for SignerWithStake {
    fn from(other: SignerRegistrationRecord) -> SignerWithStake {
        SignerWithStake {
            party_id: other.signer_id,
            verification_key: other.verification_key,
            verification_key_signature: other.verification_key_signature,
            operational_certificate: other.operational_certificate,
            kes_period: other.kes_period,
            stake: other.stake.unwrap_or_default(),
        }
    }
}

impl SqLiteEntity for SignerRegistrationRecord {
    fn hydrate(row: sqlite::Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let signer_id = row.read::<&str, _>(0).to_string();
        let epoch_setting_id_int = row.read::<i64, _>(1);
        let verification_key = row.read::<&str, _>(2).to_string();
        let verification_key_signature = row.read::<Option<&str>, _>(3).map(|s| s.to_owned());
        let operational_certificate = row.read::<Option<&str>, _>(4).map(|s| s.to_owned());
        let kes_period_int = row.read::<Option<i64>, _>(5);
        let stake_int = row.read::<Option<i64>, _>(6);
        let created_at = row.read::<&str, _>(7);

        let signer_registration_record = Self {
            signer_id,
            epoch_setting_id: Epoch(epoch_setting_id_int.try_into().map_err(|e| {
                HydrationError::InvalidData(format!(
                    "Could not cast i64 ({epoch_setting_id_int}) to u64. Error: '{e}'"
                ))
            })?),
            verification_key,
            verification_key_signature,
            operational_certificate,
            kes_period: match kes_period_int {
                Some(kes_period_int) => Some(kes_period_int.try_into().map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Could not cast i64 ({kes_period_int}) to u64. Error: '{e}'"
                    ))
                })?),
                None => None,
            },
            stake: match stake_int {
                Some(stake_int) => Some(stake_int.try_into().map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Could not cast i64 ({stake_int}) to u64. Error: '{e}'"
                    ))
                })?),
                None => None,
            },
            created_at: DateTime::parse_from_rfc3339(created_at)
                .map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{created_at}' to rfc3339 Datetime. Error: {e}"
                    ))
                })?
                .with_timezone(&Utc),
        };

        Ok(signer_registration_record)
    }

    fn get_projection() -> Projection {
        let mut projection = Projection::default();
        projection.add_field("signer_id", "{:signer_registration:}.signer_id", "text");
        projection.add_field(
            "epoch_setting_id",
            "{:signer_registration:}.epoch_setting_id",
            "integer",
        );
        projection.add_field(
            "verification_key",
            "{:signer_registration:}.verification_key",
            "text",
        );
        projection.add_field(
            "verification_key_signature",
            "{:signer_registration:}.verification_key_signature",
            "text",
        );
        projection.add_field(
            "operational_certificate",
            "{:signer_registration:}.operational_certificate",
            "text",
        );
        projection.add_field(
            "kes_period",
            "{:signer_registration:}.kes_period",
            "integer",
        );
        projection.add_field("stake", "{:signer_registration:}.stake", "integer");
        projection.add_field("created_at", "{:signer_registration:}.created_at", "text");

        projection
    }
}

/// Simple [SignerRegistrationRecord] provider.
pub struct SignerRegistrationRecordProvider<'client> {
    client: &'client Connection,
}

impl<'client> SignerRegistrationRecordProvider<'client> {
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

    fn condition_by_epoch(&self, epoch: &Epoch) -> Result<WhereCondition, StdError> {
        let epoch: i64 = i64::try_from(epoch.0)?;

        Ok(WhereCondition::new(
            "epoch_setting_id = ?*",
            vec![Value::Integer(epoch)],
        ))
    }

    /// Get SignerRegistrationRecords for given signer id and epoch.
    pub fn get_by_signer_id_and_epoch(
        &self,
        signer_id: String,
        epoch: &Epoch,
    ) -> Result<EntityCursor<SignerRegistrationRecord>, StdError> {
        let filters = self
            .condition_by_signer_id(signer_id)?
            .and_where(self.condition_by_epoch(epoch)?);
        let signer_registration_record = self.find(filters)?;

        Ok(signer_registration_record)
    }

    /// Get SignerRegistrationRecords for a given Epoch.
    pub fn get_by_epoch(
        &self,
        epoch: &Epoch,
    ) -> Result<EntityCursor<SignerRegistrationRecord>, StdError> {
        let filters = self.condition_by_epoch(epoch)?;
        let signer_registration_record = self.find(filters)?;

        Ok(signer_registration_record)
    }

    /// Get all SignerRegistrationRecords.
    pub fn get_all(&self) -> Result<EntityCursor<SignerRegistrationRecord>, StdError> {
        let filters = WhereCondition::default();
        let signer_registration_record = self.find(filters)?;

        Ok(signer_registration_record)
    }
}

impl<'client> Provider<'client> for SignerRegistrationRecordProvider<'client> {
    type Entity = SignerRegistrationRecord;

    fn get_connection(&'client self) -> &'client Connection {
        self.client
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:signer_registration:}", "sr")]);
        let projection = Self::Entity::get_projection().expand(aliases);
        format!("select {projection} from signer_registration as sr where {condition} order by ROWID desc")
    }
}

/// Query to insert or replace a signer_registration record
pub struct InsertOrReplaceSignerRegistrationRecordProvider<'conn> {
    connection: &'conn Connection,
}

impl<'conn> InsertOrReplaceSignerRegistrationRecordProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn Connection) -> Self {
        Self { connection }
    }

    fn get_insert_or_replace_condition(
        &self,
        signer_registration_record: SignerRegistrationRecord,
    ) -> WhereCondition {
        WhereCondition::new(
            "(signer_id, epoch_setting_id, verification_key, verification_key_signature, operational_certificate, kes_period, stake, created_at) values (?*, ?*, ?*, ?*, ?*, ?*, ?*, ?*)",
            vec![
                Value::String(signer_registration_record.signer_id),
                Value::Integer(
                    i64::try_from(signer_registration_record.epoch_setting_id.0).unwrap(),
                ),
                Value::String(signer_registration_record.verification_key),
                signer_registration_record
                            .verification_key_signature
                            .map(Value::String)
                            .unwrap_or(Value::Null),
                signer_registration_record
                            .operational_certificate
                            .map(Value::String)
                            .unwrap_or(Value::Null),
                signer_registration_record
                            .kes_period
                            .map(|k| Value::Integer(i64::try_from(k).unwrap()))
                            .unwrap_or(Value::Null),
                signer_registration_record
                            .stake
                            .map(|s| Value::Integer(i64::try_from(s).unwrap()))
                            .unwrap_or(Value::Null),
                            Value::String(signer_registration_record.created_at.to_rfc3339()),
            ],
        )
    }

    fn persist(
        &self,
        signer_registration_record: SignerRegistrationRecord,
    ) -> Result<SignerRegistrationRecord, StdError> {
        let filters = self.get_insert_or_replace_condition(signer_registration_record.clone());

        let entity = self.find(filters)?.next().unwrap_or_else(|| {
            panic!(
                "No entity returned by the persister, signer_registration_record = {signer_registration_record:?}"
            )
        });

        Ok(entity)
    }
}

impl<'conn> Provider<'conn> for InsertOrReplaceSignerRegistrationRecordProvider<'conn> {
    type Entity = SignerRegistrationRecord;

    fn get_connection(&'conn self) -> &'conn Connection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection().expand(SourceAlias::new(&[(
            "{:signer_registration:}",
            "signer_registration",
        )]));

        format!("insert or replace into signer_registration {condition} returning {projection}")
    }
}

/// Provider to remove old data from the signer_registration table
pub struct DeleteSignerRegistrationRecordProvider<'conn> {
    connection: &'conn Connection,
}

impl<'conn> Provider<'conn> for DeleteSignerRegistrationRecordProvider<'conn> {
    type Entity = SignerRegistrationRecord;

    fn get_connection(&'conn self) -> &'conn Connection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection().expand(SourceAlias::new(&[(
            "{:signer_registration:}",
            "signer_registration",
        )]));

        format!("delete from signer_registration where {condition} returning {projection}")
    }
}

impl<'conn> DeleteSignerRegistrationRecordProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn Connection) -> Self {
        Self { connection }
    }

    /// Create the SQL condition to delete a record given the Epoch.
    fn get_delete_condition_by_epoch(&self, epoch: Epoch) -> WhereCondition {
        let epoch_threshold = Value::Integer(i64::try_from(epoch.0).unwrap());

        WhereCondition::new("epoch_setting_id = ?*", vec![epoch_threshold])
    }

    /// Delete the epoch setting data given the Epoch
    pub fn delete(&self, epoch: Epoch) -> Result<EntityCursor<SignerRegistrationRecord>, StdError> {
        let filters = self.get_delete_condition_by_epoch(epoch);

        self.find(filters)
    }

    /// Create the SQL condition to prune data older than the given Epoch.
    fn get_prune_condition(&self, epoch_threshold: Epoch) -> WhereCondition {
        let epoch_threshold = Value::Integer(i64::try_from(epoch_threshold.0).unwrap());

        WhereCondition::new("epoch_setting_id < ?*", vec![epoch_threshold])
    }

    /// Prune the epoch setting data older than the given epoch.
    pub fn prune(
        &self,
        epoch_threshold: Epoch,
    ) -> Result<EntityCursor<SignerRegistrationRecord>, StdError> {
        let filters = self.get_prune_condition(epoch_threshold);

        self.find(filters)
    }
}

/// Service to deal with signer_registration (read & write).
pub struct SignerRegistrationStoreAdapter {
    connection: Arc<Mutex<Connection>>,
}

impl SignerRegistrationStoreAdapter {
    /// Create a new SignerRegistrationStoreAdapter service
    pub fn new(connection: Arc<Mutex<Connection>>) -> Self {
        Self { connection }
    }
}

#[async_trait]
impl StoreAdapter for SignerRegistrationStoreAdapter {
    type Key = Epoch;
    type Record = HashMap<PartyId, SignerWithStake>;

    async fn store_record(
        &mut self,
        key: &Self::Key,
        record: &Self::Record,
    ) -> Result<(), AdapterError> {
        let connection = &*self.connection.lock().await;
        let provider = InsertOrReplaceSignerRegistrationRecordProvider::new(connection);
        connection
            .execute("begin transaction")
            .map_err(|e| AdapterError::QueryError(e.into()))?;

        for signer_with_stake in record.values() {
            let _signer_registration_record = provider
                .persist(SignerRegistrationRecord::from_signer_with_stake(
                    signer_with_stake.to_owned(),
                    *key,
                ))
                .map_err(|e| AdapterError::GeneralError(format!("{e}")))?;
        }

        connection
            .execute("commit transaction")
            .map_err(|e| AdapterError::QueryError(e.into()))?;

        Ok(())
    }

    async fn get_record(&self, key: &Self::Key) -> Result<Option<Self::Record>, AdapterError> {
        let connection = &*self.connection.lock().await;
        let provider = SignerRegistrationRecordProvider::new(connection);
        let cursor = provider
            .get_by_epoch(key)
            .map_err(|e| AdapterError::GeneralError(format!("{e}")))?;
        let mut signer_with_stakes = HashMap::new();
        for signer_registration_record in cursor {
            signer_with_stakes.insert(
                signer_registration_record.signer_id.to_string(),
                signer_registration_record.into(),
            );
        }
        if signer_with_stakes.is_empty() {
            Ok(None)
        } else {
            Ok(Some(signer_with_stakes))
        }
    }

    async fn record_exists(&self, key: &Self::Key) -> Result<bool, AdapterError> {
        Ok(self.get_record(key).await?.is_some())
    }

    async fn get_last_n_records(
        &self,
        how_many: usize,
    ) -> Result<Vec<(Self::Key, Self::Record)>, AdapterError> {
        let connection = &*self.connection.lock().await;
        let provider = SignerRegistrationRecordProvider::new(connection);
        let cursor = provider
            .get_all()
            .map_err(|e| AdapterError::GeneralError(format!("{e}")))?
            .collect::<Vec<_>>()
            .into_iter()
            .rev();
        let signer_with_stake_by_epoch: BTreeMap<Self::Key, Self::Record> = cursor.fold(
            BTreeMap::<Self::Key, Self::Record>::new(),
            |mut acc, signer_registration_record| {
                let epoch = signer_registration_record.epoch_setting_id;
                let mut signer_with_stakes: Self::Record =
                    if let Some(signer_with_stakes) = acc.get_mut(&epoch) {
                        signer_with_stakes.to_owned()
                    } else {
                        HashMap::new()
                    };
                signer_with_stakes.insert(
                    signer_registration_record.signer_id.to_string(),
                    signer_registration_record.into(),
                );
                acc.insert(epoch, signer_with_stakes);
                acc
            },
        );
        Ok(signer_with_stake_by_epoch
            .into_iter()
            .rev()
            .take(how_many)
            .collect())
    }

    async fn remove(&mut self, key: &Self::Key) -> Result<Option<Self::Record>, AdapterError> {
        let connection = &*self.connection.lock().await;
        let provider = DeleteSignerRegistrationRecordProvider::new(connection);
        let cursor = provider
            .delete(*key)
            .map_err(|e| AdapterError::GeneralError(format!("{e}")))?;
        let mut signer_with_stakes = HashMap::new();
        for signer_registration_record in cursor {
            signer_with_stakes.insert(
                signer_registration_record.signer_id.to_string(),
                signer_registration_record.into(),
            );
        }

        if signer_with_stakes.is_empty() {
            Ok(None)
        } else {
            Ok(Some(signer_with_stakes))
        }
    }

    async fn get_iter(&self) -> Result<Box<dyn Iterator<Item = Self::Record> + '_>, AdapterError> {
        let records = self.get_last_n_records(usize::MAX).await?;
        Ok(Box::new(records.into_iter().map(|(_k, v)| v)))
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{BTreeMap, HashMap};

    use mithril_common::test_utils::MithrilFixtureBuilder;

    use crate::database::provider::{apply_all_migrations_to_db, disable_foreign_key_support};

    use super::*;

    pub fn setup_signer_registration_db(
        connection: &Connection,
        signer_with_stakes_by_epoch: Vec<(Epoch, Vec<SignerWithStake>)>,
    ) -> Result<(), StdError> {
        apply_all_migrations_to_db(connection)?;
        disable_foreign_key_support(connection)?;

        if signer_with_stakes_by_epoch.is_empty() {
            return Ok(());
        }

        let query = {
            // leverage the expanded parameter from this provider which is unit
            // tested on its own above.
            let insert_or_replace_provider =
                InsertOrReplaceSignerRegistrationRecordProvider::new(connection);
            let (sql_values, _) = insert_or_replace_provider
                .get_insert_or_replace_condition(SignerRegistrationRecord::from_signer_with_stake(
                    signer_with_stakes_by_epoch
                        .first()
                        .unwrap()
                        .1
                        .first()
                        .unwrap()
                        .to_owned(),
                    Epoch(1),
                ))
                .expand();
            format!("insert into signer_registration {sql_values}")
        };

        for (epoch, signer_with_stakes) in signer_with_stakes_by_epoch {
            for signer_with_stake in signer_with_stakes {
                let signer_registration_record =
                    SignerRegistrationRecord::from_signer_with_stake(signer_with_stake, epoch);
                let mut statement = connection.prepare(&query)?;
                statement
                    .bind::<&[(_, Value)]>(&[
                        (1, signer_registration_record.signer_id.into()),
                        (
                            2,
                            i64::try_from(signer_registration_record.epoch_setting_id.0)
                                .unwrap()
                                .into(),
                        ),
                        (3, signer_registration_record.verification_key.into()),
                        (
                            4,
                            signer_registration_record
                                .verification_key_signature
                                .map(Value::String)
                                .unwrap_or(Value::Null),
                        ),
                        (
                            5,
                            signer_registration_record
                                .operational_certificate
                                .map(Value::String)
                                .unwrap_or(Value::Null),
                        ),
                        (
                            6,
                            signer_registration_record
                                .kes_period
                                .map(|k| Value::Integer(k as i64))
                                .unwrap_or(Value::Null),
                        ),
                        (
                            7,
                            signer_registration_record
                                .stake
                                .map(|s| Value::Integer(s as i64))
                                .unwrap_or(Value::Null),
                        ),
                        (8, signer_registration_record.created_at.to_rfc3339().into()),
                    ])
                    .unwrap();

                statement.next().unwrap();
            }
        }

        Ok(())
    }

    #[test]
    fn test_convert_signer_registrations() {
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let signer_with_stakes = fixture.signers_with_stake();

        let mut signer_registration_records: Vec<SignerRegistrationRecord> = Vec::new();
        for signer_with_stake in signer_with_stakes.clone() {
            signer_registration_records.push(SignerRegistrationRecord::from_signer_with_stake(
                signer_with_stake,
                Epoch(1),
            ));
        }
        let mut signer_with_stakes_new: Vec<SignerWithStake> = Vec::new();
        for signer_registration_record in signer_registration_records {
            signer_with_stakes_new.push(signer_registration_record.into());
        }
        assert_eq!(signer_with_stakes, signer_with_stakes_new);
    }

    #[test]
    fn projection() {
        let projection = SignerRegistrationRecord::get_projection();
        let aliases = SourceAlias::new(&[("{:signer_registration:}", "sr")]);

        assert_eq!(
            "sr.signer_id as signer_id, sr.epoch_setting_id as epoch_setting_id, sr.verification_key as verification_key, sr.verification_key_signature as verification_key_signature, sr.operational_certificate as operational_certificate, sr.kes_period as kes_period, sr.stake as stake, sr.created_at as created_at"
                .to_string(),
            projection.expand(aliases)
        );
    }

    #[test]
    fn get_signer_registration_record_by_epoch() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = SignerRegistrationRecordProvider::new(&connection);
        let condition = provider.condition_by_epoch(&Epoch(17)).unwrap();
        let (filter, values) = condition.expand();

        assert_eq!("epoch_setting_id = ?1".to_string(), filter);
        assert_eq!(vec![Value::Integer(17)], values);
    }

    #[test]
    fn get_signer_registration_record_by_signer_id() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = SignerRegistrationRecordProvider::new(&connection);
        let condition = provider
            .condition_by_signer_id("signer-123".to_string())
            .unwrap();
        let (filter, values) = condition.expand();

        assert_eq!("signer_id = ?1".to_string(), filter);
        assert_eq!(vec![Value::String("signer-123".to_string())], values);
    }

    #[test]
    fn update_signer_registration_record() {
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let signer_with_stakes = fixture.signers_with_stake();
        let signer_registration_record = SignerRegistrationRecord::from_signer_with_stake(
            signer_with_stakes.first().unwrap().to_owned(),
            Epoch(1),
        );
        let connection = Connection::open(":memory:").unwrap();
        let provider = InsertOrReplaceSignerRegistrationRecordProvider::new(&connection);
        let condition =
            provider.get_insert_or_replace_condition(signer_registration_record.clone());
        let (values, params) = condition.expand();

        assert_eq!(
            "(signer_id, epoch_setting_id, verification_key, verification_key_signature, operational_certificate, kes_period, stake, created_at) values (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)".to_string(),
            values
        );
        assert_eq!(
            vec![
                Value::String(signer_registration_record.signer_id),
                Value::Integer(
                    i64::try_from(signer_registration_record.epoch_setting_id.0).unwrap(),
                ),
                Value::String(signer_registration_record.verification_key),
                signer_registration_record
                    .verification_key_signature
                    .map(Value::String)
                    .unwrap(),
                signer_registration_record
                    .operational_certificate
                    .map(Value::String)
                    .unwrap(),
                Value::Integer(signer_registration_record.kes_period.unwrap() as i64),
                Value::Integer(signer_registration_record.stake.unwrap() as i64),
                Value::String(signer_registration_record.created_at.to_rfc3339()),
            ],
            params
        );
    }

    #[test]
    fn delete() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = DeleteSignerRegistrationRecordProvider::new(&connection);
        let condition = provider.get_delete_condition_by_epoch(Epoch(5));
        let (condition, params) = condition.expand();

        assert_eq!("epoch_setting_id = ?1".to_string(), condition);
        assert_eq!(vec![Value::Integer(5)], params);
    }

    #[test]
    fn prune() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = DeleteSignerRegistrationRecordProvider::new(&connection);
        let condition = provider.get_prune_condition(Epoch(5));
        let (condition, params) = condition.expand();

        assert_eq!("epoch_setting_id < ?1".to_string(), condition);
        assert_eq!(vec![Value::Integer(5)], params);
    }

    #[test]
    fn test_get_signer_registration_records() {
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let signer_with_stakes = fixture.signers_with_stake();
        let signer_with_stakes_by_epoch: Vec<(Epoch, Vec<SignerWithStake>)> = (0..=3)
            .map(|e| (Epoch(e), signer_with_stakes.clone()))
            .collect();

        let connection = Connection::open(":memory:").unwrap();
        setup_signer_registration_db(&connection, signer_with_stakes_by_epoch.clone()).unwrap();

        let provider = SignerRegistrationRecordProvider::new(&connection);

        fn reset_created_at(
            signer_registration_records: Vec<SignerRegistrationRecord>,
        ) -> Vec<SignerRegistrationRecord> {
            signer_registration_records
                .into_iter()
                .map(|mut sr| {
                    sr.created_at = DateTime::<Utc>::default();
                    sr
                })
                .collect::<Vec<_>>()
        }

        let signer_registration_records: Vec<SignerRegistrationRecord> =
            provider.get_by_epoch(&Epoch(1)).unwrap().collect();
        let expected_signer_registration_records: Vec<SignerRegistrationRecord> =
            signer_with_stakes_by_epoch[1]
                .1
                .iter()
                .map(|sr| SignerRegistrationRecord::from_signer_with_stake(sr.clone(), Epoch(1)))
                .rev()
                .collect();
        assert_eq!(
            reset_created_at(expected_signer_registration_records),
            reset_created_at(signer_registration_records)
        );

        let signer_registration_records: Vec<SignerRegistrationRecord> =
            provider.get_by_epoch(&Epoch(3)).unwrap().collect();
        let expected_signer_registration_records: Vec<SignerRegistrationRecord> =
            signer_with_stakes_by_epoch[3]
                .1
                .iter()
                .map(|sr| SignerRegistrationRecord::from_signer_with_stake(sr.clone(), Epoch(3)))
                .rev()
                .collect();
        assert_eq!(
            reset_created_at(expected_signer_registration_records),
            reset_created_at(signer_registration_records)
        );

        let cursor = provider.get_by_epoch(&Epoch(5)).unwrap();
        assert_eq!(0, cursor.count());

        let signer_registration_records: Vec<SignerRegistrationRecord> =
            provider.get_all().unwrap().collect();
        let expected_signer_registration_records: Vec<SignerRegistrationRecord> =
            signer_with_stakes_by_epoch
                .iter()
                .fold(
                    Vec::<SignerRegistrationRecord>::new(),
                    |mut acc, (epoch, signer_with_stakes)| {
                        acc.extend(
                            signer_with_stakes
                                .iter()
                                .map(|sr| {
                                    SignerRegistrationRecord::from_signer_with_stake(
                                        sr.clone(),
                                        *epoch,
                                    )
                                })
                                .collect::<Vec<SignerRegistrationRecord>>(),
                        );
                        acc
                    },
                )
                .into_iter()
                .rev()
                .collect();
        assert_eq!(
            reset_created_at(expected_signer_registration_records),
            reset_created_at(signer_registration_records)
        );
    }

    #[test]
    fn test_update_signer_registration_record() {
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let signer_with_stakes = fixture.signers_with_stake();
        let signer_with_stakes_copy = signer_with_stakes
            .iter()
            .map(|s| {
                let mut s_new = s.clone();
                s_new.stake += 10;
                s_new
            })
            .collect::<Vec<SignerWithStake>>();

        let connection = Connection::open(":memory:").unwrap();
        setup_signer_registration_db(&connection, Vec::new()).unwrap();

        let provider = InsertOrReplaceSignerRegistrationRecordProvider::new(&connection);

        for signer_with_stake in signer_with_stakes {
            let signer_registration_record =
                SignerRegistrationRecord::from_signer_with_stake(signer_with_stake, Epoch(1));
            let signer_registration_record_saved = provider
                .persist(signer_registration_record.clone())
                .unwrap();
            assert_eq!(signer_registration_record, signer_registration_record_saved);
        }

        for signer_with_stake in signer_with_stakes_copy {
            let signer_registration_record =
                SignerRegistrationRecord::from_signer_with_stake(signer_with_stake, Epoch(1));
            let signer_registration_record_saved = provider
                .persist(signer_registration_record.clone())
                .unwrap();
            assert_eq!(signer_registration_record, signer_registration_record_saved);
        }
    }

    #[tokio::test]
    async fn test_store_adapter() {
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let signer_with_stakes = fixture.signers_with_stake();
        let signer_with_stakes_by_epoch: Vec<(Epoch, HashMap<PartyId, SignerWithStake>)> = (0..5)
            .map(|e| {
                (
                    Epoch(e),
                    signer_with_stakes
                        .clone()
                        .into_iter()
                        .map(|s| (s.party_id.to_owned(), s))
                        .collect(),
                )
            })
            .collect();

        let connection = Connection::open(":memory:").unwrap();
        setup_signer_registration_db(&connection, Vec::new()).unwrap();

        let mut signer_registration_store_adapter =
            SignerRegistrationStoreAdapter::new(Arc::new(Mutex::new(connection)));

        for (epoch, signer_with_stakes) in &signer_with_stakes_by_epoch {
            assert!(signer_registration_store_adapter
                .store_record(epoch, signer_with_stakes)
                .await
                .is_ok());
        }

        for (epoch, signer_with_stakes) in &signer_with_stakes_by_epoch {
            assert!(signer_registration_store_adapter
                .record_exists(epoch)
                .await
                .unwrap());
            assert_eq!(
                Some(signer_with_stakes.to_owned()),
                signer_registration_store_adapter
                    .get_record(epoch)
                    .await
                    .unwrap()
            );
        }
        assert_eq!(
            signer_with_stakes_by_epoch
                .clone()
                .into_iter()
                .map(|(k, v)| (k, BTreeMap::from_iter(v.into_iter())))
                .collect::<Vec<(Epoch, BTreeMap<PartyId, SignerWithStake>)>>(),
            signer_registration_store_adapter
                .get_last_n_records(signer_with_stakes_by_epoch.len())
                .await
                .unwrap()
                .into_iter()
                .rev()
                .map(|(k, v)| (k, BTreeMap::from_iter(v.into_iter())))
                .collect::<Vec<(Epoch, BTreeMap<PartyId, SignerWithStake>)>>()
        )
    }
}

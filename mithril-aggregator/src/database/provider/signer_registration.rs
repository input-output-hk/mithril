use anyhow::Context;
use async_trait::async_trait;
use chrono::{DateTime, Utc};
use sqlite::{Connection, Value};
use std::{collections::HashMap, sync::Arc};
use tokio::sync::Mutex;

use mithril_common::{
    crypto_helper::KESPeriod,
    entities::{
        Epoch, HexEncodedOpCert, HexEncodedVerificationKey, HexEncodedVerificationKeySignature,
        PartyId, Signer, SignerWithStake, Stake, StakeDistribution,
    },
    sqlite::{
        EntityCursor, HydrationError, Projection, Provider, SourceAlias, SqLiteEntity,
        WhereCondition,
    },
    store::{adapter::AdapterError, StoreError},
    StdResult,
};

use crate::VerificationKeyStorer;

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
            verification_key: other.verification_key.to_json_hex().unwrap(),
            verification_key_signature: other
                .verification_key_signature
                .map(|k| k.to_json_hex().unwrap()),
            operational_certificate: other
                .operational_certificate
                .map(|o| o.to_json_hex().unwrap()),
            kes_period: other.kes_period,
            stake: Some(other.stake),
            created_at: Utc::now(),
        }
    }
}

impl From<SignerRegistrationRecord> for Signer {
    fn from(other: SignerRegistrationRecord) -> Self {
        Self {
            party_id: other.signer_id,
            verification_key: other.verification_key.try_into().unwrap(),
            verification_key_signature: other
                .verification_key_signature
                .map(|k| (k.try_into().unwrap())),
            operational_certificate: other
                .operational_certificate
                .map(|o| (o.try_into().unwrap())),
            kes_period: other.kes_period,
        }
    }
}

impl From<SignerRegistrationRecord> for SignerWithStake {
    fn from(other: SignerRegistrationRecord) -> Self {
        Self {
            party_id: other.signer_id,
            verification_key: other.verification_key.try_into().unwrap(),
            verification_key_signature: other
                .verification_key_signature
                .map(|k| (k.try_into().unwrap())),
            operational_certificate: other
                .operational_certificate
                .map(|o| (o.try_into().unwrap())),
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

    fn condition_by_signer_id(&self, signer_id: String) -> StdResult<WhereCondition> {
        Ok(WhereCondition::new(
            "signer_id = ?*",
            vec![Value::String(signer_id)],
        ))
    }

    fn condition_by_epoch(&self, epoch: &Epoch) -> StdResult<WhereCondition> {
        let epoch: i64 = epoch.try_into()?;

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
    ) -> StdResult<EntityCursor<SignerRegistrationRecord>> {
        let filters = self
            .condition_by_signer_id(signer_id)?
            .and_where(self.condition_by_epoch(epoch)?);
        let signer_registration_record = self.find(filters)?;

        Ok(signer_registration_record)
    }

    /// Get SignerRegistrationRecords for a given Epoch.
    pub fn get_by_epoch(&self, epoch: &Epoch) -> StdResult<EntityCursor<SignerRegistrationRecord>> {
        let filters = self.condition_by_epoch(epoch)?;
        let signer_registration_record = self.find(filters)?;

        Ok(signer_registration_record)
    }

    /// Get all SignerRegistrationRecords.
    pub fn get_all(&self) -> StdResult<EntityCursor<SignerRegistrationRecord>> {
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
                    signer_registration_record.epoch_setting_id.try_into().unwrap(),
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
    ) -> StdResult<SignerRegistrationRecord> {
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
        let epoch_threshold = Value::Integer(epoch.try_into().unwrap());

        WhereCondition::new("epoch_setting_id = ?*", vec![epoch_threshold])
    }

    /// Delete the epoch setting data given the Epoch
    pub fn delete(&self, epoch: Epoch) -> StdResult<EntityCursor<SignerRegistrationRecord>> {
        let filters = self.get_delete_condition_by_epoch(epoch);

        self.find(filters)
    }

    /// Create the SQL condition to prune data older than the given Epoch.
    fn get_prune_condition(&self, epoch_threshold: Epoch) -> WhereCondition {
        let epoch_threshold = Value::Integer(epoch_threshold.try_into().unwrap());

        WhereCondition::new("epoch_setting_id < ?*", vec![epoch_threshold])
    }

    /// Prune the epoch setting data older than the given epoch.
    pub fn prune(
        &self,
        epoch_threshold: Epoch,
    ) -> StdResult<EntityCursor<SignerRegistrationRecord>> {
        let filters = self.get_prune_condition(epoch_threshold);

        self.find(filters)
    }
}

/// Service to deal with signer_registration (read & write).
pub struct SignerRegistrationStore {
    connection: Arc<Mutex<Connection>>,
}

impl SignerRegistrationStore {
    /// Create a new [SignerRegistrationStore] service
    pub fn new(connection: Arc<Mutex<Connection>>) -> Self {
        Self { connection }
    }
}

#[async_trait]
impl VerificationKeyStorer for SignerRegistrationStore {
    async fn save_verification_key(
        &self,
        epoch: Epoch,
        signer: SignerWithStake,
    ) -> Result<Option<SignerWithStake>, StoreError> {
        let connection = &*self.connection.lock().await;
        let provider = InsertOrReplaceSignerRegistrationRecordProvider::new(connection);
        let existing_record = SignerRegistrationRecordProvider::new(connection)
            .get_by_signer_id_and_epoch(signer.party_id.clone(), &epoch)
            .map_err(AdapterError::QueryError)?
            .next();

        let _updated_record = provider
            .persist(SignerRegistrationRecord::from_signer_with_stake(
                signer, epoch,
            ))
            .with_context(|| format!("persist verification key failure, epoch: {epoch}"))
            .map_err(AdapterError::GeneralError)?;

        match existing_record {
            None => Ok(None),
            Some(previous_record) => Ok(Some(previous_record.into())),
        }
    }

    async fn get_verification_keys(
        &self,
        epoch: Epoch,
    ) -> Result<Option<HashMap<PartyId, Signer>>, StoreError> {
        let connection = &*self.connection.lock().await;
        let provider = SignerRegistrationRecordProvider::new(connection);
        let cursor = provider
            .get_by_epoch(&epoch)
            .with_context(|| format!("get verification key failure, epoch: {epoch}"))
            .map_err(AdapterError::GeneralError)?;

        let signer_with_stakes: HashMap<PartyId, Signer> =
            HashMap::from_iter(cursor.map(|record| (record.signer_id.to_owned(), record.into())));

        match signer_with_stakes.is_empty() {
            true => Ok(None),
            false => Ok(Some(signer_with_stakes)),
        }
    }

    async fn prune_verification_keys(&self, max_epoch_to_prune: Epoch) -> Result<(), StoreError> {
        let connection = &*self.connection.lock().await;
        let _deleted_records = DeleteSignerRegistrationRecordProvider::new(connection)
            // we want to prune including the given epoch (+1)
            .prune(max_epoch_to_prune + 1)
            .map_err(AdapterError::QueryError)?
            .collect::<Vec<_>>();

        Ok(())
    }

    async fn get_stake_distribution_for_epoch(
        &self,
        epoch: Epoch,
    ) -> Result<Option<StakeDistribution>, StoreError> {
        let connection = &*self.connection.lock().await;
        let provider = SignerRegistrationRecordProvider::new(connection);
        let cursor = provider
            .get_by_epoch(&epoch)
            .with_context(|| format!("get stake distribution failure, epoch: {epoch}"))
            .map_err(AdapterError::GeneralError)?;

        let stake_distribution = StakeDistribution::from_iter(
            cursor.map(|r| (r.signer_id, r.stake.unwrap_or_default())),
        );

        match stake_distribution.is_empty() {
            true => Ok(None),
            false => Ok(Some(stake_distribution)),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use mithril_common::test_utils::MithrilFixtureBuilder;

    use crate::database::provider::{apply_all_migrations_to_db, disable_foreign_key_support};
    use crate::store::test_verification_key_storer;

    use super::*;

    pub fn setup_signer_registration_db(
        connection: &Connection,
        signer_with_stakes_by_epoch: Vec<(Epoch, Vec<SignerWithStake>)>,
    ) -> StdResult<()> {
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
                            Value::Integer(*signer_registration_record.epoch_setting_id as i64),
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

    fn insert_golden_signer_registration(connection: &Connection) {
        connection
            .execute(
                r#"
                insert into signer_registration
                values(
                    'pool1t9uuagsat8hlr0n0ga4wzge0jxlyjuhl6mugrm8atc285vzkf2e',
                    292,
                    '7b22766b223a5b3132382c3134322c31352c37322c35342c37332c32392c3135372c39302c3134392c33342c3235352c35312c31382c34342c33322c36302c34362c3130302c31342c3136342c39362c3138362c31382c32372c3231312c3130322c3130362c35352c332c3137302c3234302c3131342c3134372c3134362c3234382c31352c32312c3232392c3133322c3234362c3230322c3136322c34312c3135312c3138362c3136332c3232302c31342c3231372c3235352c3234352c35352c3231362c3235322c342c3137302c31362c3137382c3230392c3134392c32302c3230352c39322c3232312c38302c32392c3139302c3131372c3138382c3132382c3234372c3133312c37382c3138372c3232332c3231382c3131362c3235352c34332c3130392c3132362c3233302c3130382c33372c3131342c332c3138362c3136352c33322c3133312c3139332c3139302c34342c3134362c3234315d2c22706f70223a5b3134322c3133352c38352c342c3134362c32342c37382c34332c36332c3233382c3235312c37382c3138312c37342c37302c362c39342c3138372c3137382c3133332c3135352c3233342c3235352c3134352c3139372c3137302c3135352c3132392c3234332c3137332c31322c31392c36382c3132392c3131342c36392c3231312c33372c3233322c3139332c3130372c3233332c32392c3130332c3232382c34392c36392c38362c3137322c35352c39342c3132332c372c36322c3135382c33352c31372c3131332c38312c3136312c34342c3234392c35332c36362c39332c37302c3136392c3133372c3135372c3233342c3234372c3232332c37312c3135302c3231362c3130322c3139302c3137362c3135322c34362c3134332c3233302c31322c3138382c33312c3234362c3137312c3130352c3230392c3133382c35352c32382c3134312c36302c3132312c3132305d7d',
                    '7b227369676d61223a7b227369676d61223a7b227369676d61223a7b227369676d61223a7b227369676d61223a7b227369676d61223a5b35322c3230342c36342c3136342c3134382c36302c342c3133372c3132312c312c3130302c38352c3133382c3235302c33382c37352c39302c3133352c31392c35322c3135312c3130302c35392c372c33332c3131322c31332c3138342c3138342c32332c35352c37392c3134332c3231382c3231352c3136362c3137392c36312c3234312c3136342c38342c36302c3234312c3234302c3134362c33332c37342c3136372c37342c3230332c37382c34372c3231362c3132352c37382c31302c38312c3233322c3134332c37302c3234362c382c3130352c345d2c226c68735f706b223a5b3130342c3131332c39302c32332c3231392c3235342c382c3137352c3136372c3133312c3136382c3131322c3130392c3137362c31342c35372c38362c3139372c34392c35312c3136382c3131352c3138372c3137382c392c3233322c38362c3139352c3130362c3134322c3232372c3139365d2c227268735f706b223a5b38372c3136392c39392c3133382c3130352c3133312c3133322c3132362c3139382c3138352c3137302c3132352c39372c3137372c38342c3231302c3137362c3134322c3133382c32312c38362c3133312c3135382c3132332c36332c3131322c39392c3133322c3134352c3231322c382c3233315d7d2c226c68735f706b223a5b34302c38382c39392c372c31362c32302c37352c3132302c3234372c3233302c32372c3233392c35332c3235352c3137302c3132302c3131392c33372c3138362c3130322c35302c33362c39382c3139332c3130332c33372c352c3131362c3134322c3134382c3233322c32355d2c227268735f706b223a5b3130372c3135372c3234342c3230342c3133362c3139332c38382c3130322c3234312c3135392c39392c3233342c38342c3139322c3133302c34372c32302c3136362c322c3230302c37392c3133352c3230352c312c3235332c3233352c32382c3134372c3135352c3132322c33392c3234345d7d2c226c68735f706b223a5b3133332c3131362c3131342c36342c3132322c31332c3138362c3130342c39322c3233372c39372c38322c3232312c38322c35342c3132352c34352c3234342c3139322c39332c35362c3132362c34302c3134362c3131312c3132392c3232312c3234382c362c35342c3233372c3138355d2c227268735f706b223a5b3137312c3137302c33382c3232302c3133302c34372c3133362c3233362c37322c35332c36332c33382c32392c34362c3230352c32392c3234382c3235342c37362c32372c322c3132382c3130372c3131392c3132382c3137302c32322c3131322c3137362c3130322c3136332c37335d7d2c226c68735f706b223a5b3231312c3138392c38342c3139372c302c3231382c3134382c312c34332c36332c38342c3234322c3231392c39342c31302c3134302c3134372c3137322c38342c35392c31352c3131342c3230392c3235302c3230372c31342c3134322c33362c3135372c3230332c37382c3137355d2c227268735f706b223a5b3137302c3132392c3132332c342c3131332c3135322c3232392c3133372c392c32342c3133372c3136362c32352c3136352c34332c3132322c3132332c3230312c3234322c3231302c3137382c3234382c31342c3233302c33392c3231322c31382c33362c34382c38372c39302c3230365d7d2c226c68735f706b223a5b33332c3234322c3130372c39312c3130372c3130322c3136332c36342c33332c3231372c3233342c3138302c33382c312c3138352c3135382c3230372c3234352c3136372c3130352c3134322c33382c3233342c37362c34322c32322c372c3130342c39362c3139382c3234322c35335d2c227268735f706b223a5b39352c34302c392c3131372c3130382c3135362c3138342c3133392c39302c3138382c31352c32312c3131322c31382c3130302c3134362c3130342c352c3135362c39372c3134392c33322c34322c3234302c3134392c3138382c35322c33312c39312c39392c3131382c365d7d2c226c68735f706b223a5b3139332c3234322c37362c3230392c3134312c33372c3130312c36382c37302c392c3134312c33392c3230372c39342c3232362c33392c3136302c3131382c32332c3233302c3234342c3231302c31382c38322c3137332c3135382c3233312c3137392c3138322c31392c32322c3134365d2c227268735f706b223a5b3133322c3232392c3130382c3139392c37312c36392c3233362c36352c31382c3131372c39332c3234332c3234332c37342c36392c39382c3134302c3234392c342c33372c37372c38372c35382c31322c3132302c37332c3230332c39362c36312c3233302c39322c3132385d7d',
                    '5b5b5b3138362c39352c3232362c3137342c3132352c3235302c31302c3232322c3130322c3234302c36352c3235352c34372c3133382c38392c3131302c31342c3131302c32322c3138322c33322c3136362c3231312c392c32302c32302c35352c35382c3232392c3132302c3235302c37315d2c312c3136352c5b3130352c35342c3234352c35362c3231352c3130362c3133392c3231322c3137342c3232332c39302c3234392c3138372c34372c3134382c35302c34302c31352c3131372c3231372c3134392c3132362c3231382c3232352c3133362c36352c3231392c3136302c3134382c39332c3232382c3235312c31392c3231332c3136382c332c3233362c38392c3132302c3135392c3139382c38302c3234342c3138302c33332c3131392c3132382c3230312c3138362c3132302c32312c3130322c36322c3232392c32382c3135352c37362c31392c3235322c3232312c3234372c3137342c3135392c365d5d2c5b3234312c32372c31332c34342c3131342c37382c3138392c3234392c3135302c3135302c35332c3134342c3233362c3135312c38382c3134302c3132382c3136322c36302c3232382c38382c3131312c392c3134342c3233322c38332c39342c3231302c3135362c3136382c33352c3234325d5d',
                    29,
                    9497629046,
                    '2023-08-12T00:03:51.236860002+00:00'
                );
            "#,
            )
            .unwrap();
    }

    #[tokio::test]
    async fn test_golden_master() {
        let connection = Connection::open(":memory:").unwrap();
        apply_all_migrations_to_db(&connection).unwrap();
        disable_foreign_key_support(&connection).unwrap();
        insert_golden_signer_registration(&connection);

        let repository = SignerRegistrationStore::new(Arc::new(Mutex::new(connection)));
        repository
            .get_verification_keys(Epoch(292))
            .await
            .expect("Getting Golden signer registration should not fail")
            .expect("Signer registration should exist for this epoch");
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
                Value::Integer(*signer_registration_record.epoch_setting_id as i64),
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

    pub fn init_signer_registration_store(
        initial_data: Vec<(Epoch, HashMap<PartyId, SignerWithStake>)>,
    ) -> Arc<dyn VerificationKeyStorer> {
        let connection = Connection::open(":memory:").unwrap();
        let initial_data: Vec<(Epoch, Vec<SignerWithStake>)> = initial_data
            .into_iter()
            .map(|(e, signers)| (e, signers.into_values().collect::<Vec<_>>()))
            .collect();

        setup_signer_registration_db(&connection, initial_data).unwrap();

        Arc::new(SignerRegistrationStore::new(Arc::new(Mutex::new(
            connection,
        ))))
    }

    test_verification_key_storer!(
        test_signer_registration_store =>
        crate::database::provider::signer_registration::tests::init_signer_registration_store
    );
}

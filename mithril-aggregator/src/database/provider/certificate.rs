use async_trait::async_trait;
use chrono::{DateTime, Utc};
use sqlite::{Connection, Value};
use std::{iter::repeat, sync::Arc};
use tokio::sync::Mutex;

use mithril_common::{
    certificate_chain::{CertificateRetriever, CertificateRetrieverError},
    entities::{
        Beacon, Certificate, CertificateMetadata, CertificateSignature, Epoch,
        HexEncodedAgregateVerificationKey, HexEncodedKey, ProtocolMessage, ProtocolParameters,
        ProtocolVersion, SignerWithStake,
    },
    sqlite::{
        EntityCursor, HydrationError, Projection, Provider, SourceAlias, SqLiteEntity,
        WhereCondition,
    },
    StdResult,
};

#[cfg(test)]
use mithril_common::test_utils::fake_keys;

/// Certificate record is the representation of a stored certificate.
#[derive(Debug, PartialEq, Clone)]
pub struct CertificateRecord {
    /// Certificate id.
    pub certificate_id: String,

    /// Parent Certificate id.
    pub parent_certificate_id: Option<String>,

    /// Message that is signed.
    pub message: String,

    /// Signature of the certificate.
    /// Note: multi-signature if parent certificate id is set, genesis signature otherwise.
    pub signature: HexEncodedKey,

    /// Aggregate verification key
    /// Note: used only if signature is a multi-signature
    pub aggregate_verification_key: HexEncodedAgregateVerificationKey,

    /// Epoch of creation of the certificate.
    pub epoch: Epoch,

    /// Beacon used to produce the signed message
    pub beacon: Beacon,

    /// Protocol Version (semver)
    pub protocol_version: ProtocolVersion,

    /// Protocol parameters.
    pub protocol_parameters: ProtocolParameters,

    /// Structured message that is used to create the signed message
    pub protocol_message: ProtocolMessage,

    /// The list of the active signers with their stakes and verification keys
    pub signers: Vec<SignerWithStake>,

    /// Date and time when the certificate was initiated
    pub initiated_at: DateTime<Utc>,

    /// Date and time when the certificate was sealed
    pub sealed_at: DateTime<Utc>,
}

impl CertificateRecord {
    #[cfg(test)]
    pub fn dummy_genesis(id: &str, beacon: Beacon) -> Self {
        let mut record = Self::dummy(id, "", beacon);
        record.parent_certificate_id = None;
        record.signature = fake_keys::genesis_signature()[0].to_owned();
        record
    }

    #[cfg(test)]
    pub fn dummy(id: &str, parent_id: &str, beacon: Beacon) -> Self {
        Self {
            certificate_id: id.to_string(),
            parent_certificate_id: Some(parent_id.to_string()),
            message: "message".to_string(),
            signature: fake_keys::multi_signature()[0].to_owned(),
            aggregate_verification_key: "avk".to_string(),
            epoch: beacon.epoch,
            beacon,
            protocol_version: "protocol_version".to_string(),
            protocol_parameters: Default::default(),
            protocol_message: Default::default(),
            signers: vec![],
            initiated_at: DateTime::parse_from_rfc3339("2024-02-12T13:11:47Z")
                .unwrap()
                .with_timezone(&Utc),
            sealed_at: DateTime::parse_from_rfc3339("2024-02-12T13:12:57Z")
                .unwrap()
                .with_timezone(&Utc),
        }
    }
}

impl From<Certificate> for CertificateRecord {
    fn from(other: Certificate) -> Self {
        let (signature, parent_certificate_id) = match other.signature {
            CertificateSignature::GenesisSignature(signature) => (signature.to_bytes_hex(), None),
            CertificateSignature::MultiSignature(signature) => {
                (signature.to_json_hex().unwrap(), Some(other.previous_hash))
            }
        };

        CertificateRecord {
            certificate_id: other.hash,
            parent_certificate_id,
            message: other.signed_message,
            signature,
            aggregate_verification_key: other.aggregate_verification_key,
            epoch: other.beacon.epoch,
            beacon: other.beacon,
            protocol_version: other.metadata.protocol_version,
            protocol_parameters: other.metadata.protocol_parameters,
            protocol_message: other.protocol_message,
            signers: other.metadata.signers,
            initiated_at: other.metadata.initiated_at,
            sealed_at: other.metadata.sealed_at,
        }
    }
}

impl From<CertificateRecord> for Certificate {
    fn from(other: CertificateRecord) -> Self {
        let certificate_metadata = CertificateMetadata::new(
            other.protocol_version,
            other.protocol_parameters,
            other.initiated_at,
            other.sealed_at,
            other.signers,
        );
        let (previous_hash, signature) = match other.parent_certificate_id {
            None => (
                String::new(),
                CertificateSignature::GenesisSignature(other.signature.try_into().unwrap()),
            ),
            Some(parent_certificate_id) => (
                parent_certificate_id,
                CertificateSignature::MultiSignature(other.signature.try_into().unwrap()),
            ),
        };

        Certificate {
            hash: other.certificate_id,
            previous_hash,
            beacon: other.beacon,
            metadata: certificate_metadata,
            signed_message: other.protocol_message.compute_hash(),
            protocol_message: other.protocol_message,
            aggregate_verification_key: other.aggregate_verification_key,
            signature,
        }
    }
}

impl SqLiteEntity for CertificateRecord {
    fn hydrate(row: sqlite::Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let certificate_id = row.read::<&str, _>(0).to_string();
        let parent_certificate_id = row.read::<Option<&str>, _>(1).map(|s| s.to_owned());
        let message = row.read::<&str, _>(2).to_string();
        let signature = row.read::<&str, _>(3).to_string();
        let aggregate_verification_key = row.read::<&str, _>(4).to_string();
        let epoch_int = row.read::<i64, _>(5);
        let beacon_string = row.read::<&str, _>(6);
        let protocol_version = row.read::<&str, _>(7).to_string();
        let protocol_parameters_string = row.read::<&str, _>(8);
        let protocol_message_string = row.read::<&str, _>(9);
        let signers_string = row.read::<&str, _>(10);
        let initiated_at = row.read::<&str, _>(11);
        let sealed_at = row.read::<&str, _>(12);

        let certificate_record = Self {
            certificate_id,
            parent_certificate_id,
            message,
            signature,
            aggregate_verification_key,
            epoch: Epoch(epoch_int.try_into().map_err(|e| {
                HydrationError::InvalidData(format!(
                    "Could not cast i64 ({epoch_int}) to u64. Error: '{e}'"
                ))
            })?),
            beacon: serde_json::from_str(beacon_string).map_err(
                |e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{beacon_string}' to Beacon. Error: {e}"
                    ))
                },
            )?,
            protocol_version,
            protocol_parameters: serde_json::from_str(protocol_parameters_string).map_err(
                |e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{protocol_parameters_string}' to ProtocolParameters. Error: {e}"
                    ))
                },
            )?,
            protocol_message: serde_json::from_str(protocol_message_string).map_err(
                |e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{protocol_message_string}' to ProtocolMessage. Error: {e}"
                    ))
                },
            )?,
            signers: serde_json::from_str(signers_string).map_err(
                |e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{signers_string}' to Vec<SignerWithStake>. Error: {e}"
                    ))
                },
            )?,
            initiated_at: DateTime::parse_from_rfc3339(initiated_at).map_err(
                |e| {
                  HydrationError::InvalidData(format!(
                      "Could not turn string '{initiated_at}' to rfc3339 Datetime. Error: {e}"
                  ))
              },
            )?.with_timezone(&Utc),
            sealed_at: DateTime::parse_from_rfc3339(sealed_at).map_err(
                |e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{sealed_at}' to rfc3339 Datetime. Error: {e}"
                    ))
                },
            )?.with_timezone(&Utc),
        };

        Ok(certificate_record)
    }

    fn get_projection() -> Projection {
        let mut projection = Projection::default();
        projection.add_field("certificate_id", "{:certificate:}.certificate_id", "text");
        projection.add_field(
            "parent_certificate_id",
            "{:certificate:}.parent_certificate_id",
            "text",
        );
        projection.add_field("message", "{:certificate:}.message", "text");
        projection.add_field("signature", "{:certificate:}.signature", "text");
        projection.add_field(
            "aggregate_verification_key",
            "{:certificate:}.aggregate_verification_key",
            "text",
        );
        projection.add_field("epoch", "{:certificate:}.epoch", "integer");
        projection.add_field("beacon", "{:certificate:}.beacon", "text");
        projection.add_field(
            "protocol_version",
            "{:certificate:}.protocol_version",
            "text",
        );
        projection.add_field(
            "protocol_parameters",
            "{:certificate:}.protocol_parameters",
            "text",
        );
        projection.add_field(
            "protocol_message",
            "{:certificate:}.protocol_message",
            "text",
        );
        projection.add_field("signers", "{:certificate:}.signers", "text");
        projection.add_field("initiated_at", "{:certificate:}.initiated_at", "text");
        projection.add_field("sealed_at", "{:certificate:}.sealed_at", "text");

        projection
    }
}

/// Simple [CertificateRecord] provider.
pub struct CertificateRecordProvider<'client> {
    client: &'client Connection,
}

impl<'client> CertificateRecordProvider<'client> {
    /// Create a new provider
    pub fn new(client: &'client Connection) -> Self {
        Self { client }
    }

    fn condition_by_certificate_id(&self, certificate_id: &str) -> StdResult<WhereCondition> {
        Ok(WhereCondition::new(
            "certificate_id = ?*",
            vec![Value::String(certificate_id.to_owned())],
        ))
    }

    fn condition_by_epoch(&self, epoch: &Epoch) -> StdResult<WhereCondition> {
        let epoch: i64 = i64::try_from(epoch.0)?;

        Ok(WhereCondition::new(
            "epoch = ?*",
            vec![Value::Integer(epoch)],
        ))
    }

    /// Get CertificateRecords for a given certificate id.
    pub fn get_by_certificate_id(
        &self,
        certificate_id: &str,
    ) -> StdResult<EntityCursor<CertificateRecord>> {
        let filters = self.condition_by_certificate_id(certificate_id)?;
        let certificate_record = self.find(filters)?;

        Ok(certificate_record)
    }

    /// Get CertificateRecords for a given Epoch.
    pub fn get_by_epoch(&self, epoch: &Epoch) -> StdResult<EntityCursor<CertificateRecord>> {
        let filters = self.condition_by_epoch(epoch)?;
        let certificate_record = self.find(filters)?;

        Ok(certificate_record)
    }

    /// Get all CertificateRecords.
    pub fn get_all(&self) -> StdResult<EntityCursor<CertificateRecord>> {
        let filters = WhereCondition::default();
        let certificate_record = self.find(filters)?;

        Ok(certificate_record)
    }
}

impl<'client> Provider<'client> for CertificateRecordProvider<'client> {
    type Entity = CertificateRecord;

    fn get_connection(&'client self) -> &'client Connection {
        self.client
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:certificate:}", "c")]);
        let projection = Self::Entity::get_projection().expand(aliases);
        format!("select {projection} from certificate as c where {condition} order by ROWID desc")
    }
}

/// Query to insert the certificate record
pub struct InsertCertificateRecordProvider<'conn> {
    connection: &'conn Connection,
}

impl<'conn> InsertCertificateRecordProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn Connection) -> Self {
        Self { connection }
    }

    fn get_insert_condition(&self, certificate_record: &CertificateRecord) -> WhereCondition {
        self.get_insert_many_condition(&vec![certificate_record.clone()])
    }

    fn get_insert_many_condition(
        &self,
        certificates_records: &[CertificateRecord],
    ) -> WhereCondition {
        let columns = "(certificate_id, parent_certificate_id, message, signature, \
aggregate_verification_key, epoch, beacon, protocol_version, protocol_parameters, \
protocol_message, signers, initiated_at, sealed_at)";
        let values_columns: Vec<&str> =
            repeat("(?*, ?*, ?*, ?*, ?*, ?*, ?*, ?*, ?*, ?*, ?*, ?*, ?*)")
                .take(certificates_records.len())
                .collect();

        let values: Vec<Value> = certificates_records
            .iter()
            .flat_map(|certificate_record| {
                vec![
                    Value::String(certificate_record.certificate_id.to_owned()),
                    match certificate_record.parent_certificate_id.to_owned() {
                        Some(parent_certificate_id) => Value::String(parent_certificate_id),
                        None => Value::Null,
                    },
                    Value::String(certificate_record.message.to_owned()),
                    Value::String(certificate_record.signature.to_owned()),
                    Value::String(certificate_record.aggregate_verification_key.to_owned()),
                    Value::Integer(i64::try_from(certificate_record.epoch.0).unwrap()),
                    Value::String(serde_json::to_string(&certificate_record.beacon).unwrap()),
                    Value::String(certificate_record.protocol_version.to_owned()),
                    Value::String(
                        serde_json::to_string(&certificate_record.protocol_parameters).unwrap(),
                    ),
                    Value::String(
                        serde_json::to_string(&certificate_record.protocol_message).unwrap(),
                    ),
                    Value::String(serde_json::to_string(&certificate_record.signers).unwrap()),
                    Value::String(certificate_record.initiated_at.to_rfc3339()),
                    Value::String(certificate_record.sealed_at.to_rfc3339()),
                ]
            })
            .collect();

        WhereCondition::new(
            format!("{columns} values {}", values_columns.join(", ")).as_str(),
            values,
        )
    }

    fn persist(&self, certificate_record: CertificateRecord) -> StdResult<CertificateRecord> {
        let filters = self.get_insert_condition(&certificate_record);

        let entity = self.find(filters)?.next().unwrap_or_else(|| {
            panic!(
                "No entity returned by the persister, certificate_record = {certificate_record:?}"
            )
        });

        Ok(entity)
    }

    fn persist_many(
        &self,
        certificates_records: Vec<CertificateRecord>,
    ) -> StdResult<Vec<CertificateRecord>> {
        let filters = self.get_insert_many_condition(&certificates_records);

        Ok(self.find(filters)?.collect())
    }
}

impl<'conn> Provider<'conn> for InsertCertificateRecordProvider<'conn> {
    type Entity = CertificateRecord;

    fn get_connection(&'conn self) -> &'conn Connection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection()
            .expand(SourceAlias::new(&[("{:certificate:}", "certificate")]));

        format!("insert into certificate {condition} returning {projection}")
    }
}

struct MasterCertificateProvider<'conn> {
    connection: &'conn Connection,
}

impl<'conn> MasterCertificateProvider<'conn> {
    pub fn new(connection: &'conn Connection) -> Self {
        Self { connection }
    }

    pub fn get_master_certificate_condition(&self, epoch: Epoch) -> WhereCondition {
        let epoch_i64: i64 = epoch.0.try_into().unwrap();
        WhereCondition::new(
            "certificate.epoch between ?* and ?*",
            vec![Value::Integer(epoch_i64 - 1), Value::Integer(epoch_i64)],
        )
        .and_where(
            WhereCondition::new("certificate.parent_certificate_id is null", vec![]).or_where(
                WhereCondition::new("certificate.epoch != parent_certificate.epoch", vec![]),
            ),
        )
    }
}

impl<'conn> Provider<'conn> for MasterCertificateProvider<'conn> {
    type Entity = CertificateRecord;

    fn get_connection(&'conn self) -> &'conn Connection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection().expand(SourceAlias::new(&[
            ("{:certificate:}", "certificate"),
            ("{:parent_certificate:}", "parent_certificate"),
        ]));

        format!(
            r#"
select {projection}
from certificate
    left join certificate as parent_certificate 
        on parent_certificate.certificate_id = certificate.parent_certificate_id
where {condition}
order by certificate.ROWID desc"#
        )
    }
}

/// Provider to remove old data from the `certificate` table
pub struct DeleteCertificateProvider<'conn> {
    connection: &'conn Connection,
}

impl<'conn> Provider<'conn> for DeleteCertificateProvider<'conn> {
    type Entity = CertificateRecord;

    fn get_connection(&'conn self) -> &'conn Connection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection()
            .expand(SourceAlias::new(&[("{:certificate:}", "certificate")]));

        format!("delete from certificate where {condition} returning {projection}")
    }
}

impl<'conn> DeleteCertificateProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn Connection) -> Self {
        Self { connection }
    }

    /// Create the SQL condition to delete certificates with the given ids.
    fn get_delete_by_ids_condition(&self, ids: &[&str]) -> WhereCondition {
        let ids_values = ids.iter().map(|id| Value::String(id.to_string())).collect();

        WhereCondition::where_in("certificate_id", ids_values)
    }

    /// Delete the certificates with with the given ids.
    pub fn delete_by_ids(&self, ids: &[&str]) -> StdResult<EntityCursor<CertificateRecord>> {
        let filters = self.get_delete_by_ids_condition(ids);

        self.find(filters)
    }
}

/// Database frontend API for Certificate queries.
pub struct CertificateRepository {
    connection: Arc<Mutex<Connection>>,
}

impl CertificateRepository {
    /// Instantiate a new repository
    pub fn new(connection: Arc<Mutex<Connection>>) -> Self {
        Self { connection }
    }

    /// Return the certificate corresponding to the given hash if any.
    pub async fn get_certificate(&self, hash: &str) -> StdResult<Option<Certificate>> {
        let lock = self.connection.lock().await;
        let provider = CertificateRecordProvider::new(&lock);
        let mut cursor = provider.get_by_certificate_id(hash)?;

        Ok(cursor.next().map(|v| v.into()))
    }

    /// Return the latest certificates.
    pub async fn get_latest_certificates(&self, last_n: usize) -> StdResult<Vec<Certificate>> {
        let lock = self.connection.lock().await;
        let provider = CertificateRecordProvider::new(&lock);
        let cursor = provider.get_all()?;

        Ok(cursor.take(last_n).map(|v| v.into()).collect())
    }

    /// Return the first certificate signed per epoch as the reference
    /// certificate for this Epoch. This will be the parent certificate for all
    /// other certificates issued within this Epoch.
    pub async fn get_master_certificate_for_epoch(
        &self,
        epoch: Epoch,
    ) -> StdResult<Option<Certificate>> {
        let lock = self.connection.lock().await;
        let provider = MasterCertificateProvider::new(&lock);
        let mut cursor = provider.find(provider.get_master_certificate_condition(epoch))?;

        Ok(cursor.next().map(|c| c.into()))
    }

    /// Create a new certificate in the database.
    pub async fn create_certificate(&self, certificate: Certificate) -> StdResult<Certificate> {
        let lock = self.connection.lock().await;
        let provider = InsertCertificateRecordProvider::new(&lock);

        provider.persist(certificate.into()).map(|r| r.into())
    }

    /// Create many certificates at once in the database.
    pub async fn create_many_certificates(
        &self,
        certificates: Vec<Certificate>,
    ) -> StdResult<Vec<Certificate>> {
        let lock = self.connection.lock().await;
        let provider = InsertCertificateRecordProvider::new(&lock);
        let records: Vec<CertificateRecord> =
            certificates.into_iter().map(|cert| cert.into()).collect();
        let new_certificates = provider.persist_many(records)?;

        Ok(new_certificates
            .into_iter()
            .map(|cert| cert.into())
            .collect::<Vec<_>>())
    }

    /// Delete all the given certificates from the database
    pub async fn delete_certificates(&self, certificates: &[&Certificate]) -> StdResult<()> {
        let ids = certificates
            .iter()
            .map(|c| c.hash.as_str())
            .collect::<Vec<_>>();

        let connection = self.connection.lock().await;
        let provider = DeleteCertificateProvider::new(&connection);
        let _ = provider.delete_by_ids(&ids)?.collect::<Vec<_>>();

        Ok(())
    }
}

#[async_trait]
impl CertificateRetriever for CertificateRepository {
    async fn get_certificate_details(
        &self,
        certificate_hash: &str,
    ) -> Result<Certificate, CertificateRetrieverError> {
        self.get_certificate(certificate_hash)
            .await
            .map_err(|e| CertificateRetrieverError::General(e.to_string()))?
            .ok_or(CertificateRetrieverError::General(
                "certificate does not exist".to_string(),
            ))
    }
}

#[cfg(test)]
mod tests {
    use crate::database::provider::disable_foreign_key_support;
    use crate::{
        database::provider::apply_all_migrations_to_db, dependency_injection::DependenciesBuilder,
        Configuration,
    };
    use mithril_common::crypto_helper::tests_setup::setup_certificate_chain;

    use super::*;

    pub fn setup_certificate_db(
        connection: &Connection,
        certificates: Vec<Certificate>,
    ) -> StdResult<()> {
        apply_all_migrations_to_db(connection)?;
        disable_foreign_key_support(connection)?;

        if certificates.is_empty() {
            return Ok(());
        }

        let query = {
            // leverage the expanded parameter from this provider which is unit
            // tested on its own above.
            let update_provider = InsertCertificateRecordProvider::new(connection);
            let (sql_values, _) = update_provider
                .get_insert_condition(&(certificates.first().unwrap().to_owned().into()))
                .expand();
            format!("insert into certificate {sql_values}")
        };

        for certificate in certificates {
            let certificate_record: CertificateRecord = certificate.into();
            let mut statement = connection.prepare(&query)?;
            statement
                .bind::<&[(_, Value)]>(&[
                    (1, certificate_record.certificate_id.into()),
                    (
                        2,
                        match certificate_record.parent_certificate_id {
                            None => Value::Null,
                            Some(parent_certificate_id) => parent_certificate_id.into(),
                        },
                    ),
                    (3, certificate_record.message.into()),
                    (4, certificate_record.signature.into()),
                    (5, certificate_record.aggregate_verification_key.into()),
                    (6, i64::try_from(certificate_record.epoch.0).unwrap().into()),
                    (
                        7,
                        serde_json::to_string(&certificate_record.beacon)
                            .unwrap()
                            .into(),
                    ),
                    (8, certificate_record.protocol_version.into()),
                    (
                        9,
                        serde_json::to_string(&certificate_record.protocol_parameters)
                            .unwrap()
                            .into(),
                    ),
                    (
                        10,
                        serde_json::to_string(&certificate_record.protocol_message)
                            .unwrap()
                            .into(),
                    ),
                    (
                        11,
                        serde_json::to_string(&certificate_record.signers)
                            .unwrap()
                            .into(),
                    ),
                    (12, certificate_record.initiated_at.to_rfc3339().into()),
                    (13, certificate_record.sealed_at.to_rfc3339().into()),
                ])
                .unwrap();

            statement.next().unwrap();
        }

        Ok(())
    }

    fn insert_golden_certificate(connection: &Connection) {
        connection
            .execute(r#"
            insert into certificate
            values(
                'bfb4efbd48d58f7677ddb7d5fe5b5b9e998e8ca549cbf7583873bdccfc70f194',
                null,
                '08420665c56dcf6981b7d8b64b5a584e148edbf7638f466cb36b278ce962439c',
                'b7944ddc7d728812f8e68abc93b668a84876e9867b97648bc937b20debdff15a8415470ee709599d1a12a50ac5a57a3a4955cf19307d04955fcad6931c3b9505',
                '7b226d745f636f6d6d69746d656e74223a7b22726f6f74223a5b37372c3230382c3138392c3138372c37362c3136322c36382c3233382c3134342c31372c3131342c3137352c36302c3136352c3230322c3134362c3139342c31332c37332c3233392c3233372c3232322c3136392c3230362c352c3130392c3132332c35322c3235342c39382c3133312c37395d2c226e725f6c6561766573223a332c22686173686572223a6e756c6c7d2c22746f74616c5f7374616b65223a32383439323639303636317d',
                241,
                '{"network":"preview","epoch":241,"immutable_file_number":4823}',
                '0.1.0',
                '{"k":2422,"m":20973,"phi_f":0.2}',
                '{"message_parts":{
                    "next_aggregate_verification_key":"7b226d745f636f6d6d69746d656e74223a7b22726f6f74223a5b37372c3230382c3138392c3138372c37362c3136322c36382c3233382c3134342c31372c3131342c3137352c36302c3136352c3230322c3134362c3139342c31332c37332c3233392c3233372c3232322c3136392c3230362c352c3130392c3132332c35322c3235342c39382c3133312c37395d2c226e725f6c6561766573223a332c22686173686572223a6e756c6c7d2c22746f74616c5f7374616b65223a32383439323639303636317d"
                }}',
                '[{
                    "party_id":"pool1vapqexnsx6hvc588yyysxpjecf3k43hcr5mvhmstutuvy085xpa",
                    "verification_key":"7b22766b223a5b3133382c33322c3133382c3135322c3134362c3235352c3130382c3139302c37302c34322c3132362c3137322c31392c3135312c3133392c3133392c3235352c33352c3134312c38322c3138372c33372c3133332c3235322c3139322c302c32362c32342c3134342c372c3235332c3136362c3135312c3139332c392c3230392c3131392c3230302c3134312c34312c38302c342c3231372c3132322c3132302c3235332c3230382c3131312c362c37382c3234362c3134362c3131382c352c3235312c31392c3234332c3138342c3233382c3139352c39392c3235312c3135312c342c39342c3133382c3234362c33362c33372c34382c3133362c3130302c3233352c3134312c3232382c392c39362c3131332c35392c3137352c3130322c3232392c39352c39332c3134332c3137312c3130302c32302c3133362c36372c33302c3133312c3135332c32362c35372c3132385d2c22706f70223a5b3137342c3233302c33382c3138312c3131332c38332c372c34332c3130312c38392c3133372c3133302c37302c3135382c3235342c31342c31362c36372c38332c362c3234322c39312c3136372c34352c3232392c3139382c3130312c37302c3232382c36312c3138302c3132302c3130332c3232302c3231312c3134362c3136322c37302c33382c3230352c3139312c3235322c3138342c3235322c39362c3134382c3130322c3133362c3136362c34322c3137382c3133352c3130302c33312c38392c3233342c3135392c3131382c33382c3133392c31362c3134342c3132382c3134382c3132382c3139312c31382c34382c38392c3136352c35342c3134362c36332c3136302c3138362c3139362c31392c3137312c3136302c31342c39322c35382c3232312c3138352c3132392c382c3133322c35352c3231382c3235302c39352c32312c3235302c3135312c36352c3231395d7d",
                    "verification_key_signature":"7b227369676d61223a7b227369676d61223a7b227369676d61223a7b227369676d61223a7b227369676d61223a7b227369676d61223a5b35342c35372c32332c3234302c3234342c3130352c3139322c3138312c3130362c3232312c3132302c3139382c3136392c3134372c3233362c34382c32342c35382c3233352c31332c36302c31352c3231382c33312c34352c3135322c3133302c3230382c36392c38312c34372c3135302c3234352c3234332c32352c39342c3134382c3136322c39322c3136392c3131352c37382c31352c38382c3139382c38342c3233322c3138342c3135372c3139352c35342c3136352c33352c382c3232342c3130312c3138392c38372c32392c3131342c3133322c33382c3132322c31305d2c226c68735f706b223a5b3139322c3135342c3230322c3233342c36352c3234332c3132392c3230302c3131382c3137352c3131342c3233352c3232322c3235342c3134322c3232332c3137372c3233342c31352c31382c34312c31362c38382c38352c37322c3130372c33322c3134382c33352c35312c3132352c34355d2c227268735f706b223a5b3137342c39352c3132342c31382c36322c3135312c3137302c3136382c3232332c36362c3132322c36312c3234322c3130372c3132352c3137372c3137302c3132332c35382c3231362c3137362c392c3234302c3131382c3131302c35362c3232372c3230302c3131322c3130352c32392c3230385d7d2c226c68735f706b223a5b36392c3138322c39392c382c34302c39332c3130382c3233312c382c312c3235322c3131302c3132322c37332c3133302c3230372c3231332c3137312c3130352c3232322c31352c3134322c3230362c3137392c33382c3132302c39322c362c32302c3133352c3130382c3138335d2c227268735f706b223a5b33342c36372c3134302c3132392c3231352c36392c3136302c3135362c3230302c31302c3232362c35382c3132322c36342c33382c3135362c3230362c3230362c302c3137382c3132302c3139332c362c3135332c3131322c3130392c3135372c3131322c3132322c3133372c3233372c38355d7d2c226c68735f706b223a5b37332c3131342c3136352c3137312c34322c3131372c3139322c3139342c3137342c32302c38312c392c3230392c31392c3134352c3233302c3233302c3130392c34382c3135302c31332c3232392c3139322c35342c3138362c3137372c32382c3133362c31352c3230342c3231342c3132305d2c227268735f706b223a5b3139302c32322c3131312c38362c38322c3138362c3231372c3134312c302c3136382c3130382c3230362c3130392c332c3138342c3230342c382c3138362c3136362c32312c39372c34342c3135352c332c3136352c3139392c3132372c39312c3233382c38362c3139302c35305d7d2c226c68735f706b223a5b3135392c37352c3131382c3132372c3139382c34342c3137392c34322c3231382c3131382c3235332c3139392c32342c37312c3133302c362c3136332c3131342c3133392c31332c3130392c31372c3132372c35312c39342c3133312c3132382c3230332c3131382c3231312c3137392c36365d2c227268735f706b223a5b3139312c3136342c33362c3131312c37362c3132372c3231382c3230352c3234322c3134322c3230312c3233322c3235322c3233322c35372c39362c3131372c3232362c37332c34322c3231372c3235342c3130382c3233342c3234372c3137362c3234372c3133302c32342c36332c31392c38355d7d2c226c68735f706b223a5b35342c33392c3235342c33322c3131392c39332c3138322c3132372c3136352c3134362c3230352c33392c36352c3139362c3134362c36392c36392c34332c3139382c3130322c3139342c35372c31332c3230302c3232332c39382c38322c3134312c3133362c35382c3235322c3130325d2c227268735f706b223a5b3137372c34322c33372c3133322c3133352c3130322c3135342c392c3233362c31392c3235302c3235312c39382c36352c3133302c3232352c3136382c3232362c3136352c34392c35302c35322c3134312c3136392c35312c3230342c3234362c3130302c3233372c3234362c39322c32345d7d2c226c68735f706b223a5b33302c38302c3232322c3233372c3139302c342c3130352c3230362c37302c31372c3234382c3134322c362c31332c3137352c3136332c38342c3231352c3132322c3235352c3232302c3131382c34382c33312c34352c33332c3233372c3234352c3235302c3234302c3132392c3131355d2c227268735f706b223a5b3132332c31302c31352c36332c3138312c3231382c31302c36362c3138382c3138312c3130302c3138302c3130302c3139352c3137382c38372c3233362c32382c3138322c35362c3232362c35382c3234302c3131322c392c3133322c39332c33302c33372c3136332c3134322c39315d7d",
                    "operational_certificate":"5b5b5b3131322c39352c34322c39372c382c3235322c31382c3231342c31392c3231382c3231372c3234322c3233302c3138372c3234302c3133392c31342c3135382c3137392c3234392c3231312c36332c3132332c342c32362c3132362c3132312c3234372c302c35372c31362c3136315d2c312c37312c5b3132392c3234382c3133342c3132342c3230372c3130332c3233312c37302c3130372c32382c3134322c3134312c38362c3234392c3230352c31312c33392c3232382c3130382c3132322c3233312c3138322c3132372c3130312c3234352c33332c3135322c3233342c35342c36372c3138312c39362c3137372c3234362c32382c322c3235322c3130382c35392c3231352c3232372c3230392c3131382c3130352c3135342c37312c36332c3134352c3132372c3137352c3133382c3131352c39362c3233352c3131382c31322c3234302c3232352c3130392c3130382c3231322c3232392c35372c31305d5d2c5b33302c3138312c32302c37382c33392c3232332c352c3133372c3134312c3138392c372c3132372c34352c3232372c3230362c3135372c39352c3131352c36312c3132382c3135392c3135362c34332c3132372c302c34302c3134332c3138332c3233302c32352c39312c3137305d5d",
                    "kes_period":22,
                    "stake":1009497432569
                }]',
                '2023-06-23T08:37:49.066Z',
                '2023-06-23T08:37:49.066Z'
            );
            "#,
            )
            .unwrap();
    }

    #[test]
    fn test_golden_master() {
        let connection = Connection::open(":memory:").unwrap();
        setup_certificate_db(&connection, vec![]).unwrap();
        insert_golden_certificate(&connection);

        let provider = CertificateRecordProvider::new(&connection);
        let certificate_records: Vec<CertificateRecord> = provider
            .get_all()
            .expect("Getting Golden certificate should not fail")
            .collect();

        assert_eq!(certificate_records.len(), 1);
    }

    #[test]
    fn test_convert_certificates() {
        let (certificates, _) = setup_certificate_chain(20, 3);
        let mut certificate_records: Vec<CertificateRecord> = Vec::new();
        for certificate in certificates.clone() {
            certificate_records.push(certificate.into());
        }
        let mut certificates_new: Vec<Certificate> = Vec::new();
        for certificate_record in certificate_records {
            certificates_new.push(certificate_record.into());
        }
        assert_eq!(certificates, certificates_new);
    }

    #[test]
    fn converting_certificate_record_to_certificate_should_not_recompute_hash() {
        let expected_hash = "my_hash";
        let record =
            CertificateRecord::dummy_genesis(expected_hash, Beacon::new(String::new(), 1, 1));
        let certificate: Certificate = record.into();

        assert_eq!(expected_hash, &certificate.hash);
    }

    #[test]
    fn projection() {
        let projection = CertificateRecord::get_projection();
        let aliases = SourceAlias::new(&[("{:certificate:}", "c")]);

        assert_eq!(
            "c.certificate_id as certificate_id, c.parent_certificate_id as parent_certificate_id, c.message as message, c.signature as signature, c.aggregate_verification_key as aggregate_verification_key, c.epoch as epoch, c.beacon as beacon, c.protocol_version as protocol_version, c.protocol_parameters as protocol_parameters, c.protocol_message as protocol_message, c.signers as signers, c.initiated_at as initiated_at, c.sealed_at as sealed_at"
                .to_string(),
            projection.expand(aliases)
        );
    }

    #[test]
    fn get_certificate_record_by_epoch() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = CertificateRecordProvider::new(&connection);
        let condition = provider.condition_by_epoch(&Epoch(17)).unwrap();
        let (filter, values) = condition.expand();

        assert_eq!("epoch = ?1".to_string(), filter);
        assert_eq!(vec![Value::Integer(17)], values);
    }

    #[test]
    fn get_certificate_record_by_certificate_id() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = CertificateRecordProvider::new(&connection);
        let condition = provider
            .condition_by_certificate_id("certificate-123")
            .unwrap();
        let (filter, values) = condition.expand();

        assert_eq!("certificate_id = ?1".to_string(), filter);
        assert_eq!(vec![Value::String("certificate-123".to_string())], values);
    }

    #[test]
    fn insert_certificate_condition() {
        let (certificates, _) = setup_certificate_chain(2, 1);
        let certificate_record: CertificateRecord = certificates.first().unwrap().to_owned().into();
        let connection = Connection::open(":memory:").unwrap();
        let provider = InsertCertificateRecordProvider::new(&connection);
        let condition = provider.get_insert_condition(&certificate_record);
        let (values, params) = condition.expand();

        assert_eq!(
            "(certificate_id, parent_certificate_id, message, signature, aggregate_verification_key, epoch, beacon, protocol_version, protocol_parameters, protocol_message, signers, initiated_at, sealed_at) values (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12, ?13)".to_string(),
            values
        );
        assert_eq!(
            vec![
                Value::String(certificate_record.certificate_id),
                Value::String(certificate_record.parent_certificate_id.unwrap()),
                Value::String(certificate_record.message),
                Value::String(certificate_record.signature),
                Value::String(certificate_record.aggregate_verification_key),
                Value::Integer(i64::try_from(certificate_record.epoch.0).unwrap()),
                Value::String(serde_json::to_string(&certificate_record.beacon).unwrap()),
                Value::String(certificate_record.protocol_version),
                Value::String(
                    serde_json::to_string(&certificate_record.protocol_parameters).unwrap(),
                ),
                Value::String(serde_json::to_string(&certificate_record.protocol_message).unwrap()),
                Value::String(serde_json::to_string(&certificate_record.signers).unwrap()),
                Value::String(certificate_record.initiated_at.to_rfc3339()),
                Value::String(certificate_record.sealed_at.to_rfc3339()),
            ],
            params
        );
    }

    #[test]
    fn insert_many_certificates_condition() {
        let (certificates, _) = setup_certificate_chain(2, 1);
        let certificates_records: Vec<CertificateRecord> =
            certificates.into_iter().map(|c| c.into()).collect();
        let connection = Connection::open(":memory:").unwrap();
        let provider = InsertCertificateRecordProvider::new(&connection);
        let condition = provider.get_insert_many_condition(&certificates_records);
        let (values, params) = condition.expand();

        assert_eq!(
            "(certificate_id, parent_certificate_id, message, signature, \
aggregate_verification_key, epoch, beacon, protocol_version, protocol_parameters, \
protocol_message, signers, initiated_at, sealed_at) values \
(?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12, ?13), \
(?14, ?15, ?16, ?17, ?18, ?19, ?20, ?21, ?22, ?23, ?24, ?25, ?26)"
                .to_string(),
            values
        );
        assert_eq!(
            certificates_records
                .into_iter()
                .flat_map(|certificate_record| {
                    vec![
                        Value::String(certificate_record.certificate_id),
                        match certificate_record.parent_certificate_id {
                            Some(id) => Value::String(id),
                            None => Value::Null,
                        },
                        Value::String(certificate_record.message),
                        Value::String(certificate_record.signature),
                        Value::String(certificate_record.aggregate_verification_key),
                        Value::Integer(i64::try_from(certificate_record.epoch.0).unwrap()),
                        Value::String(serde_json::to_string(&certificate_record.beacon).unwrap()),
                        Value::String(certificate_record.protocol_version),
                        Value::String(
                            serde_json::to_string(&certificate_record.protocol_parameters).unwrap(),
                        ),
                        Value::String(
                            serde_json::to_string(&certificate_record.protocol_message).unwrap(),
                        ),
                        Value::String(serde_json::to_string(&certificate_record.signers).unwrap()),
                        Value::String(certificate_record.initiated_at.to_rfc3339()),
                        Value::String(certificate_record.sealed_at.to_rfc3339()),
                    ]
                })
                .collect::<Vec<_>>(),
            params
        );
    }

    #[test]
    fn test_get_certificate_records() {
        let (certificates, _) = setup_certificate_chain(20, 7);

        let connection = Connection::open(":memory:").unwrap();
        setup_certificate_db(&connection, certificates.clone()).unwrap();

        let provider = CertificateRecordProvider::new(&connection);

        let certificate_records: Vec<CertificateRecord> =
            provider.get_by_epoch(&Epoch(1)).unwrap().collect();
        let expected_certificate_records: Vec<CertificateRecord> = certificates
            .iter()
            .filter_map(|c| (c.beacon.epoch == Epoch(1)).then_some(c.to_owned().into()))
            .rev()
            .collect();
        assert_eq!(expected_certificate_records, certificate_records);

        let certificate_records: Vec<CertificateRecord> =
            provider.get_by_epoch(&Epoch(3)).unwrap().collect();
        let expected_certificate_records: Vec<CertificateRecord> = certificates
            .iter()
            .filter_map(|c| (c.beacon.epoch == Epoch(3)).then_some(c.to_owned().into()))
            .rev()
            .collect();
        assert_eq!(expected_certificate_records, certificate_records);

        let cursor = provider.get_by_epoch(&Epoch(5)).unwrap();
        assert_eq!(0, cursor.count());

        let certificate_records: Vec<CertificateRecord> = provider.get_all().unwrap().collect();
        let expected_certificate_records: Vec<CertificateRecord> = certificates
            .iter()
            .map(|c| c.to_owned().into())
            .rev()
            .collect();
        assert_eq!(expected_certificate_records, certificate_records);
    }

    #[test]
    fn test_insert_certificate_record() {
        let (certificates, _) = setup_certificate_chain(5, 2);

        let connection = Connection::open(":memory:").unwrap();
        setup_certificate_db(&connection, Vec::new()).unwrap();

        let provider = InsertCertificateRecordProvider::new(&connection);

        for certificate in certificates {
            let certificate_record: CertificateRecord = certificate.into();
            let certificate_record_saved = provider.persist(certificate_record.clone()).unwrap();
            assert_eq!(certificate_record, certificate_record_saved);
        }
    }

    #[test]
    fn test_insert_many_certificates_records() {
        let (certificates, _) = setup_certificate_chain(5, 2);
        let certificates_records: Vec<CertificateRecord> =
            certificates.into_iter().map(|cert| cert.into()).collect();

        let connection = Connection::open(":memory:").unwrap();
        setup_certificate_db(&connection, Vec::new()).unwrap();

        let provider = InsertCertificateRecordProvider::new(&connection);
        let certificates_records_saved = provider
            .persist_many(certificates_records.clone())
            .expect("saving many records should not fail");

        assert_eq!(certificates_records, certificates_records_saved);
    }

    #[tokio::test]
    async fn master_certificate_condition() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = MasterCertificateProvider::new(&connection);
        let condition = provider.get_master_certificate_condition(Epoch(10));
        let (condition_str, parameters) = condition.expand();

        assert_eq!(
            "certificate.epoch between ?1 and ?2 and (certificate.parent_certificate_id is null or certificate.epoch != parent_certificate.epoch)".to_string(),
            condition_str
        );
        assert_eq!(vec![Value::Integer(9), Value::Integer(10)], parameters);
    }

    #[tokio::test]
    async fn repository_get_certificate() {
        let (certificates, _) = setup_certificate_chain(5, 2);
        let expected_hash = certificates[0].hash.clone();
        let mut deps = DependenciesBuilder::new(Configuration::new_sample());
        let connection = deps.get_sqlite_connection().await.unwrap();
        {
            let lock = connection.lock().await;
            let provider = InsertCertificateRecordProvider::new(&lock);

            for certificate in certificates.iter().rev() {
                provider.persist(certificate.to_owned().into()).unwrap();
            }
        }

        let repository = CertificateRepository::new(connection);
        let certificate = repository.get_certificate("whatever").await.unwrap();
        assert!(certificate.is_none());

        let certificate = repository
            .get_certificate(&expected_hash)
            .await
            .unwrap()
            .expect("The certificate exist and should be returned.");

        assert_eq!(expected_hash, certificate.hash);
    }

    #[tokio::test]
    async fn repository_get_latest_certificates() {
        let (certificates, _) = setup_certificate_chain(5, 2);
        let mut deps = DependenciesBuilder::new(Configuration::new_sample());
        let connection = deps.get_sqlite_connection().await.unwrap();
        {
            let lock = connection.lock().await;
            let provider = InsertCertificateRecordProvider::new(&lock);

            for certificate in certificates.iter().rev() {
                provider.persist(certificate.to_owned().into()).unwrap();
            }
        }

        let repository = CertificateRepository::new(connection);
        let latest_certificates = repository
            .get_latest_certificates(certificates.len())
            .await
            .unwrap();

        assert_eq!(certificates, latest_certificates);
    }

    async fn insert_certificate_records(
        connection: Arc<Mutex<Connection>>,
        records: Vec<CertificateRecord>,
    ) {
        let lock = connection.lock().await;
        let provider = InsertCertificateRecordProvider::new(&lock);

        for certificate in records {
            provider.persist(certificate).unwrap();
        }
    }

    #[tokio::test]
    async fn get_master_certificate_no_certificate_recorded_returns_none() {
        let mut deps = DependenciesBuilder::new(Configuration::new_sample());
        let connection = deps.get_sqlite_connection().await.unwrap();
        let certificates = vec![];
        insert_certificate_records(connection.clone(), certificates).await;

        let repository = CertificateRepository::new(connection);
        let certificate = repository
            .get_master_certificate_for_epoch(Epoch(1))
            .await
            .unwrap();

        assert_eq!(None, certificate);
    }

    #[tokio::test]
    async fn get_master_certificate_one_cert_in_current_epoch_recorded_returns_that_one() {
        let mut deps = DependenciesBuilder::new(Configuration::new_sample());
        let connection = deps.get_sqlite_connection().await.unwrap();
        let certificate = CertificateRecord::dummy_genesis("1", Beacon::new(String::new(), 1, 1));
        let expected_certificate: Certificate = certificate.clone().into();
        insert_certificate_records(connection.clone(), vec![certificate]).await;

        let repository = CertificateRepository::new(connection);
        let certificate = repository
            .get_master_certificate_for_epoch(Epoch(1))
            .await
            .unwrap()
            .expect("This should return a certificate.");

        assert_eq!(expected_certificate, certificate);
    }

    #[tokio::test]
    async fn get_master_certificate_multiple_cert_in_current_epoch_returns_first_of_current_epoch()
    {
        let mut deps = DependenciesBuilder::new(Configuration::new_sample());
        let connection = deps.get_sqlite_connection().await.unwrap();
        let certificates = vec![
            CertificateRecord::dummy_genesis("1", Beacon::new(String::new(), 1, 1)),
            CertificateRecord::dummy("2", "1", Beacon::new(String::new(), 1, 2)),
            CertificateRecord::dummy("3", "1", Beacon::new(String::new(), 1, 3)),
        ];
        let expected_certificate: Certificate = certificates.first().unwrap().clone().into();
        insert_certificate_records(connection.clone(), certificates).await;

        let repository = CertificateRepository::new(connection);
        let certificate = repository
            .get_master_certificate_for_epoch(Epoch(1))
            .await
            .unwrap()
            .expect("This should return a certificate.");

        assert_eq!(expected_certificate, certificate);
    }

    #[tokio::test]
    async fn get_master_certificate_multiple_cert_in_previous_epoch_none_in_the_current_returns_first_of_previous_epoch(
    ) {
        let mut deps = DependenciesBuilder::new(Configuration::new_sample());
        let connection = deps.get_sqlite_connection().await.unwrap();
        let certificates = vec![
            CertificateRecord::dummy_genesis("1", Beacon::new(String::new(), 1, 1)),
            CertificateRecord::dummy("2", "1", Beacon::new(String::new(), 1, 2)),
            CertificateRecord::dummy("3", "1", Beacon::new(String::new(), 1, 3)),
        ];
        let expected_certificate: Certificate = certificates.first().unwrap().clone().into();
        insert_certificate_records(connection.clone(), certificates).await;

        let repository = CertificateRepository::new(connection);
        let certificate = repository
            .get_master_certificate_for_epoch(Epoch(2))
            .await
            .unwrap()
            .expect("This should return a certificate.");

        assert_eq!(expected_certificate, certificate);
    }

    #[tokio::test]
    async fn get_master_certificate_multiple_cert_in_previous_one_cert_in_current_epoch_returns_one_in_current_epoch(
    ) {
        let mut deps = DependenciesBuilder::new(Configuration::new_sample());
        let connection = deps.get_sqlite_connection().await.unwrap();
        let certificates = vec![
            CertificateRecord::dummy_genesis("1", Beacon::new(String::new(), 1, 1)),
            CertificateRecord::dummy("2", "1", Beacon::new(String::new(), 1, 2)),
            CertificateRecord::dummy("3", "1", Beacon::new(String::new(), 1, 3)),
            CertificateRecord::dummy("4", "1", Beacon::new(String::new(), 2, 4)),
        ];
        let expected_certificate: Certificate = certificates.last().unwrap().clone().into();
        insert_certificate_records(connection.clone(), certificates).await;

        let repository = CertificateRepository::new(connection);
        let certificate = repository
            .get_master_certificate_for_epoch(Epoch(2))
            .await
            .unwrap()
            .expect("This should return a certificate.");

        assert_eq!(expected_certificate, certificate);
    }

    #[tokio::test]
    async fn get_master_certificate_multiple_cert_in_previous_multiple_in_current_epoch_returns_first_of_current_epoch(
    ) {
        let mut deps = DependenciesBuilder::new(Configuration::new_sample());
        let connection = deps.get_sqlite_connection().await.unwrap();
        let certificates = vec![
            CertificateRecord::dummy_genesis("1", Beacon::new(String::new(), 1, 1)),
            CertificateRecord::dummy("2", "1", Beacon::new(String::new(), 1, 2)),
            CertificateRecord::dummy("3", "1", Beacon::new(String::new(), 1, 3)),
            CertificateRecord::dummy("4", "1", Beacon::new(String::new(), 2, 4)),
            CertificateRecord::dummy("5", "4", Beacon::new(String::new(), 2, 5)),
            CertificateRecord::dummy("6", "4", Beacon::new(String::new(), 2, 6)),
        ];
        let expected_certificate: Certificate = certificates.get(3).unwrap().clone().into();
        insert_certificate_records(connection.clone(), certificates).await;

        let repository = CertificateRepository::new(connection);
        let certificate = repository
            .get_master_certificate_for_epoch(Epoch(2))
            .await
            .unwrap()
            .expect("This should return a certificate.");
        assert_eq!(expected_certificate, certificate);
    }

    #[tokio::test]
    async fn get_master_certificate_multiple_cert_in_penultimate_epoch_none_in_previous_returns_none(
    ) {
        let mut deps = DependenciesBuilder::new(Configuration::new_sample());
        let connection = deps.get_sqlite_connection().await.unwrap();
        let certificates = vec![
            CertificateRecord::dummy_genesis("1", Beacon::new(String::new(), 1, 1)),
            CertificateRecord::dummy("2", "1", Beacon::new(String::new(), 1, 2)),
            CertificateRecord::dummy("3", "1", Beacon::new(String::new(), 1, 3)),
        ];
        insert_certificate_records(connection.clone(), certificates).await;

        let repository = CertificateRepository::new(connection);
        let certificate = repository
            .get_master_certificate_for_epoch(Epoch(3))
            .await
            .unwrap();

        assert_eq!(None, certificate);
    }

    #[tokio::test]
    async fn get_master_certificate_second_genesis_after_multiple_cert_in_current_epoch_returns_last_genesis(
    ) {
        let mut deps = DependenciesBuilder::new(Configuration::new_sample());
        let connection = deps.get_sqlite_connection().await.unwrap();
        let certificates = vec![
            CertificateRecord::dummy_genesis("1", Beacon::new(String::new(), 1, 1)),
            CertificateRecord::dummy("2", "1", Beacon::new(String::new(), 1, 2)),
            CertificateRecord::dummy("3", "1", Beacon::new(String::new(), 1, 3)),
            CertificateRecord::dummy_genesis("4", Beacon::new(String::new(), 1, 3)),
        ];
        let expected_certificate: Certificate = certificates.last().unwrap().clone().into();
        insert_certificate_records(connection.clone(), certificates).await;

        let repository = CertificateRepository::new(connection);
        let certificate = repository
            .get_master_certificate_for_epoch(Epoch(2))
            .await
            .unwrap()
            .expect("This should return a certificate.");

        assert_eq!(expected_certificate, certificate);
    }

    #[tokio::test]
    async fn get_master_certificate_second_genesis_after_multiple_cert_in_multiple_epochs_returns_last_genesis(
    ) {
        let mut deps = DependenciesBuilder::new(Configuration::new_sample());
        let connection = deps.get_sqlite_connection().await.unwrap();
        let certificates = vec![
            CertificateRecord::dummy_genesis("1", Beacon::new(String::new(), 1, 1)),
            CertificateRecord::dummy("2", "1", Beacon::new(String::new(), 1, 2)),
            CertificateRecord::dummy("3", "1", Beacon::new(String::new(), 1, 2)),
            CertificateRecord::dummy("4", "1", Beacon::new(String::new(), 2, 4)),
            CertificateRecord::dummy("5", "1", Beacon::new(String::new(), 2, 5)),
            CertificateRecord::dummy_genesis("6", Beacon::new(String::new(), 2, 5)),
        ];
        let expected_certificate: Certificate = certificates.last().unwrap().clone().into();
        insert_certificate_records(connection.clone(), certificates).await;

        let repository = CertificateRepository::new(connection);
        let certificate = repository
            .get_master_certificate_for_epoch(Epoch(2))
            .await
            .unwrap()
            .expect("This should return a certificate.");

        assert_eq!(expected_certificate, certificate);
    }

    #[tokio::test]
    async fn get_master_certificate_new_genesis_after_multiple_cert_in_previous_epoch_returns_last_genesis(
    ) {
        let mut deps = DependenciesBuilder::new(Configuration::new_sample());
        let connection = deps.get_sqlite_connection().await.unwrap();
        let certificates = vec![
            CertificateRecord::dummy_genesis("1", Beacon::new(String::new(), 1, 1)),
            CertificateRecord::dummy("2", "1", Beacon::new(String::new(), 1, 2)),
            CertificateRecord::dummy("3", "1", Beacon::new(String::new(), 1, 3)),
            CertificateRecord::dummy_genesis("4", Beacon::new(String::new(), 2, 3)),
        ];
        let expected_certificate: Certificate = certificates.last().unwrap().clone().into();
        insert_certificate_records(connection.clone(), certificates).await;

        let repository = CertificateRepository::new(connection);
        let certificate = repository
            .get_master_certificate_for_epoch(Epoch(2))
            .await
            .unwrap()
            .expect("This should return a certificate.");

        assert_eq!(expected_certificate, certificate);
    }

    #[tokio::test]
    async fn get_master_certificate_for_epoch() {
        let (certificates, _) = setup_certificate_chain(3, 1);
        let expected_certificate_id = &certificates[2].hash;
        let epoch = &certificates[2].beacon.epoch;
        let mut deps = DependenciesBuilder::new(Configuration::new_sample());
        let connection = deps.get_sqlite_connection().await.unwrap();
        {
            let lock = connection.lock().await;
            let provider = InsertCertificateRecordProvider::new(&lock);

            for certificate in certificates.iter().rev() {
                provider.persist(certificate.to_owned().into()).unwrap();
            }
        }

        let repository = CertificateRepository::new(connection);
        let certificate = repository
            .get_master_certificate_for_epoch(*epoch)
            .await
            .unwrap()
            .expect("This should return a certificate.");

        assert_eq!(expected_certificate_id.to_string(), certificate.hash);
    }

    #[tokio::test]
    async fn save_certificate() {
        let (certificates, _) = setup_certificate_chain(5, 3);
        let mut deps = DependenciesBuilder::new(Configuration::new_sample());
        let connection = deps.get_sqlite_connection().await.unwrap();
        let repository = CertificateRepository::new(connection);
        let certificate = repository
            .create_certificate(certificates[4].clone())
            .await
            .unwrap();

        assert_eq!(certificates[4].hash, certificate.hash);
        {
            let connection = deps.get_sqlite_connection().await.unwrap();
            let lock = connection.lock().await;
            let provider = CertificateRecordProvider::new(&lock);
            let mut cursor = provider
                .get_by_certificate_id(&certificates[4].hash)
                .unwrap();
            let cert = cursor
                .next()
                .expect("There should be a certificate in the database with this hash ID.");

            assert_eq!(certificates[4].hash, cert.certificate_id);
        }
    }

    #[test]
    fn delete_certificates_condition_correctly_joins_given_ids() {
        let connection = Connection::open(":memory:").unwrap();
        let provider = DeleteCertificateProvider::new(&connection);
        let condition = provider.get_delete_by_ids_condition(&["a", "b", "c"]);
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

    #[tokio::test]
    async fn delete_only_given_certificates() {
        let mut deps = DependenciesBuilder::new(Configuration::new_sample());
        let connection = deps.get_sqlite_connection().await.unwrap();
        let repository = CertificateRepository::new(connection.clone());
        let records = vec![
            CertificateRecord::dummy_genesis("1", Beacon::new(String::new(), 1, 1)),
            CertificateRecord::dummy("2", "1", Beacon::new(String::new(), 1, 2)),
            CertificateRecord::dummy("3", "1", Beacon::new(String::new(), 1, 3)),
        ];
        insert_certificate_records(connection, records.clone()).await;
        let certificates: Vec<Certificate> = records.into_iter().map(|c| c.into()).collect();

        // Delete all records except the first
        repository
            .delete_certificates(
                &certificates
                    .iter()
                    .filter(|r| r.beacon.immutable_file_number > 1)
                    .collect::<Vec<_>>(),
            )
            .await
            .unwrap();

        let expected_remaining_certificate = certificates.first().unwrap().clone();
        let remaining_certificates = repository
            .get_latest_certificates(usize::MAX)
            .await
            .unwrap();

        assert_eq!(vec![expected_remaining_certificate], remaining_certificates)
    }
}

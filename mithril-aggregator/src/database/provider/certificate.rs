use async_trait::async_trait;
use chrono::{DateTime, Utc};
use sqlite::{Connection, Value};
use std::iter::repeat;
use std::sync::Arc;
use tokio::sync::Mutex;

use mithril_common::{
    certificate_chain::{CertificateRetriever, CertificateRetrieverError},
    entities::{
        Beacon, Certificate, CertificateMetadata, Epoch, HexEncodedAgregateVerificationKey,
        HexEncodedKey, ProtocolMessage, ProtocolParameters, ProtocolVersion, SignerWithStake,
    },
    sqlite::{
        EntityCursor, HydrationError, Projection, Provider, SourceAlias, SqLiteEntity,
        WhereCondition,
    },
    store::adapter::{AdapterError, StoreAdapter},
    StdError, StdResult,
};

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
        record
    }

    #[cfg(test)]
    pub fn dummy(id: &str, parent_id: &str, beacon: Beacon) -> Self {
        Self {
            certificate_id: id.to_string(),
            parent_certificate_id: Some(parent_id.to_string()),
            message: "message".to_string(),
            signature: "signature".to_string(),
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
        if !other.genesis_signature.is_empty() {
            // Genesis certificate
            CertificateRecord {
                certificate_id: other.hash,
                parent_certificate_id: None,
                message: other.signed_message,
                signature: other.genesis_signature,
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
        } else {
            // Multi-signature certificate
            CertificateRecord {
                certificate_id: other.hash,
                parent_certificate_id: Some(other.previous_hash),
                message: other.signed_message,
                signature: other.multi_signature,
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
}

impl From<CertificateRecord> for Certificate {
    fn from(other: CertificateRecord) -> Certificate {
        let certificate_metadata = CertificateMetadata::new(
            other.protocol_version,
            other.protocol_parameters,
            other.initiated_at,
            other.sealed_at,
            other.signers,
        );
        let signed_message = other.protocol_message.compute_hash();

        if other.parent_certificate_id.is_none() {
            // Genesis certificate
            Certificate {
                hash: other.certificate_id,
                previous_hash: "".to_string(),
                beacon: other.beacon,
                metadata: certificate_metadata,
                protocol_message: other.protocol_message,
                signed_message,
                aggregate_verification_key: other.aggregate_verification_key,
                multi_signature: "".to_string(),
                genesis_signature: other.signature,
            }
        } else {
            // Multi-signature certificate
            Certificate {
                hash: other.certificate_id,
                previous_hash: other.parent_certificate_id.unwrap(),
                beacon: other.beacon,
                metadata: certificate_metadata,
                protocol_message: other.protocol_message,
                signed_message,
                aggregate_verification_key: other.aggregate_verification_key,
                multi_signature: other.signature,
                genesis_signature: "".to_string(),
            }
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

    fn condition_by_certificate_id(
        &self,
        certificate_id: &str,
    ) -> Result<WhereCondition, StdError> {
        Ok(WhereCondition::new(
            "certificate_id = ?*",
            vec![Value::String(certificate_id.to_owned())],
        ))
    }

    fn condition_by_epoch(&self, epoch: &Epoch) -> Result<WhereCondition, StdError> {
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
    ) -> Result<EntityCursor<CertificateRecord>, StdError> {
        let filters = self.condition_by_certificate_id(certificate_id)?;
        let certificate_record = self.find(filters)?;

        Ok(certificate_record)
    }

    /// Get CertificateRecords for a given Epoch.
    pub fn get_by_epoch(&self, epoch: &Epoch) -> Result<EntityCursor<CertificateRecord>, StdError> {
        let filters = self.condition_by_epoch(epoch)?;
        let certificate_record = self.find(filters)?;

        Ok(certificate_record)
    }

    /// Get all CertificateRecords.
    pub fn get_all(&self) -> Result<EntityCursor<CertificateRecord>, StdError> {
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

    fn persist(
        &self,
        certificate_record: CertificateRecord,
    ) -> Result<CertificateRecord, StdError> {
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
    ) -> Result<Vec<CertificateRecord>, StdError> {
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
    pub fn delete_by_ids(&self, ids: &[&str]) -> Result<EntityCursor<CertificateRecord>, StdError> {
        let filters = self.get_delete_by_ids_condition(ids);

        self.find(filters)
    }
}

/// Database frontend API for Certificate queries.
pub struct CertificateRepository {
    connection: Arc<Mutex<Connection>>,
}

impl CertificateRepository {
    /// Instanciate a new repository
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
        let mut cursor = provider.find(provider.get_insert_condition(&certificate.into()))?;

        let new_certificate = cursor
            .next()
            .ok_or_else(|| panic!("Insert certificate query should always return a record."))
            .unwrap();

        Ok(new_certificate.into())
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

/// Service to deal with certificate (read & write).
pub struct CertificateStoreAdapter {
    connection: Arc<Mutex<Connection>>,
}

impl CertificateStoreAdapter {
    /// Create a new CertificateStoreAdapter service
    pub fn new(connection: Arc<Mutex<Connection>>) -> Self {
        Self { connection }
    }
}

#[async_trait]
impl StoreAdapter for CertificateStoreAdapter {
    type Key = String;
    type Record = Certificate;

    async fn store_record(
        &mut self,
        _key: &Self::Key,
        record: &Self::Record,
    ) -> Result<(), AdapterError> {
        let connection = &*self.connection.lock().await;
        let provider = InsertCertificateRecordProvider::new(connection);
        let _certificate_record = provider
            .persist(record.to_owned().into())
            .map_err(|e| AdapterError::GeneralError(format!("{e}")))?;

        Ok(())
    }

    async fn get_record(&self, key: &Self::Key) -> Result<Option<Self::Record>, AdapterError> {
        let connection = &*self.connection.lock().await;
        let provider = CertificateRecordProvider::new(connection);
        let mut cursor = provider
            .get_by_certificate_id(key)
            .map_err(|e| AdapterError::GeneralError(format!("{e}")))?;
        let certificate = cursor
            .next()
            .map(|certificate_record| certificate_record.into());

        Ok(certificate)
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
            .map(|c| (c.hash.to_owned(), c))
            .collect())
    }

    async fn remove(&mut self, _key: &Self::Key) -> Result<Option<Self::Record>, AdapterError> {
        unimplemented!()
    }

    async fn get_iter(&self) -> Result<Box<dyn Iterator<Item = Self::Record> + '_>, AdapterError> {
        let connection = &*self.connection.lock().await;
        let provider = CertificateRecordProvider::new(connection);
        let cursor = provider
            .get_all()
            .map_err(|e| AdapterError::GeneralError(format!("{e}")))?;
        let certificates: Vec<Certificate> = cursor.map(|c| c.into()).collect();
        Ok(Box::new(certificates.into_iter()))
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
    ) -> Result<(), StdError> {
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
    async fn test_store_adapter() {
        let (certificates, _) = setup_certificate_chain(5, 2);

        let connection = Connection::open(":memory:").unwrap();
        setup_certificate_db(&connection, Vec::new()).unwrap();

        let mut certificate_store_adapter =
            CertificateStoreAdapter::new(Arc::new(Mutex::new(connection)));

        for certificate in &certificates {
            assert!(certificate_store_adapter
                .store_record(&certificate.hash, certificate)
                .await
                .is_ok());
        }

        for certificate in &certificates {
            assert!(certificate_store_adapter
                .record_exists(&certificate.hash)
                .await
                .unwrap());
            assert_eq!(
                Some(certificate.to_owned()),
                certificate_store_adapter
                    .get_record(&certificate.hash)
                    .await
                    .unwrap()
            );
        }

        assert_eq!(
            certificates,
            certificate_store_adapter
                .get_last_n_records(certificates.len())
                .await
                .unwrap()
                .into_iter()
                .map(|(_k, v)| v)
                .rev()
                .collect::<Vec<Certificate>>()
        )
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

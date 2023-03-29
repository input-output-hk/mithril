use sqlite::{Connection, Value};

use mithril_common::{
    entities::{
        Beacon, Certificate, CertificateMetadata, Epoch, HexEncodedAgregateVerificationKey,
        HexEncodedKey, ProtocolMessage, ProtocolParameters, ProtocolVersion, SignerWithStake,
    },
    sqlite::{
        EntityCursor, HydrationError, Projection, Provider, SourceAlias, SqLiteEntity,
        WhereCondition,
    },
};

use mithril_common::StdError;

/// Certificate record is the representation of a stored certificate.
#[derive(Debug, PartialEq, Clone)]
pub struct CertificateRecord {
    /// Certificate id.
    certificate_id: String,

    /// Parent Certificate id.
    parent_certificate_id: Option<String>,

    /// Message that is signed.
    message: String,

    /// Signature of the certificate.
    /// Note: multi-signature if parent certificate id is set, genesis signature otherwise.
    pub signature: HexEncodedKey,

    /// Aggregate verification key
    /// Note: used only if signature is a multi-signature
    pub aggregate_verification_key: HexEncodedAgregateVerificationKey,

    /// Epoch of creation of the certificate.
    epoch: Epoch,

    /// Beacon used to produce the signed message
    beacon: Beacon,

    /// Protocol Version (semver)
    protocol_version: ProtocolVersion,

    /// Protocol parameters.
    protocol_parameters: ProtocolParameters,

    /// Structured message that is used to create the signed message
    protocol_message: ProtocolMessage,

    /// The list of the active signers with their stakes and verification keys
    signers: Vec<SignerWithStake>,

    /// Date and time when the certificate was initiated
    initiated_at: String,

    /// Date and time when the certificate was sealed
    sealed_at: String,
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

        if other.parent_certificate_id.is_none() {
            // Genesis certificate
            Certificate::new(
                "".to_string(),
                other.beacon,
                certificate_metadata,
                other.protocol_message,
                other.aggregate_verification_key,
                "".to_string(),
                other.signature,
            )
        } else {
            // Multi-signature certificate
            Certificate::new(
                other.parent_certificate_id.unwrap(),
                other.beacon,
                certificate_metadata,
                other.protocol_message,
                other.aggregate_verification_key,
                other.signature,
                "".to_string(),
            )
        }
    }
}

impl SqLiteEntity for CertificateRecord {
    fn hydrate(row: sqlite::Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let certificate_id = row.get::<String, _>(0);
        let parent_certificate_id = row.get::<Option<String>, _>(1);
        let message = row.get::<String, _>(2);
        let signature = row.get::<String, _>(3);
        let aggregate_verification_key = row.get::<String, _>(4);
        let epoch_int = row.get::<i64, _>(5);
        let beacon_string = row.get::<String, _>(6);
        let protocol_version = row.get::<String, _>(7);
        let protocol_parameters_string = row.get::<String, _>(8);
        let protocol_message_string = row.get::<String, _>(9);
        let signers_string = row.get::<String, _>(10);
        let initiated_at = row.get::<String, _>(11);
        let sealed_at = row.get::<String, _>(12);

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
            beacon: serde_json::from_str(&beacon_string).map_err(
                |e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{beacon_string}' to Beacon. Error: {e}"
                    ))
                },
            )?,
            protocol_version,
            protocol_parameters: serde_json::from_str(&protocol_parameters_string).map_err(
                |e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{protocol_parameters_string}' to ProtocolParameters. Error: {e}"
                    ))
                },
            )?,
            protocol_message: serde_json::from_str(&protocol_message_string).map_err(
                |e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{protocol_message_string}' to ProtocolMessage. Error: {e}"
                    ))
                },
            )?,
            signers: serde_json::from_str(&signers_string).map_err(
                |e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{signers_string}' to Vec<SignerWithStake>. Error: {e}"
                    ))
                },
            )?,
            initiated_at,
            sealed_at
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
        certificate_id: String,
    ) -> Result<WhereCondition, StdError> {
        Ok(WhereCondition::new(
            "certificate_id = ?*",
            vec![Value::String(certificate_id)],
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
        certificate_id: String,
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
}

impl<'client> Provider<'client> for CertificateRecordProvider<'client> {
    type Entity = CertificateRecord;

    fn get_connection(&'client self) -> &'client Connection {
        self.client
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:certificate:}", "c")]);
        let projection = Self::Entity::get_projection().expand(aliases);
        format!(
            "select {projection} from certificate as c where {condition} order by sealed_at asc"
        )
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

    fn get_insert_condition(&self, certificate_record: CertificateRecord) -> WhereCondition {
        WhereCondition::new(
            "(certificate_id, parent_certificate_id, message, signature, aggregate_verification_key, epoch, beacon, protocol_version, protocol_parameters, protocol_message, signers, initiated_at, sealed_at) values (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12, ?13)",
            vec![
                Value::String(certificate_record.certificate_id),
                if let Some(parent_certificate_id) = certificate_record.parent_certificate_id{
                    Value::String(parent_certificate_id)
                }else{
                    Value::Null
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
                Value::String(serde_json::to_string(&certificate_record.protocol_message).unwrap()),
                Value::String(serde_json::to_string(&certificate_record.signers).unwrap()),
                Value::String(certificate_record.initiated_at),
                Value::String(certificate_record.sealed_at),
            ],
        )
    }

    fn persist(
        &self,
        certificate_record: CertificateRecord,
    ) -> Result<CertificateRecord, StdError> {
        let filters = self.get_insert_condition(certificate_record.clone());

        let entity = self.find(filters)?.next().unwrap_or_else(|| {
            panic!(
                "No entity returned by the persister, certificate_record = {certificate_record:?}"
            )
        });

        Ok(entity)
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

#[cfg(test)]
mod tests {
    use crate::database::migration::get_migrations;
    use mithril_common::crypto_helper::tests_setup::setup_certificate_chain;

    use super::*;

    pub fn setup_certificate_db(
        connection: &Connection,
        certificates: Vec<Certificate>,
    ) -> Result<(), StdError> {
        for migration in get_migrations() {
            connection.execute(&migration.alterations)?;
        }

        if certificates.is_empty() {
            return Ok(());
        }

        let query = {
            // leverage the expanded parameter from this provider which is unit
            // tested on its own above.
            let update_provider = InsertCertificateRecordProvider::new(connection);
            let (sql_values, _) = update_provider
                .get_insert_condition(certificates.first().unwrap().to_owned().into())
                .expand();
            format!("insert into certificate {sql_values}")
        };

        for certificate in certificates {
            let certificate_record: CertificateRecord = certificate.into();
            let mut statement = connection.prepare(&query)?;

            statement
                .bind(1, certificate_record.certificate_id.as_str())
                .unwrap();
            if let Some(parent_certificate_id) = certificate_record.parent_certificate_id {
                statement.bind(2, parent_certificate_id.as_str()).unwrap();
            } else {
                statement.bind(2, &Value::Null).unwrap();
            }
            statement
                .bind(3, certificate_record.message.as_str())
                .unwrap();
            statement
                .bind(4, certificate_record.signature.as_str())
                .unwrap();
            statement
                .bind(5, certificate_record.aggregate_verification_key.as_str())
                .unwrap();
            statement
                .bind(6, certificate_record.epoch.0 as i64)
                .unwrap();
            statement
                .bind(
                    7,
                    serde_json::to_string(&certificate_record.beacon)
                        .unwrap()
                        .as_str(),
                )
                .unwrap();
            statement
                .bind(8, certificate_record.protocol_version.as_str())
                .unwrap();
            statement
                .bind(
                    9,
                    serde_json::to_string(&certificate_record.protocol_parameters)
                        .unwrap()
                        .as_str(),
                )
                .unwrap();
            statement
                .bind(
                    10,
                    serde_json::to_string(&certificate_record.protocol_message)
                        .unwrap()
                        .as_str(),
                )
                .unwrap();
            statement
                .bind(
                    11,
                    serde_json::to_string(&certificate_record.signers)
                        .unwrap()
                        .as_str(),
                )
                .unwrap();
            statement
                .bind(12, certificate_record.initiated_at.as_str())
                .unwrap();
            statement
                .bind(13, certificate_record.sealed_at.as_str())
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
            .condition_by_certificate_id("cert-123".to_string())
            .unwrap();
        let (filter, values) = condition.expand();

        assert_eq!("certificate_id = ?1".to_string(), filter);
        assert_eq!(vec![Value::String("cert-123".to_string())], values);
    }

    #[test]
    fn insert_certificate_record() {
        let (certificates, _) = setup_certificate_chain(2, 1);
        let certificate_record: CertificateRecord = certificates.first().unwrap().to_owned().into();
        let connection = Connection::open(":memory:").unwrap();
        let provider = InsertCertificateRecordProvider::new(&connection);
        let condition = provider.get_insert_condition(certificate_record.clone());
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
                Value::String(certificate_record.initiated_at),
                Value::String(certificate_record.sealed_at),
            ],
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
            .collect();
        assert_eq!(expected_certificate_records, certificate_records);

        let certificate_records: Vec<CertificateRecord> =
            provider.get_by_epoch(&Epoch(3)).unwrap().collect();

        let expected_certificate_records: Vec<CertificateRecord> = certificates
            .iter()
            .filter_map(|c| (c.beacon.epoch == Epoch(3)).then_some(c.to_owned().into()))
            .collect();
        assert_eq!(expected_certificate_records, certificate_records);

        let cursor = provider.get_by_epoch(&Epoch(5)).unwrap();
        assert_eq!(0, cursor.count());
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
}

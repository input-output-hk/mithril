use std::iter::repeat;

use sqlite::{ConnectionThreadSafe, Value};

use mithril_common::StdResult;
use mithril_persistence::sqlite::{Provider, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::CertificateRecord;

/// Query to insert [CertificateRecord] in the sqlite database
pub(crate) struct InsertCertificateRecordProvider<'conn> {
    connection: &'conn ConnectionThreadSafe,
}

impl<'conn> InsertCertificateRecordProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn ConnectionThreadSafe) -> Self {
        Self { connection }
    }

    fn get_insert_condition(&self, certificate_record: &CertificateRecord) -> WhereCondition {
        self.get_insert_many_condition(&vec![certificate_record.clone()])
    }

    fn get_insert_many_condition(
        &self,
        certificates_records: &[CertificateRecord],
    ) -> WhereCondition {
        let columns = "(\
        certificate_id, \
        parent_certificate_id, \
        message, \
        signature, \
        aggregate_verification_key, \
        epoch, \
        network, \
        immutable_file_number, \
        signed_entity_type_id, \
        signed_entity_beacon, \
        protocol_version, \
        protocol_parameters, \
        protocol_message, \
        signers, \
        initiated_at, \
        sealed_at)";
        let values_columns: Vec<&str> =
            repeat("(?*, ?*, ?*, ?*, ?*, ?*, ?*, ?*, ?*, ?*, ?*, ?*, ?*, ?*, ?*, ?*)")
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
                    Value::Integer(certificate_record.epoch.try_into().unwrap()),
                    Value::String(certificate_record.network.to_owned()),
                    Value::Integer(certificate_record.immutable_file_number as i64),
                    Value::Integer(certificate_record.signed_entity_type.index() as i64),
                    Value::String(
                        certificate_record
                            .signed_entity_type
                            .get_json_beacon()
                            .unwrap(),
                    ),
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

    pub fn persist(&self, certificate_record: CertificateRecord) -> StdResult<CertificateRecord> {
        let filters = self.get_insert_condition(&certificate_record);

        let entity = self.find(filters)?.next().unwrap_or_else(|| {
            panic!(
                "No entity returned by the persister, certificate_record = {certificate_record:#?}"
            )
        });

        Ok(entity)
    }

    pub fn persist_many(
        &self,
        certificates_records: Vec<CertificateRecord>,
    ) -> StdResult<Vec<CertificateRecord>> {
        if certificates_records.is_empty() {
            Ok(vec![])
        } else {
            let filters = self.get_insert_many_condition(&certificates_records);
            Ok(self.find(filters)?.collect())
        }
    }
}

impl<'conn> Provider<'conn> for InsertCertificateRecordProvider<'conn> {
    type Entity = CertificateRecord;

    fn get_connection(&'conn self) -> &'conn ConnectionThreadSafe {
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
    use sqlite::Connection;

    use mithril_common::crypto_helper::tests_setup::setup_certificate_chain;
    use mithril_common::entities::Certificate;

    use crate::database::test_helper::{
        apply_all_migrations_to_db, disable_foreign_key_support, insert_certificate_records,
    };

    use super::*;

    pub fn setup_certificate_db(
        connection: &ConnectionThreadSafe,
        certificates: Vec<Certificate>,
    ) -> StdResult<()> {
        apply_all_migrations_to_db(connection)?;
        disable_foreign_key_support(connection)?;
        insert_certificate_records(connection, certificates);
        Ok(())
    }

    #[test]
    fn test_insert_certificate_record() {
        let (certificates, _) = setup_certificate_chain(5, 2);

        let connection = Connection::open_thread_safe(":memory:").unwrap();
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

        let connection = Connection::open_thread_safe(":memory:").unwrap();
        setup_certificate_db(&connection, Vec::new()).unwrap();

        let provider = InsertCertificateRecordProvider::new(&connection);
        let certificates_records_saved = provider
            .persist_many(certificates_records.clone())
            .expect("saving many records should not fail");

        assert_eq!(certificates_records, certificates_records_saved);
    }
}

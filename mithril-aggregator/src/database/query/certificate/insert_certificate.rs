use std::iter::repeat;

use sqlite::Value;

use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::CertificateRecord;

/// Query to insert [CertificateRecord] in the sqlite database
pub struct InsertCertificateRecordQuery {
    condition: WhereCondition,
}

impl InsertCertificateRecordQuery {
    pub fn one(certificate_record: CertificateRecord) -> Self {
        Self::many(vec![certificate_record])
    }

    pub fn many(certificates_records: Vec<CertificateRecord>) -> Self {
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
            .into_iter()
            .flat_map(|certificate_record| {
                vec![
                    Value::String(certificate_record.certificate_id),
                    match certificate_record.parent_certificate_id {
                        Some(parent_certificate_id) => Value::String(parent_certificate_id),
                        None => Value::Null,
                    },
                    Value::String(certificate_record.message),
                    Value::String(certificate_record.signature),
                    Value::String(certificate_record.aggregate_verification_key),
                    Value::Integer(certificate_record.epoch.try_into().unwrap()),
                    Value::String(certificate_record.network),
                    Value::Integer(certificate_record.immutable_file_number as i64),
                    Value::Integer(certificate_record.signed_entity_type.index() as i64),
                    Value::String(
                        certificate_record
                            .signed_entity_type
                            .get_json_beacon()
                            .unwrap(),
                    ),
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
            .collect();

        let condition = WhereCondition::new(
            format!("{columns} values {}", values_columns.join(", ")).as_str(),
            values,
        );

        Self { condition }
    }
}

impl Query for InsertCertificateRecordQuery {
    type Entity = CertificateRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
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
    use mithril_common::crypto_helper::tests_setup::setup_certificate_chain;
    use mithril_persistence::sqlite::ConnectionExtensions;

    use crate::database::test_helper::main_db_connection;

    use super::*;

    #[test]
    fn test_insert_certificate_record() {
        let (certificates, _) = setup_certificate_chain(5, 2);

        let connection = main_db_connection().unwrap();

        for certificate in certificates {
            let certificate_record: CertificateRecord = certificate.into();
            let certificate_record_saved = connection
                .fetch_first(InsertCertificateRecordQuery::one(
                    certificate_record.clone(),
                ))
                .unwrap();
            assert_eq!(Some(certificate_record), certificate_record_saved);
        }
    }

    #[test]
    fn test_insert_many_certificates_records() {
        let (certificates, _) = setup_certificate_chain(5, 2);
        let certificates_records: Vec<CertificateRecord> =
            certificates.into_iter().map(|cert| cert.into()).collect();

        let connection = main_db_connection().unwrap();

        let certificates_records_saved: Vec<CertificateRecord> = connection
            .fetch_collect(InsertCertificateRecordQuery::many(
                certificates_records.clone(),
            ))
            .expect("saving many records should not fail");

        assert_eq!(certificates_records, certificates_records_saved);
    }
}

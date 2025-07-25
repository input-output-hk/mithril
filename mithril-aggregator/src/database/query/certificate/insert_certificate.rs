use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::CertificateRecord;

use super::conditions;

/// Query to insert [CertificateRecord] in the sqlite database
pub struct InsertCertificateRecordQuery {
    condition: WhereCondition,
}

impl InsertCertificateRecordQuery {
    pub fn one(certificate_record: CertificateRecord) -> Self {
        Self::many(vec![certificate_record])
    }

    pub fn many(certificates_records: Vec<CertificateRecord>) -> Self {
        Self {
            condition: conditions::insert_many(certificates_records),
        }
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
    use mithril_common::test::crypto_helper::setup_certificate_chain;
    use mithril_persistence::sqlite::ConnectionExtensions;

    use crate::database::test_helper::main_db_connection;

    use super::*;

    #[test]
    fn test_insert_certificate_record() {
        let certificates = setup_certificate_chain(5, 2);

        let connection = main_db_connection().unwrap();

        for certificate in certificates.certificates_chained {
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
        let certificates = setup_certificate_chain(5, 2);
        let certificates_records: Vec<CertificateRecord> = certificates.into();

        let connection = main_db_connection().unwrap();

        let certificates_records_saved: Vec<CertificateRecord> = connection
            .fetch_collect(InsertCertificateRecordQuery::many(
                certificates_records.clone(),
            ))
            .expect("saving many records should not fail");

        assert_eq!(certificates_records, certificates_records_saved);
    }
}

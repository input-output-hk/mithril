use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::CertificateRecord;

use super::conditions;

/// Query to insert or replace [CertificateRecord] in the sqlite database
pub struct InsertOrReplaceCertificateRecordQuery {
    condition: WhereCondition,
}

impl InsertOrReplaceCertificateRecordQuery {
    pub fn many(certificates_records: Vec<CertificateRecord>) -> Self {
        Self {
            condition: conditions::insert_many(certificates_records),
        }
    }
}

impl Query for InsertOrReplaceCertificateRecordQuery {
    type Entity = CertificateRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection()
            .expand(SourceAlias::new(&[("{:certificate:}", "certificate")]));

        format!("insert or replace into certificate {condition} returning {projection}")
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::crypto_helper::tests_setup::setup_certificate_chain;
    use mithril_common::entities::Epoch;
    use mithril_common::test_utils::fake_data;
    use mithril_persistence::sqlite::ConnectionExtensions;

    use crate::database::query::{GetCertificateRecordQuery, InsertCertificateRecordQuery};
    use crate::database::test_helper::main_db_connection;

    use super::*;

    #[test]
    fn test_insert_many_certificates_records_in_empty_db() {
        let certificates = setup_certificate_chain(5, 2);
        let certificates_records: Vec<CertificateRecord> = certificates.into();

        let connection = main_db_connection().unwrap();

        let certificates_records_saved: Vec<CertificateRecord> = connection
            .fetch_collect(InsertOrReplaceCertificateRecordQuery::many(
                certificates_records.clone(),
            ))
            .expect("saving many records should not fail");

        assert_eq!(certificates_records, certificates_records_saved);

        // Check insertion order
        let all_records: Vec<CertificateRecord> =
            connection.fetch_collect(GetCertificateRecordQuery::all()).unwrap();
        assert_eq!(
            certificates_records.into_iter().rev().collect::<Vec<_>>(),
            all_records
        );
    }

    #[test]
    fn test_replace_one_certificate_record() {
        let certificate_record = CertificateRecord {
            epoch: Epoch(12),
            ..fake_data::certificate("hash").into()
        };

        let connection = main_db_connection().unwrap();
        let certificate_record_saved = connection
            .fetch_first(InsertCertificateRecordQuery::one(
                certificate_record.clone(),
            ))
            .unwrap();
        assert_eq!(Some(Epoch(12)), certificate_record_saved.map(|r| r.epoch));

        let modified_certificate_record = CertificateRecord {
            epoch: Epoch(23),
            ..certificate_record
        };
        let certificate_record_saved = connection
            .fetch_first(InsertOrReplaceCertificateRecordQuery::many(vec![
                modified_certificate_record.clone(),
            ]))
            .unwrap();
        assert_eq!(Some(Epoch(23)), certificate_record_saved.map(|r| r.epoch));

        let all_records_cursor = connection.fetch(GetCertificateRecordQuery::all()).unwrap();
        assert_eq!(1, all_records_cursor.count());
    }

    #[test]
    fn test_insert_and_replace_many_certificate_record() {
        let initial_records: Vec<CertificateRecord> = vec![
            fake_data::genesis_certificate("genesis").into(),
            fake_data::certificate("hash1").into(),
            fake_data::certificate("hash2").into(),
        ];
        let connection = main_db_connection().unwrap();

        let cursor = connection
            .fetch(InsertCertificateRecordQuery::many(initial_records.clone()))
            .unwrap();
        assert_eq!(3, cursor.count());

        let modified_and_added_records = vec![
            CertificateRecord {
                epoch: Epoch(23),
                ..initial_records[1].clone()
            },
            CertificateRecord {
                epoch: Epoch(41),
                ..initial_records[2].clone()
            },
            fake_data::certificate("hash3").into(),
            fake_data::certificate("hash4").into(),
        ];
        connection
            .fetch_first(InsertOrReplaceCertificateRecordQuery::many(
                modified_and_added_records.clone(),
            ))
            .unwrap();

        let all_records: Vec<CertificateRecord> =
            connection.fetch_collect(GetCertificateRecordQuery::all()).unwrap();
        assert_eq!(5, all_records.len());
        // Important: Order is also checked
        assert_eq!(
            all_records.into_iter().rev().collect::<Vec<_>>(),
            [vec![initial_records[0].clone()], modified_and_added_records].concat()
        );
    }
}

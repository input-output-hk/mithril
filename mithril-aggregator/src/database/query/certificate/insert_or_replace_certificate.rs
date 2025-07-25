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
    use std::collections::HashMap;

    use mithril_common::entities::Epoch;
    use mithril_common::test::{crypto_helper::setup_certificate_chain, double::fake_data};
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
        let tested_records: HashMap<_, CertificateRecord> = HashMap::from([
            (
                "cert1-genesis",
                fake_data::genesis_certificate("genesis").into(),
            ),
            ("cert2", fake_data::certificate("cert2").into()),
            (
                "cert2-modified",
                CertificateRecord {
                    epoch: Epoch(14),
                    ..fake_data::certificate("cert2").into()
                },
            ),
            ("cert3", fake_data::certificate("cert3").into()),
            ("cert4", fake_data::certificate("cert4").into()),
            (
                "cert4-modified",
                CertificateRecord {
                    epoch: Epoch(32),
                    ..fake_data::certificate("cert4").into()
                },
            ),
            ("cert5", fake_data::certificate("cert5").into()),
        ]);
        let connection = main_db_connection().unwrap();

        let cursor = connection
            .fetch(InsertCertificateRecordQuery::many(vec![
                tested_records["cert1-genesis"].clone(),
                tested_records["cert2"].clone(),
                tested_records["cert3"].clone(),
                tested_records["cert4"].clone(),
                tested_records["cert5"].clone(),
            ]))
            .unwrap();
        assert_eq!(5, cursor.count());

        let cursor = connection
            .fetch(InsertOrReplaceCertificateRecordQuery::many(vec![
                tested_records["cert1-genesis"].clone(),
                tested_records["cert2-modified"].clone(),
                tested_records["cert3"].clone(),
                tested_records["cert4-modified"].clone(),
            ]))
            .unwrap();
        assert_eq!(4, cursor.count());

        let all_records: Vec<CertificateRecord> =
            connection.fetch_collect(GetCertificateRecordQuery::all()).unwrap();
        assert_eq!(5, all_records.len());
        assert_eq!(
            all_records,
            vec![
                tested_records["cert4-modified"].clone(),
                tested_records["cert3"].clone(),
                tested_records["cert2-modified"].clone(),
                tested_records["cert1-genesis"].clone(),
                // Since the cert5 was not in the Insert/replace query, it now has a lower rowid and shows first
                tested_records["cert5"].clone(),
            ]
        );
    }
}

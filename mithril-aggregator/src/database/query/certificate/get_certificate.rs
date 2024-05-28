use sqlite::Value;

#[cfg(test)]
use mithril_common::entities::Epoch;
#[cfg(test)]
use mithril_common::StdResult;
use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::CertificateRecord;

/// Simple queries to retrieve [CertificateRecord] from the sqlite database.
pub struct GetCertificateRecordQuery {
    condition: WhereCondition,
}

impl GetCertificateRecordQuery {
    pub fn all() -> Self {
        Self {
            condition: WhereCondition::default(),
        }
    }

    pub fn by_certificate_id(certificate_id: &str) -> Self {
        Self {
            condition: WhereCondition::new(
                "certificate_id = ?*",
                vec![Value::String(certificate_id.to_owned())],
            ),
        }
    }

    #[cfg(test)]
    pub fn by_epoch(epoch: Epoch) -> StdResult<Self> {
        Ok(Self {
            condition: WhereCondition::new("epoch = ?*", vec![Value::Integer(epoch.try_into()?)]),
        })
    }
}

impl Query for GetCertificateRecordQuery {
    type Entity = CertificateRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:certificate:}", "c")]);
        let projection = Self::Entity::get_projection().expand(aliases);
        format!("select {projection} from certificate as c where {condition} order by ROWID desc")
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::crypto_helper::tests_setup::setup_certificate_chain;
    use mithril_persistence::sqlite::ConnectionExtensions;

    use crate::database::test_helper::{insert_certificate_records, main_db_connection};

    use super::*;

    #[test]
    fn test_get_certificate_records_by_epoch() {
        let (certificates, _) = setup_certificate_chain(20, 7);

        let connection = main_db_connection().unwrap();
        insert_certificate_records(&connection, certificates.clone());

        let certificate_records: Vec<CertificateRecord> = connection
            .fetch_collect(GetCertificateRecordQuery::by_epoch(Epoch(1)).unwrap())
            .unwrap();
        let expected_certificate_records: Vec<CertificateRecord> = certificates
            .iter()
            .filter_map(|c| (c.epoch == Epoch(1)).then_some(c.to_owned().into()))
            .rev()
            .collect();
        assert_eq!(expected_certificate_records, certificate_records);

        let certificate_records: Vec<CertificateRecord> = connection
            .fetch_collect(GetCertificateRecordQuery::by_epoch(Epoch(3)).unwrap())
            .unwrap();
        let expected_certificate_records: Vec<CertificateRecord> = certificates
            .iter()
            .filter_map(|c| (c.epoch == Epoch(3)).then_some(c.to_owned().into()))
            .rev()
            .collect();
        assert_eq!(expected_certificate_records, certificate_records);

        let cursor = connection
            .fetch(GetCertificateRecordQuery::by_epoch(Epoch(5)).unwrap())
            .unwrap();
        assert_eq!(0, cursor.count());
    }

    #[test]
    fn test_get_all_certificate_records() {
        let (certificates, _) = setup_certificate_chain(5, 2);
        let expected_certificate_records: Vec<CertificateRecord> = certificates
            .iter()
            .map(|c| c.to_owned().into())
            .rev()
            .collect();

        let connection = main_db_connection().unwrap();
        insert_certificate_records(&connection, certificates.clone());

        let certificate_records: Vec<CertificateRecord> = connection
            .fetch_collect(GetCertificateRecordQuery::all())
            .unwrap();
        assert_eq!(expected_certificate_records, certificate_records);
    }
}

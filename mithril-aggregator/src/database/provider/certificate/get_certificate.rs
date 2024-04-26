use sqlite::{ConnectionThreadSafe, Value};

#[cfg(test)]
use mithril_common::entities::Epoch;
use mithril_common::StdResult;
use mithril_persistence::sqlite::{
    EntityCursor, GetAllCondition, Provider, SourceAlias, SqLiteEntity, WhereCondition,
};

use crate::database::record::CertificateRecord;

/// Simple queries to retrieve [CertificateRecord] from the sqlite database.
pub struct GetCertificateRecordProvider<'client> {
    client: &'client ConnectionThreadSafe,
}

impl<'client> GetCertificateRecordProvider<'client> {
    /// Create a new provider
    pub fn new(client: &'client ConnectionThreadSafe) -> Self {
        Self { client }
    }

    fn condition_by_certificate_id(&self, certificate_id: &str) -> StdResult<WhereCondition> {
        Ok(WhereCondition::new(
            "certificate_id = ?*",
            vec![Value::String(certificate_id.to_owned())],
        ))
    }

    #[cfg(test)]
    fn condition_by_epoch(&self, epoch: &Epoch) -> StdResult<WhereCondition> {
        Ok(WhereCondition::new(
            "epoch = ?*",
            vec![Value::Integer(epoch.try_into()?)],
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

    #[cfg(test)]
    /// Get CertificateRecords for a given Epoch.
    pub fn get_by_epoch(&self, epoch: &Epoch) -> StdResult<EntityCursor<CertificateRecord>> {
        let filters = self.condition_by_epoch(epoch)?;
        let certificate_record = self.find(filters)?;

        Ok(certificate_record)
    }
}

impl GetAllCondition for GetCertificateRecordProvider<'_> {}

impl<'client> Provider<'client> for GetCertificateRecordProvider<'client> {
    type Entity = CertificateRecord;

    fn get_connection(&'client self) -> &'client ConnectionThreadSafe {
        self.client
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
    use mithril_persistence::sqlite::GetAllProvider;

    use crate::database::test_helper::{insert_certificate_records, main_db_connection};

    use super::*;

    #[test]
    fn test_get_certificate_records() {
        let (certificates, _) = setup_certificate_chain(20, 7);

        let connection = main_db_connection().unwrap();
        insert_certificate_records(&connection, certificates.clone());

        let provider = GetCertificateRecordProvider::new(&connection);

        let certificate_records: Vec<CertificateRecord> =
            provider.get_by_epoch(&Epoch(1)).unwrap().collect();
        let expected_certificate_records: Vec<CertificateRecord> = certificates
            .iter()
            .filter_map(|c| (c.epoch == Epoch(1)).then_some(c.to_owned().into()))
            .rev()
            .collect();
        assert_eq!(expected_certificate_records, certificate_records);

        let certificate_records: Vec<CertificateRecord> =
            provider.get_by_epoch(&Epoch(3)).unwrap().collect();
        let expected_certificate_records: Vec<CertificateRecord> = certificates
            .iter()
            .filter_map(|c| (c.epoch == Epoch(3)).then_some(c.to_owned().into()))
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
}

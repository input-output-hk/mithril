use std::sync::Arc;

use anyhow::anyhow;
use async_trait::async_trait;
use sqlite::ConnectionThreadSafe;

use mithril_common::certificate_chain::{CertificateRetriever, CertificateRetrieverError};
use mithril_common::entities::{Certificate, Epoch};
use mithril_common::StdResult;
use mithril_persistence::sqlite::Provider;

use crate::database::provider::{
    DeleteCertificateProvider, GetCertificateRecordProvider, InsertCertificateRecordProvider,
    MasterCertificateProvider,
};
use crate::database::record::CertificateRecord;

/// Database frontend API for Certificate queries.
pub struct CertificateRepository {
    connection: Arc<ConnectionThreadSafe>,
}

impl CertificateRepository {
    /// Instantiate a new repository
    pub fn new(connection: Arc<ConnectionThreadSafe>) -> Self {
        Self { connection }
    }

    /// Return the certificate corresponding to the given hash if any.
    pub async fn get_certificate<T>(&self, hash: &str) -> StdResult<Option<T>>
    where
        T: From<CertificateRecord>,
    {
        let provider = GetCertificateRecordProvider::new(&self.connection);
        let mut cursor = provider.get_by_certificate_id(hash)?;

        Ok(cursor.next().map(|v| v.into()))
    }

    /// Return the latest certificates.
    pub async fn get_latest_certificates<T>(&self, last_n: usize) -> StdResult<Vec<T>>
    where
        T: From<CertificateRecord>,
    {
        let provider = GetCertificateRecordProvider::new(&self.connection);
        let cursor = provider.get_all()?;

        Ok(cursor.take(last_n).map(|v| v.into()).collect())
    }

    /// Return the first certificate signed per epoch as the reference
    /// certificate for this Epoch. This will be the parent certificate for all
    /// other certificates issued within this Epoch.
    pub async fn get_master_certificate_for_epoch<T>(&self, epoch: Epoch) -> StdResult<Option<T>>
    where
        T: From<CertificateRecord>,
    {
        let provider = MasterCertificateProvider::new(&self.connection);
        let mut cursor = provider.find(provider.get_master_certificate_condition(epoch))?;

        Ok(cursor.next().map(|c| c.into()))
    }

    /// Create a new certificate in the database.
    pub async fn create_certificate(&self, certificate: Certificate) -> StdResult<Certificate> {
        let provider = InsertCertificateRecordProvider::new(&self.connection);

        provider.persist(certificate.into()).map(|r| r.into())
    }

    /// Create many certificates at once in the database.
    pub async fn create_many_certificates(
        &self,
        certificates: Vec<Certificate>,
    ) -> StdResult<Vec<Certificate>> {
        let provider = InsertCertificateRecordProvider::new(&self.connection);
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

        let provider = DeleteCertificateProvider::new(&self.connection);
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
            .map_err(|e| CertificateRetrieverError(anyhow!(e)))?
            .ok_or(CertificateRetrieverError(anyhow!(format!(
                "Certificate does not exist: '{}'",
                certificate_hash
            ))))
    }
}

#[cfg(test)]
mod tests {
    use sqlite::{Connection, Value};

    use mithril_common::crypto_helper::tests_setup::setup_certificate_chain;

    use crate::database::test_helper::{
        apply_all_migrations_to_db, disable_foreign_key_support, insert_certificate_records,
    };
    use crate::dependency_injection::DependenciesBuilder;
    use crate::Configuration;

    use super::*;

    #[tokio::test]
    async fn persisting_many_without_any_records_dont_crash() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        apply_all_migrations_to_db(&connection).unwrap();
        disable_foreign_key_support(&connection).unwrap();
        let repository: CertificateRepository = CertificateRepository::new(Arc::new(connection));

        let modified_records = repository
            .create_many_certificates(Vec::new())
            .await
            .expect("saving many records should not fail even with an empty list");
        let current_records = repository
            .get_latest_certificates::<CertificateRecord>(usize::MAX)
            .await
            .unwrap();

        assert_eq!(0, modified_records.len());
        assert_eq!(0, current_records.len());
    }

    #[tokio::test]
    async fn master_certificate_condition() {
        let connection = Connection::open_thread_safe(":memory:").unwrap();
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
        insert_certificate_records(&connection, certificates.clone());

        let repository: CertificateRepository = CertificateRepository::new(connection);
        let certificate = repository
            .get_certificate::<Certificate>("whatever")
            .await
            .unwrap();
        assert!(certificate.is_none());

        let certificate = repository
            .get_certificate::<Certificate>(&expected_hash)
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
        insert_certificate_records(&connection, certificates.clone());

        let repository = CertificateRepository::new(connection);
        let latest_certificates = repository
            .get_latest_certificates(certificates.len())
            .await
            .unwrap();
        let expected: Vec<Certificate> = certificates.into_iter().rev().collect();

        assert_eq!(expected, latest_certificates);
    }

    #[tokio::test]
    async fn get_master_certificate_no_certificate_recorded_returns_none() {
        let mut deps = DependenciesBuilder::new(Configuration::new_sample());
        let connection = deps.get_sqlite_connection().await.unwrap();

        let repository: CertificateRepository = CertificateRepository::new(connection);
        let certificate = repository
            .get_master_certificate_for_epoch::<Certificate>(Epoch(1))
            .await
            .unwrap();

        assert_eq!(None, certificate);
    }

    #[tokio::test]
    async fn get_master_certificate_one_cert_in_current_epoch_recorded_returns_that_one() {
        let mut deps = DependenciesBuilder::new(Configuration::new_sample());
        let connection = deps.get_sqlite_connection().await.unwrap();
        let certificate = CertificateRecord::dummy_genesis("1", Epoch(1), 1);
        let expected_certificate: Certificate = certificate.clone().into();
        insert_certificate_records(&connection, vec![certificate]);

        let repository: CertificateRepository = CertificateRepository::new(connection);
        let certificate = repository
            .get_master_certificate_for_epoch::<Certificate>(Epoch(1))
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
            CertificateRecord::dummy_genesis("1", Epoch(1), 1),
            CertificateRecord::dummy_db_snapshot("2", "1", Epoch(1), 2),
            CertificateRecord::dummy_db_snapshot("3", "1", Epoch(1), 3),
        ];
        let expected_certificate: Certificate = certificates.first().unwrap().clone().into();
        insert_certificate_records(&connection, certificates);

        let repository: CertificateRepository = CertificateRepository::new(connection);
        let certificate = repository
            .get_master_certificate_for_epoch::<Certificate>(Epoch(1))
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
            CertificateRecord::dummy_genesis("1", Epoch(1), 1),
            CertificateRecord::dummy_db_snapshot("2", "1", Epoch(1), 2),
            CertificateRecord::dummy_db_snapshot("3", "1", Epoch(1), 3),
        ];
        let expected_certificate: Certificate = certificates.first().unwrap().clone().into();
        insert_certificate_records(&connection, certificates);

        let repository: CertificateRepository = CertificateRepository::new(connection);
        let certificate = repository
            .get_master_certificate_for_epoch::<Certificate>(Epoch(2))
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
            CertificateRecord::dummy_genesis("1", Epoch(1), 1),
            CertificateRecord::dummy_db_snapshot("2", "1", Epoch(1), 2),
            CertificateRecord::dummy_db_snapshot("3", "1", Epoch(1), 3),
            CertificateRecord::dummy_db_snapshot("4", "1", Epoch(2), 4),
        ];
        let expected_certificate: Certificate = certificates.last().unwrap().clone().into();
        insert_certificate_records(&connection, certificates);

        let repository: CertificateRepository = CertificateRepository::new(connection);
        let certificate = repository
            .get_master_certificate_for_epoch::<Certificate>(Epoch(2))
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
            CertificateRecord::dummy_genesis("1", Epoch(1), 1),
            CertificateRecord::dummy_db_snapshot("2", "1", Epoch(1), 2),
            CertificateRecord::dummy_db_snapshot("3", "1", Epoch(1), 3),
            CertificateRecord::dummy_db_snapshot("4", "1", Epoch(2), 4),
            CertificateRecord::dummy_db_snapshot("5", "4", Epoch(2), 5),
            CertificateRecord::dummy_db_snapshot("6", "4", Epoch(2), 6),
        ];
        let expected_certificate: Certificate = certificates.get(3).unwrap().clone().into();
        insert_certificate_records(&connection, certificates);

        let repository: CertificateRepository = CertificateRepository::new(connection);
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
            CertificateRecord::dummy_genesis("1", Epoch(1), 1),
            CertificateRecord::dummy_db_snapshot("2", "1", Epoch(1), 2),
            CertificateRecord::dummy_db_snapshot("3", "1", Epoch(1), 3),
        ];
        insert_certificate_records(&connection, certificates);

        let repository: CertificateRepository = CertificateRepository::new(connection);
        let certificate = repository
            .get_master_certificate_for_epoch::<Certificate>(Epoch(3))
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
            CertificateRecord::dummy_genesis("1", Epoch(1), 1),
            CertificateRecord::dummy_db_snapshot("2", "1", Epoch(1), 2),
            CertificateRecord::dummy_db_snapshot("3", "1", Epoch(1), 3),
            CertificateRecord::dummy_genesis("4", Epoch(1), 3),
        ];
        let expected_certificate: Certificate = certificates.last().unwrap().clone().into();
        insert_certificate_records(&connection, certificates);

        let repository: CertificateRepository = CertificateRepository::new(connection);
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
            CertificateRecord::dummy_genesis("1", Epoch(1), 1),
            CertificateRecord::dummy_db_snapshot("2", "1", Epoch(1), 2),
            CertificateRecord::dummy_db_snapshot("3", "1", Epoch(1), 2),
            CertificateRecord::dummy_db_snapshot("4", "1", Epoch(2), 4),
            CertificateRecord::dummy_db_snapshot("5", "1", Epoch(2), 5),
            CertificateRecord::dummy_genesis("6", Epoch(2), 5),
        ];
        let expected_certificate: Certificate = certificates.last().unwrap().clone().into();
        insert_certificate_records(&connection, certificates);

        let repository: CertificateRepository = CertificateRepository::new(connection);
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
            CertificateRecord::dummy_genesis("1", Epoch(1), 1),
            CertificateRecord::dummy_db_snapshot("2", "1", Epoch(1), 2),
            CertificateRecord::dummy_db_snapshot("3", "1", Epoch(1), 3),
            CertificateRecord::dummy_genesis("4", Epoch(2), 3),
        ];
        let expected_certificate: Certificate = certificates.last().unwrap().clone().into();
        insert_certificate_records(&connection, certificates);

        let repository: CertificateRepository = CertificateRepository::new(connection);
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
        let epoch = &certificates[2].epoch;
        let mut deps = DependenciesBuilder::new(Configuration::new_sample());
        let connection = deps.get_sqlite_connection().await.unwrap();
        insert_certificate_records(&connection, certificates.clone());

        let repository: CertificateRepository = CertificateRepository::new(connection);
        let certificate = repository
            .get_master_certificate_for_epoch::<Certificate>(*epoch)
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
        let repository: CertificateRepository = CertificateRepository::new(connection);
        let certificate = repository
            .create_certificate(certificates[4].clone())
            .await
            .unwrap();

        assert_eq!(certificates[4].hash, certificate.hash);
        {
            let connection = deps.get_sqlite_connection().await.unwrap();
            let provider = GetCertificateRecordProvider::new(&connection);
            let mut cursor = provider
                .get_by_certificate_id(&certificates[4].hash)
                .unwrap();
            let cert = cursor
                .next()
                .expect("There should be a certificate in the database with this hash ID.");

            assert_eq!(certificates[4].hash, cert.certificate_id);
        }
    }

    #[tokio::test]
    async fn delete_only_given_certificates() {
        let mut deps = DependenciesBuilder::new(Configuration::new_sample());
        let connection = deps.get_sqlite_connection().await.unwrap();
        let repository = CertificateRepository::new(connection.clone());
        let records = vec![
            CertificateRecord::dummy_genesis("1", Epoch(1), 1),
            CertificateRecord::dummy_db_snapshot("2", "1", Epoch(1), 2),
            CertificateRecord::dummy_db_snapshot("3", "1", Epoch(1), 3),
        ];
        insert_certificate_records(&connection, records.clone());
        let certificates: Vec<Certificate> = records.into_iter().map(|c| c.into()).collect();

        // Delete all records except the first
        repository
            .delete_certificates(
                &certificates
                    .iter()
                    .filter(|r| r.hash != "1")
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

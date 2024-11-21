use async_trait::async_trait;
use std::sync::Arc;

use mithril_common::{entities::CertificatePending, StdResult};
use mithril_persistence::sqlite::{ConnectionExtensions, SqliteConnection};

use crate::{
    database::query::{
        DeletePendingCertificateRecordQuery, GetPendingCertificateRecordQuery,
        SavePendingCertificateRecordQuery,
    },
    store::CertificatePendingStorer,
};

/// Pending certificate repository
pub struct CertificatePendingRepository {
    connection: Arc<SqliteConnection>,
}

impl CertificatePendingRepository {
    /// Create a new CertificatePendingRepository service
    pub fn new(connection: Arc<SqliteConnection>) -> Self {
        Self { connection }
    }
}

#[async_trait]
impl CertificatePendingStorer for CertificatePendingRepository {
    /// Fetch the current [CertificatePending] if any.
    async fn get(&self) -> StdResult<Option<CertificatePending>> {
        Ok(self
            .connection
            .fetch_first(GetPendingCertificateRecordQuery::get())?
            .map(Into::into))
    }

    /// Save the given [CertificatePending].
    async fn save(&self, certificate: CertificatePending) -> StdResult<()> {
        self.connection
            .apply(SavePendingCertificateRecordQuery::save(certificate.into()))?;

        Ok(())
    }

    /// Remove the current [CertificatePending] if any.
    async fn remove(&self) -> StdResult<Option<CertificatePending>> {
        Ok(self
            .connection
            .fetch_first(DeletePendingCertificateRecordQuery::get())?
            .map(Into::into))
    }
}

#[cfg(test)]
mod test {

    use crate::database::test_helper::{main_db_connection, FakeStoreAdapter};

    use super::*;

    use mithril_common::entities::{Epoch, SignedEntityType};
    use mithril_common::test_utils::fake_data;
    use mithril_persistence::sqlite::ConnectionBuilder;

    async fn get_certificate_pending_store(is_populated: bool) -> CertificatePendingRepository {
        let connection = Arc::new(main_db_connection().unwrap());

        let store = CertificatePendingRepository::new(connection);
        if is_populated {
            let certificate_pending = CertificatePending::new(
                Epoch(0),
                SignedEntityType::dummy(),
                fake_data::protocol_parameters(),
                fake_data::protocol_parameters(),
                fake_data::signers(4),
                fake_data::signers(5),
            );

            store.save(certificate_pending).await.unwrap();
        }
        store
    }

    #[tokio::test]
    async fn get_certificate_pending_with_existing_certificate() {
        let store = get_certificate_pending_store(true).await;
        let result = store.get().await.unwrap();

        assert!(result.is_some());
    }

    #[tokio::test]
    async fn get_certificate_pending_with_no_existing_certificate() {
        let store = get_certificate_pending_store(false).await;
        let result = store.get().await.unwrap();

        assert!(result.is_none());
    }

    #[tokio::test]
    async fn save_certificate_pending_once() {
        let store = get_certificate_pending_store(false).await;
        let signed_entity_type = SignedEntityType::dummy();
        let certificate_pending = CertificatePending::new(
            Epoch(2),
            signed_entity_type,
            fake_data::protocol_parameters(),
            fake_data::protocol_parameters(),
            fake_data::signers(1),
            fake_data::signers(2),
        );

        assert!(store.save(certificate_pending).await.is_ok());
        assert!(store.get().await.unwrap().is_some());
    }

    #[tokio::test]
    async fn update_certificate_pending() {
        let store = get_certificate_pending_store(true).await;
        let certificate_pending = store.get().await.unwrap().unwrap();

        assert!(store.save(certificate_pending).await.is_ok());
    }

    #[tokio::test]
    async fn remove_certificate_pending() {
        let store = get_certificate_pending_store(true).await;
        let epoch = Epoch(0);
        let certificate_pending = store.remove().await.unwrap().unwrap();

        assert_eq!(epoch, certificate_pending.epoch);
        assert!(store.get().await.unwrap().is_none());
    }

    #[tokio::test]
    async fn should_migrate_data_from_adapter() {
        let certificate_pending = CertificatePending::new(
            Epoch(0),
            SignedEntityType::dummy(),
            fake_data::protocol_parameters(),
            fake_data::protocol_parameters(),
            fake_data::signers(4),
            fake_data::signers(5),
        );

        let migrations = crate::database::migration::get_migrations();

        let connection = Arc::new(ConnectionBuilder::open_memory().build().unwrap());
        let pending_certificate_adapter =
            FakeStoreAdapter::new(connection.clone(), "pending_certificate");
        pending_certificate_adapter.create_table();

        ConnectionBuilder::open_memory()
            .apply_migrations(
                &connection,
                migrations
                    .clone()
                    .into_iter()
                    .filter(|m| m.version < 33)
                    .collect::<Vec<_>>(),
            )
            .unwrap();

        assert!(connection
            .prepare("select key_hash from pending_certificate;")
            .is_ok());

        // Here we can add some data with the old schema.
        pending_certificate_adapter
            .store_record(
                "Certificate",
                &"certificate_pending".to_string(),
                &certificate_pending,
            )
            .unwrap();

        assert!(pending_certificate_adapter.is_key_hash_exist("Certificate"));

        // We finish the migration
        ConnectionBuilder::open_memory()
            .apply_migrations(&connection, migrations)
            .unwrap();

        assert!(connection
            .prepare("select key_hash from certificate_pending;")
            .is_err());
        assert!(connection
            .prepare("select * from pending_certificate;")
            .is_ok());

        let value: i64 = connection
            .query_single_cell("select count(*) from pending_certificate", &[])
            .unwrap();
        assert_eq!(value, 1);

        // We can check that data are migrated.
        let store = CertificatePendingRepository::new(connection);
        let pending_certificate = store.get().await.unwrap();

        assert_eq!(pending_certificate, Some(certificate_pending));
    }
}

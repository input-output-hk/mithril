use std::sync::Arc;

use mithril_common::{entities::CertificatePending, StdResult};
use mithril_persistence::sqlite::{ConnectionExtensions, SqliteConnection};

use crate::database::query::{
    DeletePendingCertificateRecordQuery, GetPendingCertificateRecordQuery,
    SavePendingCertificateRecordQuery,
};

pub struct CertificatePendingRepository {
    connection: Arc<SqliteConnection>,
}

impl CertificatePendingRepository {
    /// Create a new CertificatePendingRepository service
    pub fn new(connection: Arc<SqliteConnection>) -> Self {
        Self { connection }
    }

    /// Fetch the current [CertificatePending] if any.
    pub async fn get(&self) -> StdResult<Option<CertificatePending>> {
        Ok(self
            .connection
            .fetch_first(GetPendingCertificateRecordQuery::get())?
            .map(Into::into))
    }

    /// Save the given [CertificatePending].
    pub async fn save(&self, certificate: CertificatePending) -> StdResult<()> {
        self.connection
            .apply(SavePendingCertificateRecordQuery::save(certificate.into()))?;

        Ok(())
    }

    /// Remove the current [CertificatePending] if any.
    pub async fn remove(&self) -> StdResult<Option<CertificatePending>> {
        Ok(self
            .connection
            .fetch_first(DeletePendingCertificateRecordQuery::get())?
            .map(Into::into))
    }
}

#[cfg(test)]
mod test {

    use crate::database::test_helper::main_db_connection;

    use super::*;

    use mithril_common::entities::{Epoch, SignedEntityType};
    use mithril_common::test_utils::fake_data;

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
}

use std::collections::HashMap;
use std::sync::Arc;

use async_trait::async_trait;
use chrono::Utc;

use mithril_common::StdResult;
use mithril_persistence::sqlite::SqliteConnection;

use crate::database::provider::{
    ImportSignerRecordProvider, RegisterSignerRecordProvider, SignerRecordProvider,
};
use crate::database::record::SignerRecord;
use crate::SignerRecorder;

#[cfg(test)]
use mockall::automock;

/// Service to get [SignerRecord].
#[cfg_attr(test, automock)]
#[async_trait]
pub trait SignerGetter: Sync + Send {
    /// Return all stored records.
    async fn get_all(&self) -> StdResult<Vec<SignerRecord>>;
}

/// Service to deal with signer (read & write).
pub struct SignerStore {
    connection: Arc<SqliteConnection>,
}

impl SignerStore {
    /// Create a new SignerStore service
    pub fn new(connection: Arc<SqliteConnection>) -> Self {
        Self { connection }
    }

    /// Import a signer in the database, its last_registered_at date will be left empty
    pub async fn import_signer(
        &self,
        signer_id: String,
        pool_ticker: Option<String>,
    ) -> StdResult<()> {
        let provider = ImportSignerRecordProvider::new(&self.connection);
        let created_at = Utc::now();
        let updated_at = created_at;
        let signer_record = SignerRecord {
            signer_id,
            pool_ticker,
            created_at,
            updated_at,
            last_registered_at: None,
        };
        provider.persist(signer_record)?;

        Ok(())
    }

    /// Create many signers at once in the database, their last_registered_at date will be left empty
    pub async fn import_many_signers(
        &self,
        pool_ticker_by_id: HashMap<String, Option<String>>,
    ) -> StdResult<()> {
        let provider = ImportSignerRecordProvider::new(&self.connection);

        let created_at = Utc::now();
        let updated_at = created_at;
        let signer_records: Vec<_> = pool_ticker_by_id
            .into_iter()
            .map(|(signer_id, pool_ticker)| SignerRecord {
                signer_id,
                pool_ticker,
                created_at,
                updated_at,
                last_registered_at: None,
            })
            .collect();

        provider.persist_many(signer_records)?;

        Ok(())
    }
}

#[async_trait]
impl SignerRecorder for SignerStore {
    async fn record_signer_registration(&self, signer_id: String) -> StdResult<()> {
        let provider = RegisterSignerRecordProvider::new(&self.connection);
        let created_at = Utc::now();
        let updated_at = created_at;
        let registered_at = Some(created_at);
        let signer_record = SignerRecord {
            signer_id,
            pool_ticker: None,
            created_at,
            updated_at,
            last_registered_at: registered_at,
        };
        provider.persist(signer_record)?;

        Ok(())
    }
}

#[async_trait]
impl SignerGetter for SignerStore {
    async fn get_all(&self) -> StdResult<Vec<SignerRecord>> {
        let provider = SignerRecordProvider::new(&self.connection);
        let cursor = provider.get_all()?;

        Ok(cursor.collect())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::database::test_helper::{apply_all_migrations_to_db, insert_signers};
    use sqlite::Connection;
    use std::collections::BTreeMap;

    pub fn setup_signer_db(
        connection: &SqliteConnection,
        records: Vec<SignerRecord>,
    ) -> StdResult<()> {
        apply_all_migrations_to_db(connection)?;
        insert_signers(connection, records)
    }

    #[tokio::test]
    async fn test_get_all_signers() {
        let signer_records = SignerRecord::fake_records(5);
        let expected: Vec<_> = signer_records.iter().rev().cloned().collect();
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        setup_signer_db(&connection, signer_records).unwrap();

        let store = SignerStore::new(Arc::new(connection));

        let stored_signers = store
            .get_all()
            .await
            .expect("getting all signers should not fail");

        assert_eq!(expected, stored_signers);
    }

    #[tokio::test]
    async fn test_signer_recorder() {
        let signer_records_fake = SignerRecord::fake_records(5);

        let connection = Connection::open_thread_safe(":memory:").unwrap();
        setup_signer_db(&connection, Vec::new()).unwrap();

        let connection = Arc::new(connection);
        let store_recorder = SignerStore::new(connection.clone());

        for signer_record in signer_records_fake.clone() {
            store_recorder
                .record_signer_registration(signer_record.signer_id.clone())
                .await
                .expect("record_signer_registration should not fail");
            let provider = SignerRecordProvider::new(&connection);
            let signer_records_stored: Vec<SignerRecord> = provider
                .get_by_signer_id(signer_record.signer_id)
                .unwrap()
                .collect::<Vec<_>>();
            assert_eq!(1, signer_records_stored.len());
            assert!(
                signer_records_stored
                    .iter()
                    .all(|s| s.last_registered_at.is_some()),
                "registering a signer should set the registration date"
            )
        }
    }

    #[tokio::test]
    async fn test_store_import_signer() {
        let signer_records_fake = SignerRecord::fake_records(5);

        let connection = Connection::open_thread_safe(":memory:").unwrap();
        setup_signer_db(&connection, Vec::new()).unwrap();

        let connection = Arc::new(connection);
        let store = SignerStore::new(connection.clone());

        for signer_record in signer_records_fake {
            store
                .import_signer(
                    signer_record.signer_id.clone(),
                    signer_record.pool_ticker.clone(),
                )
                .await
                .expect("import_signer should not fail");
            let provider = SignerRecordProvider::new(&connection);
            let signer_records_stored: Vec<SignerRecord> = provider
                .get_by_signer_id(signer_record.signer_id)
                .unwrap()
                .collect::<Vec<_>>();
            assert_eq!(
                signer_record.pool_ticker,
                signer_records_stored[0].to_owned().pool_ticker
            );
            assert!(
                signer_records_stored
                    .iter()
                    .all(|s| s.last_registered_at.is_none()),
                "imported signer should not have a registration date"
            )
        }
    }

    #[tokio::test]
    async fn test_store_import_many_signers() {
        let signers_fake: BTreeMap<_, _> = SignerRecord::fake_records(5)
            .into_iter()
            .map(|r| (r.signer_id, r.pool_ticker))
            .collect();

        let connection = Connection::open_thread_safe(":memory:").unwrap();
        setup_signer_db(&connection, Vec::new()).unwrap();
        let store = SignerStore::new(Arc::new(connection));

        store
            .import_many_signers(signers_fake.clone().into_iter().collect())
            .await
            .expect("import_many_signers should not fail");

        let signer_records_stored = store.get_all().await.unwrap();
        let signers_stored = signer_records_stored
            .iter()
            .cloned()
            .map(|r| (r.signer_id, r.pool_ticker))
            .collect();
        assert_eq!(signers_fake, signers_stored);
        assert!(
            signer_records_stored
                .iter()
                .all(|s| s.last_registered_at.is_none()),
            "imported signer should not have a registration date"
        );
    }
}

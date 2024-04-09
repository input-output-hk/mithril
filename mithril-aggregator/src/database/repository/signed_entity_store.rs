use anyhow::Context;
use std::sync::Arc;

use async_trait::async_trait;

use mithril_common::entities::SignedEntityTypeDiscriminants;
use mithril_common::StdResult;
use mithril_persistence::sqlite::SqliteConnection;
use mithril_persistence::store::adapter::AdapterError;

use crate::database::provider::{
    GetSignedEntityRecordProvider, InsertSignedEntityRecordProvider, UpdateSignedEntityProvider,
};
use crate::database::record::SignedEntityRecord;

#[cfg(test)]
use mockall::automock;

/// Signed entity storer trait
#[cfg_attr(test, automock)]
#[async_trait]
pub trait SignedEntityStorer: Sync + Send {
    /// Store a signed entity
    async fn store_signed_entity(&self, signed_entity: &SignedEntityRecord) -> StdResult<()>;

    /// Get signed entity type
    async fn get_signed_entity(
        &self,
        signed_entity_id: &str,
    ) -> StdResult<Option<SignedEntityRecord>>;

    /// Get signed entity type by certificate id
    async fn get_signed_entity_by_certificate_id(
        &self,
        certificate_hash: &str,
    ) -> StdResult<Option<SignedEntityRecord>>;

    /// Get signed entities type by certificates ids
    async fn get_signed_entities_by_certificates_ids<'a>(
        &self,
        certificates_ids: &[&'a str],
    ) -> StdResult<Vec<SignedEntityRecord>>;

    /// Get last signed entities by signed entity type
    async fn get_last_signed_entities_by_type(
        &self,
        signed_entity_type_id: &SignedEntityTypeDiscriminants,
        total: usize,
    ) -> StdResult<Vec<SignedEntityRecord>>;

    /// Perform an update for all the given signed entities.
    async fn update_signed_entities(
        &self,
        signed_entities: Vec<SignedEntityRecord>,
    ) -> StdResult<Vec<SignedEntityRecord>>;
}

/// Service to deal with signed_entity (read & write).
pub struct SignedEntityStore {
    connection: Arc<SqliteConnection>,
}

impl SignedEntityStore {
    /// Create a new SignedEntityStoreAdapter service
    pub fn new(connection: Arc<SqliteConnection>) -> Self {
        Self { connection }
    }
}

#[async_trait]
impl SignedEntityStorer for SignedEntityStore {
    async fn store_signed_entity(&self, signed_entity: &SignedEntityRecord) -> StdResult<()> {
        let provider = InsertSignedEntityRecordProvider::new(&self.connection);
        let _signed_entity_record = provider.persist(signed_entity.to_owned())?;

        Ok(())
    }

    async fn get_signed_entity(
        &self,
        signed_entity_id: &str,
    ) -> StdResult<Option<SignedEntityRecord>> {
        let provider = GetSignedEntityRecordProvider::new(&self.connection);
        let mut cursor = provider
            .get_by_signed_entity_id(signed_entity_id)
            .with_context(|| format!("get signed entity by id failure, id: {signed_entity_id}"))
            .map_err(AdapterError::GeneralError)?;
        let signed_entity = cursor.next();

        Ok(signed_entity)
    }

    async fn get_signed_entity_by_certificate_id(
        &self,
        certificate_id: &str,
    ) -> StdResult<Option<SignedEntityRecord>> {
        let provider = GetSignedEntityRecordProvider::new(&self.connection);
        let mut cursor = provider
            .get_by_certificate_id(certificate_id)
            .with_context(|| {
                format!(
                    "get signed entity by certificate id failure, certificate_id: {certificate_id}"
                )
            })
            .map_err(AdapterError::GeneralError)?;
        let signed_entity = cursor.next();

        Ok(signed_entity)
    }

    async fn get_signed_entities_by_certificates_ids<'a>(
        &self,
        certificates_ids: &[&'a str],
    ) -> StdResult<Vec<SignedEntityRecord>> {
        let provider = GetSignedEntityRecordProvider::new(&self.connection);
        let cursor = provider.get_by_certificates_ids(certificates_ids)?;

        Ok(cursor.collect())
    }

    async fn get_last_signed_entities_by_type(
        &self,
        signed_entity_type_id: &SignedEntityTypeDiscriminants,
        total: usize,
    ) -> StdResult<Vec<SignedEntityRecord>> {
        let provider = GetSignedEntityRecordProvider::new(&self.connection);
        let cursor = provider
            .get_by_signed_entity_type(signed_entity_type_id)
            .with_context(|| {
                format!("get last signed entity by type failure, type: {signed_entity_type_id:?}")
            })
            .map_err(AdapterError::GeneralError)?;
        let signed_entities: Vec<SignedEntityRecord> = cursor.take(total).collect();

        Ok(signed_entities)
    }

    async fn update_signed_entities(
        &self,
        signed_entities: Vec<SignedEntityRecord>,
    ) -> StdResult<Vec<SignedEntityRecord>> {
        let provider = UpdateSignedEntityProvider::new(&self.connection);
        let mut updated_records = vec![];

        for record in signed_entities {
            updated_records.push(provider.persist(&record)?);
        }

        Ok(updated_records)
    }
}

#[cfg(test)]
mod tests {
    use crate::database::test_helper::{
        apply_all_migrations_to_db, disable_foreign_key_support, insert_signed_entities,
    };
    use sqlite::Connection;

    use super::*;

    pub fn setup_signed_entity_db(
        connection: &SqliteConnection,
        signed_entity_records: Vec<SignedEntityRecord>,
    ) -> StdResult<()> {
        apply_all_migrations_to_db(connection)?;
        disable_foreign_key_support(connection)?;
        insert_signed_entities(connection, signed_entity_records)?;

        Ok(())
    }

    #[tokio::test]
    async fn test_get_signed_entity_record_by_certificate_id() {
        let expected_record = SignedEntityRecord::fake_records(1).remove(0);
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        setup_signed_entity_db(&connection, vec![expected_record.clone()]).unwrap();
        let store = SignedEntityStore::new(Arc::new(connection));

        let record = store
            .get_signed_entity_by_certificate_id(&expected_record.certificate_id)
            .await
            .expect("querying signed entity record by certificate id should not fail");

        assert_eq!(Some(expected_record), record);
    }

    #[tokio::test]
    async fn test_get_signed_entity_record_by_certificates_ids() {
        let expected_records = SignedEntityRecord::fake_records(3);
        let connection = Connection::open_thread_safe(":memory:").unwrap();
        setup_signed_entity_db(&connection, expected_records.clone()).unwrap();
        let store = SignedEntityStore::new(Arc::new(connection));
        let certificates_ids: Vec<&str> = expected_records
            .iter()
            .map(|r| r.certificate_id.as_str())
            .collect();

        let queried_records = store
            .get_signed_entities_by_certificates_ids(&certificates_ids)
            .await
            .expect("querying signed entity record by certificates ids should not fail");

        assert_eq!(
            // Records are inserted older to earlier and queried the other way round
            expected_records.into_iter().rev().collect::<Vec<_>>(),
            queried_records
        );
    }

    #[tokio::test]
    async fn update_only_given_entities() {
        let mut signed_entity_records = SignedEntityRecord::fake_records(5);

        let connection = Connection::open_thread_safe(":memory:").unwrap();
        setup_signed_entity_db(&connection, signed_entity_records.clone()).unwrap();
        let store = SignedEntityStore::new(Arc::new(connection));

        let records_to_update: Vec<SignedEntityRecord> = signed_entity_records
            .drain(2..)
            .map(|mut r| {
                r.certificate_id = format!("updated-{}", r.certificate_id);
                r
            })
            .collect();
        let expected_records: Vec<SignedEntityRecord> = signed_entity_records
            .into_iter()
            .chain(records_to_update.clone())
            .rev() // Records are returned from latest to oldest
            .collect();

        let updated_records = store
            .update_signed_entities(records_to_update.clone())
            .await
            .expect("updating signed entities should not fail");

        let stored_records = store
            .get_last_signed_entities_by_type(
                &SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
                usize::MAX,
            )
            .await
            .expect("getting signed entities should not fail");

        assert_eq!(records_to_update, updated_records);
        assert_eq!(expected_records, stored_records);
    }
}

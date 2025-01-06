use std::sync::Arc;

use mithril_common::entities::ImmutableFileName;
use mithril_common::StdResult;
use mithril_persistence::sqlite::{ConnectionExtensions, SqliteConnection};

use crate::database::query::{
    DeleteImmutableFileDigestQuery, GetImmutableFileDigestQuery, UpsertImmutableFileDigestQuery,
};
use crate::database::record::ImmutableFileDigestRecord;

/// ImmutableFileDigestRepository store for the immutable file digests.
pub struct ImmutableFileDigestRepository {
    connection: Arc<SqliteConnection>,
}

impl ImmutableFileDigestRepository {
    /// Instantiate service
    pub fn new(connection: Arc<SqliteConnection>) -> Self {
        Self { connection }
    }

    /// Return the [ImmutableFileDigestRecord] for the given [ImmutableFileName].
    pub async fn get_immutable_file_digest(
        &self,
        immutable_file_name: &ImmutableFileName,
    ) -> StdResult<Option<ImmutableFileDigestRecord>> {
        self.connection
            .fetch_first(GetImmutableFileDigestQuery::by_immutable_file_name(
                immutable_file_name,
            )?)
    }

    /// Return all the [ImmutableFileDigestRecord]s.
    pub async fn get_all_immutable_file_digest(&self) -> StdResult<Vec<ImmutableFileDigestRecord>> {
        self.connection
            .fetch_collect(GetImmutableFileDigestQuery::all())
    }

    /// Create a new [ImmutableFileDigestRecord] in the database.
    pub async fn upsert_immutable_file_digest(
        &self,
        immutable_file_name: &ImmutableFileName,
        digest: &str,
    ) -> StdResult<ImmutableFileDigestRecord> {
        let message = self
            .connection
            .fetch_first(UpsertImmutableFileDigestQuery::one(
                immutable_file_name,
                digest,
            )?)?;

        message
            .ok_or_else(|| panic!("Upserting an immutable_file_digest should not return nothing."))
    }

    /// Delete all [ImmutableFileDigestRecord] from the database.
    pub async fn delete_all(&self) -> StdResult<()> {
        self.connection
            .fetch_first(DeleteImmutableFileDigestQuery::all())?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {

    use crate::database::test_helper::main_db_connection;

    use super::*;

    async fn get_connection() -> Arc<SqliteConnection> {
        let connection = main_db_connection().unwrap();

        Arc::new(connection)
    }

    #[tokio::test]
    async fn repository_get_immutable_file_digest() {
        let repository = ImmutableFileDigestRepository::new(get_connection().await);
        let immutable_file_name: ImmutableFileName = "123.chunk".to_string();
        let digest = "digest-123";

        let immutable_file_digest_result = repository
            .get_immutable_file_digest(&immutable_file_name)
            .await
            .unwrap();
        assert!(immutable_file_digest_result.is_none());

        repository
            .upsert_immutable_file_digest(&immutable_file_name, digest)
            .await
            .unwrap();
        let immutable_file_digest_result = repository
            .get_immutable_file_digest(&immutable_file_name)
            .await
            .unwrap();
        assert!(immutable_file_digest_result.is_some());
    }

    #[tokio::test]
    async fn repository_get_all_immutable_file_digests() {
        let repository = ImmutableFileDigestRepository::new(get_connection().await);

        let all_immutable_file_digests = repository.get_all_immutable_file_digest().await.unwrap();
        assert!(all_immutable_file_digests.is_empty());

        repository
            .upsert_immutable_file_digest(&"123.chunk".to_string(), "digest-123")
            .await
            .unwrap();
        repository
            .upsert_immutable_file_digest(&"456.chunk".to_string(), "digest-456")
            .await
            .unwrap();
        let all_immutable_file_digests = repository.get_all_immutable_file_digest().await.unwrap();
        assert_eq!(2, all_immutable_file_digests.len());
    }

    #[tokio::test]
    async fn repository_upsert_immutable_file_digest() {
        let repository = ImmutableFileDigestRepository::new(get_connection().await);
        let immutable_file_name: ImmutableFileName = "123.chunk".to_string();
        let digest = "digest-123";
        let digest_updated = "digest-456";

        repository
            .upsert_immutable_file_digest(&immutable_file_name, digest)
            .await
            .unwrap();
        let immutable_file_digest = repository
            .get_immutable_file_digest(&immutable_file_name)
            .await
            .unwrap()
            .unwrap();
        assert_eq!(immutable_file_digest.digest, digest);

        repository
            .upsert_immutable_file_digest(&immutable_file_name, digest_updated)
            .await
            .unwrap();
        let immutable_file_digest = repository
            .get_immutable_file_digest(&immutable_file_name)
            .await
            .unwrap()
            .unwrap();
        assert_eq!(immutable_file_digest.digest, digest_updated);
    }

    #[tokio::test]
    async fn repository_delete_all_immutable_file_digests() {
        let repository = ImmutableFileDigestRepository::new(get_connection().await);

        repository
            .upsert_immutable_file_digest(&"123.chunk".to_string(), "digest-123")
            .await
            .unwrap();
        repository
            .upsert_immutable_file_digest(&"456.chunk".to_string(), "digest-456")
            .await
            .unwrap();
        let all_immutable_file_digests = repository.get_all_immutable_file_digest().await.unwrap();
        assert_eq!(2, all_immutable_file_digests.len());

        repository.delete_all().await.unwrap();

        let all_immutable_file_digests = repository.get_all_immutable_file_digest().await.unwrap();
        assert!(all_immutable_file_digests.is_empty());
    }
}

use std::collections::BTreeMap;
use std::sync::Arc;

use async_trait::async_trait;

use mithril_cardano_node_internal_database::digesters::cache::{
    CacheProviderResult, ImmutableDigesterCacheGetError, ImmutableDigesterCacheStoreError,
    ImmutableFileDigestCacheProvider,
};
use mithril_cardano_node_internal_database::entities::ImmutableFile;
use mithril_common::entities::{HexEncodedDigest, ImmutableFileName};
use mithril_common::StdResult;
use mithril_persistence::sqlite::{ConnectionExtensions, SqliteConnection};

use crate::database::query::{
    DeleteImmutableFileDigestQuery, GetImmutableFileDigestQuery, UpsertImmutableFileDigestQuery,
};
use crate::database::record::ImmutableFileDigestRecord;
use crate::ImmutableFileDigestMapper;

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
        self.connection.fetch_collect(GetImmutableFileDigestQuery::all())
    }

    /// Create a new [ImmutableFileDigestRecord] in the database.
    pub async fn upsert_immutable_file_digest(
        &self,
        immutable_file_name: &ImmutableFileName,
        digest: &str,
    ) -> StdResult<ImmutableFileDigestRecord> {
        let message = self.connection.fetch_first(UpsertImmutableFileDigestQuery::one(
            immutable_file_name,
            digest,
        )?)?;

        message
            .ok_or_else(|| panic!("Upserting an immutable_file_digest should not return nothing."))
    }

    /// Delete all [ImmutableFileDigestRecord] from the database.
    pub async fn delete_all(&self) -> StdResult<()> {
        self.connection.apply(DeleteImmutableFileDigestQuery::all())?;

        Ok(())
    }
}

#[async_trait]
impl ImmutableFileDigestCacheProvider for ImmutableFileDigestRepository {
    async fn store(
        &self,
        digest_per_filenames: Vec<(ImmutableFileName, HexEncodedDigest)>,
    ) -> CacheProviderResult<()> {
        for (filename, digest) in digest_per_filenames {
            self.upsert_immutable_file_digest(&filename, &digest)
                .await
                .map_err(ImmutableDigesterCacheStoreError::StoreError)?;
        }

        Ok(())
    }

    async fn get(
        &self,
        immutables: Vec<ImmutableFile>,
    ) -> CacheProviderResult<BTreeMap<ImmutableFile, Option<HexEncodedDigest>>> {
        let mut result = BTreeMap::new();
        for immutable in immutables {
            let immutable_file_digest = self
                .get_immutable_file_digest(&immutable.filename)
                .await
                .map_err(ImmutableDigesterCacheGetError::StoreError)?;

            result.insert(immutable, immutable_file_digest.map(|f| f.digest));
        }

        Ok(result)
    }

    async fn reset(&self) -> CacheProviderResult<()> {
        self.delete_all()
            .await
            .map_err(ImmutableDigesterCacheGetError::StoreError)?;

        Ok(())
    }
}

#[async_trait]
impl ImmutableFileDigestMapper for ImmutableFileDigestRepository {
    async fn get_immutable_file_digest_map(
        &self,
    ) -> StdResult<BTreeMap<ImmutableFileName, HexEncodedDigest>> {
        let immutable_file_digest_map = BTreeMap::from_iter(
            self.get_all_immutable_file_digest()
                .await?
                .into_iter()
                .map(|record| (record.immutable_file_name, record.digest)),
        );

        Ok(immutable_file_digest_map)
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

    mod repository {
        use mithril_common::test_utils::assert_equivalent;

        use super::*;

        #[tokio::test]
        async fn repository_get_immutable_file_digest() {
            let repository = ImmutableFileDigestRepository::new(get_connection().await);
            let immutable_file_name: ImmutableFileName = "123.chunk".to_string();
            let digest = "digest-123";

            let immutable_file_digest_result = repository
                .get_immutable_file_digest(&immutable_file_name)
                .await
                .unwrap();
            assert_eq!(None, immutable_file_digest_result);

            repository
                .upsert_immutable_file_digest(&immutable_file_name, digest)
                .await
                .unwrap();
            let immutable_file_digest_result = repository
                .get_immutable_file_digest(&immutable_file_name)
                .await
                .unwrap();
            assert_eq!(
                Some(ImmutableFileDigestRecord {
                    immutable_file_name,
                    digest: digest.to_string()
                }),
                immutable_file_digest_result
            );
        }

        #[tokio::test]
        async fn repository_get_all_immutable_file_digests() {
            let repository = ImmutableFileDigestRepository::new(get_connection().await);

            let all_immutable_file_digests =
                repository.get_all_immutable_file_digest().await.unwrap();
            assert!(all_immutable_file_digests.is_empty());

            repository
                .upsert_immutable_file_digest(&"123.chunk".to_string(), "digest-123")
                .await
                .unwrap();
            repository
                .upsert_immutable_file_digest(&"456.chunk".to_string(), "digest-456")
                .await
                .unwrap();
            let all_immutable_file_digests =
                repository.get_all_immutable_file_digest().await.unwrap();

            assert_equivalent(
                vec![
                    ImmutableFileDigestRecord {
                        immutable_file_name: "123.chunk".to_string(),
                        digest: "digest-123".to_string(),
                    },
                    ImmutableFileDigestRecord {
                        immutable_file_name: "456.chunk".to_string(),
                        digest: "digest-456".to_string(),
                    },
                ],
                all_immutable_file_digests,
            );
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
            let all_immutable_file_digests =
                repository.get_all_immutable_file_digest().await.unwrap();
            assert_eq!(2, all_immutable_file_digests.len());

            repository.delete_all().await.unwrap();

            let all_immutable_file_digests =
                repository.get_all_immutable_file_digest().await.unwrap();
            assert!(all_immutable_file_digests.is_empty());
        }
    }

    mod cache_provider {
        use std::path::PathBuf;

        use mithril_cardano_node_internal_database::test::fake_data;

        use super::*;

        #[tokio::test]
        async fn can_store_values() {
            let provider = ImmutableFileDigestRepository::new(get_connection().await);
            let values_to_store = vec![
                ("0.chunk".to_string(), "digest 0".to_string()),
                ("1.chunk".to_string(), "digest 1".to_string()),
            ];
            let expected: BTreeMap<_, _> = BTreeMap::from([
                (
                    fake_data::immutable_file(PathBuf::default(), 0, "0.chunk"),
                    Some("digest 0".to_string()),
                ),
                (
                    fake_data::immutable_file(PathBuf::default(), 1, "1.chunk"),
                    Some("digest 1".to_string()),
                ),
            ]);
            let immutables = expected.keys().cloned().collect();

            provider
                .store(values_to_store)
                .await
                .expect("Cache write should not fail");
            let result = provider.get(immutables).await.expect("Cache read should not fail");

            assert_eq!(expected, result);
        }

        #[tokio::test]
        async fn returns_only_asked_immutables_cache() {
            let provider = ImmutableFileDigestRepository::new(get_connection().await);
            provider
                .store(vec![
                    ("0.chunk".to_string(), "digest 0".to_string()),
                    ("1.chunk".to_string(), "digest 1".to_string()),
                ])
                .await
                .expect("Cache write should not fail");
            let expected: BTreeMap<_, _> = BTreeMap::from([(
                fake_data::immutable_file(PathBuf::default(), 0, "0.chunk"),
                Some("digest 0".to_string()),
            )]);
            let immutables = expected.keys().cloned().collect();

            let result = provider.get(immutables).await.expect("Cache read should not fail");

            assert_eq!(expected, result);
        }

        #[tokio::test]
        async fn returns_none_for_uncached_asked_immutables() {
            let provider = ImmutableFileDigestRepository::new(get_connection().await);
            let expected: BTreeMap<_, _> = BTreeMap::from([(
                fake_data::immutable_file(PathBuf::default(), 2, "2.chunk"),
                None,
            )]);
            let immutables = expected.keys().cloned().collect();

            let result = provider.get(immutables).await.expect("Cache read should not fail");

            assert_eq!(expected, result);
        }

        #[tokio::test]
        async fn store_erase_existing_values() {
            let provider = ImmutableFileDigestRepository::new(get_connection().await);
            provider
                .store(vec![
                    ("0.chunk".to_string(), "to erase".to_string()),
                    ("1.chunk".to_string(), "keep me".to_string()),
                    ("2.chunk".to_string(), "keep me too".to_string()),
                ])
                .await
                .expect("Cache write should not fail");
            let values_to_store = vec![
                ("0.chunk".to_string(), "updated".to_string()),
                ("1.chunk".to_string(), "keep me".to_string()),
            ];
            let expected: BTreeMap<_, _> = BTreeMap::from([
                (
                    fake_data::immutable_file(PathBuf::default(), 0, "0.chunk"),
                    Some("updated".to_string()),
                ),
                (
                    fake_data::immutable_file(PathBuf::default(), 1, "1.chunk"),
                    Some("keep me".to_string()),
                ),
                (
                    fake_data::immutable_file(PathBuf::default(), 2, "2.chunk"),
                    Some("keep me too".to_string()),
                ),
                (
                    fake_data::immutable_file(PathBuf::default(), 3, "3.chunk"),
                    None,
                ),
            ]);
            let immutables = expected.keys().cloned().collect();

            provider
                .store(values_to_store)
                .await
                .expect("Cache write should not fail");
            let result = provider.get(immutables).await.expect("Cache read should not fail");

            assert_eq!(expected, result);
        }

        #[tokio::test]
        async fn reset_clear_existing_values() {
            let provider = ImmutableFileDigestRepository::new(get_connection().await);
            let values_to_store = vec![
                ("0.chunk".to_string(), "digest 0".to_string()),
                ("1.chunk".to_string(), "digest 1".to_string()),
            ];
            let expected: BTreeMap<_, _> = BTreeMap::from([
                (
                    fake_data::immutable_file(PathBuf::default(), 0, "0.chunk"),
                    Some("digest 0".to_string()),
                ),
                (
                    fake_data::immutable_file(PathBuf::default(), 1, "1.chunk"),
                    Some("digest 1".to_string()),
                ),
            ]);
            let immutables = expected.keys().cloned().collect();

            provider
                .store(values_to_store)
                .await
                .expect("Cache write should not fail");
            provider.reset().await.expect("reset should not fails");

            let result: BTreeMap<_, _> =
                provider.get(immutables).await.expect("Cache read should not fail");

            assert!(result.into_iter().all(|(_, cache)| cache.is_none()));
        }
    }

    #[cfg(test)]
    mod digest_mapper {

        use mithril_cardano_node_internal_database::digesters::cache::ImmutableFileDigestCacheProvider;

        use super::*;

        #[tokio::test]
        async fn get_immutable_file_digest_map() {
            let provider = ImmutableFileDigestRepository::new(get_connection().await);
            let immutable_file_digest_records = vec![
                ("0.chunk".to_string(), "digest 0".to_string()),
                ("1.chunk".to_string(), "digest 1".to_string()),
                ("2.chunk".to_string(), "digest 2".to_string()),
            ];
            let expected_immutable_file_digest_map =
                BTreeMap::from_iter(immutable_file_digest_records.clone().into_iter());
            provider
                .store(immutable_file_digest_records)
                .await
                .expect("Cache write should not fail");

            let immutable_file_digest_map = provider.get_immutable_file_digest_map().await.unwrap();

            assert_eq!(
                expected_immutable_file_digest_map,
                immutable_file_digest_map
            );
        }
    }
}

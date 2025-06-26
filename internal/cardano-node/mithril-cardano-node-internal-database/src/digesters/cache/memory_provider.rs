use async_trait::async_trait;
use std::collections::{BTreeMap, HashMap};
use tokio::sync::RwLock;

use mithril_common::entities::{HexEncodedDigest, ImmutableFileName};

use crate::entities::ImmutableFile;
use crate::{
    digesters::cache::CacheProviderResult, digesters::cache::ImmutableFileDigestCacheProvider,
};

/// A in memory [ImmutableFileDigestCacheProvider].
pub struct MemoryImmutableFileDigestCacheProvider {
    store: RwLock<HashMap<ImmutableFileName, HexEncodedDigest>>,
}

impl MemoryImmutableFileDigestCacheProvider {
    /// Build a new [MemoryImmutableFileDigestCacheProvider] that contains the given values.
    pub fn from(values: HashMap<ImmutableFileName, HexEncodedDigest>) -> Self {
        Self {
            store: RwLock::new(values),
        }
    }
}

impl Default for MemoryImmutableFileDigestCacheProvider {
    fn default() -> Self {
        Self {
            store: RwLock::new(HashMap::new()),
        }
    }
}

#[async_trait]
impl ImmutableFileDigestCacheProvider for MemoryImmutableFileDigestCacheProvider {
    async fn store(
        &self,
        digest_per_filenames: Vec<(ImmutableFileName, HexEncodedDigest)>,
    ) -> CacheProviderResult<()> {
        let mut store = self.store.write().await;
        for (filename, digest) in digest_per_filenames {
            store.insert(filename, digest);
        }

        Ok(())
    }

    async fn get(
        &self,
        immutables: Vec<ImmutableFile>,
    ) -> CacheProviderResult<BTreeMap<ImmutableFile, Option<HexEncodedDigest>>> {
        let store = self.store.read().await;
        let mut result = BTreeMap::new();

        for immutable in immutables {
            let value = store.get(&immutable.filename).map(|f| f.to_owned());
            result.insert(immutable, value);
        }

        Ok(result)
    }

    async fn reset(&self) -> CacheProviderResult<()> {
        let mut store = self.store.write().await;
        store.clear();
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{BTreeMap, HashMap};
    use std::path::PathBuf;

    use crate::digesters::cache::{
        ImmutableFileDigestCacheProvider, MemoryImmutableFileDigestCacheProvider,
    };
    use crate::test::fake_data;

    #[tokio::test]
    async fn can_store_values() {
        let provider = MemoryImmutableFileDigestCacheProvider::default();
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
        let provider = MemoryImmutableFileDigestCacheProvider::from(HashMap::from([
            ("0.chunk".to_string(), "digest 0".to_string()),
            ("1.chunk".to_string(), "digest 1".to_string()),
        ]));
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
        let provider = MemoryImmutableFileDigestCacheProvider::from(HashMap::from([(
            "0.chunk".to_string(),
            "digest 0".to_string(),
        )]));
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
        let provider = MemoryImmutableFileDigestCacheProvider::from(HashMap::from([
            ("0.chunk".to_string(), "to erase".to_string()),
            ("1.chunk".to_string(), "keep me".to_string()),
            ("2.chunk".to_string(), "keep me too".to_string()),
        ]));
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
        let provider = MemoryImmutableFileDigestCacheProvider::default();
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

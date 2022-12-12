use super::CardanoImmutableDigesterCacheProvider;
use crate::digesters::ImmutableFile;
use crate::entities::{HexEncodedDigest, ImmutableFileName};

use async_trait::async_trait;
use std::collections::{BTreeMap, HashMap};
use tokio::sync::RwLock;

/// A in memory [CardanoImmutableDigesterCacheProvider].
pub struct MemoryCardanoImmutableDigesterCacheProvider {
    store: RwLock<HashMap<ImmutableFileName, HexEncodedDigest>>,
}

impl MemoryCardanoImmutableDigesterCacheProvider {
    /// Build a new [MemoryCardanoImmutableDigesterCacheProvider] that contains the given values.
    pub fn from(values: HashMap<ImmutableFileName, HexEncodedDigest>) -> Self {
        Self {
            store: RwLock::new(values),
        }
    }
}

impl Default for MemoryCardanoImmutableDigesterCacheProvider {
    fn default() -> Self {
        Self {
            store: RwLock::new(HashMap::new()),
        }
    }
}

#[async_trait]
impl CardanoImmutableDigesterCacheProvider for MemoryCardanoImmutableDigesterCacheProvider {
    async fn store(&self, digest_per_filenames: Vec<(ImmutableFileName, HexEncodedDigest)>) {
        let mut store = self.store.write().await;
        for (filename, digest) in digest_per_filenames {
            store.insert(filename, digest);
        }
    }

    async fn get(
        &self,
        immutables: Vec<ImmutableFile>,
    ) -> BTreeMap<ImmutableFile, Option<HexEncodedDigest>> {
        let store = self.store.read().await;
        let mut result = BTreeMap::new();

        for immutable in immutables {
            let value = store.get(&immutable.filename).map(|f| f.to_owned());
            result.insert(immutable, value);
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use crate::digesters::{
        CardanoImmutableDigesterCacheProvider, ImmutableFile,
        MemoryCardanoImmutableDigesterCacheProvider,
    };
    use std::collections::{BTreeMap, HashMap};
    use std::path::PathBuf;

    #[tokio::test]
    async fn can_store_values() {
        let provider = MemoryCardanoImmutableDigesterCacheProvider::default();
        let values_to_store = vec![
            ("0.chunk".to_string(), "digest 0".to_string()),
            ("1.chunk".to_string(), "digest 1".to_string()),
        ];
        let expected: BTreeMap<_, _> = BTreeMap::from([
            (
                ImmutableFile::dummy(PathBuf::default(), 0, "0.chunk".to_string()),
                Some("digest 0".to_string()),
            ),
            (
                ImmutableFile::dummy(PathBuf::default(), 1, "1.chunk".to_string()),
                Some("digest 1".to_string()),
            ),
        ]);
        let immutables = expected.keys().cloned().collect();

        provider.store(values_to_store).await;
        let result = provider.get(immutables).await;

        assert_eq!(expected, result);
    }

    #[tokio::test]
    async fn returns_only_asked_immutables_cache() {
        let provider = MemoryCardanoImmutableDigesterCacheProvider::from(HashMap::from([
            ("0.chunk".to_string(), "digest 0".to_string()),
            ("1.chunk".to_string(), "digest 1".to_string()),
        ]));
        let expected: BTreeMap<_, _> = BTreeMap::from([(
            ImmutableFile::dummy(PathBuf::default(), 0, "0.chunk".to_string()),
            Some("digest 0".to_string()),
        )]);
        let immutables = expected.keys().cloned().collect();

        let result = provider.get(immutables).await;

        assert_eq!(expected, result);
    }

    #[tokio::test]
    async fn returns_none_for_uncached_asked_immutables() {
        let provider = MemoryCardanoImmutableDigesterCacheProvider::from(HashMap::from([(
            "0.chunk".to_string(),
            "digest 0".to_string(),
        )]));
        let expected: BTreeMap<_, _> = BTreeMap::from([(
            ImmutableFile::dummy(PathBuf::default(), 2, "2.chunk".to_string()),
            None,
        )]);
        let immutables = expected.keys().cloned().collect();

        let result = provider.get(immutables).await;

        assert_eq!(expected, result);
    }

    #[tokio::test]
    async fn store_erase_existing_values() {
        let provider = MemoryCardanoImmutableDigesterCacheProvider::from(HashMap::from([
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
                ImmutableFile::dummy(PathBuf::default(), 0, "0.chunk".to_string()),
                Some("updated".to_string()),
            ),
            (
                ImmutableFile::dummy(PathBuf::default(), 1, "1.chunk".to_string()),
                Some("keep me".to_string()),
            ),
            (
                ImmutableFile::dummy(PathBuf::default(), 2, "2.chunk".to_string()),
                Some("keep me too".to_string()),
            ),
            (
                ImmutableFile::dummy(PathBuf::default(), 3, "3.chunk".to_string()),
                None,
            ),
        ]);
        let immutables = expected.keys().cloned().collect();

        provider.store(values_to_store).await;
        let result = provider.get(immutables).await;

        assert_eq!(expected, result);
    }
}

use crate::{
    digesters::{
        cache::provider::{ImmutableDigesterCacheGetError, ImmutableDigesterCacheStoreError},
        cache::CacheProviderResult,
        cache::ImmutableFileDigestCacheProvider,
        ImmutableFile,
    },
    entities::{HexEncodedDigest, ImmutableFileName},
};

use async_trait::async_trait;
use std::{
    collections::BTreeMap,
    path::{Path, PathBuf},
};
use tokio::{
    fs,
    fs::File,
    io::{AsyncReadExt, AsyncWriteExt},
};

type InnerStructure = BTreeMap<ImmutableFileName, HexEncodedDigest>;

/// A in memory [ImmutableFileDigestCacheProvider].
pub struct JsonImmutableFileDigestCacheProvider {
    filepath: PathBuf,
}

impl JsonImmutableFileDigestCacheProvider {
    /// [JsonImmutableFileDigestCacheProvider] factory
    pub fn new(filepath: &Path) -> Self {
        Self {
            filepath: filepath.to_path_buf(),
        }
    }

    #[cfg(test)]
    /// [Test Only] Build a new [JsonImmutableFileDigestCacheProvider] that contains the given values.
    pub async fn from(filepath: &Path, values: InnerStructure) -> Self {
        let provider = Self::new(filepath);
        provider.write_data(values).await.unwrap();
        provider
    }

    async fn write_data(
        &self,
        values: InnerStructure,
    ) -> Result<(), ImmutableDigesterCacheStoreError> {
        let mut file = File::create(&self.filepath).await?;
        file.write_all(serde_json::to_string_pretty(&values)?.as_bytes())
            .await?;

        Ok(())
    }

    async fn read_data(&self) -> Result<InnerStructure, ImmutableDigesterCacheGetError> {
        match self.filepath.exists() {
            true => {
                let mut file = File::open(&self.filepath).await?;
                let mut json_string = String::new();
                file.read_to_string(&mut json_string).await?;
                let values: InnerStructure = serde_json::from_str(&json_string)?;
                Ok(values)
            }
            false => Ok(BTreeMap::new()),
        }
    }
}

#[async_trait]
impl ImmutableFileDigestCacheProvider for JsonImmutableFileDigestCacheProvider {
    async fn store(
        &self,
        digest_per_filenames: Vec<(ImmutableFileName, HexEncodedDigest)>,
    ) -> CacheProviderResult<()> {
        let mut data = self.read_data().await?;
        for (filename, digest) in digest_per_filenames {
            data.insert(filename, digest);
        }
        self.write_data(data).await?;

        Ok(())
    }

    async fn get(
        &self,
        immutables: Vec<ImmutableFile>,
    ) -> CacheProviderResult<BTreeMap<ImmutableFile, Option<HexEncodedDigest>>> {
        let values = self.read_data().await?;
        let mut result = BTreeMap::new();

        for immutable in immutables {
            let value = values.get(&immutable.filename).map(|f| f.to_owned());
            result.insert(immutable, value);
        }

        Ok(result)
    }

    async fn reset(&self) -> CacheProviderResult<()> {
        fs::remove_file(&self.filepath)
            .await
            .map_err(ImmutableDigesterCacheStoreError::from)?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::digesters::cache::{
        ImmutableFileDigestCacheProvider, JsonImmutableFileDigestCacheProvider,
    };
    use crate::digesters::ImmutableFile;
    use std::{collections::BTreeMap, fs, path::PathBuf};

    fn get_test_dir(subdir_name: &str) -> PathBuf {
        let parent_dir = std::env::temp_dir()
            .join("mithril_test")
            .join("json_digester_cache_provider")
            .join(subdir_name);

        if parent_dir.exists() {
            fs::remove_dir_all(&parent_dir)
                .unwrap_or_else(|e| panic!("Could not remove dir {parent_dir:?}: {e}"));
        }
        fs::create_dir_all(&parent_dir)
            .unwrap_or_else(|e| panic!("Could not create dir {parent_dir:?}: {e}"));

        parent_dir
    }

    #[tokio::test]
    async fn can_store_values() {
        let file = get_test_dir("can_store_values").join("immutable-cache-store.json");
        let provider = JsonImmutableFileDigestCacheProvider::new(&file);
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

        provider
            .store(values_to_store)
            .await
            .expect("Cache write should not fail");
        let result = provider
            .get(immutables)
            .await
            .expect("Cache read should not fail");

        assert_eq!(expected, result);
    }

    #[tokio::test]
    async fn returns_only_asked_immutables_cache() {
        let file =
            get_test_dir("returns_only_asked_immutables_cache").join("immutable-cache-store.json");
        let provider = JsonImmutableFileDigestCacheProvider::from(
            &file,
            BTreeMap::from([
                ("0.chunk".to_string(), "digest 0".to_string()),
                ("1.chunk".to_string(), "digest 1".to_string()),
            ]),
        )
        .await;
        let expected: BTreeMap<_, _> = BTreeMap::from([(
            ImmutableFile::dummy(PathBuf::default(), 0, "0.chunk".to_string()),
            Some("digest 0".to_string()),
        )]);
        let immutables = expected.keys().cloned().collect();

        let result = provider
            .get(immutables)
            .await
            .expect("Cache read should not fail");

        assert_eq!(expected, result);
    }

    #[tokio::test]
    async fn returns_none_for_uncached_asked_immutables() {
        let file = get_test_dir("returns_none_for_uncached_asked_immutables")
            .join("immutable-cache-store.json");
        let provider = JsonImmutableFileDigestCacheProvider::from(
            &file,
            BTreeMap::from([("0.chunk".to_string(), "digest 0".to_string())]),
        )
        .await;
        let expected: BTreeMap<_, _> = BTreeMap::from([(
            ImmutableFile::dummy(PathBuf::default(), 2, "2.chunk".to_string()),
            None,
        )]);
        let immutables = expected.keys().cloned().collect();

        let result = provider
            .get(immutables)
            .await
            .expect("Cache read should not fail");

        assert_eq!(expected, result);
    }

    #[tokio::test]
    async fn store_erase_existing_values() {
        let file = get_test_dir("store_erase_existing_values").join("immutable-cache-store.json");
        let provider = JsonImmutableFileDigestCacheProvider::from(
            &file,
            BTreeMap::from([
                ("0.chunk".to_string(), "to erase".to_string()),
                ("1.chunk".to_string(), "keep me".to_string()),
                ("2.chunk".to_string(), "keep me too".to_string()),
            ]),
        )
        .await;
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

        provider
            .store(values_to_store)
            .await
            .expect("Cache write should not fail");
        let result = provider
            .get(immutables)
            .await
            .expect("Cache read should not fail");

        assert_eq!(expected, result);
    }

    #[tokio::test]
    async fn reset_clear_existing_values() {
        let file = get_test_dir("reset_clear_existing_values").join("immutable-cache-store.json");
        let provider = JsonImmutableFileDigestCacheProvider::new(&file);
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

        provider
            .store(values_to_store)
            .await
            .expect("Cache write should not fail");
        provider.reset().await.expect("reset should not fails");

        let result: BTreeMap<_, _> = provider
            .get(immutables)
            .await
            .expect("Cache read should not fail");

        assert!(result.into_iter().all(|(_, cache)| cache.is_none()));
    }
}

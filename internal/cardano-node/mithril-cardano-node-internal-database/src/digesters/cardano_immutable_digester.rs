use async_trait::async_trait;
use slog::{Logger, debug, info, warn};
use std::{collections::BTreeMap, io, ops::RangeInclusive, path::Path, sync::Arc};

use mithril_common::crypto_helper::{MKTree, MKTreeStoreInMemory};
use mithril_common::entities::{CardanoDbBeacon, HexEncodedDigest, ImmutableFileNumber};
use mithril_common::logging::LoggerExtensions;

use crate::{
    digesters::{
        ImmutableDigester, ImmutableDigesterError, cache::ImmutableFileDigestCacheProvider,
    },
    entities::ImmutableFile,
};

use super::immutable_digester::ComputedImmutablesDigests;

/// A digester working directly on a Cardano DB immutables files
pub struct CardanoImmutableDigester {
    /// A [ImmutableFileDigestCacheProvider] instance
    cache_provider: Option<Arc<dyn ImmutableFileDigestCacheProvider>>,

    /// The logger where the logs should be written
    logger: Logger,
}

impl CardanoImmutableDigester {
    /// ImmutableDigester factory
    pub fn new(
        cache_provider: Option<Arc<dyn ImmutableFileDigestCacheProvider>>,
        logger: Logger,
    ) -> Self {
        Self {
            cache_provider,
            logger: logger.new_with_component_name::<Self>(),
        }
    }

    async fn process_immutables(
        &self,
        immutables: Vec<ImmutableFile>,
    ) -> Result<ComputedImmutablesDigests, ImmutableDigesterError> {
        let cached_values = self.fetch_immutables_cached(immutables).await;

        // The computation of immutable files digests is done in a separate thread because it is blocking the whole task
        let logger = self.logger.clone();
        let computed_digests =
            tokio::task::spawn_blocking(move || -> Result<ComputedImmutablesDigests, io::Error> {
                ComputedImmutablesDigests::compute_immutables_digests(cached_values, logger)
            })
            .await
            .map_err(|e| ImmutableDigesterError::DigestComputationError(e.into()))??;

        Ok(computed_digests)
    }

    async fn fetch_immutables_cached(
        &self,
        immutables: Vec<ImmutableFile>,
    ) -> BTreeMap<ImmutableFile, Option<String>> {
        match self.cache_provider.as_ref() {
            None => BTreeMap::from_iter(immutables.into_iter().map(|i| (i, None))),
            Some(cache_provider) => match cache_provider.get(immutables.clone()).await {
                Ok(values) => values,
                Err(error) => {
                    warn!(
                        self.logger, "Error while getting cached immutable files digests";
                        "error" => ?error
                    );
                    BTreeMap::from_iter(immutables.into_iter().map(|i| (i, None)))
                }
            },
        }
    }

    async fn update_cache(&self, computed_immutables_digests: &ComputedImmutablesDigests) {
        if let Some(cache_provider) = self.cache_provider.as_ref() {
            let new_cached_entries = computed_immutables_digests
                .entries
                .iter()
                .filter(|(file, _hash)| {
                    computed_immutables_digests
                        .new_cached_entries
                        .contains(&file.filename)
                })
                .map(|(file, hash)| (file.filename.clone(), hash.clone()))
                .collect();

            if let Err(error) = cache_provider.store(new_cached_entries).await {
                warn!(
                    self.logger, "Error while storing new immutable files digests to cache";
                    "error" => ?error
                );
            }
        }
    }
}

#[async_trait]
impl ImmutableDigester for CardanoImmutableDigester {
    async fn compute_digests_for_range(
        &self,
        dirpath: &Path,
        range: &RangeInclusive<ImmutableFileNumber>,
    ) -> Result<ComputedImmutablesDigests, ImmutableDigesterError> {
        let immutables_to_process = list_immutable_files_to_process_for_range(dirpath, range)?;
        info!(self.logger, ">> compute_digests_for_range"; "nb_of_immutables" => immutables_to_process.len());
        let computed_immutables_digests = self.process_immutables(immutables_to_process).await?;

        self.update_cache(&computed_immutables_digests).await;

        debug!(
            self.logger,
            "Successfully computed Digests for Cardano database"; "range" => #?range);

        Ok(computed_immutables_digests)
    }

    async fn compute_merkle_tree(
        &self,
        dirpath: &Path,
        beacon: &CardanoDbBeacon,
    ) -> Result<MKTree<MKTreeStoreInMemory>, ImmutableDigesterError> {
        let immutables_to_process =
            list_immutable_files_to_process(dirpath, beacon.immutable_file_number)?;
        info!(self.logger, ">> compute_merkle_tree"; "beacon" => #?beacon, "nb_of_immutables" => immutables_to_process.len());
        let computed_immutables_digests = self.process_immutables(immutables_to_process).await?;

        self.update_cache(&computed_immutables_digests).await;

        let digests: Vec<HexEncodedDigest> =
            computed_immutables_digests.entries.into_values().collect();
        let mktree =
            MKTree::new(&digests).map_err(ImmutableDigesterError::MerkleTreeComputationError)?;

        debug!(
            self.logger,
            "Successfully computed Merkle tree for Cardano database"; "beacon" => #?beacon);

        Ok(mktree)
    }
}

fn list_immutable_files_to_process(
    dirpath: &Path,
    up_to_file_number: ImmutableFileNumber,
) -> Result<Vec<ImmutableFile>, ImmutableDigesterError> {
    let immutables: Vec<ImmutableFile> = ImmutableFile::list_all_in_dir(dirpath)?
        .into_iter()
        .filter(|f| f.number <= up_to_file_number)
        .collect();

    match immutables.last() {
        None => Err(ImmutableDigesterError::NotEnoughImmutable {
            expected_number: up_to_file_number,
            found_number: None,
            db_dir: dirpath.to_owned(),
        }),
        Some(last_immutable_file) if last_immutable_file.number < up_to_file_number => {
            Err(ImmutableDigesterError::NotEnoughImmutable {
                expected_number: up_to_file_number,
                found_number: Some(last_immutable_file.number),
                db_dir: dirpath.to_owned(),
            })
        }
        Some(_) => Ok(immutables),
    }
}

fn list_immutable_files_to_process_for_range(
    dirpath: &Path,
    range: &RangeInclusive<ImmutableFileNumber>,
) -> Result<Vec<ImmutableFile>, ImmutableDigesterError> {
    let immutables: Vec<ImmutableFile> = ImmutableFile::list_all_in_dir(dirpath)?
        .into_iter()
        .filter(|f| range.contains(&f.number))
        .collect();

    Ok(immutables)
}

#[cfg(test)]
mod tests {
    use sha2::Sha256;
    use std::{collections::BTreeMap, io, sync::Arc};

    use crate::digesters::cache::{
        ImmutableDigesterCacheGetError, ImmutableDigesterCacheProviderError,
        ImmutableDigesterCacheStoreError, MemoryImmutableFileDigestCacheProvider,
        MockImmutableFileDigestCacheProvider,
    };
    use crate::test::{DummyCardanoDbBuilder, TestLogger};

    use super::*;

    fn db_builder(dir_name: &str) -> DummyCardanoDbBuilder {
        DummyCardanoDbBuilder::new(&format!("cardano_immutable_digester/{dir_name}"))
    }

    #[tokio::test]
    async fn fail_if_no_file_in_folder() {
        let cardano_db = db_builder("fail_if_no_file_in_folder").build();

        let result = list_immutable_files_to_process(cardano_db.get_immutable_dir(), 1)
            .expect_err("list_immutable_files_to_process should have failed");

        assert_eq!(
            format!(
                "{:?}",
                ImmutableDigesterError::NotEnoughImmutable {
                    expected_number: 1,
                    found_number: None,
                    db_dir: cardano_db.get_immutable_dir().to_path_buf(),
                }
            ),
            format!("{result:?}")
        );
    }

    #[tokio::test]
    async fn fail_if_a_invalid_file_is_in_immutable_folder() {
        let cardano_db = db_builder("fail_if_no_immutable_exist")
            .with_non_immutables(&["not_immutable"])
            .build();

        assert!(list_immutable_files_to_process(cardano_db.get_immutable_dir(), 1).is_err());
    }

    #[tokio::test]
    async fn can_list_files_to_process_even_if_theres_only_the_uncompleted_immutable_trio() {
        let cardano_db = db_builder(
            "can_list_files_to_process_even_if_theres_only_the_uncompleted_immutable_trio",
        )
        .with_immutables(&[1])
        .build();

        let processable_files =
            list_immutable_files_to_process(cardano_db.get_immutable_dir(), 1).unwrap();

        assert_eq!(
            vec![
                "00001.chunk".to_string(),
                "00001.primary".to_string(),
                "00001.secondary".to_string()
            ],
            processable_files.into_iter().map(|f| f.filename).collect::<Vec<_>>()
        );
    }

    #[tokio::test]
    async fn fail_if_less_immutable_than_what_required_in_beacon() {
        let cardano_db = db_builder("fail_if_less_immutable_than_what_required_in_beacon")
            .with_immutables(&[1, 2, 3, 4, 5])
            .append_immutable_trio()
            .build();

        let result = list_immutable_files_to_process(cardano_db.get_immutable_dir(), 10)
            .expect_err("list_immutable_files_to_process should've failed");

        assert_eq!(
            format!(
                "{:?}",
                ImmutableDigesterError::NotEnoughImmutable {
                    expected_number: 10,
                    found_number: Some(6),
                    db_dir: cardano_db.get_immutable_dir().to_path_buf(),
                }
            ),
            format!("{result:?}")
        );
    }

    #[tokio::test]
    async fn can_compute_merkle_tree_of_a_hundred_immutable_file_trio() {
        let cardano_db = db_builder("can_compute_merkle_tree_of_a_hundred_immutable_file_trio")
            .with_immutables(&(1..=100).collect::<Vec<ImmutableFileNumber>>())
            .append_immutable_trio()
            .build();
        let logger = TestLogger::stdout();
        let digester = CardanoImmutableDigester::new(
            Some(Arc::new(MemoryImmutableFileDigestCacheProvider::default())),
            logger.clone(),
        );
        let beacon = CardanoDbBeacon::new(1, 100);

        let result = digester
            .compute_merkle_tree(cardano_db.get_immutable_dir(), &beacon)
            .await
            .expect("compute_merkle_tree must not fail");

        let expected_merkle_root = result.compute_root().unwrap().to_hex();

        assert_eq!(
            "8552f75838176c967a33eb6da1fe5f3c9940b706d75a9c2352c0acd8439f3d84".to_string(),
            expected_merkle_root
        )
    }

    #[tokio::test]
    async fn can_compute_digests_for_range_of_a_hundred_immutable_file_trio() {
        let immutable_range = 1..=100;
        let cardano_db =
            db_builder("can_compute_digests_for_range_of_a_hundred_immutable_file_trio")
                .with_immutables(&immutable_range.clone().collect::<Vec<ImmutableFileNumber>>())
                .append_immutable_trio()
                .build();
        let logger = TestLogger::stdout();
        let digester = CardanoImmutableDigester::new(
            Some(Arc::new(MemoryImmutableFileDigestCacheProvider::default())),
            logger.clone(),
        );

        let result = digester
            .compute_digests_for_range(cardano_db.get_immutable_dir(), &immutable_range)
            .await
            .expect("compute_digests_for_range must not fail");

        assert_eq!(cardano_db.get_immutable_files().len(), result.entries.len())
    }

    #[tokio::test]
    async fn can_compute_consistent_digests_for_range() {
        let immutable_range = 1..=1;
        let cardano_db = db_builder("can_compute_digests_for_range_consistently")
            .with_immutables(&immutable_range.clone().collect::<Vec<ImmutableFileNumber>>())
            .append_immutable_trio()
            .build();
        let logger = TestLogger::stdout();
        let digester = CardanoImmutableDigester::new(
            Some(Arc::new(MemoryImmutableFileDigestCacheProvider::default())),
            logger.clone(),
        );

        let result = digester
            .compute_digests_for_range(cardano_db.get_immutable_dir(), &immutable_range)
            .await
            .expect("compute_digests_for_range must not fail");

        assert_eq!(
            BTreeMap::from([
                (
                    ImmutableFile {
                        path: cardano_db.get_immutable_dir().join("00001.chunk"),
                        number: 1,
                        filename: "00001.chunk".to_string()
                    },
                    "faebbf47077f68ef57219396ff69edc738978a3eca946ac7df1983dbf11364ec".to_string()
                ),
                (
                    ImmutableFile {
                        path: cardano_db.get_immutable_dir().join("00001.primary"),
                        number: 1,
                        filename: "00001.primary".to_string()
                    },
                    "f11bdb991fc7e72970be7d7f666e10333f92c14326d796fed8c2c041675fa826".to_string()
                ),
                (
                    ImmutableFile {
                        path: cardano_db.get_immutable_dir().join("00001.secondary"),
                        number: 1,
                        filename: "00001.secondary".to_string()
                    },
                    "b139684b968fa12ce324cce464d000de0e2c2ded0fd3e473a666410821d3fde3".to_string()
                )
            ]),
            result.entries
        );
    }

    #[tokio::test]
    async fn compute_merkle_tree_store_digests_into_cache_provider() {
        let cardano_db = db_builder("compute_merkle_tree_store_digests_into_cache_provider")
            .with_immutables(&[1, 2])
            .append_immutable_trio()
            .build();
        let immutables = cardano_db.get_immutable_files().clone();
        let cache = Arc::new(MemoryImmutableFileDigestCacheProvider::default());
        let logger = TestLogger::stdout();
        let digester = CardanoImmutableDigester::new(Some(cache.clone()), logger.clone());
        let beacon = CardanoDbBeacon::new(1, 2);

        digester
            .compute_merkle_tree(cardano_db.get_immutable_dir(), &beacon)
            .await
            .expect("compute_digest must not fail");

        let cached_entries = cache
            .get(immutables.clone())
            .await
            .expect("Cache read should not fail");
        let expected: BTreeMap<_, _> = immutables
            .into_iter()
            .map(|i| {
                let digest = hex::encode(i.compute_raw_hash::<Sha256>().unwrap());
                (i, Some(digest))
            })
            .collect();

        assert_eq!(expected, cached_entries);
    }

    #[tokio::test]
    async fn compute_digests_for_range_stores_digests_into_cache_provider() {
        let cardano_db = db_builder("compute_digests_for_range_stores_digests_into_cache_provider")
            .with_immutables(&[1, 2])
            .append_immutable_trio()
            .build();
        let immutables = cardano_db.get_immutable_files().clone();
        let cache = Arc::new(MemoryImmutableFileDigestCacheProvider::default());
        let logger = TestLogger::stdout();
        let digester = CardanoImmutableDigester::new(Some(cache.clone()), logger.clone());
        let immutable_range = 1..=2;

        digester
            .compute_digests_for_range(cardano_db.get_immutable_dir(), &immutable_range)
            .await
            .expect("compute_digests_for_range must not fail");

        let cached_entries = cache
            .get(immutables.clone())
            .await
            .expect("Cache read should not fail");
        let expected: BTreeMap<_, _> = immutables
            .into_iter()
            .filter(|i| immutable_range.contains(&i.number))
            .map(|i| {
                let digest = hex::encode(i.compute_raw_hash::<Sha256>().unwrap());
                (i.to_owned(), Some(digest))
            })
            .collect();

        assert_eq!(expected, cached_entries);
    }

    #[tokio::test]
    async fn computed_merkle_tree_with_cold_or_hot_or_without_any_cache_are_equals() {
        let cardano_db = DummyCardanoDbBuilder::new(
            "computed_merkle_tree_with_cold_or_hot_or_without_any_cache_are_equals",
        )
        .with_immutables(&[1, 2, 3])
        .append_immutable_trio()
        .build();
        let logger = TestLogger::stdout();
        let no_cache_digester = CardanoImmutableDigester::new(None, logger.clone());
        let cache_digester = CardanoImmutableDigester::new(
            Some(Arc::new(MemoryImmutableFileDigestCacheProvider::default())),
            logger.clone(),
        );
        let beacon = CardanoDbBeacon::new(1, 3);

        let without_cache_digest = no_cache_digester
            .compute_merkle_tree(cardano_db.get_immutable_dir(), &beacon)
            .await
            .expect("compute_merkle_tree must not fail");

        let cold_cache_digest = cache_digester
            .compute_merkle_tree(cardano_db.get_immutable_dir(), &beacon)
            .await
            .expect("compute_merkle_tree must not fail");

        let full_cache_digest = cache_digester
            .compute_merkle_tree(cardano_db.get_immutable_dir(), &beacon)
            .await
            .expect("compute_merkle_tree must not fail");

        let without_cache_merkle_root = without_cache_digest.compute_root().unwrap();
        let cold_cache_merkle_root = cold_cache_digest.compute_root().unwrap();
        let full_cache_merkle_root = full_cache_digest.compute_root().unwrap();
        assert_eq!(
            without_cache_merkle_root, full_cache_merkle_root,
            "Merkle roots with or without cache should be the same"
        );

        assert_eq!(
            cold_cache_merkle_root, full_cache_merkle_root,
            "Merkle roots with cold or with hot cache should be the same"
        );
    }

    #[tokio::test]
    async fn computed_digests_for_range_with_cold_or_hot_or_without_any_cache_are_equals() {
        let cardano_db = DummyCardanoDbBuilder::new(
            "computed_digests_for_range_with_cold_or_hot_or_without_any_cache_are_equals",
        )
        .with_immutables(&[1, 2, 3])
        .append_immutable_trio()
        .build();
        let logger = TestLogger::stdout();
        let no_cache_digester = CardanoImmutableDigester::new(None, logger.clone());
        let cache_digester = CardanoImmutableDigester::new(
            Some(Arc::new(MemoryImmutableFileDigestCacheProvider::default())),
            logger.clone(),
        );
        let immutable_range = 1..=3;

        let without_cache_digests = no_cache_digester
            .compute_digests_for_range(cardano_db.get_immutable_dir(), &immutable_range)
            .await
            .expect("compute_digests_for_range must not fail");

        let cold_cache_digests = cache_digester
            .compute_digests_for_range(cardano_db.get_immutable_dir(), &immutable_range)
            .await
            .expect("compute_digests_for_range must not fail");

        let full_cache_digests = cache_digester
            .compute_digests_for_range(cardano_db.get_immutable_dir(), &immutable_range)
            .await
            .expect("compute_digests_for_range must not fail");

        let without_cache_entries = without_cache_digests.entries;
        let cold_cache_entries = cold_cache_digests.entries;
        let full_cache_entries = full_cache_digests.entries;
        assert_eq!(
            without_cache_entries, full_cache_entries,
            "Digests for range with or without cache should be the same"
        );

        assert_eq!(
            cold_cache_entries, full_cache_entries,
            "Digests for range with cold or with hot cache should be the same"
        );
    }

    #[tokio::test]
    async fn cache_read_failure_dont_block_computations() {
        let cardano_db = db_builder("cache_read_failure_dont_block_computation")
            .with_immutables(&[1, 2, 3])
            .append_immutable_trio()
            .build();
        let mut cache = MockImmutableFileDigestCacheProvider::new();
        cache.expect_get().returning(|_| Ok(BTreeMap::new()));
        cache.expect_store().returning(|_| {
            Err(ImmutableDigesterCacheProviderError::Store(
                ImmutableDigesterCacheStoreError::Io(io::Error::other("error")),
            ))
        });
        let logger = TestLogger::stdout();
        let digester = CardanoImmutableDigester::new(Some(Arc::new(cache)), logger.clone());
        let beacon = CardanoDbBeacon::new(1, 3);

        digester
            .compute_merkle_tree(cardano_db.get_immutable_dir(), &beacon)
            .await
            .expect("compute_merkle_tree must not fail even with cache write failure");
    }

    #[tokio::test]
    async fn cache_write_failure_dont_block_computation() {
        let cardano_db = db_builder("cache_write_failure_dont_block_computation")
            .with_immutables(&[1, 2, 3])
            .append_immutable_trio()
            .build();
        let mut cache = MockImmutableFileDigestCacheProvider::new();
        cache.expect_get().returning(|_| {
            Err(ImmutableDigesterCacheProviderError::Get(
                ImmutableDigesterCacheGetError::Io(io::Error::other("error")),
            ))
        });
        cache.expect_store().returning(|_| Ok(()));
        let logger = TestLogger::stdout();
        let digester = CardanoImmutableDigester::new(Some(Arc::new(cache)), logger.clone());
        let beacon = CardanoDbBeacon::new(1, 3);

        digester
            .compute_merkle_tree(cardano_db.get_immutable_dir(), &beacon)
            .await
            .expect("compute_merkle_tree must not fail even with cache read failure");
    }
}

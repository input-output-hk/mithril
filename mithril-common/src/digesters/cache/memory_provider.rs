use super::CardanoImmutableDigesterCacheProvider;
use crate::digesters::ImmutableFile;
use crate::entities::{HexEncodedDigest, ImmutableFileName};

use async_trait::async_trait;
use std::collections::BTreeMap;
use tokio::sync::RwLock;

/// A in memory [CardanoImmutableDigesterCacheProvider].
pub struct MemoryCardanoImmutableDigesterCacheProvider {
    store: RwLock<BTreeMap<ImmutableFileName, HexEncodedDigest>>,
}

impl Default for MemoryCardanoImmutableDigesterCacheProvider {
    fn default() -> Self {
        Self {
            store: RwLock::new(BTreeMap::new()),
        }
    }
}

#[async_trait]
impl CardanoImmutableDigesterCacheProvider for MemoryCardanoImmutableDigesterCacheProvider {
    async fn store(&self, digest_per_filenames: BTreeMap<ImmutableFileName, HexEncodedDigest>) {
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

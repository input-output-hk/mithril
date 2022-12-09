use crate::digesters::ImmutableFile;
use crate::entities::{HexEncodedDigest, ImmutableFileName};

use async_trait::async_trait;
use std::collections::BTreeMap;

/// A cache provider that store individual [ImmutableFile] digests.
#[async_trait]
pub trait CardanoImmutableDigesterCacheProvider: Sync + Send {
    /// Store the given digests
    async fn store(&self, digest_per_filenames: BTreeMap<ImmutableFileName, HexEncodedDigest>);

    /// Get all digest for the given [ImmutableFile] if a cached value can be found.
    async fn get(
        &self,
        immutables: Vec<ImmutableFile>,
    ) -> BTreeMap<ImmutableFile, Option<HexEncodedDigest>>;
}

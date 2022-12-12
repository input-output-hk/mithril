use crate::digesters::ImmutableFile;
use crate::entities::{HexEncodedDigest, ImmutableFileName};

use async_trait::async_trait;
use std::collections::BTreeMap;

/// A cache provider that store individual [ImmutableFile] digests.
#[async_trait]
pub trait CardanoImmutableDigesterCacheProvider: Sync + Send {
    /// Store the given digests
    async fn store(&self, digest_per_filenames: Vec<(ImmutableFileName, HexEncodedDigest)>);

    /// Associate each given [immutable files][ImmutableFile] with a cached value if one exist.
    async fn get(
        &self,
        immutables: Vec<ImmutableFile>,
    ) -> BTreeMap<ImmutableFile, Option<HexEncodedDigest>>;
}

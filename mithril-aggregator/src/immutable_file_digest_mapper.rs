use std::collections::BTreeMap;

use async_trait::async_trait;

use mithril_common::{
    entities::{HexEncodedDigest, ImmutableFileName},
    StdResult,
};

/// A trait for mapping [ImmutableFileName]s to their digests.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait ImmutableFileDigestMapper: Sync + Send {
    /// Associate each given [immutable files][ImmutableFileName] with a digest.
    async fn get_immutable_file_digest_map(
        &self,
    ) -> StdResult<BTreeMap<ImmutableFileName, HexEncodedDigest>>;
}

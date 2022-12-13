use crate::digesters::ImmutableFile;
use crate::entities::{HexEncodedDigest, ImmutableFileName};

use async_trait::async_trait;
use std::collections::BTreeMap;
use std::io;
use thiserror::Error;

#[cfg(test)]
pub use mockall::automock;

/// A specialized result type for [ImmutableFileDigestCacheProvider].
pub type CacheProviderResult<T> = Result<T, ImmutableDigesterCacheProviderError>;

/// [ImmutableFileDigestCacheProvider] related errors.
#[derive(Error, Debug)]
pub enum ImmutableDigesterCacheProviderError {
    /// Error raised by [ImmutableFileDigestCacheProvider::store].
    #[error("Could not store immutable file digests cache: {0}")]
    Store(#[from] ImmutableDigesterCacheStoreError),

    /// Error raised by [ImmutableFileDigestCacheProvider::get].
    #[error("Could not read immutable file digests cache: {0}")]
    Get(#[from] ImmutableDigesterCacheGetError),
}

/// [ImmutableFileDigestCacheProvider::store] related errors.
#[derive(Error, Debug)]
pub enum ImmutableDigesterCacheStoreError {
    /// Raised when an IO error is raised when storing a cache.
    #[error("IO error when storing cache: {0}")]
    Io(#[from] io::Error),

    /// Raised when json cache serialization fails.
    #[error("IO error when serializing json cache: {0}")]
    JsonSerialization(#[from] serde_json::Error),
}

/// [ImmutableFileDigestCacheProvider::get] related errors.
#[derive(Error, Debug)]
pub enum ImmutableDigesterCacheGetError {
    /// Raised when an IO error is raised when getting a cache.
    #[error("IO error when getting cache: {0}")]
    Io(#[from] io::Error),

    /// Raised when json cache deserialization fails.
    #[error("IO error when deserializing json cache: {0}")]
    JsonDeserialization(#[from] serde_json::Error),
}

/// A cache provider that store individual [ImmutableFile] digests.
#[cfg_attr(test, automock)]
#[async_trait]
pub trait ImmutableFileDigestCacheProvider: Sync + Send {
    /// Store the given digests
    async fn store(
        &self,
        digest_per_filenames: Vec<(ImmutableFileName, HexEncodedDigest)>,
    ) -> CacheProviderResult<()>;

    /// Associate each given [immutable files][ImmutableFile] with a cached value if one exist.
    async fn get(
        &self,
        immutables: Vec<ImmutableFile>,
    ) -> CacheProviderResult<BTreeMap<ImmutableFile, Option<HexEncodedDigest>>>;
}

use std::collections::BTreeMap;
use std::io;

use async_trait::async_trait;
use thiserror::Error;

use crate::digesters::ImmutableFile;
use crate::entities::{HexEncodedDigest, ImmutableFileName};
use crate::StdError;

/// A specialized result type for [ImmutableFileDigestCacheProvider].
pub type CacheProviderResult<T> = Result<T, ImmutableDigesterCacheProviderError>;

/// [ImmutableFileDigestCacheProvider] related errors.
#[derive(Error, Debug)]
pub enum ImmutableDigesterCacheProviderError {
    /// Error raised by [ImmutableFileDigestCacheProvider::store].
    #[error("Could not store immutable file digests cache")]
    Store(#[from] ImmutableDigesterCacheStoreError),

    /// Error raised by [ImmutableFileDigestCacheProvider::get].
    #[error("Could not read immutable file digests cache")]
    Get(#[from] ImmutableDigesterCacheGetError),
}

/// [ImmutableFileDigestCacheProvider::store] related errors.
#[derive(Error, Debug)]
pub enum ImmutableDigesterCacheStoreError {
    /// Raised when an IO error is raised when storing a cache.
    #[error("IO error when storing cache")]
    Io(#[from] io::Error),

    /// Raised when json cache serialization fails.
    #[error("IO error when serializing json cache")]
    JsonSerialization(#[from] serde_json::Error),

    /// Raised when the underlying store failed storing a cache.
    #[error("Underlying store raised an error when storing cache")]
    StoreError(#[source] StdError),
}

/// [ImmutableFileDigestCacheProvider::get] related errors.
#[derive(Error, Debug)]
pub enum ImmutableDigesterCacheGetError {
    /// Raised when an IO error is raised when getting a cache.
    #[error("IO error when getting cache")]
    Io(#[from] io::Error),

    /// Raised when json cache deserialization fails.
    #[error("IO error when deserializing json cache")]
    JsonDeserialization(#[from] serde_json::Error),

    /// Raised when the underlying store failed getting a cache.
    #[error("Underlying store raised an error when getting cache")]
    StoreError(#[source] StdError),
}

/// A cache provider that store individual [ImmutableFile] digests.
#[cfg_attr(test, mockall::automock)]
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

    /// Reset the stored values
    async fn reset(&self) -> CacheProviderResult<()>;
}

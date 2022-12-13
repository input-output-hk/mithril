//! Cache provider to accelerate [super::CardanoImmutableDigester] computation by storing and
//! reusing [super::ImmutableFile] digests.

mod json_provider;
mod memory_provider;
mod provider;

pub use json_provider::JsonImmutableFileDigestCacheProvider;
pub use memory_provider::MemoryImmutableFileDigestCacheProvider;
#[cfg(test)]
pub use provider::MockImmutableFileDigestCacheProvider;
pub use provider::{
    CacheProviderResult, ImmutableDigesterCacheGetError, ImmutableDigesterCacheProviderError,
    ImmutableDigesterCacheStoreError, ImmutableFileDigestCacheProvider,
};

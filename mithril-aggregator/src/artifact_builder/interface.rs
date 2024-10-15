use async_trait::async_trait;
use mithril_common::{
    entities::Certificate,
    signable_builder::{Artifact, Beacon},
    StdResult,
};

/// ArtifactBuilder is trait for building an artifact
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait ArtifactBuilder<U, W>: Send + Sync
where
    U: Beacon,
    W: Artifact,
{
    /// Compute an artifact
    async fn compute_artifact(&self, beacon: U, certificate: &Certificate) -> StdResult<W>;
}

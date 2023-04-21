use async_trait::async_trait;
use mithril_common::{
    entities::Certificate,
    signable_builder::{Artifact, Beacon},
    StdResult,
};

/// ArtifactBuilder is trait for building an artifact
#[async_trait]
pub trait ArtifactBuilder<'a, U, W>
where
    U: Beacon,
    W: Artifact<'a>,
{
    /// Compute an artifact
    async fn compute_artifact(&'a self, beacon: U, certificate: &Certificate) -> StdResult<W>;
}

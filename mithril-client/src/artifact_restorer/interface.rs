use async_trait::async_trait;
use mithril_common::{entities::Certificate, signable_builder::Beacon, StdResult};
use serde::{Deserialize, Serialize};
use std::fmt::Debug;

/// ArtifactRestorer is trait for restoring an artifact
#[async_trait]
trait ArtifactRestorer<'a, V, W>
where
    V: Signable,
    W: Artifact<'a>,
{
    /// Compute a signable from an artifact
    async fn compute_signable(&self, artifact: W) -> StdResult<V>;

    /// Restore an artifact
    async fn restore_artifact(&'a self, artifact: W) -> StdResult<()>;
}

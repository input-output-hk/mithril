use async_trait::async_trait;
use mithril_common::{entities::Certificate, signable_builder::Beacon, StdResult};
use serde::{Deserialize, Serialize};
use std::fmt::Debug;

/// Artifact is a trait for types that represent signed artifacts
pub trait Artifact<'a>: Serialize + Deserialize<'a> + PartialEq + Debug {}

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

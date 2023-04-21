use std::marker::PhantomData;

use async_trait::async_trait;
use mithril_common::{
    entities::Certificate,
    signable_builder::{DummyBeacon, DummySignable},
    StdResult,
};
use serde::{Deserialize, Serialize};

use crate::artifact_builder::{Artifact, ArtifactRestorer};

/// Dummy artifact
#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub struct DummyArtifact<'a> {
    message: String,
    beacon: DummyBeacon,
    phantom: PhantomData<&'a DummyBeacon>,
}

impl<'a> DummyArtifact<'a> {
    /// Dummy artifact factory
    pub fn new(message: String, beacon: DummyBeacon) -> Self {
        Self {
            message,
            beacon,
            phantom: PhantomData,
        }
    }
}

impl<'a> Artifact<'a> for DummyArtifact<'a> {}

/// A [DummyArtifact] restorer
pub struct DummyArtifactRestorer {}

impl DummyArtifactRestorer {
    /// Dummy artifact builder restorer
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for DummyArtifactRestorer {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl<'a> ArtifactRestorer<'a, DummyBeacon, DummyArtifact<'a>> for DummyArtifactRestorer {
    async fn compute_artifact(
        &'a self,
        beacon: DummyBeacon,
        certificate: &Certificate,
    ) -> StdResult<DummyArtifact> {
        Ok(DummyArtifact::new(
            format!("certificate id is {}", certificate.hash),
            beacon,
        ))
    }

    /// Compute a signable from an artifact
    async fn compute_signable(&self, artifact: DummyArtifact) -> StdResult<DummySignable> {
        Ok(DummySignable::new(artifact.message, artifact.beacon))
    }

    /// Restore an artifact
    async fn restore_artifact(&'a self, artifact: DummyArtifact) -> StdResult<()> {
        Ok(())
    }
}

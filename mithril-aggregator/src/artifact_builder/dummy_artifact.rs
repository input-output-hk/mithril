use async_trait::async_trait;
use mithril_common::{
    entities::Certificate,
    signable_builder::{Artifact, DummyBeacon},
    StdResult,
};
use serde::{Deserialize, Serialize};

use crate::artifact_builder::ArtifactBuilder;

/// Dummy artifact
#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub struct DummyArtifact {
    message: String,
    beacon: DummyBeacon,
}

impl DummyArtifact {
    /// Dummy artifact factory
    pub fn new(message: String, beacon: DummyBeacon) -> Self {
        Self { message, beacon }
    }
}

impl Artifact for DummyArtifact {}

/// A [DummyArtifact] builder
pub struct DummyArtifactBuilder {}

impl DummyArtifactBuilder {
    /// Dummy artifact builder factory
    pub fn new() -> Self {
        Self {}
    }
}

impl Default for DummyArtifactBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl ArtifactBuilder<DummyBeacon, DummyArtifact> for DummyArtifactBuilder {
    async fn compute_artifact(
        &self,
        beacon: DummyBeacon,
        certificate: &Certificate,
    ) -> StdResult<DummyArtifact> {
        Ok(DummyArtifact::new(
            format!("certificate id is {}", certificate.hash),
            beacon,
        ))
    }
}

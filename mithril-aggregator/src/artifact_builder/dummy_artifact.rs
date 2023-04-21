use std::marker::PhantomData;

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
impl<'a> ArtifactBuilder<'a, DummyBeacon, DummyArtifact<'a>> for DummyArtifactBuilder {
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
}

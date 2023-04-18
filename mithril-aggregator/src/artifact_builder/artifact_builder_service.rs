use std::sync::Arc;

use mithril_common::{
    entities::{Certificate, SignedEntityType},
    signable_builder::DummyBeacon,
    StdResult,
};

use crate::artifact_builder::{Artifact, DummyArtifactBuilder};

use super::ArtifactBuilder;

/// ArtifactBuilder Service
// TODO: temporary implementation
pub struct ArtifactBuilderService {
    dummy_artifact_builder: DummyArtifactBuilder,
}

impl ArtifactBuilderService {
    /// ArtifactBuilderService factory
    pub fn new(dummy_artifact_builder: DummyArtifactBuilder) -> Self {
        Self {
            dummy_artifact_builder,
        }
    }
}

impl ArtifactBuilderService {
    #[allow(dead_code)]
    async fn compute_artifact(
        &self,
        signed_entity_type: SignedEntityType,
        certificate: Certificate,
    ) -> StdResult<Arc<impl Artifact>> {
        let artifact = match signed_entity_type {
            SignedEntityType::MithrilStakeDistribution(e) => Arc::new(
                self.dummy_artifact_builder
                    .compute_artifact(DummyBeacon { epoch: e }, certificate)
                    .await?,
            ),
            SignedEntityType::CardanoStakeDistribution(e) => Arc::new(
                self.dummy_artifact_builder
                    .compute_artifact(DummyBeacon { epoch: e }, certificate)
                    .await?,
            ),
            SignedEntityType::CardanoImmutableFilesFull(b) => Arc::new(
                self.dummy_artifact_builder
                    .compute_artifact(DummyBeacon { epoch: b.epoch }, certificate)
                    .await?,
            ),
        };

        Ok(artifact)
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::{Beacon, Epoch};

    use super::*;

    // TODO: temporary test
    #[tokio::test]
    async fn test_artifact_builder_service() {
        let dummy_artifact_builder = DummyArtifactBuilder::default();
        let artifact_builder_service = ArtifactBuilderService::new(dummy_artifact_builder);
        let certificate = Certificate::default();

        let signed_entity_type_1 = SignedEntityType::MithrilStakeDistribution(Epoch(1));
        let artifact_1 = artifact_builder_service
            .compute_artifact(signed_entity_type_1, certificate.clone())
            .await
            .unwrap();

        let signed_entity_type_2 = SignedEntityType::CardanoStakeDistribution(Epoch(0));
        let artifact_2 = artifact_builder_service
            .compute_artifact(signed_entity_type_2, certificate.clone())
            .await
            .unwrap();

        let signed_entity_type_3 = SignedEntityType::CardanoImmutableFilesFull(Beacon::default());
        let artifact_3 = artifact_builder_service
            .compute_artifact(signed_entity_type_3, certificate)
            .await
            .unwrap();

        assert_ne!(artifact_1, artifact_2);
        assert_ne!(artifact_1, artifact_3);
        assert_eq!(artifact_2, artifact_3);
    }
}

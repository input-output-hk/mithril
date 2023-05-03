use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio::sync::RwLock;

use super::ArtifactBuilder;
use crate::MultiSigner;
use mithril_common::{
    entities::{Certificate, Epoch, SignerWithStake},
    signable_builder::Artifact,
    StdResult,
};

/// Mithril Stake Distribution
#[derive(Debug, PartialEq, Serialize, Deserialize, Clone)]
pub struct MithrilStakeDistribution {
    signers_with_stake: Vec<SignerWithStake>,
}

impl MithrilStakeDistribution {
    /// MithrilStakeDistribution artifact factory
    pub fn new(signers_with_stake: Vec<SignerWithStake>) -> Self {
        Self { signers_with_stake }
    }
}

#[typetag::serde]
impl Artifact for MithrilStakeDistribution {}

/// A [MithrilStakeDistributionArtifact] builder
pub struct MithrilStakeDistributionArtifactBuilder {
    multi_signer: Arc<RwLock<dyn MultiSigner>>,
}

impl MithrilStakeDistributionArtifactBuilder {
    /// MithrilStakeDistribution artifact builder factory
    pub fn new(multi_signer: Arc<RwLock<dyn MultiSigner>>) -> Self {
        Self { multi_signer }
    }
}

#[async_trait]
impl ArtifactBuilder<Epoch, MithrilStakeDistribution> for MithrilStakeDistributionArtifactBuilder {
    async fn compute_artifact(
        &self,
        _beacon: Epoch,
        _certificate: &Certificate,
    ) -> StdResult<MithrilStakeDistribution> {
        let multi_signer = self.multi_signer.read().await;
        Ok(MithrilStakeDistribution::new(
            multi_signer.get_next_signers_with_stake().await?,
        ))
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::test_utils::fake_data;

    use super::*;

    use crate::multi_signer::MockMultiSigner;

    #[tokio::test]
    async fn test_compute_artifact() {
        let signers_with_stake = fake_data::signers_with_stakes(5);
        let signers_with_stake_clone = signers_with_stake.clone();
        let certificate = fake_data::certificate("cert-123".to_string());
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_get_next_signers_with_stake()
            .return_once(move || Ok(signers_with_stake_clone));
        let mithril_stake_distribution_artifact_builder =
            MithrilStakeDistributionArtifactBuilder::new(Arc::new(RwLock::new(mock_multi_signer)));
        let artifact = mithril_stake_distribution_artifact_builder
            .compute_artifact(Epoch(1), &certificate)
            .await
            .unwrap();
        let artifact_expected = MithrilStakeDistribution::new(signers_with_stake);
        assert_eq!(artifact_expected, artifact);
    }
}

use async_trait::async_trait;
use std::sync::Arc;
use tokio::sync::RwLock;

use super::ArtifactBuilder;
use crate::MultiSigner;
use mithril_common::{
    entities::{Certificate, Epoch, MithrilStakeDistribution},
    StdResult,
};

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
        beacon: Epoch,
        _certificate: &Certificate,
    ) -> StdResult<MithrilStakeDistribution> {
        let multi_signer = self.multi_signer.read().await;
        Ok(MithrilStakeDistribution::new(
            beacon,
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
    async fn should_compute_valid_artifact() {
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
        let artifact_expected = MithrilStakeDistribution::new(Epoch(1), signers_with_stake);
        assert_eq!(artifact_expected, artifact);
    }

    #[test]
    fn sort_given_signers_when_created() {
        let signers_with_stake = fake_data::signers_with_stakes(5);

        assert_eq!(
            MithrilStakeDistribution::new(Epoch(1), signers_with_stake.clone()),
            MithrilStakeDistribution::new(Epoch(1), signers_with_stake.into_iter().rev().collect())
        );
    }

    #[test]
    fn hash_value_doesnt_change_if_signers_order_change() {
        let signers_with_stake = fake_data::signers_with_stakes(5);

        let sd = MithrilStakeDistribution::new(Epoch(1), signers_with_stake.clone());
        let sd2 =
            MithrilStakeDistribution::new(Epoch(1), signers_with_stake.into_iter().rev().collect());

        assert_eq!(sd.hash, sd2.hash);
    }
}

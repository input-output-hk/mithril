use anyhow::anyhow;
use async_trait::async_trait;
use std::sync::Arc;

use mithril_common::{
    entities::{CardanoStakeDistribution, Certificate, Epoch},
    StdResult,
};
use mithril_persistence::store::StakeStorer;

use crate::ArtifactBuilder;

/// A [CardanoStakeDistributionArtifact] builder
pub struct CardanoStakeDistributionArtifactBuilder {
    stake_store: Arc<dyn StakeStorer>,
}

impl CardanoStakeDistributionArtifactBuilder {
    /// CardanoStakeDistribution artifact builder factory
    pub fn new(stake_store: Arc<dyn StakeStorer>) -> Self {
        Self { stake_store }
    }
}

#[async_trait]
impl ArtifactBuilder<Epoch, CardanoStakeDistribution> for CardanoStakeDistributionArtifactBuilder {
    async fn compute_artifact(
        &self,
        epoch: Epoch,
        _certificate: &Certificate,
    ) -> StdResult<CardanoStakeDistribution> {
        let stake_distribution = self
            .stake_store
            .get_stakes(epoch)
            .await?
            .ok_or_else(|| anyhow!("No stake distribution found for epoch '{}'", epoch))?;

        Ok(CardanoStakeDistribution::new(epoch, stake_distribution))
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::{entities::StakeDistribution, test_utils::fake_data};
    use mockall::{mock, predicate::eq};

    use super::*;

    mock! {
        pub StakeStorerImpl {}

        #[async_trait]
        impl StakeStorer for StakeStorerImpl {
            async fn save_stakes(
                &self,
                epoch: Epoch,
                stakes: StakeDistribution,
            ) -> StdResult<Option<StakeDistribution>>;
            async fn get_stakes(&self, epoch: Epoch) -> StdResult<Option<StakeDistribution>>;
        }
    }

    #[tokio::test]
    async fn compute_artifact_returns_valid_artifact_if_stakes_stored_for_epoch() {
        let epoch = Epoch(1);
        let certificate = fake_data::certificate("whatever".to_string());
        let stake_distribution = StakeDistribution::from([("pool-123".to_string(), 123)]);
        let stake_distribution_clone = stake_distribution.clone();
        let mut mock_storer = MockStakeStorerImpl::new();
        mock_storer
            .expect_get_stakes()
            .with(eq(epoch))
            .return_once(move |_| Ok(Some(stake_distribution_clone)));
        let builder = CardanoStakeDistributionArtifactBuilder::new(Arc::new(mock_storer));

        let cardano_stake_distribution =
            builder.compute_artifact(epoch, &certificate).await.unwrap();

        let expected = CardanoStakeDistribution::new(epoch, stake_distribution);
        assert_eq!(cardano_stake_distribution, expected);
    }

    #[tokio::test]
    async fn compute_artifact_returns_error_if_no_stakes_found_for_epoch() {
        let epoch = Epoch(1);
        let certificate = fake_data::certificate("whatever".to_string());
        let mut mock_storer = MockStakeStorerImpl::new();
        mock_storer
            .expect_get_stakes()
            .with(eq(epoch))
            .return_once(move |_| Ok(None));
        let builder = CardanoStakeDistributionArtifactBuilder::new(Arc::new(mock_storer));

        builder
            .compute_artifact(epoch, &certificate)
            .await
            .expect_err("Should return error");
    }
}

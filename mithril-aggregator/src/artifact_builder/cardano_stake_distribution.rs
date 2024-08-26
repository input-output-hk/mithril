use anyhow::anyhow;
use async_trait::async_trait;
use std::sync::Arc;

use mithril_common::{
    entities::{CardanoStakeDistribution, Certificate, Epoch},
    signable_builder::StakeDistributionRetriever,
    StdResult,
};

use crate::ArtifactBuilder;

/// A [CardanoStakeDistributionArtifact] builder
pub struct CardanoStakeDistributionArtifactBuilder {
    stake_distribution_retriever: Arc<dyn StakeDistributionRetriever>,
}

impl CardanoStakeDistributionArtifactBuilder {
    /// CardanoStakeDistribution artifact builder factory
    pub fn new(stake_distribution_retriever: Arc<dyn StakeDistributionRetriever>) -> Self {
        Self {
            stake_distribution_retriever,
        }
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
            .stake_distribution_retriever
            .retrieve(epoch.offset_to_cardano_stake_distribution_snapshot_epoch())
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
        pub StakeDistributionRetrieverImpl {}

        #[async_trait]
        impl StakeDistributionRetriever for StakeDistributionRetrieverImpl {
            async fn retrieve(&self, epoch: Epoch) -> StdResult<Option<StakeDistribution>>;
        }
    }

    #[tokio::test]
    async fn compute_artifact_returns_valid_artifact_and_retrieve_with_epoch_offset() {
        let epoch = Epoch(1);
        let epoch_to_retrieve = Epoch(3);
        let certificate = fake_data::certificate("whatever".to_string());
        let stake_distribution = StakeDistribution::from([("pool-123".to_string(), 123)]);
        let stake_distribution_clone = stake_distribution.clone();
        let mut mock_storer = MockStakeDistributionRetrieverImpl::new();
        mock_storer
            .expect_retrieve()
            .with(eq(epoch_to_retrieve))
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
        let epoch_to_retrieve = Epoch(3);
        let certificate = fake_data::certificate("whatever".to_string());
        let mut mock_storer = MockStakeDistributionRetrieverImpl::new();
        mock_storer
            .expect_retrieve()
            .with(eq(epoch_to_retrieve))
            .return_once(move |_| Ok(None));
        let builder = CardanoStakeDistributionArtifactBuilder::new(Arc::new(mock_storer));

        builder
            .compute_artifact(epoch, &certificate)
            .await
            .expect_err("Should return error");
    }
}

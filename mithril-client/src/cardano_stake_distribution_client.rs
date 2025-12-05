//! A client to retrieve Cardano stake distributions data from an Aggregator.
//!
//! In order to do so it defines a [CardanoStakeDistributionClient] which exposes the following features:
//!  - [get][CardanoStakeDistributionClient::get]: get a Cardano stake distribution data from its hash
//!  - [get_by_epoch][CardanoStakeDistributionClient::get_by_epoch]: get a Cardano stake distribution data from its epoch
//!  - [list][CardanoStakeDistributionClient::list]: get the list of available Cardano stake distribution
//!
//! # Get a Cardano stake distribution
//!
//! To get a Cardano stake distribution using the [ClientBuilder][crate::client::ClientBuilder].
//!
//! ```no_run
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::ClientBuilder;
//!
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//! let cardano_stake_distribution = client.cardano_stake_distribution().get("CARDANO_STAKE_DISTRIBUTION_HASH").await?.unwrap();
//!
//! println!(
//!     "Cardano stake distribution hash={}, epoch={}, stake_distribution={:?}",
//!     cardano_stake_distribution.hash,
//!     cardano_stake_distribution.epoch,
//!     cardano_stake_distribution.stake_distribution
//! );
//! #    Ok(())
//! # }
//! ```
//!
//! # List available Cardano stake distributions
//!
//! To list available Cardano stake distributions using the [ClientBuilder][crate::client::ClientBuilder].
//!
//! ```no_run
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::ClientBuilder;
//!
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//! let cardano_stake_distributions = client.cardano_stake_distribution().list().await?;
//!
//! for cardano_stake_distribution in cardano_stake_distributions {
//!     println!("Cardano stake distribution hash={}, epoch={}", cardano_stake_distribution.hash, cardano_stake_distribution.epoch);
//! }
//! #    Ok(())
//! # }
//! ```
//!
//! # Get a Cardano stake distribution by epoch
//!
//! To get a Cardano stake distribution by epoch using the [ClientBuilder][crate::client::ClientBuilder].
//!
//! **Note:** The epoch represents the epoch at the end of which the Cardano stake distribution is computed by the Cardano node
//!
//! ```no_run
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::{ClientBuilder, common::Epoch};
//!
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//! // For a specific epoch
//! let cardano_stake_distribution = client.cardano_stake_distribution().get_by_epoch(Epoch(500)).await?.unwrap();
//! // For the latest epoch known by the Mithril aggregator
//! let cardano_stake_distribution = client.cardano_stake_distribution().get_for_latest_epoch().await?.unwrap();
//! // For the latest epoch known by the Mithril aggregator with an offset
//! let cardano_stake_distribution = client.cardano_stake_distribution().get_for_latest_epoch_with_offset(4).await?.unwrap();
//!
//! println!(
//!     "Cardano stake distribution hash={}, epoch={}, stake_distribution={:?}",
//!     cardano_stake_distribution.hash,
//!     cardano_stake_distribution.epoch,
//!     cardano_stake_distribution.stake_distribution
//! );
//! #    Ok(())
//! # }
//! ```

use std::sync::Arc;

use crate::common::{Epoch, EpochSpecifier};
use crate::{CardanoStakeDistribution, CardanoStakeDistributionListItem, MithrilResult};

/// HTTP client for CardanoStakeDistribution API from the Aggregator
pub struct CardanoStakeDistributionClient {
    aggregator_requester: Arc<dyn CardanoStakeDistributionAggregatorRequest>,
}

/// Define the requests against an Aggregator related to Cardano stake distribution.
#[cfg_attr(test, mockall::automock)]
#[cfg_attr(target_family = "wasm", async_trait::async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait::async_trait)]
pub trait CardanoStakeDistributionAggregatorRequest: Send + Sync {
    /// Get the list of latest Cardano stake distributions from the Aggregator.
    async fn list_latest(&self) -> MithrilResult<Vec<CardanoStakeDistributionListItem>>;

    /// Get a Cardano stake distribution for a given hash from the Aggregator.
    async fn get_by_hash(&self, hash: &str) -> MithrilResult<Option<CardanoStakeDistribution>>;

    /// Get a Cardano stake distribution for an [EpochSpecifier] from the Aggregator.
    async fn get_by_epoch(
        &self,
        specifier: EpochSpecifier,
    ) -> MithrilResult<Option<CardanoStakeDistribution>>;
}

impl CardanoStakeDistributionClient {
    /// Constructs a new `CardanoStakeDistribution`.
    pub fn new(aggregator_requester: Arc<dyn CardanoStakeDistributionAggregatorRequest>) -> Self {
        Self {
            aggregator_requester,
        }
    }

    /// Fetch a list of signed CardanoStakeDistribution
    pub async fn list(&self) -> MithrilResult<Vec<CardanoStakeDistributionListItem>> {
        self.aggregator_requester.list_latest().await
    }

    /// Get the given Cardano stake distribution data by hash.
    pub async fn get(&self, hash: &str) -> MithrilResult<Option<CardanoStakeDistribution>> {
        self.aggregator_requester.get_by_hash(hash).await
    }

    /// Get the given Cardano stake distribution data by epoch.
    pub async fn get_by_epoch(
        &self,
        epoch: Epoch,
    ) -> MithrilResult<Option<CardanoStakeDistribution>> {
        self.aggregator_requester
            .get_by_epoch(EpochSpecifier::Number(epoch))
            .await
    }

    /// Get the given Cardano stake distribution data by epoch.
    pub async fn get_for_latest_epoch(&self) -> MithrilResult<Option<CardanoStakeDistribution>> {
        self.aggregator_requester.get_by_epoch(EpochSpecifier::Latest).await
    }

    /// Get the given Cardano stake distribution data by epoch.
    pub async fn get_for_latest_epoch_with_offset(
        &self,
        offset: u64,
    ) -> MithrilResult<Option<CardanoStakeDistribution>> {
        self.aggregator_requester
            .get_by_epoch(EpochSpecifier::LatestMinusOffset(offset))
            .await
    }
}

#[cfg(test)]
mod tests {
    use mockall::predicate::eq;

    use mithril_common::test::mock_extensions::MockBuilder;

    use crate::common::test::Dummy;

    use super::*;

    #[tokio::test]
    async fn list_cardano_stake_distributions_returns_messages() {
        let aggregator_requester =
            MockBuilder::<MockCardanoStakeDistributionAggregatorRequest>::configure(|mock| {
                let messages = vec![
                    CardanoStakeDistributionListItem {
                        hash: "hash-123".to_string(),
                        ..Dummy::dummy()
                    },
                    CardanoStakeDistributionListItem {
                        hash: "hash-456".to_string(),
                        ..Dummy::dummy()
                    },
                ];
                mock.expect_list_latest().return_once(|| Ok(messages));
            });
        let client = CardanoStakeDistributionClient::new(aggregator_requester);

        let messages = client.list().await.unwrap();

        assert_eq!(2, messages.len());
        assert_eq!("hash-123".to_string(), messages[0].hash);
        assert_eq!("hash-456".to_string(), messages[1].hash);
    }

    #[tokio::test]
    async fn get_cardano_stake_distribution_returns_message() {
        let aggregator_requester =
            MockBuilder::<MockCardanoStakeDistributionAggregatorRequest>::configure(|mock| {
                let message = CardanoStakeDistribution {
                    hash: "hash_1".to_string(),
                    certificate_hash: "certificate_123".to_string(),
                    ..Dummy::dummy()
                };
                mock.expect_get_by_hash()
                    .with(eq(message.hash.clone()))
                    .return_once(|_| Ok(Some(message)));
            });
        let client = CardanoStakeDistributionClient::new(aggregator_requester);

        let cardano_stake_distribution = client
            .get("hash_1")
            .await
            .unwrap()
            .expect("This test returns a Cardano stake distribution");

        assert_eq!("hash_1", &cardano_stake_distribution.hash);
        assert_eq!(
            "certificate_123",
            &cardano_stake_distribution.certificate_hash
        );
    }

    #[tokio::test]
    async fn get_cardano_stake_distribution_by_epoch_returns_message() {
        let aggregator_requester =
            MockBuilder::<MockCardanoStakeDistributionAggregatorRequest>::configure(|mock| {
                let message = CardanoStakeDistribution {
                    hash: "hash_2".to_string(),
                    epoch: Epoch(2),
                    ..Dummy::dummy()
                };
                mock.expect_get_by_epoch()
                    .with(eq(EpochSpecifier::Number(Epoch(2))))
                    .return_once(|_| Ok(Some(message)));
            });
        let client = CardanoStakeDistributionClient::new(aggregator_requester);

        let cardano_stake_distribution = client
            .get_by_epoch(Epoch(2))
            .await
            .unwrap()
            .expect("This test returns a Cardano stake distribution");

        assert_eq!("hash_2", &cardano_stake_distribution.hash);
        assert_eq!(Epoch(2), &cardano_stake_distribution.epoch);
    }

    #[tokio::test]
    async fn get_cardano_stake_distribution_for_latest_epoch_returns_message() {
        let aggregator_requester =
            MockBuilder::<MockCardanoStakeDistributionAggregatorRequest>::configure(|mock| {
                let message = CardanoStakeDistribution {
                    hash: "hash_3".to_string(),
                    epoch: Epoch(3),
                    ..Dummy::dummy()
                };
                mock.expect_get_by_epoch()
                    .with(eq(EpochSpecifier::Latest))
                    .return_once(|_| Ok(Some(message)));
            });
        let client = CardanoStakeDistributionClient::new(aggregator_requester);

        let cardano_stake_distribution = client
            .get_for_latest_epoch()
            .await
            .unwrap()
            .expect("This test returns a Cardano stake distribution");

        assert_eq!("hash_3", &cardano_stake_distribution.hash);
        assert_eq!(Epoch(3), &cardano_stake_distribution.epoch);
    }

    #[tokio::test]
    async fn get_cardano_stake_distribution_for_latest_with_offset_epoch_returns_message() {
        let aggregator_requester =
            MockBuilder::<MockCardanoStakeDistributionAggregatorRequest>::configure(|mock| {
                let message = CardanoStakeDistribution {
                    hash: "hash_4".to_string(),
                    epoch: Epoch(4),
                    ..Dummy::dummy()
                };
                mock.expect_get_by_epoch()
                    .with(eq(EpochSpecifier::LatestMinusOffset(4)))
                    .return_once(|_| Ok(Some(message)));
            });
        let client = CardanoStakeDistributionClient::new(aggregator_requester);

        let cardano_stake_distribution = client
            .get_for_latest_epoch_with_offset(4)
            .await
            .unwrap()
            .expect("This test returns a Cardano stake distribution");

        assert_eq!("hash_4", &cardano_stake_distribution.hash);
        assert_eq!(Epoch(4), &cardano_stake_distribution.epoch);
    }
}

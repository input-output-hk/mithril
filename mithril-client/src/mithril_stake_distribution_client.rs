//! A client to retrieve Mithril stake distributions data from an aggregator.
//!
//! In order to do so it defines a [MithrilStakeDistributionClient] which exposes the following features:
//!  - [get][MithrilStakeDistributionClient::get]: get a Mithril stake distribution data from its hash
//!  - [list][MithrilStakeDistributionClient::list]: get the list of available Mithril stake distribution
//!
//! # Get a Mithril stake distribution
//!
//! To get a Mithril stake distribution using the [ClientBuilder][crate::client::ClientBuilder].
//!
//! ```no_run
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::ClientBuilder;
//!
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//! let mithril_stake_distribution = client.mithril_stake_distribution().get("MITHRIL_STAKE_DISTRIBUTION_HASH").await?.unwrap();
//!
//! println!("Mithril stake distribution hash={}, epoch={}", mithril_stake_distribution.hash, mithril_stake_distribution.epoch);
//! #    Ok(())
//! # }
//! ```
//!
//! # List available Mithril stake distributions
//!
//! To list available Mithril stake distributions using the [ClientBuilder][crate::client::ClientBuilder].
//!
//! ```no_run
//! # async fn run() -> mithril_client::MithrilResult<()> {
//! use mithril_client::ClientBuilder;
//!
//! let client = ClientBuilder::aggregator("YOUR_AGGREGATOR_ENDPOINT", "YOUR_GENESIS_VERIFICATION_KEY").build()?;
//! let mithril_stake_distributions = client.mithril_stake_distribution().list().await?;
//!
//! for mithril_stake_distribution in mithril_stake_distributions {
//!     println!("Mithril stake distribution hash={}, epoch={}", mithril_stake_distribution.hash, mithril_stake_distribution.epoch);
//! }
//! #    Ok(())
//! # }
//! ```

use std::sync::Arc;

use crate::{MithrilResult, MithrilStakeDistribution, MithrilStakeDistributionListItem};

/// HTTP client for MithrilStakeDistribution API from the aggregator
pub struct MithrilStakeDistributionClient {
    aggregator_requester: Arc<dyn MithrilStakeDistributionAggregatorRequest>,
}

/// Define the requests against an aggregator related to Mithril stake distribution.
#[cfg_attr(test, mockall::automock)]
#[cfg_attr(target_family = "wasm", async_trait::async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait::async_trait)]
pub trait MithrilStakeDistributionAggregatorRequest: Send + Sync {
    /// Get the list of latest Mithril stake distributions from the aggregator.
    async fn list_latest(&self) -> MithrilResult<Vec<MithrilStakeDistributionListItem>>;

    /// Get a Mithril stake distribution for a given hash from the aggregator.
    async fn get_by_hash(&self, hash: &str) -> MithrilResult<Option<MithrilStakeDistribution>>;
}

impl MithrilStakeDistributionClient {
    /// Constructs a new `MithrilStakeDistributionClient`.
    pub fn new(aggregator_requester: Arc<dyn MithrilStakeDistributionAggregatorRequest>) -> Self {
        Self {
            aggregator_requester,
        }
    }

    /// Fetch a list of signed MithrilStakeDistribution
    pub async fn list(&self) -> MithrilResult<Vec<MithrilStakeDistributionListItem>> {
        self.aggregator_requester.list_latest().await
    }

    /// Get the given stake distribution data. If it cannot be found, None is returned.
    pub async fn get(&self, hash: &str) -> MithrilResult<Option<MithrilStakeDistribution>> {
        self.aggregator_requester.get_by_hash(hash).await
    }
}

#[cfg(test)]
mod tests {
    use mockall::predicate::eq;

    use mithril_common::test::double::fake_data;
    use mithril_common::test::mock_extensions::MockBuilder;

    use crate::MithrilSigner;
    use crate::common::test::Dummy;

    use super::*;

    #[tokio::test]
    async fn get_mithril_stake_distribution_list() {
        let requester =
            MockBuilder::<MockMithrilStakeDistributionAggregatorRequest>::configure(|mock| {
                let messages = vec![
                    MithrilStakeDistributionListItem {
                        hash: "hash-123".to_string(),
                        ..Dummy::dummy()
                    },
                    MithrilStakeDistributionListItem {
                        hash: "hash-456".to_string(),
                        ..Dummy::dummy()
                    },
                ];
                mock.expect_list_latest().return_once(move || Ok(messages));
            });
        let client = MithrilStakeDistributionClient::new(requester);
        let items = client.list().await.unwrap();

        assert_eq!(2, items.len());
        assert_eq!("hash-123".to_string(), items[0].hash);
        assert_eq!("hash-456".to_string(), items[1].hash);
    }

    #[tokio::test]
    async fn get_mithril_stake_distribution() {
        let requester =
            MockBuilder::<MockMithrilStakeDistributionAggregatorRequest>::configure(|mock| {
                let message = MithrilStakeDistribution {
                    hash: "hash".to_string(),
                    signers_with_stake: MithrilSigner::from_signers(
                        fake_data::signers_with_stakes(2),
                    ),
                    ..Dummy::dummy()
                };
                mock.expect_get_by_hash()
                    .with(eq(message.hash.clone()))
                    .return_once(move |_| Ok(Some(message)));
            });
        let client = MithrilStakeDistributionClient::new(requester);

        let stake_distribution_entity = client
            .get("hash")
            .await
            .unwrap()
            .expect("should return a mithril stake distribution");

        assert_eq!("hash", &stake_distribution_entity.hash);
        assert_eq!(2, stake_distribution_entity.signers_with_stake.len(),);
    }
}

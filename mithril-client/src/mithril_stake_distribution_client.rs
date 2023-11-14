//! A client to retrieve Mithril stake distributions data from an Aggregator.
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

use crate::aggregator_client::{AggregatorClient, AggregatorClientError, AggregatorRequest};
use anyhow::Context;

use crate::{MithrilResult, MithrilStakeDistribution, MithrilStakeDistributionListItem};

/// HTTP client for MithrilStakeDistribution API from the Aggregator
pub struct MithrilStakeDistributionClient {
    aggregator_client: Arc<dyn AggregatorClient>,
}

impl MithrilStakeDistributionClient {
    /// Constructs a new `MithrilStakeDistributionClient`.
    pub fn new(aggregator_client: Arc<dyn AggregatorClient>) -> Self {
        Self { aggregator_client }
    }

    /// Fetch a list of signed MithrilStakeDistribution
    pub async fn list(&self) -> MithrilResult<Vec<MithrilStakeDistributionListItem>> {
        let response = self
            .aggregator_client
            .get_content(AggregatorRequest::ListMithrilStakeDistributions)
            .await
            .with_context(|| "MithrilStakeDistribution Client can not get the artifact list")?;
        let items = serde_json::from_str::<Vec<MithrilStakeDistributionListItem>>(&response)
            .with_context(|| "MithrilStakeDistribution Client can not deserialize artifact list")?;

        Ok(items)
    }

    /// Get the given stake distribution data. If it cannot be found, a None is returned.
    pub async fn get(&self, hash: &str) -> MithrilResult<Option<MithrilStakeDistribution>> {
        match self
            .aggregator_client
            .get_content(AggregatorRequest::GetMithrilStakeDistribution {
                hash: hash.to_string(),
            })
            .await
        {
            Ok(content) => {
                let stake_distribution_entity: MithrilStakeDistribution =
                    serde_json::from_str(&content).with_context(|| {
                        "MithrilStakeDistribution Client can not deserialize artifact"
                    })?;

                Ok(Some(stake_distribution_entity))
            }
            Err(AggregatorClientError::RemoteServerLogical(_)) => Ok(None),
            Err(e) => Err(e.into()),
        }
    }
}

#[cfg(test)]
mod tests {
    use chrono::{DateTime, Utc};
    use mithril_common::{
        entities::Epoch, messages::SignerWithStakeMessagePart, test_utils::fake_data,
    };

    use crate::aggregator_client::MockAggregatorHTTPClient;

    use super::*;

    fn fake_messages() -> Vec<MithrilStakeDistributionListItem> {
        vec![
            MithrilStakeDistributionListItem {
                epoch: Epoch(1),
                hash: "hash-123".to_string(),
                certificate_hash: "cert-hash-123".to_string(),
                created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                    .unwrap()
                    .with_timezone(&Utc),
            },
            MithrilStakeDistributionListItem {
                epoch: Epoch(2),
                hash: "hash-456".to_string(),
                certificate_hash: "cert-hash-456".to_string(),
                created_at: DateTime::parse_from_rfc3339("2023-01-19T13:43:05.618857482Z")
                    .unwrap()
                    .with_timezone(&Utc),
            },
        ]
    }

    #[tokio::test]
    async fn get_mithril_stake_distribution_list() {
        let message = fake_messages();
        let mut http_client = MockAggregatorHTTPClient::new();
        http_client
            .expect_get_content()
            .return_once(move |_| Ok(serde_json::to_string(&message).unwrap()));
        let client = MithrilStakeDistributionClient::new(Arc::new(http_client));
        let items = client.list().await.unwrap();

        assert_eq!(2, items.len());
        assert_eq!("hash-123".to_string(), items[0].hash);
        assert_eq!("hash-456".to_string(), items[1].hash);
    }

    #[tokio::test]
    async fn get_mithril_stake_distribution() {
        let mut http_client = MockAggregatorHTTPClient::new();
        let message = MithrilStakeDistribution {
            certificate_hash: "certificate-hash-123".to_string(),
            epoch: Epoch(1),
            signers_with_stake: SignerWithStakeMessagePart::from_signers(
                fake_data::signers_with_stakes(2),
            ),
            hash: "hash".to_string(),
            created_at: DateTime::<Utc>::default(),
            protocol_parameters: fake_data::protocol_parameters(),
        };
        http_client
            .expect_get_content()
            .return_once(move |_| Ok(serde_json::to_string(&message).unwrap()));
        let client = MithrilStakeDistributionClient::new(Arc::new(http_client));
        let stake_distribution_entity = client
            .get("hash")
            .await
            .unwrap()
            .expect("This test returns a stake distribution");

        assert_eq!("hash".to_string(), stake_distribution_entity.hash);
        assert_eq!(2, stake_distribution_entity.signers_with_stake.len(),);
    }
}

use std::sync::Arc;

use crate::aggregator_client::{AggregatorClient, AggregatorClientError, AggregatorRequest};
use anyhow::Context;

use crate::{MithrilResult, MithrilStakeDistribution, MithrilStakeDistributionListItem};

/// HTTP client for MithrilStakeDistribution API from the Aggregator
pub struct MithrilStakeDistributionClient {
    aggregator_client: Arc<dyn AggregatorClient>,
}

impl MithrilStakeDistributionClient {
    /// Constructor
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

    fn golden_message() -> Vec<MithrilStakeDistributionListItem> {
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
        let message = golden_message();
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

use std::sync::Arc;

use mithril_common::{
    entities::MithrilStakeDistribution,
    messages::{MessageAdapter, MithrilStakeDistributionMessage},
    messages::{MithrilStakeDistributionListItemMessage, MithrilStakeDistributionListMessage},
    StdResult,
};

use crate::message_adapters::FromMithrilStakeDistributionMessageAdapter;

use super::{AggregatorClient, AggregatorHTTPClientError};

/// HTTP client for MithrilStakeDistribution API from the Aggregator
pub struct MithrilStakeDistributionClient {
    http_client: Arc<dyn AggregatorClient>,
}

impl MithrilStakeDistributionClient {
    /// Constructor
    pub fn new(http_client: Arc<dyn AggregatorClient>) -> Self {
        Self { http_client }
    }

    /// Fetch a list of signed MithrilStakeDistribution
    pub async fn list(&self) -> StdResult<Vec<MithrilStakeDistributionListItemMessage>> {
        let url = "artifact/mithril-stake-distributions";
        let response = self.http_client.get_content(url).await?;
        let items = serde_json::from_str::<MithrilStakeDistributionListMessage>(&response)?;

        Ok(items)
    }

    /// Download the given stake distribution. If it cannot be found, a None is returned.
    pub async fn get(&self, hash: &str) -> StdResult<Option<MithrilStakeDistribution>> {
        let url = format!("artifact/mithril-stake-distribution/{hash}");

        match self.http_client.get_content(&url).await {
            Ok(content) => {
                let message: MithrilStakeDistributionMessage = serde_json::from_str(&content)?;
                let stake_distribution: MithrilStakeDistribution =
                    FromMithrilStakeDistributionMessageAdapter::adapt(message);

                Ok(Some(stake_distribution))
            }
            Err(e) if matches!(e, AggregatorHTTPClientError::RemoteServerLogical(_)) => Ok(None),
            Err(e) => Err(e.into()),
        }
    }
}

#[cfg(test)]
mod tests {
    use chrono::{DateTime, Utc};
    use mithril_common::{entities::Epoch, test_utils::fake_data};

    use crate::aggregator_client::MockAggregatorHTTPClient;

    use super::*;

    fn golden_message() -> MithrilStakeDistributionListMessage {
        vec![
            MithrilStakeDistributionListItemMessage {
                epoch: Epoch(1),
                hash: "hash-123".to_string(),
                certificate_hash: "cert-hash-123".to_string(),
            },
            MithrilStakeDistributionListItemMessage {
                epoch: Epoch(2),
                hash: "hash-456".to_string(),
                certificate_hash: "cert-hash-456".to_string(),
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
        let message = MithrilStakeDistributionMessage {
            certificate_hash: "certificate-hash-123".to_string(),
            epoch: Epoch(1),
            signers_with_stake: fake_data::signers_with_stakes(2),
            hash: "hash".to_string(),
            created_at: DateTime::<Utc>::default(),
            protocol_parameters: fake_data::protocol_parameters(),
        };
        http_client
            .expect_get_content()
            .return_once(move |_| Ok(serde_json::to_string(&message).unwrap()));
        let client = MithrilStakeDistributionClient::new(Arc::new(http_client));
        let stake_distribution = client
            .get("hash")
            .await
            .unwrap()
            .expect("This test returns a stake distribution");

        assert_eq!("hash".to_string(), stake_distribution.hash);
        assert_eq!(2, stake_distribution.signers_with_stake.len(),);
    }
}

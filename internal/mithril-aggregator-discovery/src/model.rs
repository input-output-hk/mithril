use std::time::Duration;

use serde::Serialize;

use mithril_aggregator_client::{AggregatorHttpClient, query::GetAggregatorFeaturesQuery};
use mithril_common::{StdResult, messages::AggregatorCapabilities};

/// Representation of an aggregator endpoint
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct AggregatorEndpoint {
    url: String,
}

impl AggregatorEndpoint {
    const HTTP_TIMEOUT: Duration = Duration::from_secs(5);

    /// Create a new AggregatorEndpoint instance
    pub fn new(url: String) -> Self {
        Self { url }
    }

    /// Retrieve the capabilities of the aggregator
    pub async fn retrieve_capabilities(&self) -> StdResult<AggregatorCapabilities> {
        let aggregator_client = AggregatorHttpClient::builder(self.url.clone())
            .with_timeout(Self::HTTP_TIMEOUT)
            .build()?;

        Ok(aggregator_client
            .send(GetAggregatorFeaturesQuery::current())
            .await?
            .capabilities)
    }
}

impl From<AggregatorEndpoint> for String {
    fn from(endpoint: AggregatorEndpoint) -> Self {
        endpoint.url
    }
}

impl std::fmt::Display for AggregatorEndpoint {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.url)
    }
}

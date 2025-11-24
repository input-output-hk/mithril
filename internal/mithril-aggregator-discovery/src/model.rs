use std::time::Duration;

use mithril_aggregator_client::{AggregatorHttpClient, query::GetAggregatorFeaturesQuery};
use mithril_common::{StdResult, messages::AggregatorCapabilities};

/// Representation of a Mithril network
// TODO: to move to mithril common
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MithrilNetwork(String);

impl MithrilNetwork {
    /// Create a new MithrilNetwork instance
    pub fn new(name: String) -> Self {
        Self(name)
    }

    /// Create a dummy MithrilNetwork instance for testing purposes
    pub fn dummy() -> Self {
        Self("dummy".to_string())
    }

    /// Retrieve the name of the Mithril network
    pub fn name(&self) -> &str {
        &self.0
    }
}

impl From<String> for MithrilNetwork {
    fn from(name: String) -> Self {
        MithrilNetwork::new(name)
    }
}

/// Representation of an aggregator endpoint
#[derive(Debug, Clone, PartialEq, Eq)]
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

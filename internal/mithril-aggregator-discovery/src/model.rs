use std::time::Duration;

use serde::Serialize;

use mithril_aggregator_client::{AggregatorHttpClient, query::GetAggregatorFeaturesQuery};
use mithril_common::{StdError, StdResult, messages::AggregatorCapabilities};

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

impl From<AggregatorEndpointWithCapabilities> for AggregatorEndpoint {
    fn from(endpoint: AggregatorEndpointWithCapabilities) -> Self {
        Self::new(endpoint.url)
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

/// Representation of an aggregator endpoint with capabilities
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct AggregatorEndpointWithCapabilities {
    url: String,
    capabilities: AggregatorCapabilities,
}
impl AggregatorEndpointWithCapabilities {
    /// Create a new AggregatorEndpointWithCapabilities instance
    pub fn new(url: String, capabilities: AggregatorCapabilities) -> Self {
        Self { url, capabilities }
    }

    /// Get the capabilities of the aggregator
    pub fn capabilities(&self) -> &AggregatorCapabilities {
        &self.capabilities
    }
}

impl TryFrom<AggregatorEndpoint> for AggregatorEndpointWithCapabilities {
    type Error = StdError;

    fn try_from(endpoint: AggregatorEndpoint) -> Result<Self, Self::Error> {
        let endpoint_clone = endpoint.clone();
        let aggregator_capabilities = tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current()
                .block_on(async move { endpoint_clone.retrieve_capabilities().await })
        });

        Ok(Self::new(endpoint.url, aggregator_capabilities?))
    }
}

impl From<AggregatorEndpointWithCapabilities> for String {
    fn from(endpoint: AggregatorEndpointWithCapabilities) -> Self {
        endpoint.url
    }
}

impl std::fmt::Display for AggregatorEndpointWithCapabilities {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.url)
    }
}

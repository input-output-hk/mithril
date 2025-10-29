use mithril_common::StdResult;

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
}

/// Representation of an aggregator endpoint
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AggregatorEndpoint {
    url: String,
}

impl AggregatorEndpoint {
    /// Create a new AggregatorEndpoint instance
    pub fn new(url: String) -> Self {
        Self { url }
    }

    /// Retrieve the capabilities of the aggregator
    pub fn capabilities(&self) -> StdResult<()> {
        todo!("Implement capabilities retrieval")
    }
}

impl From<AggregatorEndpoint> for String {
    fn from(endpoint: AggregatorEndpoint) -> Self {
        endpoint.url
    }
}

//! Interface definition for Mithril Protocol Configuration provider.

use mithril_common::StdResult;

use crate::model::{AggregatorEndpoint, MithrilNetwork};

/// An aggregator discoverer.
#[cfg_attr(test, mockall::automock)]
#[async_trait::async_trait]
pub trait AggregatorDiscoverer: Sync + Send {
    /// Get an iterator over a list of available aggregators in a Mithril network.
    ///
    /// Note: there is no guarantee that the returned aggregators are sorted, complete or up-to-date.
    async fn get_available_aggregators(
        &self,
        network: MithrilNetwork,
    ) -> StdResult<Box<dyn Iterator<Item = AggregatorEndpoint>>>;
}

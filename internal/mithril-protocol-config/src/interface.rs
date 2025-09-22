use async_trait::async_trait;
use mithril_common::StdResult;

use crate::model::MithrilNetworkConfiguration;

/// Trait to provide the current Mithril network configuration.
#[async_trait]
pub trait MithrilNetworkConfigurationProvider: Sync + Send {
    /// Get the Mithril network configuration for the current epoch.
    async fn get(&self) -> StdResult<MithrilNetworkConfiguration>;
}

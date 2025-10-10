//! Interface definition for Mithril Protocol Configuration provider.

use async_trait::async_trait;
use mithril_common::StdResult;

use crate::model::MithrilNetworkConfiguration;

/// A provider for the Mithril network configuration of the current epoch.
#[async_trait]
pub trait MithrilNetworkConfigurationProvider: Sync + Send {
    /// Get the Mithril network configuration for the current epoch.
    async fn get_network_configuration(&self) -> StdResult<MithrilNetworkConfiguration>;
}

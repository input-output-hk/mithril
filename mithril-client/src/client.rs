//! Client module
//!
//! The client wraps the initialization of the Mithril Client services and provides a simple API for the library
//!
//! ```
//! use mithril_client::client::Client;
//! use mithril_client::common::*;
//!
//! #[tokio::main]
//! async fn main() -> StdResult<()> {
//!     let client = Client::new("https://aggregator.release-preprod.api.mithril.network/aggregator", "YOUR_GENESIS_VERIFICATION_KEY").await?;
//!     let snapshot = client.show_snapshot("5a1288f7164bec049f34e46002e939f4c609a0ddf86636fdc4180ea22342cab7").await?;
//!
//!     println!("Snapshot id={}, size={}", snapshot.digest, snapshot.size);
//!
//!     Ok(())
//! }
//! ```
//!
use std::sync::Arc;

use crate::dependencies::{ConfigParameters, DependenciesBuilder};
use anyhow::Context;

use crate::common::*;
use crate::services::{MithrilStakeDistributionService, SnapshotService};
/// Client structure that instanciates required dependencies
pub struct Client {
    snapshot_service: Arc<dyn SnapshotService>,
    mithril_stake_distribution_service: Arc<dyn MithrilStakeDistributionService>,
}

impl Client {
    /// Client constructor
    pub async fn new(aggregator_endpoint: &str, genesis_verification_key: &str) -> StdResult<Self> {
        let mut config = ConfigParameters::default();
        config
            .add_parameter("genesis_verification_key", genesis_verification_key)
            .add_parameter("aggregator_endpoint", aggregator_endpoint);
        let mut dependencies_builder = DependenciesBuilder::new(Arc::new(config));
        let snapshot_service = dependencies_builder
            .get_snapshot_service()
            .await
            .with_context(|| "Cannot instantiate Client")?;
        let mithril_stake_distribution_service = dependencies_builder
            .get_mithril_stake_distribution_service()
            .await
            .with_context(|| "Cannot instantiate Client")?;

        Ok(Self {
            snapshot_service,
            mithril_stake_distribution_service,
        })
    }

    /// Call the snapshot service to get a snapshot message from a digest
    pub async fn show_snapshot(&self, digest: &str) -> StdResult<SnapshotMessage> {
        self.snapshot_service.show(digest).await
    }

    /// Call the snapshot service to get the list of available snapshots
    pub async fn list_snapshots(&self) -> StdResult<Vec<SnapshotListItemMessage>> {
        self.snapshot_service.list().await
    }

    /// Call the mithril stake distribution service for the list of available mithril stake distributions
    pub async fn list_mithril_stake_distributions(
        &self,
    ) -> StdResult<MithrilStakeDistributionListMessage> {
        self.mithril_stake_distribution_service.list().await
    }
}

#[cfg(test)]
mod tests {
    use warp::{self, Filter};

    use mithril_common::{
        messages::MithrilStakeDistributionListItemMessage,
        test_utils::test_http_server::test_http_server,
    };

    use super::*;

    #[tokio::test]
    async fn show_snapshot() -> StdResult<()> {
        let snapshot_message = SnapshotMessage::dummy();
        let json_snapshot_message = serde_json::to_string(&snapshot_message)?;
        let http_server = test_http_server(
            warp::path!("artifact" / "snapshot" / String)
                .map(move |_digest| json_snapshot_message.clone()),
        );
        let client = Client::new(&http_server.url(), "WRITE THE VKEY HERE").await?;
        let response = client.show_snapshot(&snapshot_message.digest).await?;

        assert_eq!(snapshot_message, response);

        Ok(())
    }

    #[tokio::test]
    async fn list_snapshots() -> StdResult<()> {
        let snapshot_list_message = vec![
            SnapshotListItemMessage::dummy(),
            SnapshotListItemMessage {
                digest: "0b107cafc5638403cc523775c82249294eb8a1541d54f08eb3ec7c69f5ad7f33"
                    .to_string(),
                ..SnapshotListItemMessage::dummy()
            },
        ];
        let json_snapshot_list_message = serde_json::to_string(&snapshot_list_message)?;
        let http_server = test_http_server(
            warp::path!("artifact" / "snapshots").map(move || json_snapshot_list_message.clone()),
        );
        let client = Client::new(&http_server.url(), "WRITE THE VKEY HERE").await?;
        let response = client.list_snapshots().await?;

        assert_eq!(snapshot_list_message, response);

        Ok(())
    }

    #[tokio::test]
    async fn list_mithril_stake_distribution() -> StdResult<()> {
        let mithril_stake_distribution_list_message: MithrilStakeDistributionListMessage = vec![
            MithrilStakeDistributionListItemMessage::dummy(),
            MithrilStakeDistributionListItemMessage {
                hash: "12345".to_string(),
                ..MithrilStakeDistributionListItemMessage::dummy()
            },
        ];
        let mithril_stake_distribution_list_message_json =
            serde_json::to_string(&mithril_stake_distribution_list_message)?;
        let http_server = test_http_server(
            warp::path!("artifact" / "mithril-stake-distributions")
                .map(move || mithril_stake_distribution_list_message_json.clone()),
        );
        let client = Client::new(&http_server.url(), "WRITE THE VKEY HERE").await?;
        let response = client.list_mithril_stake_distributions().await?;

        assert_eq!(mithril_stake_distribution_list_message, response);

        Ok(())
    }
}

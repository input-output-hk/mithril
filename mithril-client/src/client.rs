//! Client module
//!
//! The client wraps the tedious initialization of the Mithril Client services;
//! It offers a simple API to use the underlying mechanics of the library.
//!
//! ```
//! use mithril-client::client::Client;
//! use mithril-client::common::*:
//!
//! #[tokio::main]
//! async fn main() -> StdResult<()> {
//!     let client = Client::new("WRITE THE VKEY HERE", &http_server.url()).await?;
//!     let snapshot = client.show_snapshot(&snapshot_message.digest).await?;
//!
//!     println!("Snapshot id={}, size={}", snapshot.digest, snapshot.size);
//!
//!     Ok(())
//! }
//! ```
//!
use std::sync::Arc;

use crate::dependencies::DependenciesBuilder;
use anyhow::Context;
use config::builder::DefaultState;
use config::ConfigBuilder;
use mithril_common::messages::MithrilStakeDistributionListMessage;
use mithril_common::StdResult;

use crate::common::*;
use crate::services::{MithrilStakeDistributionService, SnapshotService};
/// Client structure that instanciates required dependencies
pub struct Client {
    snapshot_service: Arc<dyn SnapshotService>,
    mithril_stake_distribution_service: Arc<dyn MithrilStakeDistributionService>,
}

impl Client {
    /// Client constructor
    pub async fn new(genesis_verification_key: &str, aggregator_endpoint: &str) -> StdResult<Self> {
        let config_builder: ConfigBuilder<DefaultState> = ConfigBuilder::default()
            .set_default("genesis_verification_key", genesis_verification_key)?
            .set_default("aggregator_endpoint", aggregator_endpoint)?;
        let mut dependencies_builder = DependenciesBuilder::new(Arc::new(config_builder.build()?));
        let snapshot_service = dependencies_builder
            .get_snapshot_service()
            .await
            .with_context(|| "Cannot instanciate Client.")?;
        let mithril_stake_distribution_service = dependencies_builder
            .get_mithril_stake_distribution_service()
            .await
            .with_context(|| "Cannot instanciate Client")?;

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
        let client = Client::new("WRITE THE VKEY HERE", &http_server.url()).await?;
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
        let client = Client::new("WRITE THE VKEY HERE", &http_server.url()).await?;
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
        let client = Client::new("WRITE THE VKEY HERE", &http_server.url()).await?;
        let response = client.list_mithril_stake_distributions().await?;

        assert_eq!(mithril_stake_distribution_list_message, response);

        Ok(())
    }
}

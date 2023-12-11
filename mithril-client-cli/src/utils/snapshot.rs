use anyhow::{anyhow, Context};
use futures::Future;
use indicatif::{MultiProgress, ProgressBar};
use std::time::Duration;

use super::SnapshotUnpackerError;
use crate::http_client::{AggregatorClient, AggregatorHTTPClient};

use mithril_client::{Client, MithrilResult};
use mithril_common::{
    api_version::APIVersionProvider, messages::SnapshotMessage, StdError, StdResult,
};

/// Utility functions for to the Snapshot commands
pub struct SnapshotUtils;

impl SnapshotUtils {
    /// Return latest snpashot digest if `latest` is specified as digest
    pub async fn expand_eventual_snapshot_alias(
        client: &Client,
        snapshot_id: &str,
    ) -> StdResult<String> {
        if snapshot_id.to_lowercase() == "latest" {
            let last_snapshot = client.snapshot().list().await.with_context(|| {
                "Can not get the list of snapshots while retrieving the latest snapshot digest"
            })?;
            let last_snapshot = last_snapshot.first().ok_or_else(|| {
                anyhow!(
                    "Snapshot not found for digest: '{}'",
                    snapshot_id.to_string()
                )
            })?;
            Ok(last_snapshot.digest.to_owned())
        } else {
            Ok(snapshot_id.to_owned())
        }
    }

    /// Handle the error return by `check_prerequisites`
    pub fn check_disk_space_error(error: StdError) -> StdResult<String> {
        if let Some(SnapshotUnpackerError::NotEnoughSpace {
            left_space: _,
            pathdir: _,
            archive_size: _,
        }) = error.downcast_ref::<SnapshotUnpackerError>()
        {
            Ok(format!("Warning: {}", error))
        } else {
            Err(error)
        }
    }

    /// Display a spinner while waiting for the result of a future
    pub async fn wait_spinner<T>(
        progress_bar: &MultiProgress,
        future: impl Future<Output = MithrilResult<T>>,
    ) -> MithrilResult<T> {
        let pb = progress_bar.add(ProgressBar::new_spinner());
        let spinner = async move {
            loop {
                pb.tick();
                tokio::time::sleep(Duration::from_millis(50)).await;
            }
        };

        tokio::select! {
            _ = spinner => Err(anyhow!("timeout")),
            res = future => res,
        }
    }

    /// Increments Aggregator's download statistics
    pub async fn add_statistics(
        aggregator_endpoint: &str,
        snapshot: &SnapshotMessage,
    ) -> StdResult<()> {
        let url = "statistics/snapshot";
        let json = serde_json::to_string(&snapshot)?;
        let http_client = AggregatorHTTPClient::new(
            aggregator_endpoint,
            APIVersionProvider::compute_all_versions_sorted()?,
        );
        let _response = http_client.post_content(url, &json).await?;

        Ok(())
    }
}

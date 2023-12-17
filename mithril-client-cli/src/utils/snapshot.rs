use anyhow::anyhow;
use futures::Future;
use indicatif::{MultiProgress, ProgressBar};
use std::time::Duration;

use super::SnapshotUnpackerError;
use crate::http_client::AggregatorHTTPClient;

use mithril_client::MithrilResult;
use mithril_common::{
    api_version::APIVersionProvider, messages::SnapshotMessage, StdError, StdResult,
};

/// Utility functions for to the Snapshot commands
pub struct SnapshotUtils;

impl SnapshotUtils {
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

#[cfg(test)]
mod test {
    use super::*;
    use mithril_common::test_utils::test_http_server::test_http_server;
    use std::path::PathBuf;
    use warp::Filter;

    #[tokio::test]
    async fn add_statistics_should_return_ok() {
        let server =
            test_http_server(warp::path!("statistics" / "snapshot").map(move || "".to_string()));
        let snapshot_message = SnapshotMessage::dummy();

        let result = SnapshotUtils::add_statistics(&server.url(), &snapshot_message).await;

        assert!(result.is_ok())
    }

    #[tokio::test]
    async fn add_statistics_should_return_error() {
        let snapshot_message = SnapshotMessage::dummy();

        let result = SnapshotUtils::add_statistics("http://whatever", &snapshot_message).await;

        assert!(result.is_err())
    }

    #[test]
    fn check_disk_space_error_should_return_warning_message_if_error_is_not_enough_space() {
        let not_enough_space_error = SnapshotUnpackerError::NotEnoughSpace {
            left_space: 1_f64,
            pathdir: PathBuf::new(),
            archive_size: 2_f64,
        };
        let expected = format!("Warning: {}", not_enough_space_error);

        let result = SnapshotUtils::check_disk_space_error(anyhow!(not_enough_space_error))
            .expect("check_disk_space_error should not error");

        assert!(result.contains(&expected));
    }

    #[test]
    fn check_disk_space_error_should_return_error_if_error_is_not_error_not_enough_space() {
        let error = SnapshotUnpackerError::UnpackFailed {
            dirpath: PathBuf::from(""),
            error: anyhow::Error::msg("Some error message"),
        };

        let error = SnapshotUtils::check_disk_space_error(anyhow!(error))
            .expect_err("check_disk_space_error should fail");

        assert!(
            matches!(
                error.downcast_ref::<SnapshotUnpackerError>(),
                Some(SnapshotUnpackerError::UnpackFailed {
                    dirpath: _,
                    error: _
                })
            ),
            "Unexpected error: {:?}",
            error
        );
    }
}

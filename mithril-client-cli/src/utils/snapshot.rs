use anyhow::{anyhow, Context};
use futures::Future;
use indicatif::{MultiProgress, ProgressBar};
use std::time::Duration;

use super::SnapshotUnpackerError;
use crate::http_client::AggregatorHTTPClient;

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

#[cfg(test)]
mod test {
    use super::*;
    use crate::extensions::fake::FakeCertificateVerifier;
    use mithril_client::{ClientBuilder, SnapshotListItem};
    use mithril_common::test_utils::{
        fake_keys,
        test_http_server::{test_http_server, TestHttpServer},
    };
    use std::path::PathBuf;
    use warp::Filter;

    fn get_snapshots(digests: Vec<&str>) -> String {
        serde_json::to_string(
            &digests
                .iter()
                .map(|d| SnapshotListItem {
                    digest: d.to_string(),
                    ..SnapshotListItem::dummy()
                })
                .collect::<Vec<_>>(),
        )
        .unwrap()
    }

    fn create_client_with_snapshots(snapshots: String) -> (Client, TestHttpServer) {
        let server =
            test_http_server(warp::path!("artifact" / "snapshots").map(move || snapshots.clone()));
        let genesis_verification_key = fake_keys::genesis_verification_key()[0];
        let client = ClientBuilder::aggregator(&server.url(), genesis_verification_key)
            .with_certificate_verifier(
                FakeCertificateVerifier::build_that_validate_any_certificate(),
            )
            .build()
            .unwrap();

        (client, server)
    }

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

    #[tokio::test]
    async fn expand_eventual_snapshot_alias_should_returns_id() {
        let snapshots = get_snapshots(vec!["digest-123", "digest-234", "digest-345"]);
        let (client, _server) = create_client_with_snapshots(snapshots);

        let digest = SnapshotUtils::expand_eventual_snapshot_alias(&client, "digest-234")
            .await
            .unwrap();

        assert_eq!("digest-234", digest);
    }

    #[tokio::test]
    async fn expand_eventual_snapshot_alias_latest_lowercase() {
        let snapshots = get_snapshots(vec!["digest-123", "digest-234", "digest-345"]);
        let (client, _server) = create_client_with_snapshots(snapshots);

        let digest = SnapshotUtils::expand_eventual_snapshot_alias(&client, "latest")
            .await
            .expect("expand_eventual_snapshot_alias should not error when latest is passed as parameter.");

        assert_eq!("digest-123".to_string(), digest);
    }

    #[tokio::test]
    async fn expand_eventual_snapshot_alias_latest_uppercase() {
        let snapshots = get_snapshots(vec!["digest-123", "digest-234", "digest-345"]);
        let (client, _server) = create_client_with_snapshots(snapshots);

        let digest = SnapshotUtils::expand_eventual_snapshot_alias(&client, "LATEST")
            .await
            .expect("expand_eventual_snapshot_alias should not error when latest is passed as parameter.");

        assert_eq!("digest-123".to_string(), digest);
    }

    #[tokio::test]
    async fn expand_eventual_snapshot_alias_should_error() {
        let snapshots = "[]";
        let (client, _server) = create_client_with_snapshots(snapshots.to_string());

        let _ = SnapshotUtils::expand_eventual_snapshot_alias(&client, "LATEST")
            .await
            .expect_err(
                "expand_eventual_snapshot_alias should returns an error if there is no latest.",
            );
    }
}

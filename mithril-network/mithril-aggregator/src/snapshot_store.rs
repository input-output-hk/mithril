use anyhow::{anyhow, Result};
use async_trait::async_trait;
use log::debug;
use reqwest::{self, StatusCode};

use crate::entities::*;

#[cfg(test)]
use mockall::automock;

/// SnapshotStorer represents a snapshot store interactor
#[cfg_attr(test, automock)]
#[async_trait]
pub trait SnapshotStorer: Sync + Send {
    /// List snapshots
    async fn list_snapshots(&self) -> Result<Vec<Snapshot>>;

    /// Get snapshot details
    async fn get_snapshot_details(&self, digest: String) -> Result<Option<Snapshot>>;
}

/// SnapshotStoreHTTPClient is a http client for an remote snapshot manifest
pub struct SnapshotStoreHTTPClient {
    url_manifest: String,
}

impl SnapshotStoreHTTPClient {
    /// SnapshotStoreHTTPClient factory
    pub fn new(url_manifest: String) -> Self {
        debug!("New SnapshotStoreHTTPClient created");
        Self { url_manifest }
    }
}

#[async_trait]
impl SnapshotStorer for SnapshotStoreHTTPClient {
    /// List snapshots
    async fn list_snapshots(&self) -> Result<Vec<Snapshot>> {
        debug!("List snapshots from {}", self.url_manifest);

        let response = reqwest::get(&self.url_manifest).await?;
        match response.status() {
            StatusCode::OK => Ok(response.json::<Vec<Snapshot>>().await?),
            status_error => Err(anyhow!("error {} received", status_error)),
        }
    }

    /// Get snapshot details
    async fn get_snapshot_details(&self, digest: String) -> Result<Option<Snapshot>> {
        for snapshot in self.list_snapshots().await? {
            if digest.eq(&snapshot.digest) {
                return Ok(Some(snapshot));
            }
        }
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use httpmock::prelude::*;
    use serde_json::json;

    use crate::fake_data;

    fn setup_test() -> MockServer {
        MockServer::start()
    }

    #[tokio::test]
    async fn test_list_snapshots_ok() {
        let server = setup_test();
        let snapshots_expected = fake_data::snapshots(5);
        let _snapshots_mock = server.mock(|when, then| {
            when.path("/snapshots-manifest");
            then.status(200).body(json!(snapshots_expected).to_string());
        });
        let snapshot_store = SnapshotStoreHTTPClient::new(server.url("/snapshots-manifest"));
        let snapshots = snapshot_store.list_snapshots().await;
        snapshots.as_ref().expect("unexpected error");
        assert_eq!(snapshots.unwrap(), snapshots_expected);
    }

    #[tokio::test]
    async fn test_get_snapshot_details_ok() {
        let server = setup_test();
        let all_snapshots = fake_data::snapshots(5);
        let snapshot_expected = &all_snapshots[2];
        let _snapshots_mock = server.mock(|when, then| {
            when.path("/snapshots-manifest");
            then.status(200).body(json!(all_snapshots).to_string());
        });
        let snapshot_store = SnapshotStoreHTTPClient::new(server.url("/snapshots-manifest"));
        let snapshot = snapshot_store
            .get_snapshot_details(snapshot_expected.digest.clone())
            .await;
        snapshot.as_ref().expect("unexpected error");
        assert_eq!(Some(snapshot_expected.clone()), snapshot.unwrap());
    }

    #[tokio::test]
    async fn test_list_snapshots_ko_500() {
        let server = setup_test();
        let _snapshots_mock = server.mock(|when, then| {
            when.path("/snapshots-manifest");
            then.status(500);
        });
        let snapshot_store = SnapshotStoreHTTPClient::new(server.url("/snapshots-manifest"));
        let snapshots = snapshot_store.list_snapshots().await;
        assert!(snapshots.is_err());
    }

    #[tokio::test]
    async fn test_get_snapshot_details_ko_500() {
        let server = setup_test();
        let _snapshots_mock = server.mock(|when, then| {
            when.path("/snapshots-manifest");
            then.status(500);
        });
        let snapshot_store = SnapshotStoreHTTPClient::new(server.url("/snapshots-manifest"));
        let snapshots = snapshot_store.get_snapshot_details("abc".to_string()).await;
        assert!(snapshots.is_err());
    }

    #[tokio::test]
    async fn test_list_snapshots_ko_unreachable() {
        let snapshot_store = SnapshotStoreHTTPClient::new("http123://unreachable".to_string());
        let snapshots = snapshot_store.list_snapshots().await;
        assert!(snapshots.is_err());
    }
}

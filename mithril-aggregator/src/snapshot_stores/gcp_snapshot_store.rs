use crate::snapshot_stores::SnapshotStoreError;
use crate::SnapshotStore;

use crate::tools::GcpFileUploader;
use async_trait::async_trait;
use mithril_common::entities::Snapshot;
use reqwest::{self, StatusCode};
use slog_scope::debug;
use slog_scope::info;
use std::fs::File;
use std::path::Path;

/// GoogleCloudPlatformSnapshotStore is a snapshot store working using Google Cloud Platform services
pub struct GCPSnapshotStore {
    file_uploader: Box<dyn GcpFileUploader>,

    url_manifest: String,
}

impl GCPSnapshotStore {
    /// SnapshotStoreHTTPClient factory
    pub fn new(file_uploader: Box<dyn GcpFileUploader>, url_manifest: String) -> Self {
        debug!("New SnapshotStoreHTTPClient created");
        Self {
            file_uploader,
            url_manifest,
        }
    }
}

#[async_trait]
impl SnapshotStore for GCPSnapshotStore {
    /// List snapshots
    async fn list_snapshots(&self) -> Result<Vec<Snapshot>, String> {
        debug!("List snapshots from {}", self.url_manifest);

        let response = reqwest::get(&self.url_manifest).await;
        match response {
            Ok(response) => match response.status() {
                StatusCode::OK => match response.json::<Vec<Snapshot>>().await {
                    Ok(snapshots) => Ok(snapshots),
                    Err(err) => Err(err.to_string()),
                },
                status_error => Err(format!("error {} received", status_error)),
            },
            Err(err) => Err(err.to_string()),
        }
    }

    /// Get snapshot details
    async fn get_snapshot_details(&self, digest: String) -> Result<Option<Snapshot>, String> {
        for snapshot in self.list_snapshots().await? {
            if digest.eq(&snapshot.digest) {
                return Ok(Some(snapshot));
            }
        }
        Ok(None)
    }

    async fn add_snapshot(&mut self, snapshot: Snapshot) -> Result<(), SnapshotStoreError> {
        info!(
            "Adding snapshot to GCP: {}",
            serde_json::to_string(&snapshot).unwrap()
        );

        let snapshots = vec![snapshot];
        let manifest_to_upload_path = Path::new("snapshots.json");
        serde_json::to_writer(&File::create(manifest_to_upload_path).unwrap(), &snapshots).unwrap();

        self.file_uploader
            .upload_file(manifest_to_upload_path)
            .await
            .map_err(SnapshotStoreError::GcpError)?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use httpmock::prelude::*;
    use mithril_common::fake_data;
    use serde_json::json;

    use crate::tools::MockGcpFileUploader;

    fn setup_test() -> MockServer {
        MockServer::start()
    }

    #[tokio::test]
    async fn test_list_snapshots_ok() {
        let file_uploader = MockGcpFileUploader::new();
        let server = setup_test();
        let snapshots_expected = fake_data::snapshots(5);
        let _snapshots_mock = server.mock(|when, then| {
            when.path("/snapshots-manifest");
            then.status(200).body(json!(snapshots_expected).to_string());
        });
        let snapshot_store =
            GCPSnapshotStore::new(Box::new(file_uploader), server.url("/snapshots-manifest"));
        let snapshots = snapshot_store.list_snapshots().await;
        snapshots.as_ref().expect("unexpected error");
        assert_eq!(snapshots.unwrap(), snapshots_expected);
    }

    #[tokio::test]
    async fn test_get_snapshot_details_ok() {
        let file_uploader = MockGcpFileUploader::new();
        let server = setup_test();
        let all_snapshots = fake_data::snapshots(5);
        let snapshot_expected = &all_snapshots[2];
        let _snapshots_mock = server.mock(|when, then| {
            when.path("/snapshots-manifest");
            then.status(200).body(json!(all_snapshots).to_string());
        });
        let snapshot_store =
            GCPSnapshotStore::new(Box::new(file_uploader), server.url("/snapshots-manifest"));
        let snapshot = snapshot_store
            .get_snapshot_details(snapshot_expected.digest.clone())
            .await;
        snapshot.as_ref().expect("unexpected error");
        assert_eq!(Some(snapshot_expected.clone()), snapshot.unwrap());
    }

    #[tokio::test]
    async fn test_list_snapshots_ko_500() {
        let file_uploader = MockGcpFileUploader::new();
        let server = setup_test();
        let _snapshots_mock = server.mock(|when, then| {
            when.path("/snapshots-manifest");
            then.status(500);
        });
        let snapshot_store =
            GCPSnapshotStore::new(Box::new(file_uploader), server.url("/snapshots-manifest"));
        let snapshots = snapshot_store.list_snapshots().await;
        assert!(snapshots.is_err());
    }

    #[tokio::test]
    async fn test_get_snapshot_details_ko_500() {
        let file_uploader = MockGcpFileUploader::new();
        let server = setup_test();
        let _snapshots_mock = server.mock(|when, then| {
            when.path("/snapshots-manifest");
            then.status(500);
        });
        let snapshot_store =
            GCPSnapshotStore::new(Box::new(file_uploader), server.url("/snapshots-manifest"));
        let snapshots = snapshot_store.get_snapshot_details("abc".to_string()).await;
        assert!(snapshots.is_err());
    }

    #[tokio::test]
    async fn test_list_snapshots_ko_unreachable() {
        let file_uploader = MockGcpFileUploader::new();
        let snapshot_store =
            GCPSnapshotStore::new(Box::new(file_uploader), "http123://unreachable".to_string());
        let snapshots = snapshot_store.list_snapshots().await;
        assert!(snapshots.is_err());
    }

    #[tokio::test]
    async fn test_add_snapshot_ok() {
        let mut file_uploader = MockGcpFileUploader::new();
        file_uploader.expect_upload_file().return_const(Ok(()));
        let mut snapshot_store =
            GCPSnapshotStore::new(Box::new(file_uploader), "http123://unreachable".to_string());
        let snapshot = Snapshot {
            digest: "abc".to_string(),
            certificate_hash: "abc".to_string(),
            size: 0,
            created_at: "abc".to_string(),
            locations: vec!["abc".to_string()],
        };

        snapshot_store
            .add_snapshot(snapshot)
            .await
            .expect("test_add_snapshot_ok unexpected error");
    }

    #[tokio::test]
    async fn test_add_snapshot_ko() {
        let mut file_uploader = MockGcpFileUploader::new();
        file_uploader
            .expect_upload_file()
            .return_const(Err("unexpected error".to_string()));
        let mut snapshot_store =
            GCPSnapshotStore::new(Box::new(file_uploader), "http123://unreachable".to_string());
        let snapshot = Snapshot {
            digest: "abc".to_string(),
            certificate_hash: "abc".to_string(),
            size: 0,
            created_at: "abc".to_string(),
            locations: vec!["abc".to_string()],
        };

        let result = snapshot_store.add_snapshot(snapshot).await;
        assert_eq!(
            SnapshotStoreError::GcpError("unexpected error".to_string()).to_string(),
            result.unwrap_err().to_string()
        );
    }
}

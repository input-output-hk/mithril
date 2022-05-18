use crate::snapshot_stores::SnapshotStoreError;
use crate::SnapshotStore;
use mithril_common::entities::Snapshot;

use async_trait::async_trait;
use chrono::prelude::*;
use cloud_storage::bucket::Entity;
use cloud_storage::bucket_access_control::Role;
use cloud_storage::object_access_control::NewObjectAccessControl;
use cloud_storage::Client;
use reqwest::{self, StatusCode};
use slog_scope::debug;
use slog_scope::info;
use std::env;
use std::fs::File;
use std::io::{Seek, SeekFrom};
use tokio_util::codec::BytesCodec;
use tokio_util::codec::FramedRead;

/// GoogleCloudPlatformSnapshotStore is a snapshot store working using Google Cloud Platform services
pub struct GCPSnapshotStore {
    url_manifest: String,
}

impl GCPSnapshotStore {
    /// SnapshotStoreHTTPClient factory
    pub fn new(url_manifest: String) -> Self {
        debug!("New SnapshotStoreHTTPClient created");
        Self { url_manifest }
    }

    async fn upload_file(&self, filename: &str) -> Result<(), SnapshotStoreError> {
        if env::var("GOOGLE_APPLICATION_CREDENTIALS_JSON").is_err() {
            return Err(SnapshotStoreError::UploadFileError(
                "Missing GOOGLE_APPLICATION_CREDENTIALS_JSON environment variable".to_string(),
            ));
        };

        info!("uploading {}", filename);
        let client = Client::default();
        let file = tokio::fs::File::open(filename).await.unwrap();
        let stream = FramedRead::new(file, BytesCodec::new());
        client
            .object()
            .create_streamed(
                "cardano-testnet",
                stream,
                None,
                filename,
                "application/octet-stream",
            )
            .await
            .map_err(|e| SnapshotStoreError::UploadFileError(e.to_string()))?;

        info!("uploaded {}", filename);

        // ensure the uploaded file as public read access
        // when a file is uploaded to gcloud storage its permissions are overwritten so
        // we need to put them back
        let new_bucket_access_control = NewObjectAccessControl {
            entity: Entity::AllUsers,
            role: Role::Reader,
        };

        info!(
            "updating acl for {}: {:?}",
            filename, new_bucket_access_control
        );

        client
            .object_access_control()
            .create("cardano-testnet", filename, &new_bucket_access_control)
            .await
            .map_err(|e| SnapshotStoreError::UploadFileError(e.to_string()))?;

        info!("updated acl for {} ", filename);

        Ok(())
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

    async fn upload_snapshot(
        &mut self,
        digest: String,
        mut snapshot: File,
    ) -> Result<(), SnapshotStoreError> {
        let timestamp: DateTime<Utc> = Utc::now();
        let created_at = format!("{:?}", timestamp);
        let archive_name = "testnet.tar.gz";

        info!("snapshot hash: {}", digest);

        let size: u64 = snapshot
            .seek(SeekFrom::End(0))
            .map_err(|e| SnapshotStoreError::UploadFileError(e.to_string()))?;

        let snapshots = vec![Snapshot {
            digest,
            certificate_hash: "".to_string(),
            size,
            created_at,
            locations: vec![format!(
                "https://storage.googleapis.com/cardano-testnet/{}",
                archive_name
            )],
        }];

        info!("snapshot: {}", serde_json::to_string(&snapshots).unwrap());
        serde_json::to_writer(&File::create("snapshots.json").unwrap(), &snapshots).unwrap();

        self.upload_file(archive_name).await?;
        self.upload_file("snapshots.json").await?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use httpmock::prelude::*;
    use mithril_common::fake_data;
    use serde_json::json;

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
        let snapshot_store = GCPSnapshotStore::new(server.url("/snapshots-manifest"));
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
        let snapshot_store = GCPSnapshotStore::new(server.url("/snapshots-manifest"));
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
        let snapshot_store = GCPSnapshotStore::new(server.url("/snapshots-manifest"));
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
        let snapshot_store = GCPSnapshotStore::new(server.url("/snapshots-manifest"));
        let snapshots = snapshot_store.get_snapshot_details("abc".to_string()).await;
        assert!(snapshots.is_err());
    }

    #[tokio::test]
    async fn test_list_snapshots_ko_unreachable() {
        let snapshot_store = GCPSnapshotStore::new("http123://unreachable".to_string());
        let snapshots = snapshot_store.list_snapshots().await;
        assert!(snapshots.is_err());
    }
}

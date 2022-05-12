use mithril_common::entities::Snapshot;

use chrono::prelude::*;
use cloud_storage::bucket::Entity;
use cloud_storage::bucket_access_control::Role;
use cloud_storage::object_access_control::NewObjectAccessControl;
use cloud_storage::Client;
use flate2::write::GzEncoder;
use flate2::Compression;
use slog_scope::info;
use std::env;
use std::fs::File;
use std::io;
use std::io::{Seek, SeekFrom};
use std::path::Path;
use thiserror::Error;
use tokio_util::codec::BytesCodec;
use tokio_util::codec::FramedRead;

/// Snapshotter
pub struct Snapshotter {
    /// DB directory to snapshot
    db_directory: String,
}

#[derive(Error, Debug)]
pub enum SnapshotError {
    #[error("Create archive error: ")]
    CreateArchiveError(#[from] io::Error),

    #[error("Upload file error: `{0}`")]
    UploadFileError(String),
}

impl Snapshotter {
    /// Snapshotter factory
    pub fn new(db_directory: String) -> Self {
        Self { db_directory }
    }

    pub async fn snapshot(&self, immutable_digest: String) -> Result<(), SnapshotError> {
        let archive_name = "testnet.tar.gz";
        info!("snapshot hash: {}", immutable_digest);

        let size = self.create_archive(archive_name)?;

        let timestamp: DateTime<Utc> = Utc::now();
        let created_at = format!("{:?}", timestamp);

        let snapshots = vec![Snapshot {
            digest: immutable_digest,
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

    fn create_archive(&self, archive_name: &str) -> Result<u64, SnapshotError> {
        let path = Path::new(".").join(archive_name);
        let tar_gz = File::create(&path).map_err(SnapshotError::CreateArchiveError)?;
        let enc = GzEncoder::new(tar_gz, Compression::default());
        let mut tar = tar::Builder::new(enc);

        info!(
            "compressing {} into {}",
            &self.db_directory,
            &path.to_str().unwrap()
        );

        tar.append_dir_all(".", &self.db_directory)
            .map_err(SnapshotError::CreateArchiveError)?;

        // complete gz encoding and retrieve underlying file to compute size accurately
        let mut gz = tar
            .into_inner()
            .map_err(SnapshotError::CreateArchiveError)?;
        gz.try_finish().map_err(SnapshotError::CreateArchiveError)?;
        let mut f = gz.finish().map_err(SnapshotError::CreateArchiveError)?;
        let size: u64 = f
            .seek(SeekFrom::End(0))
            .map_err(SnapshotError::CreateArchiveError)?;

        Ok(size)
    }

    async fn upload_file(&self, filename: &str) -> Result<(), SnapshotError> {
        if env::var("GOOGLE_APPLICATION_CREDENTIALS_JSON").is_err() {
            return Err(SnapshotError::UploadFileError(
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
            .map_err(|e| SnapshotError::UploadFileError(e.to_string()))?;

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
            .map_err(|e| SnapshotError::UploadFileError(e.to_string()))?;

        info!("updated acl for {} ", filename);

        Ok(())
    }
}

#[cfg(test)]
mod tests {}

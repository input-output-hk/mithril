use async_trait::async_trait;
use cloud_storage::bucket::Entity;
use cloud_storage::bucket_access_control::Role;
use cloud_storage::object_access_control::NewObjectAccessControl;
use cloud_storage::Client;
use slog_scope::info;
use std::env;
use std::path::Path;
use tokio_util::codec::BytesCodec;
use tokio_util::codec::FramedRead;

#[cfg(test)]
use mockall::automock;

/// RemoteFileUploader represents a remote file uploader interactor
#[cfg_attr(test, automock)]
#[async_trait]
pub trait RemoteFileUploader: Sync + Send {
    /// Upload a snapshot
    async fn upload_file(&self, filepath: &Path) -> Result<(), String>;
}

/// GcpFileUploader represents a Google Cloud Platform file uploader interactor
pub struct GcpFileUploader {
    bucket: String,
}

impl Default for GcpFileUploader {
    fn default() -> Self {
        Self {
            bucket: "cardano-testnet".to_string(),
        }
    }
}

#[async_trait]
impl RemoteFileUploader for GcpFileUploader {
    async fn upload_file(&self, filepath: &Path) -> Result<(), String> {
        if env::var("GOOGLE_APPLICATION_CREDENTIALS_JSON").is_err() {
            return Err(
                "Missing GOOGLE_APPLICATION_CREDENTIALS_JSON environment variable".to_string(),
            );
        };

        let filename = filepath.file_name().unwrap().to_str().unwrap();

        info!("uploading {}", filename);
        let client = Client::default();
        let file = tokio::fs::File::open(filepath).await.unwrap();
        let stream = FramedRead::new(file, BytesCodec::new());
        client
            .object()
            .create_streamed(
                &self.bucket,
                stream,
                None,
                filename,
                "application/octet-stream",
            )
            .await
            .map_err(|e| e.to_string())?;

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
            .create(&self.bucket, filename, &new_bucket_access_control)
            .await
            .map_err(|e| e.to_string())?;

        info!("updated acl for {} ", filename);

        Ok(())
    }
}

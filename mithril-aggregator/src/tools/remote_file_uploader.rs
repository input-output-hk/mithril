use anyhow::{anyhow, Context};
use async_trait::async_trait;
use cloud_storage::{
    bucket::Entity, bucket_access_control::Role, object_access_control::NewObjectAccessControl,
    Client,
};
use slog::{info, Logger};
use std::{env, path::Path};
use tokio_util::{codec::BytesCodec, codec::FramedRead};

use mithril_common::logging::LoggerExtensions;
use mithril_common::StdResult;

/// RemoteFileUploader represents a remote file uploader interactor
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait RemoteFileUploader: Sync + Send {
    /// Upload a snapshot
    async fn upload_file(&self, filepath: &Path) -> StdResult<()>;
}

/// GcpFileUploader represents a Google Cloud Platform file uploader interactor
pub struct GcpFileUploader {
    bucket: String,
    logger: Logger,
}

impl GcpFileUploader {
    /// GcpFileUploader factory
    pub fn new(bucket: String, logger: Logger) -> Self {
        Self {
            bucket,
            logger: logger.new_with_component_name::<Self>(),
        }
    }
}

#[async_trait]
impl RemoteFileUploader for GcpFileUploader {
    async fn upload_file(&self, filepath: &Path) -> StdResult<()> {
        if env::var("GOOGLE_APPLICATION_CREDENTIALS_JSON").is_err() {
            return Err(anyhow!(
                "Missing GOOGLE_APPLICATION_CREDENTIALS_JSON environment variable".to_string()
            ));
        };

        let filename = filepath.file_name().unwrap().to_str().unwrap();

        info!(self.logger, "Uploading {filename}");
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
            .with_context(|| "remote uploading failure")?;

        info!(self.logger, "Uploaded {filename}");

        // ensure the uploaded file as public read access
        // when a file is uploaded to Google cloud storage its permissions are overwritten so
        // we need to put them back
        let new_bucket_access_control = NewObjectAccessControl {
            entity: Entity::AllUsers,
            role: Role::Reader,
        };

        info!(
            self.logger,
            "Updating acl for {filename}: {new_bucket_access_control:?}"
        );

        client
            .object_access_control()
            .create(&self.bucket, filename, &new_bucket_access_control)
            .await
            .with_context(|| "updating acl failure")?;

        info!(self.logger, "Updated acl for {filename}");

        Ok(())
    }
}

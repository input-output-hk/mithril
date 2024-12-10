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

use crate::file_uploaders::FileLocation;
use crate::FileUploader;

/// GcpFileUploader represents a Google Cloud Platform file uploader interactor
pub struct GcpFileUploader {
    bucket: String,
    use_cdn_domain: bool,
    logger: Logger,
}

impl GcpFileUploader {
    /// GcpFileUploader factory
    pub fn new(bucket: String, use_cdn_domain: bool, logger: Logger) -> Self {
        Self {
            bucket,
            use_cdn_domain,
            logger: logger.new_with_component_name::<Self>(),
        }
    }

    fn get_location(&self, filename: &str) -> String {
        if self.use_cdn_domain {
            format!("https://{}/{}", self.bucket, filename)
        } else {
            format!(
                "https://storage.googleapis.com/{}/{}",
                self.bucket, filename
            )
        }
    }
}

#[async_trait]
impl FileUploader for GcpFileUploader {
    async fn upload(&self, filepath: &Path) -> StdResult<FileLocation> {
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

        Ok(self.get_location(filename))
    }
}

#[cfg(test)]
mod tests {
    use crate::test_tools::TestLogger;

    use super::*;

    #[tokio::test]
    async fn get_location_not_using_cdn_domain_return_google_api_uri() {
        let use_cdn_domain = false;

        let file_uploader = GcpFileUploader::new(
            "cardano-testnet".to_string(),
            use_cdn_domain,
            TestLogger::stdout(),
        );
        let filename = "snapshot.xxx.tar.gz";
        let expected_location =
            "https://storage.googleapis.com/cardano-testnet/snapshot.xxx.tar.gz".to_string();

        let location = file_uploader.get_location(filename);

        assert_eq!(expected_location, location);
    }

    #[tokio::test]
    async fn get_location_using_cdn_domain_return_cdn_in_uri() {
        let use_cdn_domain = true;

        let file_uploader = GcpFileUploader::new(
            "cdn.mithril.network".to_string(),
            use_cdn_domain,
            TestLogger::stdout(),
        );
        let filename = "snapshot.xxx.tar.gz";
        let expected_location = "https://cdn.mithril.network/snapshot.xxx.tar.gz".to_string();

        let location = file_uploader.get_location(filename);

        assert_eq!(expected_location, location);
    }
}

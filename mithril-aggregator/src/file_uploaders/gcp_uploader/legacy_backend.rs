use std::env;
use std::path::Path;

use anyhow::{anyhow, Context};
use async_trait::async_trait;
use cloud_storage::bucket::Entity;
use cloud_storage::bucket_access_control::Role;
use cloud_storage::object_access_control::NewObjectAccessControl;
use cloud_storage::Client;
use slog::{info, Logger};
use tokio_util::codec::{BytesCodec, FramedRead};

use mithril_common::entities::FileUri;
use mithril_common::logging::LoggerExtensions;
use mithril_common::StdResult;

use crate::file_uploaders::gcp_uploader::CloudBackendUploader;
use crate::file_uploaders::CloudRemotePath;
use crate::tools::DEFAULT_GCP_CREDENTIALS_JSON_ENV_VAR;

/// GcpBackendUploader represents a Google Cloud Platform file uploader
#[derive(Debug)]
pub struct GcpBackendUploaderLegacy {
    bucket: String,
    use_cdn_domain: bool,
    client: Client,
    logger: Logger,
}

impl GcpBackendUploaderLegacy {
    /// GcpBackendUploader factory
    pub fn try_new(bucket: String, use_cdn_domain: bool, logger: Logger) -> StdResult<Self> {
        if env::var(DEFAULT_GCP_CREDENTIALS_JSON_ENV_VAR).is_err() {
            return Err(anyhow!(format!(
                "Missing {DEFAULT_GCP_CREDENTIALS_JSON_ENV_VAR} environment variable"
            )));
        };

        Ok(Self {
            bucket,
            use_cdn_domain,
            client: Client::default(),
            logger: logger.new_with_component_name::<Self>(),
        })
    }

    fn get_location(&self, remote_file_path: &CloudRemotePath) -> FileUri {
        let mut uri = vec![];
        if !self.use_cdn_domain {
            uri.push("storage.googleapis.com");
        }
        uri.push(&self.bucket);
        let file_path = remote_file_path.to_string();
        uri.push(&file_path);

        FileUri(format!("https://{}", uri.join("/")))
    }
}

#[async_trait]
impl CloudBackendUploader for GcpBackendUploaderLegacy {
    async fn file_exists(&self, remote_file_path: &CloudRemotePath) -> StdResult<Option<FileUri>> {
        info!(self.logger, "Reading file metadata {remote_file_path}");
        let file_uri = match self
            .client
            .object()
            .read(&self.bucket, &remote_file_path.to_string())
            .await
            .with_context(|| "remote reading file metadata failure")
        {
            Ok(_) => {
                info!(self.logger, "Found file metadata {remote_file_path}");

                Some(self.get_location(remote_file_path))
            }
            Err(_) => {
                info!(self.logger, "Missing file metadata {remote_file_path}");

                None
            }
        };

        Ok(file_uri)
    }

    async fn upload_file(
        &self,
        local_file_path: &Path,
        remote_file_path: &CloudRemotePath,
    ) -> StdResult<FileUri> {
        info!(
            self.logger,
            "Uploading {} to {remote_file_path}",
            local_file_path.display()
        );
        let file = tokio::fs::File::open(local_file_path).await.unwrap();
        let stream = FramedRead::new(file, BytesCodec::new());
        self.client
            .object()
            .create_streamed(
                &self.bucket,
                stream,
                None,
                &remote_file_path.to_string(),
                "application/octet-stream",
            )
            .await
            .with_context(|| "remote uploading failure")?;
        info!(
            self.logger,
            "Uploaded {} to {remote_file_path}",
            local_file_path.display()
        );

        Ok(self.get_location(remote_file_path))
    }

    async fn make_file_public(&self, remote_file_path: &CloudRemotePath) -> StdResult<()> {
        let new_bucket_access_control = NewObjectAccessControl {
            entity: Entity::AllUsers,
            role: Role::Reader,
        };
        info!(
            self.logger,
            "Updating acl for {remote_file_path}: {new_bucket_access_control:?}"
        );
        self.client
            .object_access_control()
            .create(
                &self.bucket,
                &remote_file_path.to_string(),
                &new_bucket_access_control,
            )
            .await
            .with_context(|| "updating acl failure")?;

        info!(self.logger, "Updated acl for {remote_file_path}");

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::test_tools::TestLogger;

    use super::*;

    #[tokio::test]
    async fn get_location_not_using_cdn_domain_return_google_api_uri() {
        env::set_var(DEFAULT_GCP_CREDENTIALS_JSON_ENV_VAR, "credentials");
        let use_cdn_domain = false;
        let gcp_file_uploader = GcpBackendUploaderLegacy::try_new(
            "cdn.mithril.network".to_string(),
            use_cdn_domain,
            TestLogger::stdout(),
        )
        .unwrap();
        let remote_file_path = CloudRemotePath::new("remote_folder").join("snapshot.xxx.tar.gz");
        let expected_location =
            "https://storage.googleapis.com/cdn.mithril.network/remote_folder/snapshot.xxx.tar.gz"
                .to_string();

        let location = gcp_file_uploader.get_location(&remote_file_path);

        assert_eq!(FileUri(expected_location), location);
    }

    #[tokio::test]
    async fn get_location_using_cdn_domain_return_cdn_in_uri() {
        env::set_var(DEFAULT_GCP_CREDENTIALS_JSON_ENV_VAR, "credentials");
        let use_cdn_domain = true;
        let gcp_file_uploader = GcpBackendUploaderLegacy::try_new(
            "cdn.mithril.network".to_string(),
            use_cdn_domain,
            TestLogger::stdout(),
        )
        .unwrap();
        let remote_file_path = CloudRemotePath::new("remote_folder").join("snapshot.xxx.tar.gz");
        let expected_location =
            "https://cdn.mithril.network/remote_folder/snapshot.xxx.tar.gz".to_string();

        let location = gcp_file_uploader.get_location(&remote_file_path);

        assert_eq!(FileUri(expected_location), location);
    }
}

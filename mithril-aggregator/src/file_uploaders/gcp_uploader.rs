use anyhow::{anyhow, Context};
use async_trait::async_trait;
use cloud_storage::{
    bucket::Entity, bucket_access_control::Role, object_access_control::NewObjectAccessControl,
    Client,
};
use slog::{info, Logger};
use std::{env, path::Path, sync::Arc};
use tokio_util::codec::{BytesCodec, FramedRead};

use mithril_common::{entities::FileUri, logging::LoggerExtensions, StdResult};

use crate::FileUploader;

/// CloudBackendUploader represents a cloud backend uploader
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait CloudBackendUploader: Send + Sync {
    /// Upload a file to the Cloud backend
    async fn upload_file(&self, file_path: &Path) -> StdResult<FileUri>;

    /// Make a file public in the Cloud backend
    async fn make_file_public(&self, file_path: &Path) -> StdResult<()>;
}

/// GcpBackendUploader represents a Google Cloud Platform file uploader
#[derive(Debug)]
pub struct GcpBackendUploader {
    bucket: String,
    use_cdn_domain: bool,
    client: Client,
    logger: Logger,
}

impl GcpBackendUploader {
    /// GcpBackendUploader factory
    pub fn try_new(bucket: String, use_cdn_domain: bool, logger: Logger) -> StdResult<Self> {
        if env::var("GOOGLE_APPLICATION_CREDENTIALS_JSON").is_err() {
            return Err(anyhow!(
                "Missing GOOGLE_APPLICATION_CREDENTIALS_JSON environment variable".to_string()
            ));
        };

        Ok(Self {
            bucket,
            use_cdn_domain,
            logger: logger.new_with_component_name::<Self>(),
            client: Client::default(),
        })
    }

    fn get_location(&self, filename: &str) -> FileUri {
        let mut uri = vec![];
        if !self.use_cdn_domain {
            uri.push("storage.googleapis.com");
        }
        uri.push(&self.bucket);
        uri.push(filename);

        FileUri(format!("https://{}", uri.join("/")))
    }
}

#[async_trait]
impl CloudBackendUploader for GcpBackendUploader {
    async fn upload_file(&self, file_path: &Path) -> StdResult<FileUri> {
        let file_name =
            get_file_name(file_path).with_context(|| "computing file name from path")?;
        info!(self.logger, "Uploading {file_name}");
        let file = tokio::fs::File::open(file_path).await.unwrap();
        let stream = FramedRead::new(file, BytesCodec::new());
        self.client
            .object()
            .create_streamed(
                &self.bucket,
                stream,
                None,
                file_name,
                "application/octet-stream",
            )
            .await
            .with_context(|| "remote uploading failure")?;
        info!(self.logger, "Uploaded {file_name}");

        Ok(self.get_location(file_name))
    }

    async fn make_file_public(&self, file_path: &Path) -> StdResult<()> {
        let file_name =
            get_file_name(file_path).with_context(|| "computing file name from path")?;
        let new_bucket_access_control = NewObjectAccessControl {
            entity: Entity::AllUsers,
            role: Role::Reader,
        };
        info!(
            self.logger,
            "Updating acl for {file_name}: {new_bucket_access_control:?}"
        );
        self.client
            .object_access_control()
            .create(&self.bucket, file_name, &new_bucket_access_control)
            .await
            .with_context(|| "updating acl failure")?;

        info!(self.logger, "Updated acl for {file_name}");

        Ok(())
    }
}

fn get_file_name(file_path: &Path) -> StdResult<&str> {
    file_path
        .file_name()
        .map(|s| s.to_str())
        .ok_or(anyhow!("Could not convert file path to file name"))?
        .ok_or(anyhow!("Could not find the final component of the path"))
}

/// GcpUploader represents a Google Cloud Platform file uploader interactor
pub struct GcpUploader {
    cloud_backend_uploader: Arc<dyn CloudBackendUploader>,
}

impl GcpUploader {
    /// GcpUploader factory
    pub fn new(cloud_backend_uploader: Arc<dyn CloudBackendUploader>) -> Self {
        Self {
            cloud_backend_uploader,
        }
    }
}

#[async_trait]
impl FileUploader for GcpUploader {
    async fn upload(&self, file_path: &Path) -> StdResult<FileUri> {
        let file_uri = self
            .cloud_backend_uploader
            .upload_file(file_path)
            .await
            .with_context(|| "uploading file to cloud")?;

        self.cloud_backend_uploader
            .make_file_public(file_path)
            .await
            .with_context(|| "making file public in cloud")?;

        Ok(file_uri)
    }
}

#[cfg(test)]
mod tests {
    use crate::test_tools::TestLogger;

    use super::*;

    mod cloud_backend_uploader {
        use mockall::predicate::eq;

        use super::*;

        #[tokio::test]
        async fn upload_public_file_success() {
            let file_path = Path::new("snapshot.xxx.tar.gz");
            let expected_file_uri = FileUri("https://cloud-host/snapshot.xxx.tar.gz".to_string());
            let expected_file_uri_clone = expected_file_uri.clone();
            let cloud_backend_uploader = {
                let mut mock_cloud_backend_uploader = MockCloudBackendUploader::new();
                mock_cloud_backend_uploader
                    .expect_upload_file()
                    .with(eq(file_path))
                    .return_once(move |_| Ok(expected_file_uri_clone))
                    .once();
                mock_cloud_backend_uploader
                    .expect_make_file_public()
                    .with(eq(file_path))
                    .return_once(move |_| Ok(()))
                    .once();

                mock_cloud_backend_uploader
            };
            let file_uploader = GcpUploader::new(Arc::new(cloud_backend_uploader));

            let file_uri = file_uploader.upload(file_path).await.unwrap();

            assert_eq!(expected_file_uri, file_uri);
        }

        #[tokio::test]
        async fn upload_public_file_fails_when_upload_fails() {
            let file_path = Path::new("snapshot.xxx.tar.gz");
            let cloud_backend_uploader = {
                let mut mock_cloud_backend_uploader = MockCloudBackendUploader::new();
                mock_cloud_backend_uploader
                    .expect_upload_file()
                    .with(eq(file_path))
                    .return_once(move |_| Err(anyhow!("upload error")))
                    .once();

                mock_cloud_backend_uploader
            };
            let file_uploader = GcpUploader::new(Arc::new(cloud_backend_uploader));

            file_uploader
                .upload(file_path)
                .await
                .expect_err("should have failed");
        }

        #[tokio::test]
        async fn upload_public_file_fails_when_make_public_fails() {
            let file_path = Path::new("snapshot.xxx.tar.gz");
            let expected_file_uri = FileUri("https://cloud-host/snapshot.xxx.tar.gz".to_string());
            let expected_file_uri_clone = expected_file_uri.clone();
            let cloud_backend_uploader = {
                let mut mock_cloud_backend_uploader = MockCloudBackendUploader::new();
                mock_cloud_backend_uploader
                    .expect_upload_file()
                    .with(eq(file_path))
                    .return_once(move |_| Ok(expected_file_uri_clone))
                    .once();
                mock_cloud_backend_uploader
                    .expect_make_file_public()
                    .with(eq(file_path))
                    .return_once(move |_| Err(anyhow!("make public error")))
                    .once();

                mock_cloud_backend_uploader
            };
            let file_uploader = GcpUploader::new(Arc::new(cloud_backend_uploader));

            file_uploader
                .upload(file_path)
                .await
                .expect_err("should have failed");
        }
    }

    mod gcp_backend_uploader {
        use super::*;

        #[tokio::test]
        async fn get_location_not_using_cdn_domain_return_google_api_uri() {
            env::set_var("GOOGLE_APPLICATION_CREDENTIALS_JSON", "credentials");
            let use_cdn_domain = false;
            let gcp_file_uploader = GcpBackendUploader::try_new(
                "cdn.mithril.network".to_string(),
                use_cdn_domain,
                TestLogger::stdout(),
            )
            .unwrap();
            let filename = "snapshot.xxx.tar.gz";
            let expected_location =
                "https://storage.googleapis.com/cdn.mithril.network/snapshot.xxx.tar.gz"
                    .to_string();

            let location = gcp_file_uploader.get_location(filename);

            assert_eq!(FileUri(expected_location), location);
        }

        #[tokio::test]
        async fn get_location_using_cdn_domain_return_cdn_in_uri() {
            env::set_var("GOOGLE_APPLICATION_CREDENTIALS_JSON", "credentials");
            let use_cdn_domain = true;
            let gcp_file_uploader = GcpBackendUploader::try_new(
                "cdn.mithril.network".to_string(),
                use_cdn_domain,
                TestLogger::stdout(),
            )
            .unwrap();
            let filename = "snapshot.xxx.tar.gz";
            let expected_location = "https://cdn.mithril.network/snapshot.xxx.tar.gz".to_string();

            let location = gcp_file_uploader.get_location(filename);

            assert_eq!(FileUri(expected_location), location);
        }
    }
}

use anyhow::{anyhow, Context};
use async_trait::async_trait;
use cloud_storage::{
    bucket::Entity, bucket_access_control::Role, object_access_control::NewObjectAccessControl,
    Client,
};
use slog::{info, Logger};
use std::{
    env,
    fmt::Display,
    path::{Path, PathBuf},
    sync::Arc,
};
use tokio_util::codec::{BytesCodec, FramedRead};

use mithril_common::{entities::FileUri, logging::LoggerExtensions, StdResult};

use crate::FileUploader;

use super::FileUploadRetryPolicy;

/// CloudRemotePath represents a cloud remote path
#[derive(Debug, Clone, PartialEq)]
pub struct CloudRemotePath(PathBuf);

impl CloudRemotePath {
    /// CloudRemotePath factory
    pub fn new(file_path: &str) -> Self {
        Self(PathBuf::from(file_path))
    }

    /// Join a file path to the current remote path
    pub fn join(&self, file_path: &str) -> Self {
        Self(self.0.join(file_path))
    }
}

impl Display for CloudRemotePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.to_string_lossy())
    }
}

impl From<&Path> for CloudRemotePath {
    fn from(path: &Path) -> Self {
        CloudRemotePath(path.to_path_buf())
    }
}

fn get_file_name(file_path: &Path) -> StdResult<&str> {
    file_path
        .file_name()
        .map(|s| s.to_str())
        .ok_or(anyhow!("Could not convert file path to file name"))?
        .ok_or(anyhow!("Could not find the final component of the path"))
}

/// CloudBackendUploader represents a cloud backend uploader
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait CloudBackendUploader: Send + Sync {
    /// Check if a file exists in the cloud backend
    async fn file_exists(&self, remote_file_path: &CloudRemotePath) -> StdResult<Option<FileUri>>;

    /// Upload a file to the cloud backend
    async fn upload_file(
        &self,
        local_file_path: &Path,
        remote_file_path: &CloudRemotePath,
    ) -> StdResult<FileUri>;

    /// Make a file public in the cloud backend
    async fn make_file_public(&self, remote_file_path: &CloudRemotePath) -> StdResult<()>;
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
impl CloudBackendUploader for GcpBackendUploader {
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

/// GcpUploader represents a Google Cloud Platform file uploader interactor
pub struct GcpUploader {
    cloud_backend_uploader: Arc<dyn CloudBackendUploader>,
    remote_folder: CloudRemotePath,
    allow_overwrite: bool,
    retry_policy: FileUploadRetryPolicy,
}

impl GcpUploader {
    /// GcpUploader factory
    pub fn new(
        cloud_backend_uploader: Arc<dyn CloudBackendUploader>,
        remote_folder: CloudRemotePath,
        allow_overwrite: bool,
    ) -> Self {
        Self {
            cloud_backend_uploader,
            remote_folder,
            allow_overwrite,
            retry_policy: FileUploadRetryPolicy::never(),
        }
    }

    /// Create a new instance with a custom retry policy.
    pub fn with_retry_policy(
        cloud_backend_uploader: Arc<dyn CloudBackendUploader>,
        remote_folder: CloudRemotePath,
        allow_overwrite: bool,
        retry_policy: FileUploadRetryPolicy,
    ) -> Self {
        Self {
            cloud_backend_uploader,
            remote_folder,
            allow_overwrite,
            retry_policy,
        }
    }
}

#[async_trait]
impl FileUploader for GcpUploader {
    async fn upload_without_retry(&self, file_path: &Path) -> StdResult<FileUri> {
        let remote_file_path = self.remote_folder.join(get_file_name(file_path)?);
        if !self.allow_overwrite {
            if let Some(file_uri) = self
                .cloud_backend_uploader
                .file_exists(&remote_file_path)
                .await
                .with_context(|| "checking if file exists in cloud")?
            {
                return Ok(file_uri);
            }
        }

        let file_uri = self
            .cloud_backend_uploader
            .upload_file(file_path, &remote_file_path)
            .await
            .with_context(|| "uploading file to cloud")?;
        self.cloud_backend_uploader
            .make_file_public(&remote_file_path)
            .await
            .with_context(|| "making file public in cloud")?;

        Ok(file_uri)
    }

    fn retry_policy(&self) -> FileUploadRetryPolicy {
        self.retry_policy.clone()
    }
}

#[cfg(test)]
mod tests {
    use std::time::Duration;

    use crate::{file_uploaders::FileUploadRetryPolicy, test_tools::TestLogger};

    use super::*;

    mod cloud_backend_uploader {
        use mockall::predicate::eq;

        use super::*;

        #[tokio::test]
        async fn upload_public_file_succeeds_when_file_does_not_exist_remotely_and_without_overwriting_allowed(
        ) {
            let allow_overwrite = false;
            let local_file_path = Path::new("local_folder").join("snapshot.xxx.tar.gz");
            let remote_folder_path = CloudRemotePath::new("remote_folder");
            let remote_file_path = remote_folder_path.join("snapshot.xxx.tar.gz");
            let expected_file_uri =
                FileUri("https://cloud-host/remote_folder/snapshot.xxx.tar.gz".to_string());
            let cloud_backend_uploader = {
                let mut mock_cloud_backend_uploader = MockCloudBackendUploader::new();
                mock_cloud_backend_uploader
                    .expect_file_exists()
                    .with(eq(remote_file_path.clone()))
                    .return_once(move |_| Ok(None))
                    .once();
                let expected_file_uri_clone = expected_file_uri.clone();
                mock_cloud_backend_uploader
                    .expect_upload_file()
                    .with(eq(local_file_path.clone()), eq(remote_file_path.clone()))
                    .return_once(move |_, _| Ok(expected_file_uri_clone))
                    .once();
                mock_cloud_backend_uploader
                    .expect_make_file_public()
                    .with(eq(remote_file_path.clone()))
                    .return_once(move |_| Ok(()))
                    .once();

                mock_cloud_backend_uploader
            };
            let file_uploader = GcpUploader::new(
                Arc::new(cloud_backend_uploader),
                remote_folder_path,
                allow_overwrite,
            );

            let file_uri = file_uploader.upload(&local_file_path).await.unwrap();

            assert_eq!(expected_file_uri, file_uri);
        }

        #[tokio::test]
        async fn upload_public_file_succeeds_when_file_exists_remotely_and_without_overwriting_allowed(
        ) {
            let allow_overwrite = false;
            let local_file_path = Path::new("local_folder").join("snapshot.xxx.tar.gz");
            let remote_folder_path = CloudRemotePath::new("remote_folder");
            let remote_file_path = remote_folder_path.join("snapshot.xxx.tar.gz");
            let expected_file_uri =
                FileUri("https://cloud-host/remote_folder/snapshot.xxx.tar.gz".to_string());
            let cloud_backend_uploader = {
                let mut mock_cloud_backend_uploader = MockCloudBackendUploader::new();
                let expected_file_uri_clone = expected_file_uri.clone();
                mock_cloud_backend_uploader
                    .expect_file_exists()
                    .with(eq(remote_file_path))
                    .return_once(move |_| Ok(Some(expected_file_uri_clone)))
                    .once();

                mock_cloud_backend_uploader
            };
            let file_uploader = GcpUploader::new(
                Arc::new(cloud_backend_uploader),
                remote_folder_path,
                allow_overwrite,
            );

            let file_uri = file_uploader.upload(&local_file_path).await.unwrap();

            assert_eq!(expected_file_uri, file_uri);
        }

        #[tokio::test]
        async fn upload_public_file_succeeds_with_overwriting_allowed() {
            let allow_overwrite = true;
            let local_file_path = Path::new("local_folder").join("snapshot.xxx.tar.gz");
            let remote_folder_path = CloudRemotePath::new("remote_folder");
            let remote_file_path = remote_folder_path.join("snapshot.xxx.tar.gz");
            let expected_file_uri =
                FileUri("https://cloud-host/remote_folder/snapshot.xxx.tar.gz".to_string());
            let cloud_backend_uploader = {
                let mut mock_cloud_backend_uploader = MockCloudBackendUploader::new();
                let expected_file_uri_clone = expected_file_uri.clone();
                mock_cloud_backend_uploader
                    .expect_upload_file()
                    .with(eq(local_file_path.clone()), eq(remote_file_path.clone()))
                    .return_once(move |_, _| Ok(expected_file_uri_clone))
                    .once();
                mock_cloud_backend_uploader
                    .expect_make_file_public()
                    .with(eq(remote_file_path))
                    .return_once(move |_| Ok(()))
                    .once();

                mock_cloud_backend_uploader
            };
            let file_uploader = GcpUploader::new(
                Arc::new(cloud_backend_uploader),
                remote_folder_path,
                allow_overwrite,
            );

            let file_uri = file_uploader.upload(&local_file_path).await.unwrap();

            assert_eq!(expected_file_uri, file_uri);
        }

        #[tokio::test]
        async fn upload_public_file_fails_when_file_exists_fails_and_without_overwriting_allowed() {
            let allow_overwrite = false;
            let cloud_backend_uploader = {
                let mut mock_cloud_backend_uploader = MockCloudBackendUploader::new();
                mock_cloud_backend_uploader
                    .expect_file_exists()
                    .returning(|_| Err(anyhow!("file exists error")));

                mock_cloud_backend_uploader
            };
            let file_uploader = GcpUploader::new(
                Arc::new(cloud_backend_uploader),
                CloudRemotePath::new("remote_folder"),
                allow_overwrite,
            );

            file_uploader
                .upload(Path::new("whatever"))
                .await
                .expect_err("should have failed");
        }

        #[tokio::test]
        async fn upload_public_file_fails_when_upload_fails() {
            let allow_overwrite = true;
            let cloud_backend_uploader = {
                let mut mock_cloud_backend_uploader = MockCloudBackendUploader::new();
                mock_cloud_backend_uploader
                    .expect_upload_file()
                    .return_once(move |_, _| Err(anyhow!("upload error")))
                    .once();

                mock_cloud_backend_uploader
            };
            let file_uploader = GcpUploader::new(
                Arc::new(cloud_backend_uploader),
                CloudRemotePath::new("remote_folder"),
                allow_overwrite,
            );

            file_uploader
                .upload(Path::new("whatever"))
                .await
                .expect_err("should have failed");
        }

        #[tokio::test]
        async fn upload_public_file_fails_when_make_public_fails() {
            let allow_overwrite = true;
            let cloud_backend_uploader = {
                let mut mock_cloud_backend_uploader = MockCloudBackendUploader::new();
                mock_cloud_backend_uploader
                    .expect_upload_file()
                    .returning(|_, _| Ok(FileUri("https://whatever".to_string())));
                mock_cloud_backend_uploader
                    .expect_make_file_public()
                    .returning(|_| Err(anyhow!("make public error")));

                mock_cloud_backend_uploader
            };
            let file_uploader = GcpUploader::new(
                Arc::new(cloud_backend_uploader),
                CloudRemotePath::new("remote_folder"),
                allow_overwrite,
            );

            file_uploader
                .upload(Path::new("whatever"))
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
            let remote_file_path =
                CloudRemotePath::new("remote_folder").join("snapshot.xxx.tar.gz");
            let expected_location =
                "https://storage.googleapis.com/cdn.mithril.network/remote_folder/snapshot.xxx.tar.gz"
                    .to_string();

            let location = gcp_file_uploader.get_location(&remote_file_path);

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
            let remote_file_path =
                CloudRemotePath::new("remote_folder").join("snapshot.xxx.tar.gz");
            let expected_location =
                "https://cdn.mithril.network/remote_folder/snapshot.xxx.tar.gz".to_string();

            let location = gcp_file_uploader.get_location(&remote_file_path);

            assert_eq!(FileUri(expected_location), location);
        }
    }

    #[tokio::test]
    async fn retry_policy_from_file_uploader_trait_should_be_implemented() {
        let expected_policy = FileUploadRetryPolicy {
            attempts: 10,
            delay_between_attempts: Duration::from_millis(123),
        };

        let file_uploader: Box<dyn FileUploader> = Box::new(GcpUploader::with_retry_policy(
            Arc::new(MockCloudBackendUploader::new()),
            CloudRemotePath::new("remote_folder"),
            true,
            expected_policy.clone(),
        ));

        assert_eq!(expected_policy, file_uploader.retry_policy());
    }
}

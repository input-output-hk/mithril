use std::{path::Path, sync::Arc};

use anyhow::{Context, anyhow};
use async_trait::async_trait;
use mithril_common::{StdResult, entities::FileUri};

use super::CloudBackendUploader;
use crate::FileUploader;
use crate::file_uploaders::{CloudRemotePath, FileUploadRetryPolicy};

fn get_file_name(file_path: &Path) -> StdResult<&str> {
    file_path
        .file_name()
        .map(|s| s.to_str())
        .ok_or(anyhow!("Could not convert file path to file name"))?
        .ok_or(anyhow!("Could not find the final component of the path"))
}

/// The `CloudUploader` struct is responsible for managing the upload of files to a cloud storage backend.
pub struct CloudUploader {
    cloud_backend_uploader: Arc<dyn CloudBackendUploader>,
    remote_folder: CloudRemotePath,
    allow_overwrite: bool,
    retry_policy: FileUploadRetryPolicy,
}

impl CloudUploader {
    /// `CloudUploader` factory
    pub fn new(
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
impl FileUploader for CloudUploader {
    async fn upload_without_retry(&self, file_path: &Path) -> StdResult<FileUri> {
        let remote_file_path = self.remote_folder.join(get_file_name(file_path)?);
        if !self.allow_overwrite
            && let Some(file_uri) = self
                .cloud_backend_uploader
                .file_exists(&remote_file_path)
                .await
                .with_context(|| "checking if file exists in cloud")?
        {
            return Ok(file_uri);
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

    use mockall::predicate::eq;

    use super::*;
    use crate::file_uploaders::FileUploadRetryPolicy;
    use crate::file_uploaders::cloud_uploader::MockCloudBackendUploader;

    #[tokio::test]
    async fn upload_public_file_succeeds_when_file_does_not_exist_remotely_and_without_overwriting_allowed()
     {
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
        let file_uploader = CloudUploader::new(
            Arc::new(cloud_backend_uploader),
            remote_folder_path,
            allow_overwrite,
            FileUploadRetryPolicy::never(),
        );

        let file_uri = file_uploader.upload(&local_file_path).await.unwrap();

        assert_eq!(expected_file_uri, file_uri);
    }

    #[tokio::test]
    async fn upload_public_file_succeeds_when_file_exists_remotely_and_without_overwriting_allowed()
    {
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
        let file_uploader = CloudUploader::new(
            Arc::new(cloud_backend_uploader),
            remote_folder_path,
            allow_overwrite,
            FileUploadRetryPolicy::never(),
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
        let file_uploader = CloudUploader::new(
            Arc::new(cloud_backend_uploader),
            remote_folder_path,
            allow_overwrite,
            FileUploadRetryPolicy::never(),
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
        let file_uploader = CloudUploader::new(
            Arc::new(cloud_backend_uploader),
            CloudRemotePath::new("remote_folder"),
            allow_overwrite,
            FileUploadRetryPolicy::never(),
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
        let file_uploader = CloudUploader::new(
            Arc::new(cloud_backend_uploader),
            CloudRemotePath::new("remote_folder"),
            allow_overwrite,
            FileUploadRetryPolicy::never(),
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
        let file_uploader = CloudUploader::new(
            Arc::new(cloud_backend_uploader),
            CloudRemotePath::new("remote_folder"),
            allow_overwrite,
            FileUploadRetryPolicy::never(),
        );

        file_uploader
            .upload(Path::new("whatever"))
            .await
            .expect_err("should have failed");
    }

    #[tokio::test]
    async fn retry_policy_from_file_uploader_trait_should_be_implemented() {
        let expected_policy = FileUploadRetryPolicy {
            attempts: 10,
            delay_between_attempts: Duration::from_millis(123),
        };

        let file_uploader: Box<dyn FileUploader> = Box::new(CloudUploader::new(
            Arc::new(MockCloudBackendUploader::new()),
            CloudRemotePath::new("remote_folder"),
            true,
            expected_policy.clone(),
        ));

        assert_eq!(expected_policy, file_uploader.retry_policy());
    }
}

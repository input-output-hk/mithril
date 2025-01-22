use std::path::Path;

use async_trait::async_trait;
use mithril_common::{entities::FileUri, StdResult};

use super::FileUploader;

/// A FileUploader decorator that retries the upload using the decorated FileUploader until it succeeds.
/// The upload fails after a number of attempts.
pub struct RetryableFileUploader<T: FileUploader> {
    wrapped_uploader: T,
    call_limit: u32,
}

impl<T: FileUploader> RetryableFileUploader<T> {
    pub fn new(wrapped_uploader: T, call_limit: u32) -> Self {
        Self {
            wrapped_uploader,
            call_limit,
        }
    }
}

#[async_trait]
impl<T: FileUploader> FileUploader for RetryableFileUploader<T> {
    async fn upload(&self, filepath: &Path) -> StdResult<FileUri> {
        for _retry in 0..self.call_limit {
            let result = self.wrapped_uploader.upload(filepath).await;
            if result.is_ok() {
                return result;
            }
        }

        Err(anyhow::anyhow!("Upload retry limit reached"))
    }
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;
    use std::path::PathBuf;

    use mithril_common::entities::FileUri;
    use mockall::predicate::eq;

    use crate::file_uploaders::MockFileUploader;

    use super::*;

    #[tokio::test]
    async fn should_call_inner_uploader() {
        let mut inner_uploader = MockFileUploader::new();
        inner_uploader
            .expect_upload()
            .with(eq(PathBuf::from("file_to_upload")))
            .times(1)
            .returning(|_| Ok(FileUri("http://test.com".to_string())));

        let retry_uploader = RetryableFileUploader::new(inner_uploader, 4);
        retry_uploader
            .upload(&PathBuf::from("file_to_upload"))
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn should_recall_inner_uploader_if_fail() {
        let mut inner_uploader = MockFileUploader::new();
        inner_uploader
            .expect_upload()
            .with(eq(PathBuf::from("file_to_upload")))
            .times(2)
            .returning(|_| Err(anyhow!("Failure while uploading...")));
        inner_uploader
            .expect_upload()
            .with(eq(PathBuf::from("file_to_upload")))
            .times(1)
            .returning(|_| Ok(FileUri("http://test.com".to_string())));

        let retry_uploader = RetryableFileUploader::new(inner_uploader, 4);
        retry_uploader
            .upload(&PathBuf::from("file_to_upload"))
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn should_recall_a_failing_inner_uploader_up_to_the_limit() {
        let mut inner_uploader = MockFileUploader::new();

        inner_uploader
            .expect_upload()
            .with(eq(PathBuf::from("file_to_upload")))
            .times(4)
            .returning(move |_| Err(anyhow!("Failure while uploading...")));

        let retry_uploader = RetryableFileUploader::new(inner_uploader, 4);

        retry_uploader
            .upload(&PathBuf::from("file_to_upload"))
            .await
            .expect_err("An error should be returned when all retries are done");
    }
}

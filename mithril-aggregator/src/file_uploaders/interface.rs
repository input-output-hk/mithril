use async_trait::async_trait;
use mithril_common::{entities::FileUri, StdResult};
use std::{
    any::{Any, TypeId},
    path::Path,
    time::Duration,
};

/// Policy for retrying file uploads.
pub struct FileUploadRetryPolicy {
    attempts: usize,
    delay_between_attempts: Duration,
}

impl FileUploadRetryPolicy {
    fn never() -> Self {
        Self {
            attempts: 1,
            delay_between_attempts: Duration::from_secs(0),
        }
    }
}

impl Default for FileUploadRetryPolicy {
    fn default() -> Self {
        Self {
            attempts: 3,
            delay_between_attempts: Duration::from_secs(5),
        }
    }
}

/// FileUploader represents a file uploader interactor.
/// It retries the upload operation according to the retry policy.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait FileUploader: Any + Sync + Send {
    /// Try to upload once.
    async fn upload_without_retry(&self, filepath: &Path) -> StdResult<FileUri>;

    fn retry_policy(&self) -> FileUploadRetryPolicy {
        FileUploadRetryPolicy::never()
    }

    // Upload a file
    async fn upload(&self, filepath: &Path) -> StdResult<FileUri> {
        let retry_policy = self.retry_policy();

        let mut nb_attempts = 0;
        loop {
            nb_attempts += 1;
            match self.upload_without_retry(filepath).await {
                Ok(result) => return Ok(result),
                Err(_) if nb_attempts >= retry_policy.attempts => {
                    return Err(anyhow::anyhow!("Upload retry limit reached"));
                }
                _ => tokio::time::sleep(retry_policy.delay_between_attempts).await,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{path::PathBuf, time::Instant};

    use super::*;
    use anyhow::anyhow;
    use mockall::{mock, predicate::eq};

    mock! {
        TestFileUploaderWithDefaultRetryPolicy {
        }
        #[async_trait]
        impl FileUploader for TestFileUploaderWithDefaultRetryPolicy {
            async fn upload_without_retry(&self, filepath: &Path) -> StdResult<FileUri>;
        }
    }

    mock! {
        TestFileUploader {
        }

        #[async_trait]
        impl FileUploader for TestFileUploader {
            async fn upload_without_retry(&self, filepath: &Path) -> StdResult<FileUri>;
            fn retry_policy(&self) -> FileUploadRetryPolicy;
        }
    }

    #[tokio::test]
    async fn upload_return_the_result_of_upload_without_retry() {
        let mut uploader = MockTestFileUploaderWithDefaultRetryPolicy::new();
        uploader
            .expect_upload_without_retry()
            .with(eq(PathBuf::from("file_to_upload")))
            .times(1)
            .returning(|_| Ok(FileUri("file_uploaded".to_string())));

        let file_uploaded = uploader.upload(Path::new("file_to_upload")).await.unwrap();
        assert_eq!(FileUri("file_uploaded".to_string()), file_uploaded);
    }

    #[tokio::test]
    async fn when_upload_fails_do_not_retry_by_default() {
        let mut uploader = MockTestFileUploaderWithDefaultRetryPolicy::new();
        uploader
            .expect_upload_without_retry()
            .with(eq(PathBuf::from("file_to_upload")))
            .times(1)
            .returning(|_| Err(anyhow!("Failure while uploading...")));

        uploader
            .upload(Path::new("file_to_upload"))
            .await
            .expect_err("Should fail on upload");
    }

    #[tokio::test]
    async fn should_retry_if_fail() {
        let mut uploader = MockTestFileUploader::new();

        uploader
            .expect_retry_policy()
            .returning(|| FileUploadRetryPolicy {
                attempts: 50,
                delay_between_attempts: Duration::ZERO,
            });

        uploader
            .expect_upload_without_retry()
            .with(eq(PathBuf::from("file_to_upload")))
            .times(2)
            .returning(|_| Err(anyhow!("Failure while uploading...")));
        uploader
            .expect_upload_without_retry()
            .with(eq(PathBuf::from("file_to_upload")))
            .times(1)
            .returning(|_| Ok(FileUri("file_uploaded".to_string())));

        let file_uploaded = uploader.upload(Path::new("file_to_upload")).await.unwrap();
        assert_eq!(FileUri("file_uploaded".to_string()), file_uploaded);
    }

    #[tokio::test]
    async fn should_recall_a_failing_inner_uploader_up_to_the_limit() {
        let mut uploader = MockTestFileUploader::new();

        uploader
            .expect_retry_policy()
            .returning(|| FileUploadRetryPolicy {
                attempts: 4,
                delay_between_attempts: Duration::ZERO,
            });

        uploader
            .expect_upload_without_retry()
            .with(eq(PathBuf::from("file_to_upload")))
            .times(4)
            .returning(|_| Err(anyhow!("Failure while uploading...")));

        uploader
            .upload(&PathBuf::from("file_to_upload"))
            .await
            .expect_err("An error should be returned when all retries are done");
    }

    #[tokio::test]
    async fn should_delay_between_retries() {
        let mut uploader = MockTestFileUploader::new();

        let delay = Duration::from_millis(50);
        uploader
            .expect_retry_policy()
            .returning(move || FileUploadRetryPolicy {
                attempts: 4,
                delay_between_attempts: delay,
            });

        uploader
            .expect_upload_without_retry()
            .times(4)
            .returning(move |_| Err(anyhow!("Failure while uploading...")));

        let start = Instant::now();
        uploader
            .upload(&PathBuf::from("file_to_upload"))
            .await
            .expect_err("An error should be returned when all retries are done");
        let duration = start.elapsed();

        assert!(
            duration >= delay * 3,
            "Duration should be at least 3 times the delay ({}ms) but was {}ms",
            delay.as_millis() * 3,
            duration.as_millis()
        );
        assert!(
            duration < delay * 4,
            "Duration should be less than 4 times the delay ({}ms) but was {}ms",
            delay.as_millis() * 4,
            duration.as_millis()
        );
    }
}

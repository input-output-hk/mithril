use std::{path::Path, time::Duration};

use async_trait::async_trait;
use mithril_common::{entities::FileUri, StdResult};
use tokio::time;

use super::FileUploader;

/// A FileUploader decorator that retries the upload using the decorated FileUploader until it succeeds.
/// The upload fails after a number of attempts.
pub struct RetryableFileUploader<T: FileUploader> {
    wrapped_uploader: T,
    call_limit: u32,
    delay_between_attempts: Duration,
}

impl<T: FileUploader> RetryableFileUploader<T> {
    pub fn new(wrapped_uploader: T, call_limit: u32, delay_between_attempts: Duration) -> Self {
        Self {
            wrapped_uploader,
            call_limit,
            delay_between_attempts,
        }
    }
}

#[async_trait]
impl<T: FileUploader> FileUploader for RetryableFileUploader<T> {
    async fn upload(&self, filepath: &Path) -> StdResult<FileUri> {
        let mut nb_attemps = 0;
        loop {
            nb_attemps += 1;
            match self.wrapped_uploader.upload(filepath).await {
                Ok(result) => return Ok(result),
                Err(_) if nb_attemps >= self.call_limit => {
                    return Err(anyhow::anyhow!("Upload retry limit reached"));
                }
                _ => time::sleep(self.delay_between_attempts).await,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;
    use std::{path::PathBuf, time::Instant};

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

        let retry_uploader = RetryableFileUploader::new(inner_uploader, 4, Duration::ZERO);
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

        let retry_uploader = RetryableFileUploader::new(inner_uploader, 4, Duration::ZERO);

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

        let retry_uploader = RetryableFileUploader::new(inner_uploader, 4, Duration::ZERO);

        retry_uploader
            .upload(&PathBuf::from("file_to_upload"))
            .await
            .expect_err("An error should be returned when all retries are done");
    }

    #[tokio::test]
    async fn should_delay_between_retries() {
        let mut inner_uploader = MockFileUploader::new();

        inner_uploader
            .expect_upload()
            .times(4)
            .returning(move |_| Err(anyhow!("Failure while uploading...")));

        let delay = Duration::from_millis(50);
        let retry_uploader = RetryableFileUploader::new(inner_uploader, 4, delay);

        let start = Instant::now();
        retry_uploader
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

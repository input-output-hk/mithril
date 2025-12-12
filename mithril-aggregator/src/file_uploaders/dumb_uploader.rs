use anyhow::anyhow;
use async_trait::async_trait;
use mithril_common::{StdResult, entities::FileUri};
use std::{path::Path, sync::RwLock};

use crate::file_uploaders::{FileUploadRetryPolicy, FileUploader};

/// Dummy uploader for test purposes.
///
/// It actually does NOT upload any file but keep the history of all the files it
/// was asked to upload. This is intended to by used by integration tests.
pub struct DumbUploader {
    uploads_history: RwLock<Vec<FileUri>>,
    retry_policy: FileUploadRetryPolicy,
}

impl DumbUploader {
    /// Create a new instance with a custom retry policy.
    pub fn new(retry_policy: FileUploadRetryPolicy) -> Self {
        Self {
            uploads_history: RwLock::new(Vec::new()),
            retry_policy,
        }
    }

    /// Return the last upload that was triggered.
    pub fn get_last_upload(&self) -> StdResult<Option<FileUri>> {
        let last_upload = self.get_last_n_uploads(1)?;
        Ok(last_upload.first().cloned())
    }

    /// Return the last `n` uploads that were triggered in anti-chronological order.
    pub fn get_last_n_uploads(&self, n: usize) -> StdResult<Vec<FileUri>> {
        let value = self
            .uploads_history
            .read()
            .map_err(|e| anyhow!(e.to_string()).context("Error while reading filepath location"))?;

        Ok(value.iter().rev().take(n).cloned().collect::<Vec<FileUri>>())
    }
}

impl Default for DumbUploader {
    fn default() -> Self {
        Self::new(FileUploadRetryPolicy::never())
    }
}

#[async_trait]
impl FileUploader for DumbUploader {
    /// Upload a file
    async fn upload_without_retry(&self, filepath: &Path) -> StdResult<FileUri> {
        let mut uploads_history = self
            .uploads_history
            .write()
            .map_err(|e| anyhow!(e.to_string()).context("Error while saving filepath location"))?;

        let location = FileUri(filepath.to_string_lossy().to_string());
        uploads_history.push(location.clone());

        Ok(location)
    }

    fn retry_policy(&self) -> FileUploadRetryPolicy {
        self.retry_policy.clone()
    }
}

#[cfg(test)]
mod tests {
    use std::time::Duration;

    use super::*;

    #[tokio::test]
    async fn test_dumb_uploader() {
        let uploader = DumbUploader::default();
        assert!(
            uploader
                .get_last_upload()
                .expect("uploader should not fail")
                .is_none()
        );
        let res = uploader
            .upload(Path::new("/tmp/whatever"))
            .await
            .expect("uploading with a dumb uploader should not fail");
        assert_eq!(res, FileUri("/tmp/whatever".to_string()));
        assert_eq!(
            Some(FileUri("/tmp/whatever".to_string())),
            uploader
                .get_last_upload()
                .expect("getting dumb uploader last value after a fake download should not fail")
        );
    }

    #[tokio::test]
    async fn get_history_of_multiple_uploads() {
        let uploader = DumbUploader::default();
        assert_eq!(
            Vec::<FileUri>::new(),
            uploader.get_last_n_uploads(usize::MAX).unwrap()
        );

        uploader.upload(Path::new("/tmp/whatever")).await.unwrap();
        uploader.upload(Path::new("/tmp/whatever2")).await.unwrap();
        uploader.upload(Path::new("/tmp/whatever3")).await.unwrap();

        assert_eq!(
            vec![
                FileUri("/tmp/whatever3".to_string()),
                FileUri("/tmp/whatever2".to_string()),
                FileUri("/tmp/whatever".to_string()),
            ],
            uploader.get_last_n_uploads(usize::MAX).unwrap()
        );

        assert_eq!(
            vec![
                FileUri("/tmp/whatever3".to_string()),
                FileUri("/tmp/whatever2".to_string()),
            ],
            uploader.get_last_n_uploads(2).unwrap()
        );
    }

    #[tokio::test]
    async fn retry_policy_from_file_uploader_trait_should_be_implemented() {
        let expected_policy = FileUploadRetryPolicy {
            attempts: 10,
            delay_between_attempts: Duration::from_millis(123),
        };

        let uploader: Box<dyn FileUploader> = Box::new(DumbUploader::new(expected_policy.clone()));

        assert_eq!(expected_policy, uploader.retry_policy());
    }
}

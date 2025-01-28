use anyhow::anyhow;
use async_trait::async_trait;
use mithril_common::{entities::FileUri, StdResult};
use std::{path::Path, sync::RwLock};

use crate::file_uploaders::{FileUploadRetryPolicy, FileUploader};

/// Dummy uploader for test purposes.
///
/// It actually does NOT upload any file but remembers the last file it
/// was asked to upload. This is intended to by used by integration tests.
pub struct DumbUploader {
    last_uploaded: RwLock<Option<FileUri>>,
    retry_policy: FileUploadRetryPolicy,
}

impl DumbUploader {
    /// Create a new instance with a custom retry policy.
    pub fn new(retry_policy: FileUploadRetryPolicy) -> Self {
        Self {
            last_uploaded: RwLock::new(None),
            retry_policy,
        }
    }

    /// Return the last upload that was triggered.
    pub fn get_last_upload(&self) -> StdResult<Option<FileUri>> {
        let value = self
            .last_uploaded
            .read()
            .map_err(|e| anyhow!("Error while saving filepath location: {e}"))?;

        Ok(value.as_ref().map(Clone::clone))
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
        let mut value = self
            .last_uploaded
            .write()
            .map_err(|e| anyhow!("Error while saving filepath location: {e}"))?;

        let location = FileUri(filepath.to_string_lossy().to_string());
        *value = Some(location.clone());

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
        assert!(uploader
            .get_last_upload()
            .expect("uploader should not fail")
            .is_none());
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
    async fn retry_policy_from_file_uploader_trait_should_be_implemented() {
        let expected_policy = FileUploadRetryPolicy {
            attempts: 10,
            delay_between_attempts: Duration::from_millis(123),
        };

        let uploader: Box<dyn FileUploader> = Box::new(DumbUploader::new(expected_policy.clone()));

        assert_eq!(expected_policy, uploader.retry_policy());
    }
}

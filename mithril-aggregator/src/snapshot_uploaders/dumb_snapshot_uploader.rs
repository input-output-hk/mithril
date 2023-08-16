use anyhow::anyhow;
use async_trait::async_trait;
use mithril_common::StdResult;
use std::{path::Path, sync::RwLock};

use super::{SnapshotLocation, SnapshotUploader};

/// Dummy uploader for test purposes.
///
/// It actually does NOT upload any snapshot but remembers the last snapshot it
/// was asked to upload. This is intended to by used by integration tests.
pub struct DumbSnapshotUploader {
    last_uploaded: RwLock<Option<String>>,
}

impl DumbSnapshotUploader {
    /// Create a new instance.
    pub fn new() -> Self {
        Self {
            last_uploaded: RwLock::new(None),
        }
    }

    /// Return the last upload that was triggered.
    pub fn get_last_upload(&self) -> StdResult<Option<String>> {
        let value = self
            .last_uploaded
            .read()
            .map_err(|e| anyhow!("Error while saving filepath location: {e}"))?;

        Ok(value.as_ref().map(|v| v.to_string()))
    }
}

impl Default for DumbSnapshotUploader {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl SnapshotUploader for DumbSnapshotUploader {
    /// Upload a snapshot
    async fn upload_snapshot(&self, snapshot_filepath: &Path) -> StdResult<SnapshotLocation> {
        let mut value = self
            .last_uploaded
            .write()
            .map_err(|e| anyhow!("Error while saving filepath location: {e}"))?;

        let location = snapshot_filepath.to_string_lossy().to_string();
        *value = Some(location.clone());

        Ok(location)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_dumb_uploader() {
        let uploader = DumbSnapshotUploader::new();
        assert!(uploader
            .get_last_upload()
            .expect("uploader should not fail")
            .is_none());
        let res = uploader
            .upload_snapshot(Path::new("/tmp/whatever"))
            .await
            .expect("uploading with a dumb uploader should not fail");
        assert_eq!(res, "/tmp/whatever".to_string());
        assert_eq!(
            Some("/tmp/whatever".to_string()),
            uploader
                .get_last_upload()
                .expect("getting dumb uploader last value after a fake download should not fail")
        );
    }
}

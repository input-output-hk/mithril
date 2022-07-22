use async_trait::async_trait;
use std::{error::Error, path::Path, sync::RwLock};

use super::{SnapshotLocation, SnapshotUploader};

pub struct DumbSnapshotUploader {
    last_uploaded: RwLock<Option<String>>,
}

impl DumbSnapshotUploader {
    pub fn new() -> Self {
        Self {
            last_uploaded: RwLock::new(None),
        }
    }

    pub fn get_last_upload(&self) -> Result<Option<String>, Box<dyn Error + Sync + Send>> {
        let value = self
            .last_uploaded
            .read()
            .map_err(|e| format!("Error while saving filepath location: {}", e))?;

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
    async fn upload_snapshot(&self, snapshot_filepath: &Path) -> Result<SnapshotLocation, String> {
        let mut value = self
            .last_uploaded
            .write()
            .map_err(|e| format!("Error while saving filepath location: {}", e))?;

        *value = Some(snapshot_filepath.to_string_lossy().to_string());

        Ok("http://whatev.er".into())
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
        let _res = uploader
            .upload_snapshot(Path::new("/tmp/whatever"))
            .await
            .expect("uploading with a dumb uploader should not fail");
        assert_eq!(
            Some("/tmp/whatever".to_string()),
            uploader
                .get_last_upload()
                .expect("getting dumb uploader last value after a fake download should not fail")
        );
    }
}

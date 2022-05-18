use crate::snapshot_stores::SnapshotStoreError;
use crate::SnapshotStore;

use async_trait::async_trait;
use chrono::{DateTime, Utc};
use mithril_common::entities::Snapshot;
use slog_scope::info;
use std::fs::File;
use std::io::{Seek, SeekFrom};

pub struct LocalSnapshotStore {
    current_snapshot: Option<Snapshot>,
    current_snapshot_file: Option<File>,
}

// @todo: Serve local file over http
impl LocalSnapshotStore {
    /// SnapshotStoreHTTPClient factory
    pub fn new() -> Self {
        Self {
            current_snapshot: None,
            current_snapshot_file: None,
        }
    }
}

#[async_trait]
impl SnapshotStore for LocalSnapshotStore {
    async fn list_snapshots(&self) -> Result<Vec<Snapshot>, String> {
        match &self.current_snapshot {
            Some(snapshot) => Ok(vec![snapshot.clone()]),
            None => Ok(vec![]),
        }
    }

    async fn get_snapshot_details(&self, digest: String) -> Result<Option<Snapshot>, String> {
        match &self.current_snapshot {
            Some(snapshot) => {
                if snapshot.digest == digest {
                    Ok(Some(snapshot.clone()))
                } else {
                    Ok(None)
                }
            }
            None => Ok(None),
        }
    }

    async fn upload_snapshot(
        &mut self,
        digest: String,
        mut snapshot_file: File,
    ) -> Result<(), SnapshotStoreError> {
        let timestamp: DateTime<Utc> = Utc::now();
        let created_at = format!("{:?}", timestamp);

        info!("snapshot hash: {}", digest);

        let size: u64 = snapshot_file
            .seek(SeekFrom::End(0))
            .map_err(|e| SnapshotStoreError::UploadFileError(e.to_string()))?;

        let location = format!("https://0.0.0.0/snapshot/{}/download", &digest);
        let snapshot = Snapshot {
            digest,
            certificate_hash: "".to_string(),
            size,
            created_at,
            locations: vec![location],
        };

        info!("snapshot: {}", serde_json::to_string(&snapshot).unwrap());

        self.current_snapshot = Some(snapshot);
        self.current_snapshot_file = Some(snapshot_file);

        Ok(())
    }
}

#[cfg(test)]
mod tests {}

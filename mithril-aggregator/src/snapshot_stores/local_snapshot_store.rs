use crate::snapshot_stores::SnapshotStoreError;
use crate::SnapshotStore;

use async_trait::async_trait;
use mithril_common::entities::Snapshot;
use slog_scope::info;

pub struct LocalSnapshotStore {
    current_snapshot: Option<Snapshot>,
}

// @todo: Serve local file over http
impl LocalSnapshotStore {
    /// SnapshotStoreHTTPClient factory
    pub fn new() -> Self {
        Self {
            current_snapshot: None,
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

    async fn add_snapshot(&mut self, snapshot: Snapshot) -> Result<(), SnapshotStoreError> {
        info!(
            "Adding snapshot: {}",
            serde_json::to_string(&snapshot).unwrap()
        );

        self.current_snapshot = Some(snapshot);

        Ok(())
    }
}

#[cfg(test)]
mod tests {}

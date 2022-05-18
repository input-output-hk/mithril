use crate::snapshot_stores::SnapshotStoreError;
use crate::SnapshotStore;

use async_trait::async_trait;
use mithril_common::entities::Snapshot;
use slog_scope::info;

pub struct LocalSnapshotStore {
    current_snapshot: Option<Snapshot>,
}

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
mod tests {
    use super::LocalSnapshotStore;
    use crate::SnapshotStore;
    use mithril_common::entities::Snapshot;

    #[tokio::test]
    async fn can_list_added_snapshot() {
        let snapshot = Snapshot {
            digest: "abc".to_string(),
            certificate_hash: "abc".to_string(),
            size: 0,
            created_at: "abc".to_string(),
            locations: vec!["abc".to_string()],
        };
        let mut store = LocalSnapshotStore::new();

        store
            .add_snapshot(snapshot.clone())
            .await
            .expect("can_list_added_snapshot add_snapshot error");
        assert_eq!(store.list_snapshots().await, Ok(vec![snapshot]));
    }

    #[tokio::test]
    async fn can_get_added_snapshot_details() {
        let snapshot = Snapshot {
            digest: "abc".to_string(),
            certificate_hash: "abc".to_string(),
            size: 0,
            created_at: "abc".to_string(),
            locations: vec!["abc".to_string()],
        };
        let mut store = LocalSnapshotStore::new();

        store
            .add_snapshot(snapshot.clone())
            .await
            .expect("can_get_added_snapshot_details add_snapshot error");
        assert_eq!(
            store.get_snapshot_details(snapshot.digest.clone()).await,
            Ok(Some(snapshot))
        );
    }
}

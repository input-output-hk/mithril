mod local_snapshot_store;
mod snapshot_store;

pub use local_snapshot_store::LocalSnapshotStore;
pub use snapshot_store::SnapshotStore;
pub use snapshot_store::SnapshotStoreError;

#[cfg(test)]
pub use snapshot_store::MockSnapshotStore;

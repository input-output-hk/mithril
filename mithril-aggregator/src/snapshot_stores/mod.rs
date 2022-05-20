mod local_snapshot_store;
mod remote_snapshot_store;
mod snapshot_store;

pub use local_snapshot_store::LocalSnapshotStore;
pub use remote_snapshot_store::RemoteSnapshotStore;
pub use snapshot_store::SnapshotStore;
pub use snapshot_store::SnapshotStoreError;

#[cfg(test)]
pub use snapshot_store::MockSnapshotStore;

mod gcp_snapshot_store;
mod snapshot_store;

pub use gcp_snapshot_store::GCPSnapshotStore;
pub use snapshot_store::SnapshotStore;

#[cfg(test)]
pub use snapshot_store::MockSnapshotStore;

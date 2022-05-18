mod http_snapshot_store;
mod snapshot_store;

pub use http_snapshot_store::SnapshotStoreHTTPClient;
pub use snapshot_store::SnapshotStore;

#[cfg(test)]
pub use snapshot_store::MockSnapshotStore;

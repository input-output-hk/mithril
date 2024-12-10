mod dumb_snapshot_uploader;
mod file_uploader;
mod local_snapshot_uploader;
mod remote_snapshot_uploader;

pub use dumb_snapshot_uploader::*;
pub use file_uploader::SnapshotLocation;
pub use file_uploader::SnapshotUploader;
pub use local_snapshot_uploader::LocalSnapshotUploader;
pub use remote_snapshot_uploader::RemoteSnapshotUploader;

#[cfg(test)]
pub use file_uploader::MockSnapshotUploader;

mod cloud_uploader;
mod dumb_uploader;
mod interface;
mod local_uploader;

pub use cloud_uploader::{CloudRemotePath, CloudUploader, GCloudBackendUploader};
pub use dumb_uploader::*;
#[cfg(test)]
pub use interface::MockFileUploader;
pub use interface::{FileUploadRetryPolicy, FileUploader};
pub use local_uploader::LocalUploader;

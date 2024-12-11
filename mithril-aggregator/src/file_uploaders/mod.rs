mod dumb_uploader;
mod gcp_uploader;
mod interface;
mod local_uploader;
mod remote_uploader;

pub use dumb_uploader::*;
pub use gcp_uploader::GcpUploader;
pub use interface::FileLocation;
pub use interface::FileUploader;
pub use local_uploader::LocalUploader;
pub use remote_uploader::RemoteUploader;

#[cfg(test)]
pub use interface::MockFileUploader;

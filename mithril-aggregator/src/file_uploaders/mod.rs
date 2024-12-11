mod dumb_uploader;
mod gcp_uploader;
mod interface;
mod local_uploader;

pub use dumb_uploader::*;
pub use gcp_uploader::GcpUploader;
pub use interface::FileUploader;
pub use interface::FileUri;
pub use local_uploader::LocalUploader;

#[cfg(test)]
pub use interface::MockFileUploader;

use std::fmt::Display;
use std::path::{Path, PathBuf};

use async_trait::async_trait;

use mithril_common::entities::FileUri;
use mithril_common::StdResult;

/// CloudRemotePath represents a cloud remote path
#[derive(Debug, Clone, PartialEq)]
pub struct CloudRemotePath(PathBuf);

impl CloudRemotePath {
    /// CloudRemotePath factory
    pub fn new(file_path: &str) -> Self {
        Self(PathBuf::from(file_path))
    }

    /// Join a file path to the current remote path
    pub fn join(&self, file_path: &str) -> Self {
        Self(self.0.join(file_path))
    }
}

impl Display for CloudRemotePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.to_string_lossy())
    }
}

impl From<&Path> for CloudRemotePath {
    fn from(path: &Path) -> Self {
        CloudRemotePath(path.to_path_buf())
    }
}

/// CloudBackendUploader represents a cloud backend uploader
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait CloudBackendUploader: Send + Sync {
    /// Check if a file exists in the cloud backend
    async fn file_exists(&self, remote_file_path: &CloudRemotePath) -> StdResult<Option<FileUri>>;

    /// Upload a file to the cloud backend
    async fn upload_file(
        &self,
        local_file_path: &Path,
        remote_file_path: &CloudRemotePath,
    ) -> StdResult<FileUri>;

    /// Make a file public in the cloud backend
    async fn make_file_public(&self, remote_file_path: &CloudRemotePath) -> StdResult<()>;
}

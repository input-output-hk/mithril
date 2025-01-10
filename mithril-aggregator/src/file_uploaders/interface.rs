use async_trait::async_trait;
use mithril_common::StdResult;
use std::path::Path;

/// FileUri represents a file URI used to identify the file's location
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub struct FileUri(pub String);

impl From<FileUri> for String {
    fn from(file_uri: FileUri) -> Self {
        file_uri.0
    }
}

/// FileUploader represents a file uploader interactor
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait FileUploader: Sync + Send {
    /// Upload a file
    async fn upload(&self, filepath: &Path) -> StdResult<FileUri>;
}

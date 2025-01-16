use async_trait::async_trait;
use mithril_common::{entities::FileUri, StdResult};
use std::path::Path;

/// FileUploader represents a file uploader interactor
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait FileUploader: Sync + Send {
    /// Upload a file
    async fn upload(&self, filepath: &Path) -> StdResult<FileUri>;
}

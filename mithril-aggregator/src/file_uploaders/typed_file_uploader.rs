use async_trait::async_trait;
use mithril_common::StdResult;
use std::path::Path;

use crate::file_uploaders::{FileUploader, FileUri};

pub struct TypedFileUploader<T: Sync + Send, U: FileUploader> {
    location_type: T,
    wrapped_uploader: U,
}

impl<T: Sync + Send, U: FileUploader> TypedFileUploader<T, U> {
    /// Create a new instance.
    pub fn new(location_type: T, wrapped_uploader: U) -> Self {
        Self {
            location_type,
            wrapped_uploader,
        }
    }

    pub fn get_location_type(&self) -> &T {
        &self.location_type
    }
}

#[async_trait]
impl<T: Sync + Send, U: FileUploader> FileUploader for TypedFileUploader<T, U> {
    /// Upload a file
    async fn upload(&self, filepath: &Path) -> StdResult<FileUri> {
        self.wrapped_uploader.upload(filepath).await
    }
}

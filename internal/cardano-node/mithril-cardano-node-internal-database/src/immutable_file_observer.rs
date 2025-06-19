use anyhow::{anyhow, Context};
use async_trait::async_trait;
use std::path::PathBuf;
use thiserror::Error;

use mithril_common::entities::ImmutableFileNumber;
use mithril_common::{StdError, StdResult};

use crate::entities::ImmutableFile;

/// Retrieve data on [ImmutableFile] from a cardano database.
#[async_trait]
pub trait ImmutableFileObserver
where
    Self: Sync + Send,
{
    /// Get the [ImmutableFileNumber] of the last immutable file in the cardano database.
    async fn get_last_immutable_number(&self) -> StdResult<ImmutableFileNumber>;
}

/// [ImmutableFileObserver] related errors.
#[derive(Error, Debug)]
pub enum ImmutableFileObserverError {
    /// Raised when the no immutables files were available.
    #[error("no immutable file was returned")]
    Missing(),

    /// Raised when [immutable file listing][ImmutableFile::list_completed_in_dir] fails.
    #[error("immutable file creation error")]
    ImmutableFileListing(#[source] StdError),
}

/// An [ImmutableFileObserver] using the filesystem.
pub struct ImmutableFileSystemObserver {
    db_path: PathBuf,
}

impl ImmutableFileSystemObserver {
    /// [ImmutableFileSystemObserver] factory.
    pub fn new(db_path: &PathBuf) -> Self {
        let db_path = db_path.to_owned();

        Self { db_path }
    }
}

#[async_trait]
impl ImmutableFileObserver for ImmutableFileSystemObserver {
    async fn get_last_immutable_number(&self) -> StdResult<u64> {
        let immutable_file_number = ImmutableFile::list_completed_in_dir(&self.db_path)
            .map_err(|e| anyhow!(e))
            .with_context(|| "Immutable File System Observer can not list all immutable files")?
            .into_iter()
            .next_back()
            .ok_or(anyhow!(ImmutableFileObserverError::Missing()))?
            .number;

        Ok(immutable_file_number)
    }
}

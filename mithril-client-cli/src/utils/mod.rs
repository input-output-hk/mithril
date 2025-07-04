//! Utilities module
//! This module contains tools needed for the commands layer.

mod archive_unpacker;
mod cardano_db;
mod cardano_db_download_checker;
mod expander;
mod feedback_receiver;
mod forced_era_fetcher;
mod fs;
mod github_release_retriever;
mod http_downloader;
mod multi_download_progress_reporter;
mod progress_reporter;

pub use archive_unpacker::*;
pub use cardano_db::*;
pub use cardano_db_download_checker::*;
pub use expander::*;
pub use feedback_receiver::*;
pub use forced_era_fetcher::*;
pub use fs::*;
pub use github_release_retriever::*;
pub use http_downloader::*;
pub use multi_download_progress_reporter::*;
pub use progress_reporter::*;

use anyhow::anyhow;
use mithril_client::MithrilResult;
use std::path::Path;

/// The key used to store the caution message when printing a JSON directly to stderr
pub(crate) const JSON_CAUTION_KEY: &str = "caution";

/// Converts a [Path] to a [String], returning an error if the path is not valid UTF-8.
pub(crate) fn path_to_string(path: &Path) -> MithrilResult<String> {
    let path = path
        .to_str()
        .ok_or_else(|| {
            anyhow!(
                "Path '{}' contains invalid UTF-8 characters.",
                path.display()
            )
        })?
        .to_string();

    Ok(path)
}

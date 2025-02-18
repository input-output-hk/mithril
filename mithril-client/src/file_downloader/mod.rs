//! File downloader module.
//!
//! This module provides the necessary abstractions to download files from different sources.

mod http;
mod interface;
#[cfg(test)]
mod mock_builder;
mod retry;

pub use http::HttpFileDownloader;
#[cfg(test)]
pub use interface::MockFileDownloader;
pub use interface::{DownloadEvent, FileDownloader, FileDownloaderUri};
#[cfg(test)]
pub use mock_builder::MockFileDownloaderBuilder;
pub use retry::{FileDownloadRetryPolicy, RetryDownloader};

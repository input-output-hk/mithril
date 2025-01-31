//! File downloader module.
//!
//! This module provides the necessary abstractions to download files from different sources.

mod interface;

#[cfg(test)]
pub use interface::MockFileDownloader;
pub use interface::{FileDownloader, FileDownloaderUri};

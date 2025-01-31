//! File downloader module.
//!
//! This module provides the necessary abstractions to download files from different sources.

mod http;
mod interface;
mod resolver;

pub use http::HttpFileDownloader;
#[cfg(test)]
pub use interface::MockFileDownloader;
pub use interface::{FileDownloader, FileDownloaderUri};
#[cfg(test)]
pub use resolver::MockFileDownloaderResolver;
pub use resolver::{FileDownloaderResolver, ImmutablesFileDownloaderResolver};

//! File downloader module.
//!
//! This module provides the necessary abstractions to download files from different sources.

mod http;
mod interface;
#[cfg(test)]
mod mock_builder;
mod resolver;
mod retry;

pub use http::HttpFileDownloader;
#[cfg(test)]
pub use interface::MockFileDownloader;
pub use interface::{FeedbackEventBuilder, FileDownloader, FileDownloaderUri};
#[cfg(test)]
pub use mock_builder::MockFileDownloaderBuilder;
#[cfg(test)]
pub use resolver::MockFileDownloaderResolver;
pub use resolver::{
    AncillaryFileDownloaderResolver, DigestFileDownloaderResolver, FileDownloaderResolver,
    ImmutablesFileDownloaderResolver,
};
pub use retry::{FileDownloadRetryPolicy, RetryDownloader};

//! File downloader module.
//!
//! This module provides the necessary abstractions to download files from different sources.

mod interface;
mod resolver;
mod retry;

#[cfg(test)]
pub use interface::MockFileDownloader;
pub use interface::{FeedbackEventBuilder, FileDownloader, FileDownloaderUri};
#[cfg(test)]
pub use resolver::MockFileDownloaderResolver;
pub use resolver::{
    AncillaryFileDownloaderResolver, DigestFileDownloaderResolver, FileDownloaderResolver,
    ImmutablesFileDownloaderResolver,
};
pub use retry::{FileDownloadRetryPolicy, RetryDownloader};

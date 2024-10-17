//! Snapshot tarball download and unpack mechanism.
//!
//! The [SnapshotDownloader] trait abstracts how to download and unpack snapshots
//! tarballs.
//!
//! Snapshots locations can be of various kinds, right now we only support HTTP
//! download (using the [HttpSnapshotDownloader]) but other types may be added in
//! the future.

use anyhow::{anyhow, Context};
use async_trait::async_trait;
use futures::StreamExt;
use reqwest::Url;
use reqwest::{Response, StatusCode};
use slog::{debug, Logger};
use std::fs;
use std::path::Path;
use tokio::fs::File;
use tokio::io::AsyncReadExt;

use mithril_common::logging::LoggerExtensions;

use crate::common::CompressionAlgorithm;
use crate::feedback::{FeedbackSender, MithrilEvent};
use crate::utils::SnapshotUnpacker;
use crate::MithrilResult;

#[cfg(test)]
use mockall::automock;

/// API that defines a snapshot downloader
#[async_trait]
pub trait SnapshotDownloader: Sync + Send {
    /// Download and unpack a snapshot archive on the disk.
    ///
    /// The `download_id` is a unique identifier that allow
    /// [feedback receivers][crate::feedback::FeedbackReceiver] to track concurrent downloads.
    ///
    /// Warning: this can be a quite long operation depending on the snapshot size.
    async fn download_unpack(
        &self,
        location: &str,
        target_dir: &Path,
        compression_algorithm: CompressionAlgorithm,
        download_id: &str,
        snapshot_size: u64,
    ) -> MithrilResult<()>;

    /// Test if the given snapshot location exists.
    async fn probe(&self, location: &str) -> MithrilResult<()>;
}

/// A snapshot downloader that only handles download through HTTP.
pub struct HttpSnapshotDownloader {
    http_client: reqwest::Client,
    feedback_sender: FeedbackSender,
    logger: Logger,
}

impl HttpSnapshotDownloader {
    /// Constructs a new `HttpSnapshotDownloader`.
    pub fn new(feedback_sender: FeedbackSender, logger: Logger) -> MithrilResult<Self> {
        let http_client = reqwest::ClientBuilder::new()
            .build()
            .with_context(|| "Building http client for HttpSnapshotDownloader failed")?;

        Ok(Self {
            http_client,
            feedback_sender,
            logger: logger.new_with_component_name::<Self>(),
        })
    }

    async fn get(&self, location: &str) -> MithrilResult<Response> {
        debug!(self.logger, "GET Snapshot location='{location}'.");
        let request_builder = self.http_client.get(location);
        let response = request_builder.send().await.with_context(|| {
            format!("Cannot perform a GET for the snapshot (location='{location}')")
        })?;

        match response.status() {
            StatusCode::OK => Ok(response),
            StatusCode::NOT_FOUND => Err(anyhow!("Location='{location} not found")),
            status_code => Err(anyhow!("Unhandled error {status_code}")),
        }
    }

    fn file_scheme_to_local_path(file_url: &str) -> Option<String> {
        Url::parse(file_url)
            .ok()
            .filter(|url| url.scheme() == "file")
            .and_then(|url| url.to_file_path().ok())
            .map(|path| path.to_string_lossy().into_owned())
    }

    async fn download_local_file<F, Fut>(
        &self,
        local_path: &str,
        sender: &flume::Sender<Vec<u8>>,
        report_progress: F,
    ) -> MithrilResult<()>
    where
        F: Fn(u64) -> Fut,
        Fut: std::future::Future<Output = ()>,
    {
        // Stream the `location` directly from the local filesystem
        let mut downloaded_bytes: u64 = 0;
        let mut file = File::open(local_path).await?;

        loop {
            // We can either allocate here each time, or clone a shared buffer into sender.
            // A larger read buffer is faster, less context switches:
            let mut buffer = vec![0; 16 * 1024 * 1024];
            let bytes_read = file.read(&mut buffer).await?;
            if bytes_read == 0 {
                break;
            }
            buffer.truncate(bytes_read);
            sender.send_async(buffer).await.with_context(|| {
                format!(
                    "Local file read: could not write {} bytes to stream.",
                    bytes_read
                )
            })?;
            downloaded_bytes += bytes_read as u64;
            report_progress(downloaded_bytes).await
        }
        Ok(())
    }

    async fn download_remote_file<F, Fut>(
        &self,
        location: &str,
        sender: &flume::Sender<Vec<u8>>,
        report_progress: F,
    ) -> MithrilResult<()>
    where
        F: Fn(u64) -> Fut,
        Fut: std::future::Future<Output = ()>,
    {
        let mut downloaded_bytes: u64 = 0;
        let mut remote_stream = self.get(location).await?.bytes_stream();
        while let Some(item) = remote_stream.next().await {
            let chunk = item.with_context(|| "Download: Could not read from byte stream")?;

            sender.send_async(chunk.to_vec()).await.with_context(|| {
                format!("Download: could not write {} bytes to stream.", chunk.len())
            })?;

            downloaded_bytes += chunk.len() as u64;
            report_progress(downloaded_bytes).await
        }
        Ok(())
    }
}

#[cfg_attr(test, automock)]
#[async_trait]
impl SnapshotDownloader for HttpSnapshotDownloader {
    async fn download_unpack(
        &self,
        location: &str,
        target_dir: &Path,
        compression_algorithm: CompressionAlgorithm,
        download_id: &str,
        snapshot_size: u64,
    ) -> MithrilResult<()> {
        if !target_dir.is_dir() {
            Err(
                anyhow!("target path is not a directory or does not exist: `{target_dir:?}`")
                    .context("Download-Unpack: prerequisite error"),
            )?;
        }
        let (sender, receiver) = flume::bounded(5);

        let dest_dir = target_dir.to_path_buf();
        let unpack_thread = tokio::task::spawn_blocking(move || -> MithrilResult<()> {
            let unpacker = SnapshotUnpacker;
            unpacker.unpack_snapshot(receiver, compression_algorithm, &dest_dir)
        });

        let report_progress = |downloaded_bytes: u64| async move {
            self.feedback_sender
                .send_event(MithrilEvent::SnapshotDownloadProgress {
                    download_id: download_id.to_owned(),
                    downloaded_bytes,
                    size: snapshot_size,
                })
                .await
        };

        if let Some(local_path) = Self::file_scheme_to_local_path(location) {
            self.download_local_file(&local_path, &sender, report_progress)
                .await?;
        } else {
            self.download_remote_file(location, &sender, report_progress)
                .await?;
        }

        drop(sender); // Signal EOF
        unpack_thread
            .await
            .with_context(|| {
                format!(
                    "Unpack: panic while unpacking to dir '{}'",
                    target_dir.display()
                )
            })?
            .with_context(|| {
                format!("Unpack: could not unpack to dir '{}'", target_dir.display())
            })?;

        Ok(())
    }

    async fn probe(&self, location: &str) -> MithrilResult<()> {
        debug!(self.logger, "HEAD Snapshot location='{location}'.");

        if let Some(local_path) = Self::file_scheme_to_local_path(location) {
            fs::metadata(local_path)
                .with_context(|| format!("Local snapshot location='{location}' not found"))
                .map(drop)
        } else {
            let request_builder = self.http_client.head(location);
            let response = request_builder.send().await.with_context(|| {
                format!("Cannot perform a HEAD for snapshot at location='{location}'")
            })?;

            match response.status() {
                StatusCode::OK => Ok(()),
                StatusCode::NOT_FOUND => Err(anyhow!("Snapshot location='{location} not found")),
                status_code => Err(anyhow!("Unhandled error {status_code}")),
            }
        }
    }
}

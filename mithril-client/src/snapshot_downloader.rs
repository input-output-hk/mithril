use anyhow::{anyhow, Context};
use async_trait::async_trait;
use futures::StreamExt;
use reqwest::{Response, StatusCode};
use slog::{debug, Logger};
use std::path::Path;

#[cfg(test)]
use mockall::automock;

use crate::common::CompressionAlgorithm;
use crate::utils::SnapshotUnpacker;
use crate::MithrilResult;

/// API that defines a snapshot downloader
#[async_trait]
pub trait SnapshotDownloader: Sync + Send {
    /// Download and unpack a snapshot archives on the disk.
    ///
    /// Warning: this can be a quite long operation depending on the snapshot size.
    async fn download_unpack(
        &self,
        location: &str,
        target_dir: &Path,
        compression_algorithm: CompressionAlgorithm,
    ) -> MithrilResult<()>;

    /// Test if the given snapshot location exist.
    async fn probe(&self, location: &str) -> MithrilResult<()>;
}

/// A snapshot downloader that only handle download through HTTP.
pub struct HttpSnapshotDownloader {
    http_client: reqwest::Client,
    logger: Logger,
}

impl HttpSnapshotDownloader {
    /// HttpSnapshotDownloader factory
    pub fn new(logger: Logger) -> MithrilResult<Self> {
        let http_client = reqwest::ClientBuilder::new()
            .build()
            .with_context(|| "Building http client for HttpSnapshotDownloader failed")?;

        Ok(Self {
            http_client,
            logger,
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
}

#[cfg_attr(test, automock)]
#[async_trait]
impl SnapshotDownloader for HttpSnapshotDownloader {
    async fn download_unpack(
        &self,
        location: &str,
        target_dir: &Path,
        compression_algorithm: CompressionAlgorithm,
    ) -> MithrilResult<()> {
        if !target_dir.is_dir() {
            Err(
                anyhow!("target path is not a directory or does not exist: `{target_dir:?}`")
                    .context("Download-Unpack: prerequisite error"),
            )?;
        }

        let mut downloaded_bytes: u64 = 0;
        let mut remote_stream = self.get(location).await?.bytes_stream();
        let (sender, receiver) = flume::bounded(5);

        let dest_dir = target_dir.to_path_buf();
        let unpack_thread = tokio::task::spawn_blocking(move || -> MithrilResult<()> {
            let unpacker = SnapshotUnpacker;
            unpacker.unpack_snapshot(receiver, compression_algorithm, &dest_dir)
        });

        while let Some(item) = remote_stream.next().await {
            let chunk = item.with_context(|| "Download: Could not read from byte stream")?;

            sender.send_async(chunk.to_vec()).await.with_context(|| {
                format!("Download: could not write {} bytes to stream.", chunk.len())
            })?;

            downloaded_bytes += chunk.len() as u64;
            // todo: report download progress here
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

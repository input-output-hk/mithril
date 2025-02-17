use std::{
    io::{BufReader, Read, Write},
    path::Path,
};

use anyhow::{anyhow, Context};
use async_trait::async_trait;
use flate2::read::GzDecoder;
use flume::{Receiver, Sender};
use futures::StreamExt;
use reqwest::{Response, StatusCode, Url};
use slog::{debug, Logger};
use tar::Archive;
use tokio::fs::File;
use tokio::io::AsyncReadExt;

use mithril_common::{logging::LoggerExtensions, StdResult};

use crate::common::CompressionAlgorithm;
use crate::feedback::FeedbackSender;
use crate::utils::StreamReader;

use super::{interface::DownloadEvent, FileDownloader, FileDownloaderUri};

/// A file downloader that only handles download through HTTP.
pub struct HttpFileDownloader {
    http_client: reqwest::Client,
    feedback_sender: FeedbackSender,
    logger: Logger,
}

impl HttpFileDownloader {
    /// Constructs a new `HttpFileDownloader`.
    pub fn new(feedback_sender: FeedbackSender, logger: Logger) -> StdResult<Self> {
        let http_client = reqwest::ClientBuilder::new()
            .build()
            .with_context(|| "Building http client for HttpFileDownloader failed")?;

        Ok(Self {
            http_client,
            feedback_sender,
            logger: logger.new_with_component_name::<Self>(),
        })
    }

    async fn get(&self, location: &str) -> StdResult<Response> {
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

    /// Stream the `location` directly from the local filesystem
    async fn download_local_file(
        &self,
        local_path: &str,
        sender: &Sender<Vec<u8>>,
        download_event_type: DownloadEvent,
        file_size: u64,
    ) -> StdResult<()> {
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
            let event =
                download_event_type.build_download_progress_event(downloaded_bytes, file_size);
            self.feedback_sender.send_event(event).await;
        }

        Ok(())
    }

    /// Stream the `location` remotely
    async fn download_remote_file(
        &self,
        location: &str,
        sender: &Sender<Vec<u8>>,
        download_event_type: DownloadEvent,
        file_size: u64,
    ) -> StdResult<()> {
        let mut downloaded_bytes: u64 = 0;
        let mut remote_stream = self.get(location).await?.bytes_stream();

        while let Some(item) = remote_stream.next().await {
            let chunk = item.with_context(|| "Download: Could not read from byte stream")?;
            sender.send_async(chunk.to_vec()).await.with_context(|| {
                format!("Download: could not write {} bytes to stream.", chunk.len())
            })?;
            downloaded_bytes += chunk.len() as u64;
            let event =
                download_event_type.build_download_progress_event(downloaded_bytes, file_size);
            self.feedback_sender.send_event(event).await;
        }

        Ok(())
    }

    fn unpack_file(
        stream: Receiver<Vec<u8>>,
        compression_algorithm: Option<CompressionAlgorithm>,
        unpack_dir: &Path,
        download_id: String,
    ) -> StdResult<()> {
        let input = StreamReader::new(stream);
        match compression_algorithm {
            Some(CompressionAlgorithm::Gzip) => {
                let gzip_decoder = GzDecoder::new(input);
                let mut file_archive = Archive::new(gzip_decoder);
                file_archive.unpack(unpack_dir).with_context(|| {
                    format!(
                        "Could not unpack with 'Gzip' from streamed data to directory '{}'",
                        unpack_dir.display()
                    )
                })?;
            }
            Some(CompressionAlgorithm::Zstandard) => {
                let zstandard_decoder = zstd::Decoder::new(input)
                    .with_context(|| "Unpack failed: Create Zstandard decoder error")?;
                let mut file_archive = Archive::new(zstandard_decoder);
                file_archive.unpack(unpack_dir).with_context(|| {
                    format!(
                        "Could not unpack with 'Zstd' from streamed data to directory '{}'",
                        unpack_dir.display()
                    )
                })?;
            }
            None => {
                let file_path = unpack_dir.join(download_id);
                if file_path.exists() {
                    std::fs::remove_file(file_path.clone())?;
                }
                let mut file = std::fs::File::create(file_path)?;
                let input_buffered = BufReader::new(input);
                for byte in input_buffered.bytes() {
                    file.write_all(&[byte?])?;
                }
                file.flush()?;
            }
        };

        Ok(())
    }
}

#[async_trait]
impl FileDownloader for HttpFileDownloader {
    async fn download_unpack(
        &self,
        location: &FileDownloaderUri,
        target_dir: &Path,
        compression_algorithm: Option<CompressionAlgorithm>,
        download_event_type: DownloadEvent,
    ) -> StdResult<()> {
        if !target_dir.is_dir() {
            Err(
                anyhow!("target path is not a directory or does not exist: `{target_dir:?}`")
                    .context("Download-Unpack: prerequisite error"),
            )?;
        }

        let (sender, receiver) = flume::bounded(32);
        let dest_dir = target_dir.to_path_buf();
        let download_id = download_event_type.download_id().to_owned();
        let unpack_thread = tokio::task::spawn_blocking(move || -> StdResult<()> {
            Self::unpack_file(receiver, compression_algorithm, &dest_dir, download_id)
        });
        // The size will be completed with the uncompressed file size when available in the location
        // (see https://github.com/input-output-hk/mithril/issues/2291)
        let file_size = 0;
        if let Some(local_path) = Self::file_scheme_to_local_path(location.as_str()) {
            self.download_local_file(&local_path, &sender, download_event_type, file_size)
                .await?;
        } else {
            self.download_remote_file(location.as_str(), &sender, download_event_type, file_size)
                .await?;
        }
        drop(sender);
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
}

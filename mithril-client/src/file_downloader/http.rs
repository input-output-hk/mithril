use std::path::Path;

use anyhow::anyhow;
use async_trait::async_trait;

use mithril_common::{entities::CompressionAlgorithm, StdResult};

use crate::snapshot_downloader::{HttpSnapshotDownloader, SnapshotDownloader};

use super::{FileDownloader, FileDownloaderUri};

/// A file downloader that uses HTTP to download files.
pub struct HttpFileDownloader(HttpSnapshotDownloader);

#[async_trait]
impl FileDownloader for HttpFileDownloader {
    async fn download_unpack(
        &self,
        location: &FileDownloaderUri,
        target_dir: &Path,
        compression_algorithm: Option<CompressionAlgorithm>,
        download_id: &str,
    ) -> StdResult<()> {
        // TODO: temporary implementation to be refactored
        self.0
            .download_unpack(
                location.as_str(),
                target_dir,
                compression_algorithm.ok_or(anyhow!("No compression algorithm provided"))?,
                download_id,
                0,
            )
            .await
    }
}

use std::{path::Path, sync::Arc};

use async_trait::async_trait;

use mithril_common::{entities::CompressionAlgorithm, StdResult};

use crate::snapshot_downloader::{HttpSnapshotDownloader, SnapshotDownloader};

use super::{FeedbackEventBuilder, FileDownloader, FileDownloaderUri};

/// A file downloader that uses HTTP to download files.
pub struct HttpFileDownloader(pub(crate) Arc<HttpSnapshotDownloader>);

#[async_trait]
impl FileDownloader for HttpFileDownloader {
    async fn download_unpack(
        &self,
        location: &FileDownloaderUri,
        target_dir: &Path,
        compression_algorithm: Option<CompressionAlgorithm>,
        download_id: &str,
        _feedback_event: FeedbackEventBuilder,
    ) -> StdResult<()> {
        // TODO: temporary implementation to be refactored
        if let Some(compression_algorithm) = compression_algorithm {
            self.0
                .download_unpack(
                    location.as_str(),
                    target_dir,
                    compression_algorithm,
                    download_id,
                    0,
                )
                .await
        } else {
            use std::io::Cursor;
            let response = reqwest::get(location.as_str()).await?;
            let mut file = std::fs::File::create(target_dir.join("temp.txt"))?;
            let mut content = Cursor::new(response.bytes().await?);
            std::io::copy(&mut content, &mut file)?;

            Ok(())
        }
    }
}

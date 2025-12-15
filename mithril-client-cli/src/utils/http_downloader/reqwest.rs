use std::{
    fs::File,
    io::Write,
    path::{Path, PathBuf},
};

use anyhow::Context;
use async_trait::async_trait;
use mithril_client::MithrilResult;
use reqwest::{Client, Url};

use super::HttpDownloader;

/// [ReqwestHttpDownloader] is an implementation of the [HttpDownloader].
pub struct ReqwestHttpDownloader {
    client: Client,
}

impl ReqwestHttpDownloader {
    /// Creates a new instance of [ReqwestHttpDownloader].
    pub fn new() -> MithrilResult<Self> {
        let client = Client::builder()
            .build()
            .with_context(|| "Failed to build Reqwest HTTP client")?;

        Ok(Self { client })
    }
}

#[async_trait]
impl HttpDownloader for ReqwestHttpDownloader {
    async fn download_file(
        &self,
        url: Url,
        download_dir: &Path,
        filename: &str,
    ) -> MithrilResult<PathBuf> {
        let response = self
            .client
            .get(url.clone())
            .send()
            .await
            .with_context(|| format!("Failed to download file from URL: {url}"))?;

        let bytes = response.bytes().await?;
        let download_filepath = download_dir.join(filename);
        let mut file = File::create(&download_filepath)?;
        file.write_all(&bytes)?;

        Ok(download_filepath)
    }
}

use async_trait::async_trait;
use futures::StreamExt;
use log::debug;
use reqwest::{self, StatusCode};
use std::env;
use std::fs;
use std::io::Write;
use std::path;
use std::sync::Arc;

use crate::entities::*;

#[cfg(test)]
use mockall::automock;

/// AggregatorHandler represents a read interactor with an aggregator
#[cfg_attr(test, automock)]
#[async_trait]
pub trait AggregatorHandler {
    /// List snapshots
    async fn list_snapshots(&self) -> Result<Vec<Snapshot>, String>;

    /// Get snapshot details
    async fn get_snapshot_details(&self, digest: String) -> Result<Snapshot, String>;

    /// Download snapshot
    async fn download_snapshot(&self, digest: String, location: String) -> Result<String, String>;
}

/// AggregatorHTTPClient is a http client for an aggregator
pub struct AggregatorHTTPClient {
    config: Arc<Config>,
}

impl AggregatorHTTPClient {
    /// AggregatorHTTPClient factory
    pub fn new(config: Arc<Config>) -> Self {
        debug!("New AggregatorHTTPClient created");
        Self { config }
    }
}

#[async_trait]
impl AggregatorHandler for AggregatorHTTPClient {
    /// List snapshots
    async fn list_snapshots(&self) -> Result<Vec<Snapshot>, String> {
        debug!("List snapshots");

        let config = self.config.clone();
        let url = format!("{}/snapshots", config.aggregator_endpoint);
        let response = reqwest::get(url.clone()).await;
        match response {
            Ok(response) => match response.status() {
                StatusCode::OK => match response.json::<Vec<Snapshot>>().await {
                    Ok(snapshots) => Ok(snapshots),
                    Err(err) => Err(err.to_string()),
                },
                status_error => Err(format!("error {} received", status_error)),
            },
            Err(err) => Err(err.to_string()),
        }
    }

    /// Get snapshot details
    async fn get_snapshot_details(&self, digest: String) -> Result<Snapshot, String> {
        debug!("Details snapshot {}", digest);

        let config = self.config.clone();
        let url = format!("{}/snapshot/{}", config.aggregator_endpoint, digest);
        let response = reqwest::get(url.clone()).await;
        match response {
            Ok(response) => match response.status() {
                StatusCode::OK => match response.json::<Snapshot>().await {
                    Ok(snapshot) => Ok(snapshot),
                    Err(err) => Err(err.to_string()),
                },
                StatusCode::NOT_FOUND => Err("Snapshot not found".to_string()),
                status_error => Err(format!("error {} received", status_error)),
            },
            Err(err) => Err(err.to_string()),
        }
    }

    /// Download Snapshot
    async fn download_snapshot(&self, digest: String, location: String) -> Result<String, String> {
        debug!("Download snapshot {} from {}", digest, location);
        let remote_url = location.clone();
        let response = reqwest::get(remote_url.clone()).await;
        match response {
            Ok(response) => match response.status() {
                StatusCode::OK => {
                    let local_path = env::current_dir()
                        .map_err(|e| format!("current dir not available: {}", e))?
                        .join(path::Path::new(&format!(
                            "data/{}/{}/snapshot.archive",
                            self.config.network, digest
                        )));
                    fs::create_dir_all(&local_path.parent().unwrap())
                        .map_err(|e| format!("can't create snapshot dir: {}", e))?;
                    let mut local_file = fs::File::create(&local_path)
                        .map_err(|e| format!("can't access snapshot file: {}", e))?;
                    let bytes_total = response
                        .content_length()
                        .ok_or_else(|| "can't get content length".to_string())?;
                    let mut bytes_downloaded = 0;
                    let mut remote_stream = response.bytes_stream();
                    while let Some(item) = remote_stream.next().await {
                        let chunk = item.map_err(|e| format!("download failed: {}", e))?;
                        local_file
                            .write_all(&chunk)
                            .map_err(|e| format!("can't write to snapshot file: {}", e))?;
                        bytes_downloaded += chunk.len() as u64;
                        debug!(
                            "Downloaded {}% - {} Bytes",
                            100 * bytes_downloaded / bytes_total,
                            bytes_downloaded
                        );
                    }
                    Ok(local_path.into_os_string().into_string().unwrap())
                }
                StatusCode::NOT_FOUND => Err("snapshot archive not found".to_string()),
                status_error => Err(format!("error {} received", status_error)),
            },
            Err(err) => Err(err.to_string()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use httpmock::prelude::*;
    use serde_json::json;
    use std::io::Read;

    use mithril_aggregator::fake_data;

    fn setup_test() -> (MockServer, Arc<Config>) {
        let server = MockServer::start();
        let config = Arc::new(Config {
            network: "testnet".to_string(),
            aggregator_endpoint: server.url(""),
        });
        (server, config)
    }

    #[tokio::test]
    async fn test_list_snapshots_ok() {
        let (server, config) = setup_test();
        let snapshots_expected = fake_data::snapshots(5);
        let _snapshots_mock = server.mock(|when, then| {
            when.path("/snapshots");
            then.status(200).body(json!(snapshots_expected).to_string());
        });
        let aggregator_client = AggregatorHTTPClient::new(config.clone());
        let snapshots = aggregator_client.list_snapshots().await;
        snapshots.as_ref().expect("unexpected error");
        assert_eq!(snapshots.unwrap(), snapshots_expected);
    }

    #[tokio::test]
    async fn test_list_snapshots_ko_500() {
        let (server, config) = setup_test();
        let _snapshots_mock = server.mock(|when, then| {
            when.path("/snapshots");
            then.status(500);
        });
        let aggregator_client = AggregatorHTTPClient::new(config.clone());
        let snapshots = aggregator_client.list_snapshots().await;
        assert!(snapshots.is_err());
    }

    #[tokio::test]
    async fn test_list_snapshots_ko_unreachable() {
        let config = Arc::new(Config {
            network: "testnet".to_string(),
            aggregator_endpoint: "http://unreachable".to_string(),
        });
        let aggregator_client = AggregatorHTTPClient::new(config.clone());
        let snapshots = aggregator_client.list_snapshots().await;
        assert!(snapshots.is_err());
    }

    #[tokio::test]
    async fn get_snapshot_details_ok() {
        let digest = "digest123".to_string();
        let (server, config) = setup_test();
        let snapshot_expected = fake_data::snapshots(1).first().unwrap().to_owned();
        let _snapshots_mock = server.mock(|when, then| {
            when.path(format!("/snapshot/{}", digest));
            then.status(200).body(json!(snapshot_expected).to_string());
        });
        let aggregator_client = AggregatorHTTPClient::new(config.clone());
        let snapshot = aggregator_client.get_snapshot_details(digest).await;
        snapshot.as_ref().expect("unexpected error");
        assert_eq!(snapshot.unwrap(), snapshot_expected);
    }

    #[tokio::test]
    async fn get_snapshot_details_ko_404() {
        let digest = "digest123".to_string();
        let (server, config) = setup_test();
        let _snapshots_mock = server.mock(|when, then| {
            when.path(format!("/snapshot/{}", digest));
            then.status(404);
        });
        let aggregator_client = AggregatorHTTPClient::new(config.clone());
        let snapshot = aggregator_client.get_snapshot_details(digest).await;
        assert!(snapshot.is_err());
    }

    #[tokio::test]
    async fn get_snapshot_details_ko_500() {
        let digest = "digest123".to_string();
        let (server, config) = setup_test();
        let _snapshots_mock = server.mock(|when, then| {
            when.path(format!("/snapshot/{}", digest));
            then.status(500);
        });
        let aggregator_client = AggregatorHTTPClient::new(config.clone());
        let snapshot = aggregator_client.get_snapshot_details(digest).await;
        assert!(snapshot.is_err());
    }

    #[tokio::test]
    async fn get_snapshot_details_ko_unreachable() {
        let digest = "digest123".to_string();
        let config = Arc::new(Config {
            network: "testnet".to_string(),
            aggregator_endpoint: "http123://unreachable".to_string(),
        });
        let aggregator_client = AggregatorHTTPClient::new(config.clone());
        let snapshot = aggregator_client.get_snapshot_details(digest).await;
        assert!(snapshot.is_err());
    }

    #[tokio::test]
    async fn get_download_snapshot_ok() {
        let digest = "digest123".to_string();
        let url_path = "/download";
        let (server, config) = setup_test();
        let data_expected = "1234567890".repeat(1024).to_string();
        let _download_mock = server.mock(|when, then| {
            when.path(url_path.to_string());
            then.status(200).body(&data_expected);
        });
        let aggregator_client = AggregatorHTTPClient::new(config.clone());
        let location = server.url(url_path);
        let local_file_path = aggregator_client.download_snapshot(digest, location).await;
        local_file_path.as_ref().expect("unexpected error");
        let local_file_path = local_file_path.unwrap();
        let mut local_file = fs::File::open(&local_file_path).unwrap();
        let mut data_downloaded = "".to_string();
        local_file.read_to_string(&mut data_downloaded).unwrap();
        assert_eq!(data_downloaded, data_expected);
        fs::remove_file(&local_file_path).unwrap();
        fs::remove_dir_all(path::Path::new(&local_file_path).parent().unwrap()).unwrap();
    }

    #[tokio::test]
    async fn get_download_snapshot_ko_unreachable() {
        let digest = "digest123".to_string();
        let url_path = "/download";
        let (server, config) = setup_test();
        let _snapshots_mock = server.mock(|when, then| {
            when.path(url_path.to_string());
            then.status(500);
        });
        let aggregator_client = AggregatorHTTPClient::new(config.clone());
        let location = server.url(url_path);
        let local_file_path = aggregator_client.download_snapshot(digest, location).await;
        assert!(local_file_path.is_err());
    }

    #[tokio::test]
    async fn get_download_snapshot_ko_500() {
        let digest = "digest123".to_string();
        let (_, config) = setup_test();
        let aggregator_client = AggregatorHTTPClient::new(config.clone());
        let location = "http123://unreachable".to_string();
        let local_file_path = aggregator_client.download_snapshot(digest, location).await;
        assert!(local_file_path.is_err());
    }
}

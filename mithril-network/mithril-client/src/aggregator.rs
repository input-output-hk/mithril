use async_trait::async_trait;
use flate2::read::GzDecoder;
use futures::StreamExt;
use log::debug;
use reqwest::{self, StatusCode};
use std::env;
use std::fs;
use std::io::{self, Write};
use std::path;
use tar::Archive;

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

    /// Unpack snapshot
    async fn unpack_snapshot(&self, digest: String) -> Result<String, String>;
}

/// AggregatorHTTPClient is a http client for an aggregator
pub struct AggregatorHTTPClient {
    network: String,
    aggregator_endpoint: String,
}

impl AggregatorHTTPClient {
    /// AggregatorHTTPClient factory
    pub fn new(network: String, aggregator_endpoint: String) -> Self {
        debug!("New AggregatorHTTPClient created");
        Self {
            network,
            aggregator_endpoint,
        }
    }
}

#[async_trait]
impl AggregatorHandler for AggregatorHTTPClient {
    /// List snapshots
    async fn list_snapshots(&self) -> Result<Vec<Snapshot>, String> {
        debug!("List snapshots");

        let url = format!("{}/snapshots", self.aggregator_endpoint);
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

        let url = format!("{}/snapshot/{}", self.aggregator_endpoint, digest);
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
                    let local_path = archive_file_path(digest, self.network.clone())?;
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
                        print!(
                            "Downloaded {}% - {}/{} Bytes\r",
                            100 * bytes_downloaded / bytes_total,
                            bytes_downloaded,
                            bytes_total
                        );
                        io::stdout().flush().expect("Could not flush stdout");
                    }
                    Ok(local_path.into_os_string().into_string().unwrap())
                }
                StatusCode::NOT_FOUND => Err("snapshot archive not found".to_string()),
                status_error => Err(format!("error {} received", status_error)),
            },
            Err(err) => Err(err.to_string()),
        }
    }

    /// Unpack snapshot
    async fn unpack_snapshot(&self, digest: String) -> Result<String, String> {
        debug!("Unpack snapshot {}", digest);
        println!("Unpacking snapshot...");
        let local_path = archive_file_path(digest, self.network.clone())?;
        let snapshot_file_tar_gz = fs::File::open(local_path.clone())
            .map_err(|e| format!("can't open snapshot file: {}", e))?;
        let snapshot_file_tar = GzDecoder::new(snapshot_file_tar_gz);
        let unpack_dir_path = local_path.parent().unwrap().join(path::Path::new("db"));
        let mut snapshot_archive = Archive::new(snapshot_file_tar);
        snapshot_archive
            .unpack(&unpack_dir_path)
            .map_err(|e| format!("can't unpack snapshot archive: {}", e))?;
        Ok(unpack_dir_path.into_os_string().into_string().unwrap())
    }
}

/// Computes local archive filepath
fn archive_file_path(digest: String, network: String) -> Result<path::PathBuf, String> {
    Ok(env::current_dir()
        .map_err(|e| format!("current dir not available: {}", e))?
        .join(path::Path::new(&format!(
            "data/{}/{}/snapshot.archive.tar.gz",
            network, digest
        ))))
}

#[cfg(test)]
mod tests {
    use super::*;
    use flate2::write::GzEncoder;
    use flate2::Compression;
    use httpmock::prelude::*;
    use serde_json::json;
    use std::io::Read;

    use mithril_aggregator::fake_data;

    fn setup_test() -> (MockServer, Config) {
        let server = MockServer::start();
        let config = Config {
            network: "testnet".to_string(),
            aggregator_endpoint: server.url(""),
        };
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
        let aggregator_client =
            AggregatorHTTPClient::new(config.network, config.aggregator_endpoint);
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
        let aggregator_client =
            AggregatorHTTPClient::new(config.network, config.aggregator_endpoint);
        let snapshots = aggregator_client.list_snapshots().await;
        assert!(snapshots.is_err());
    }

    #[tokio::test]
    async fn test_list_snapshots_ko_unreachable() {
        let aggregator_client =
            AggregatorHTTPClient::new("testnet".to_string(), "http://unreachable".to_string());
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
        let aggregator_client =
            AggregatorHTTPClient::new(config.network, config.aggregator_endpoint);
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
        let aggregator_client =
            AggregatorHTTPClient::new(config.network, config.aggregator_endpoint);
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
        let aggregator_client =
            AggregatorHTTPClient::new(config.network, config.aggregator_endpoint);
        let snapshot = aggregator_client.get_snapshot_details(digest).await;
        assert!(snapshot.is_err());
    }

    #[tokio::test]
    async fn get_snapshot_details_ko_unreachable() {
        let digest = "digest123".to_string();
        let aggregator_client =
            AggregatorHTTPClient::new("testnet".to_string(), "http123://unreachable".to_string());
        let snapshot = aggregator_client.get_snapshot_details(digest).await;
        assert!(snapshot.is_err());
    }

    #[tokio::test]
    async fn get_download_snapshot_ok() {
        let digest = "digest123".to_string();
        let url_path = "/download";
        let (server, config) = setup_test();
        let data_expected = "1234567890".repeat(1024).to_string();
        server.mock(|when, then| {
            when.path(url_path.to_string());
            then.status(200).body(&data_expected);
        });
        let aggregator_client =
            AggregatorHTTPClient::new(config.network, config.aggregator_endpoint);
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
        let aggregator_client =
            AggregatorHTTPClient::new(config.network, config.aggregator_endpoint);
        let location = server.url(url_path);
        let local_file_path = aggregator_client.download_snapshot(digest, location).await;
        assert!(local_file_path.is_err());
    }

    #[tokio::test]
    async fn get_download_snapshot_ko_500() {
        let digest = "digest123".to_string();
        let (_, config) = setup_test();
        let aggregator_client =
            AggregatorHTTPClient::new(config.network, config.aggregator_endpoint);
        let location = "http123://unreachable".to_string();
        let local_file_path = aggregator_client.download_snapshot(digest, location).await;
        assert!(local_file_path.is_err());
    }

    #[tokio::test]
    async fn unpack_snapshot_ok() {
        let network = "testnet".to_string();
        let digest = "digest123".to_string();
        let (_, config) = setup_test();
        let data_expected = "1234567890".repeat(1024).to_string();
        let data_file_name = "data.txt";
        let archive_file_path = archive_file_path(digest.clone(), network).unwrap();
        let source_directory_name = "src";
        let source_file_path = archive_file_path
            .parent()
            .unwrap()
            .join(path::Path::new(source_directory_name))
            .join(path::Path::new(data_file_name));
        fs::create_dir_all(&source_file_path.parent().unwrap()).unwrap();
        let mut source_file = fs::File::create(&source_file_path).unwrap();
        write!(source_file, "{}", data_expected).unwrap();
        let archive_file = fs::File::create(&archive_file_path).unwrap();
        let archive_encoder = GzEncoder::new(&archive_file, Compression::default());
        let mut archive_builder = tar::Builder::new(archive_encoder);
        archive_builder
            .append_dir_all(".", &source_file_path.parent().unwrap())
            .unwrap();

        archive_builder.into_inner().unwrap().finish().unwrap();
        let aggregator_client =
            AggregatorHTTPClient::new(config.network, config.aggregator_endpoint);
        let local_dir_path = aggregator_client.unpack_snapshot(digest.clone()).await;
        local_dir_path.expect("unexpected error");
    }

    #[tokio::test]
    async fn unpack_snapshot_ko_noarchive() {
        let digest = "digest123".to_string();
        let (_, config) = setup_test();
        let aggregator_client =
            AggregatorHTTPClient::new(config.network, config.aggregator_endpoint);
        let local_dir_path = aggregator_client.unpack_snapshot(digest).await;
        assert!(local_dir_path.is_err());
    }
}

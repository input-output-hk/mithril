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
use thiserror::Error;

use crate::entities::*;

#[cfg(test)]
use mockall::automock;

#[derive(Error, Debug)]
pub enum AggregatorHandlerError {
    #[error("remote server technical error: '{0}'")]
    RemoteServerTechnical(String),
    #[error("remote server logical error: '{0}'")]
    RemoteServerLogical(String),
    #[error("remote server unreachable: '{0}'")]
    RemoteServerUnreachable(String),
    #[error("json parsing failed: '{0}'")]
    JsonParseFailed(String),
    #[error("io error:")]
    IOError(#[from] io::Error),
}

/// AggregatorHandler represents a read interactor with an aggregator
#[cfg_attr(test, automock)]
#[async_trait]
pub trait AggregatorHandler {
    /// List snapshots
    async fn list_snapshots(&self) -> Result<Vec<Snapshot>, AggregatorHandlerError>;

    /// Get snapshot details
    async fn get_snapshot_details(&self, digest: &str) -> Result<Snapshot, AggregatorHandlerError>;

    /// Download snapshot
    async fn download_snapshot(
        &self,
        digest: &str,
        location: &str,
    ) -> Result<String, AggregatorHandlerError>;

    /// Unpack snapshot
    async fn unpack_snapshot(&self, digest: &str) -> Result<String, AggregatorHandlerError>;

    /// Get certificate details
    async fn get_certificate_details(
        &self,
        certificate_hash: &str,
    ) -> Result<Certificate, AggregatorHandlerError>;
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
    async fn list_snapshots(&self) -> Result<Vec<Snapshot>, AggregatorHandlerError> {
        debug!("List snapshots");
        let url = format!("{}/snapshots", self.aggregator_endpoint);
        let response = reqwest::get(url.clone()).await;
        match response {
            Ok(response) => match response.status() {
                StatusCode::OK => match response.json::<Vec<Snapshot>>().await {
                    Ok(snapshots) => Ok(snapshots),
                    Err(err) => Err(AggregatorHandlerError::JsonParseFailed(err.to_string())),
                },
                status_error => Err(AggregatorHandlerError::RemoteServerTechnical(
                    status_error.to_string(),
                )),
            },
            Err(err) => Err(AggregatorHandlerError::RemoteServerUnreachable(
                err.to_string(),
            )),
        }
    }

    /// Get snapshot details
    async fn get_snapshot_details(&self, digest: &str) -> Result<Snapshot, AggregatorHandlerError> {
        debug!("Details snapshot {}", digest);
        let url = format!("{}/snapshot/{}", self.aggregator_endpoint, digest);
        let response = reqwest::get(url.clone()).await;
        match response {
            Ok(response) => match response.status() {
                StatusCode::OK => match response.json::<Snapshot>().await {
                    Ok(snapshot) => Ok(snapshot),
                    Err(err) => Err(AggregatorHandlerError::JsonParseFailed(err.to_string())),
                },
                StatusCode::NOT_FOUND => Err(AggregatorHandlerError::RemoteServerLogical(
                    "snapshot not found".to_string(),
                )),
                status_error => Err(AggregatorHandlerError::RemoteServerTechnical(
                    status_error.to_string(),
                )),
            },
            Err(err) => Err(AggregatorHandlerError::RemoteServerUnreachable(
                err.to_string(),
            )),
        }
    }

    /// Download Snapshot
    async fn download_snapshot(
        &self,
        digest: &str,
        location: &str,
    ) -> Result<String, AggregatorHandlerError> {
        debug!("Download snapshot {} from {}", digest, location);
        let response = reqwest::get(location).await;
        match response {
            Ok(response) => match response.status() {
                StatusCode::OK => {
                    let local_path = archive_file_path(digest, &self.network)?;
                    fs::create_dir_all(&local_path.parent().unwrap())?;
                    let mut local_file = fs::File::create(&local_path)?;
                    let bytes_total = response.content_length().ok_or_else(|| {
                        AggregatorHandlerError::RemoteServerTechnical(
                            "can't get content length".to_string(),
                        )
                    })?;
                    let mut bytes_downloaded = 0;
                    let mut remote_stream = response.bytes_stream();
                    while let Some(item) = remote_stream.next().await {
                        let chunk = item.map_err(|e| {
                            AggregatorHandlerError::RemoteServerTechnical(e.to_string())
                        })?;
                        local_file.write_all(&chunk)?;
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
                StatusCode::NOT_FOUND => Err(AggregatorHandlerError::RemoteServerLogical(
                    "snapshot archive not found".to_string(),
                )),
                status_error => Err(AggregatorHandlerError::RemoteServerTechnical(
                    status_error.to_string(),
                )),
            },
            Err(err) => Err(AggregatorHandlerError::RemoteServerUnreachable(
                err.to_string(),
            )),
        }
    }

    /// Unpack snapshot
    async fn unpack_snapshot(&self, digest: &str) -> Result<String, AggregatorHandlerError> {
        debug!("Unpack snapshot {}", digest);
        println!("Unpacking snapshot...");
        let local_path = archive_file_path(digest, &self.network)?;
        let snapshot_file_tar_gz = fs::File::open(local_path.clone())?;
        let snapshot_file_tar = GzDecoder::new(snapshot_file_tar_gz);
        let unpack_dir_path = local_path.parent().unwrap().join(path::Path::new("db"));
        let mut snapshot_archive = Archive::new(snapshot_file_tar);
        snapshot_archive.unpack(&unpack_dir_path)?;
        Ok(unpack_dir_path.into_os_string().into_string().unwrap())
    }

    /// Get certificate details
    async fn get_certificate_details(
        &self,
        certificate_hash: &str,
    ) -> Result<Certificate, AggregatorHandlerError> {
        debug!("Details certificate {}", certificate_hash);
        let url = format!(
            "{}/certificate/{}",
            self.aggregator_endpoint, certificate_hash
        );
        let response = reqwest::get(url.clone()).await;
        match response {
            Ok(response) => match response.status() {
                StatusCode::OK => match response.json::<Certificate>().await {
                    Ok(certificate) => Ok(certificate),
                    Err(err) => Err(AggregatorHandlerError::JsonParseFailed(err.to_string())),
                },
                StatusCode::NOT_FOUND => Err(AggregatorHandlerError::RemoteServerLogical(
                    "certificate not found".to_string(),
                )),
                status_error => Err(AggregatorHandlerError::RemoteServerTechnical(
                    status_error.to_string(),
                )),
            },
            Err(err) => Err(AggregatorHandlerError::RemoteServerUnreachable(
                err.to_string(),
            )),
        }
    }
}

/// Computes local archive filepath
fn archive_file_path(digest: &str, network: &str) -> Result<path::PathBuf, AggregatorHandlerError> {
    Ok(env::current_dir()?.join(path::Path::new(&format!(
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

    use mithril_common::fake_data;

    fn setup_test() -> (MockServer, Config) {
        let server = MockServer::start();
        let config = Config {
            network: "testnet".to_string(),
            aggregator_endpoint: server.url(""),
        };
        (server, config)
    }

    /// see [`archive_file_path`] to see the path that will be removed if it exist
    fn ensure_snapshot_dir_does_not_exist(digest: &str, network: &str) {
        let archive_file_path = archive_file_path(digest, network).unwrap();
        let archive_folder_path = archive_file_path.parent().unwrap();
        if archive_folder_path.exists() {
            fs::remove_dir_all(archive_folder_path).unwrap();
        }
    }

    /// see [`archive_file_path`] to see where the dummy will be created
    fn build_dummy_snapshot(digest: &str, network: &str, data_expected: &str) {
        let data_file_name = "data.txt";
        let archive_file_path = archive_file_path(digest, network).unwrap();
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
        let digest = "digest123";
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
        let digest = "digest123";
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
        let digest = "digest123";
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
        let digest = "digest123";
        let aggregator_client =
            AggregatorHTTPClient::new("testnet".to_string(), "http123://unreachable".to_string());
        let snapshot = aggregator_client.get_snapshot_details(digest).await;
        assert!(snapshot.is_err());
    }

    #[tokio::test]
    async fn get_download_snapshot_ok() {
        let digest = "digest_get_download_snapshot_ok";
        let url_path = "/download";
        let (server, config) = setup_test();
        let data_expected = "1234567890".repeat(1024).to_string();
        server.mock(|when, then| {
            when.path(url_path.to_string());
            then.status(200).body(&data_expected);
        });
        ensure_snapshot_dir_does_not_exist(digest, &config.network);

        let aggregator_client =
            AggregatorHTTPClient::new(config.network, config.aggregator_endpoint);
        let location = server.url(url_path);
        let local_file_path = aggregator_client.download_snapshot(digest, &location).await;
        local_file_path.as_ref().expect("unexpected error");
        let data_downloaded = fs::read_to_string(&local_file_path.unwrap()).unwrap();

        assert_eq!(data_downloaded, data_expected);
    }

    #[tokio::test]
    async fn get_download_snapshot_ko_unreachable() {
        let digest = "digest123";
        let url_path = "/download";
        let (server, config) = setup_test();
        let _snapshots_mock = server.mock(|when, then| {
            when.path(url_path.to_string());
            then.status(500);
        });
        let aggregator_client =
            AggregatorHTTPClient::new(config.network, config.aggregator_endpoint);
        let location = server.url(url_path);
        let local_file_path = aggregator_client.download_snapshot(digest, &location).await;
        assert!(local_file_path.is_err());
    }

    #[tokio::test]
    async fn get_download_snapshot_ko_500() {
        let digest = "digest_get_download_snapshot_ko_500";
        let (_, config) = setup_test();
        let aggregator_client =
            AggregatorHTTPClient::new(config.network, config.aggregator_endpoint);
        let location = "http123://unreachable".to_string();
        let local_file_path = aggregator_client.download_snapshot(digest, &location).await;
        assert!(local_file_path.is_err());
    }

    #[tokio::test]
    async fn unpack_snapshot_ok() {
        let digest = "digest_unpack_snapshot_ok";
        let (_, config) = setup_test();
        let data_expected = "1234567890".repeat(1024);

        ensure_snapshot_dir_does_not_exist(digest, &config.network);
        build_dummy_snapshot(digest, &config.network, &data_expected);

        let aggregator_client =
            AggregatorHTTPClient::new(config.network, config.aggregator_endpoint);
        let local_dir_path = aggregator_client.unpack_snapshot(digest).await;
        local_dir_path.expect("unexpected error");
    }

    #[tokio::test]
    async fn unpack_snapshot_ko_noarchive() {
        let digest = "digest_unpack_snapshot_ko_noarchive";
        let (_, config) = setup_test();
        ensure_snapshot_dir_does_not_exist(digest, &config.network);

        let aggregator_client =
            AggregatorHTTPClient::new(config.network, config.aggregator_endpoint);
        let local_dir_path = aggregator_client.unpack_snapshot(digest).await;
        assert!(local_dir_path.is_err());
    }

    #[tokio::test]
    async fn get_certificate_details_ok() {
        let certificate_hash = "certificate-hash-123";
        let (server, config) = setup_test();
        let certificate_expected = fake_data::certificate(certificate_hash.to_string());
        let _certificate_mock = server.mock(|when, then| {
            when.path(format!("/certificate/{}", certificate_hash));
            then.status(200)
                .body(json!(certificate_expected).to_string());
        });
        let aggregator_client =
            AggregatorHTTPClient::new(config.network, config.aggregator_endpoint);
        let certificate = aggregator_client
            .get_certificate_details(certificate_hash)
            .await;
        certificate.as_ref().expect("unexpected error");
        assert_eq!(certificate.unwrap(), certificate_expected);
    }

    #[tokio::test]
    async fn get_certificate_details_ko_404() {
        let certificate_hash = "certificate-hash-123";
        let (server, config) = setup_test();
        let _certificate_mock = server.mock(|when, then| {
            when.path(format!("/certificate/{}", certificate_hash));
            then.status(404);
        });
        let aggregator_client =
            AggregatorHTTPClient::new(config.network, config.aggregator_endpoint);
        let certificate = aggregator_client
            .get_certificate_details(certificate_hash)
            .await;
        assert!(certificate.is_err());
    }

    #[tokio::test]
    async fn get_certificate_details_ko_500() {
        let certificate_hash = "certificate-hash-123";
        let (server, config) = setup_test();
        let _certificate_mock = server.mock(|when, then| {
            when.path(format!("/certificate/{}", certificate_hash));
            then.status(500);
        });
        let aggregator_client =
            AggregatorHTTPClient::new(config.network, config.aggregator_endpoint);
        let certificate = aggregator_client
            .get_certificate_details(certificate_hash)
            .await;
        assert!(certificate.is_err());
    }

    #[tokio::test]
    async fn get_certificate_details_ko_unreachable() {
        let certificate_hash = "certificate-hash-123";
        let aggregator_client =
            AggregatorHTTPClient::new("testnet".to_string(), "http123://unreachable".to_string());
        let certificate = aggregator_client
            .get_certificate_details(certificate_hash)
            .await;
        assert!(certificate.is_err());
    }
}

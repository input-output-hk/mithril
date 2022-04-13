use async_trait::async_trait;
use log::debug;
use reqwest::{self, StatusCode};
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

    /// Download Snapshot
    async fn download_snapshot(&self, digest: String) -> Result<(), String>;
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
                StatusCode::NOT_FOUND => Err(format!("Snapshot not found")),
                status_error => Err(format!("error {} received", status_error)),
            },
            Err(err) => Err(err.to_string()),
        }
    }

    /// Download Snapshot
    async fn download_snapshot(&self, _digest: String) -> Result<(), String> {
        unimplemented!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use httpmock::prelude::*;
    use serde_json::json;

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
            aggregator_endpoint: "http://unreachable".to_string(),
        });
        let aggregator_client = AggregatorHTTPClient::new(config.clone());
        let snapshot = aggregator_client.get_snapshot_details(digest).await;
        assert!(snapshot.is_err());
    }
}

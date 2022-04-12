use async_trait::async_trait;
use log::debug;
use reqwest::{self, StatusCode};
use std::sync::Arc;

use crate::entities::*;
use mithril_aggregator::entities::Snapshot;

#[cfg(test)]
use mockall::automock;

/// AggregatorHandler represents a read interactor with a snapshot
#[cfg_attr(test, automock)]
#[async_trait]
pub trait AggregatorHandler {
    /// List snapshots
    async fn list_snapshots(&self, config: Arc<Config>) -> Result<Vec<SnapshotListItem>, String>;

    /// Download Snapshot
    async fn download_snapshot(&self, config: Arc<Config>, digest: String) -> Result<(), String>;
}

/// AggregatorHandlerFake is a fake AggregatorHandler
pub struct AggregatorHandlerFake {}

impl AggregatorHandlerFake {
    /// AggregatorHandlerFake factory
    #[allow(dead_code)]
    pub fn new() -> Self {
        debug!("New AggregatorHandlerFake created");
        Self {}
    }
}

#[async_trait]
impl AggregatorHandler for AggregatorHandlerFake {
    /// List snapshots
    async fn list_snapshots(&self, _config: Arc<Config>) -> Result<Vec<SnapshotListItem>, String> {
        debug!("List snapshots");
        let snapshots = (0..9)
            .into_iter()
            .map(|snapshot_id| {
                let snapshot_id = snapshot_id + 1;
                SnapshotListItem::new(
                    "testnet".to_string(),
                    format!("{}", snapshot_id).repeat(35),
                    false,
                    snapshot_id * 100000000000,
                    snapshot_id as u16,
                    "2022-07-21T17:32:28Z".to_string(),
                )
            })
            .collect();
        Ok(snapshots)
    }

    /// Download Snapshot
    async fn download_snapshot(&self, _config: Arc<Config>, digest: String) -> Result<(), String> {
        debug!("Download snapshot {}", digest);
        Ok(())
    }
}

/// AggregatorHTTPClient is a http client for an aggregator
pub struct AggregatorHTTPClient {}

impl AggregatorHTTPClient {
    /// AggregatorHTTPClient factory
    pub fn new() -> Self {
        debug!("New AggregatorHTTPClient created");
        Self {}
    }
}

#[async_trait]
impl AggregatorHandler for AggregatorHTTPClient {
    /// List snapshots
    async fn list_snapshots(&self, config: Arc<Config>) -> Result<Vec<SnapshotListItem>, String> {
        debug!("List snapshots");

        let url = format!("{}/snapshots", config.aggregator_endpoint);
        println!("URL={:?}", url);
        let response = reqwest::get(url.clone()).await;
        match response {
            Ok(response) => match response.status() {
                StatusCode::OK => match response.json::<Vec<Snapshot>>().await {
                    Ok(snapshots) => {
                        let snapshot_items = snapshots
                            .iter()
                            .map(|snapshot| {
                                let downloaded = false;
                                SnapshotListItem::new(
                                    config.network.clone(),
                                    snapshot.digest.clone(),
                                    downloaded,
                                    snapshot.size,
                                    snapshot.locations.len() as u16,
                                    snapshot.created_at.clone(),
                                )
                            })
                            .collect::<Vec<SnapshotListItem>>();
                        Ok(snapshot_items)
                    }
                    Err(err) => Err(err.to_string()),
                },
                StatusCode::NOT_FOUND => Err("404 received".to_string()),
                status_error => Err(format!("error {} received", status_error)),
            },
            Err(err) => Err(err.to_string()),
        }
    }

    /// Download Snapshot
    async fn download_snapshot(&self, _config: Arc<Config>, digest: String) -> Result<(), String> {
        debug!("Download snapshot {}", digest);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use httpmock::prelude::*;
    use mithril_aggregator::fake_data;
    use serde_json::json;

    #[tokio::test]
    async fn test_list_snapshots_ok() {
        let server = MockServer::start();

        let fake_snapshots = fake_data::snapshots(5);

        let _snapshots_mock = server.mock(|when, then| {
            when.path("/snapshots");
            then.status(200).body(json!(fake_snapshots).to_string());
        });

        let config = Config {
            network: "testnet".to_string(),
            aggregator_endpoint: server.url(""),
        };

        let aggregator_client = AggregatorHTTPClient::new();
        let snapshots = aggregator_client.list_snapshots(Arc::new(config)).await;
        snapshots.as_ref().expect("unexpected error");
        assert_eq!(snapshots.unwrap().len(), fake_snapshots.clone().len());
    }

    #[tokio::test]
    async fn test_list_snapshots_error() {
        let server = MockServer::start();

        let _snapshots_mock = server.mock(|when, then| {
            when.path("/snapshots");
            then.status(500);
        });

        let config = Config {
            network: "testnet".to_string(),
            aggregator_endpoint: server.url(""),
        };

        let aggregator_client = AggregatorHTTPClient::new();
        let snapshots = aggregator_client.list_snapshots(Arc::new(config)).await;
        assert!(snapshots.is_err());
    }
}

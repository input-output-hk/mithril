use log::debug;
use std::sync::Arc;

use crate::aggregator::AggregatorHandler;
use crate::entities::*;
use crate::errors;

/// Mithril client wrapper
pub struct Client<R>
where
    R: AggregatorHandler,
{
    pub config: Arc<Config>,
    pub aggregator_handler: Option<Box<R>>,
}

impl<R> Client<R>
where
    R: AggregatorHandler,
{
    // Client factory
    pub fn new(config: Arc<Config>) -> Self {
        Self {
            config: config,
            aggregator_handler: None,
        }
    }

    // With AggregatorHandler
    pub fn with_aggregator_handler(&mut self, aggregator_handler: R) -> &mut Self {
        self.aggregator_handler = Some(Box::new(aggregator_handler));
        self
    }
}

impl<R> Client<R>
where
    R: AggregatorHandler,
{
    /// List snapshots
    pub async fn list_snapshots(&self) -> Result<Vec<SnapshotListItem>, String> {
        debug!("List snapshots");
        if let Some(aggregator_handler) = &self.aggregator_handler {
            aggregator_handler.list_snapshots(self.config.clone()).await
        } else {
            Err(errors::MISSING_AGGREGATOR_HANDLER.to_string())
        }
    }

    /// Download a snapshot by digest
    pub async fn download_snapshot(&self, digest: String) -> Result<(), String> {
        debug!("Download snapshot {}", digest);
        if let Some(aggregator_handler) = &self.aggregator_handler {
            aggregator_handler
                .download_snapshot(self.config.clone(), digest.clone())
                .await
        } else {
            Err(errors::MISSING_AGGREGATOR_HANDLER.to_string())
        }
    }

    /// Restore a snapshot by hash
    pub async fn restore_snapshot(&self, hash: String) -> Result<(), String> {
        debug!("Restore snapshot {}", hash);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::aggregator::MockAggregatorHandler;

    fn get_fake_snapshots() -> Vec<SnapshotListItem> {
        (0..5)
            .into_iter()
            .map(|snapshot_id| {
                SnapshotListItem::new(
                    "testnet".to_string(),
                    format!("{}", snapshot_id).repeat(35),
                    false,
                    snapshot_id * 100000000000,
                    snapshot_id as u16,
                    "2022-07-21T17:32:28Z".to_string(),
                )
            })
            .collect()
    }

    #[tokio::test]
    async fn test_list_snapshots_ok() {
        let config = Config {
            network: "testnet".to_string(),
            aggregator_endpoint: "http://endpoint".to_string(),
        };
        let fake_snapshots = get_fake_snapshots();
        let mut mock_aggregator_handler = MockAggregatorHandler::new();
        mock_aggregator_handler
            .expect_list_snapshots()
            .return_const(Ok(fake_snapshots.clone()))
            .once();
        let mut client = Client::new(Arc::new(config));
        client.with_aggregator_handler(mock_aggregator_handler);
        let snapshots = client.list_snapshots().await;
        snapshots.as_ref().expect("unexpected error");
        assert_eq!(snapshots.unwrap(), fake_snapshots.clone());
    }

    #[tokio::test]
    async fn test_list_snapshots_ko() {
        let config = Config {
            network: "testnet".to_string(),
            aggregator_endpoint: "http://endpoint".to_string(),
        };
        let mut mock_aggregator_handler = MockAggregatorHandler::new();
        mock_aggregator_handler
            .expect_list_snapshots()
            .return_const(Err("error occurred".to_string()))
            .once();
        let mut client = Client::new(Arc::new(config));
        client.with_aggregator_handler(mock_aggregator_handler);
        let snapshots = client.list_snapshots().await;
        assert!(snapshots.is_err(), "an error should have occurred");
    }
}

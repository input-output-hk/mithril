// TODO: To delete once it is not needed anymore
use async_trait::async_trait;
use log::debug;
use std::sync::Arc;

use crate::aggregator::AggregatorHandler;
use crate::entities::*;
use mithril_aggregator::fake_data;

/// AggregatorHandlerFake is a fake AggregatorHandler
pub struct AggregatorHandlerFake {}

impl AggregatorHandlerFake {
    /// AggregatorHandlerFake factory
    #[allow(dead_code)]
    pub fn new(_config: Arc<Config>) -> Self {
        debug!("New AggregatorHandlerFake created");
        Self {}
    }
}

#[async_trait]
impl AggregatorHandler for AggregatorHandlerFake {
    /// List snapshots
    async fn list_snapshots(&self) -> Result<Vec<Snapshot>, String> {
        let snapshots = fake_data::snapshots(5);
        Ok(snapshots)
    }

    /// Get snapshot details
    async fn get_snapshot_details(&self, _digest: String) -> Result<Snapshot, String> {
        let snapshot = fake_data::snapshots(1).first().unwrap().to_owned();
        Ok(snapshot)
    }

    /// Download Snapshot
    async fn download_snapshot(&self, digest: String, location: String) -> Result<String, String> {
        unimplemented!("Download snapshot {} at {}", digest, location);
    }

    /// Unarchive snapshot
    async fn unarchive_snapshot(&self, digest: String) -> Result<String, String> {
        unimplemented!("Unarchive snapshot {}", digest);
    }
}

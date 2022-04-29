use log::debug;

use crate::aggregator::AggregatorHandler;
use crate::entities::*;
use crate::errors;

/// Mithril client wrapper
pub struct Client<R>
where
    R: AggregatorHandler,
{
    pub network: String,
    pub aggregator_handler: Option<Box<R>>,
}

impl<R> Client<R>
where
    R: AggregatorHandler,
{
    /// Client factory
    pub fn new(network: String) -> Self {
        Self {
            network,
            aggregator_handler: None,
        }
    }

    /// With AggregatorHandler
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
        match &self.aggregator_handler {
            Some(aggregator_handler) => match aggregator_handler.list_snapshots().await {
                Ok(snapshots) => Ok(snapshots
                    .iter()
                    .map(|snapshot| convert_to_list_item(snapshot, self.network.clone()))
                    .collect::<Vec<SnapshotListItem>>()),
                Err(err) => Err(err),
            },
            None => Err(errors::MISSING_AGGREGATOR_HANDLER.to_string()),
        }
    }

    /// Show a snapshot
    pub async fn show_snapshot(&self, digest: &str) -> Result<Vec<SnapshotFieldItem>, String> {
        debug!("Show snapshot {}", digest);
        match &self.aggregator_handler {
            Some(aggregator_handler) => match aggregator_handler.get_snapshot_details(digest).await
            {
                Ok(snapshot) => Ok(convert_to_field_items(&snapshot, self.network.clone())),
                Err(err) => Err(err),
            },
            None => Err(errors::MISSING_AGGREGATOR_HANDLER.to_string()),
        }
    }

    /// Download a snapshot by digest
    pub async fn download_snapshot(
        &self,
        digest: &str,
        location_index: isize,
    ) -> Result<(String, String), String> {
        debug!("Download snapshot {}", digest);
        match &self.aggregator_handler {
            Some(aggregator_handler) => match aggregator_handler.get_snapshot_details(digest).await
            {
                Ok(snapshot) => {
                    let from = snapshot
                        .locations
                        .get((location_index - 1) as usize)
                        .unwrap()
                        .to_owned();
                    match aggregator_handler.download_snapshot(digest, &from).await {
                        Ok(to) => Ok((from, to)),
                        Err(err) => Err(err),
                    }
                }
                Err(err) => Err(err),
            },
            None => Err(errors::MISSING_AGGREGATOR_HANDLER.to_string()),
        }
    }

    /// Restore a snapshot by hash
    pub async fn restore_snapshot(&self, digest: &str) -> Result<String, String> {
        debug!("Restore snapshot {}", digest);
        match &self.aggregator_handler {
            Some(aggregator_handler) => match aggregator_handler.unpack_snapshot(digest).await {
                Ok(to) => Ok(to),
                Err(err) => Err(err),
            },
            None => Err(errors::MISSING_AGGREGATOR_HANDLER.to_string()),
        }
    }
}

/// Convert Snapshot to SnapshotListItem routine
pub(crate) fn convert_to_list_item(snapshot: &Snapshot, network: String) -> SnapshotListItem {
    SnapshotListItem::new(
        network,
        snapshot.digest.clone(),
        snapshot.size,
        snapshot.locations.len() as u16,
        snapshot.created_at.clone(),
    )
}

/// Convert Snapshot to SnapshotFieldItems routine
pub(crate) fn convert_to_field_items(
    snapshot: &Snapshot,
    network: String,
) -> Vec<SnapshotFieldItem> {
    let mut field_items = vec![
        SnapshotFieldItem::new("Network".to_string(), network),
        SnapshotFieldItem::new("Digest".to_string(), snapshot.digest.to_string()),
        SnapshotFieldItem::new("Size".to_string(), format!("{}", snapshot.size)),
    ];
    for (idx, location) in snapshot.locations.iter().enumerate() {
        field_items.push(SnapshotFieldItem::new(
            format!("Location {}", idx + 1),
            location.to_string(),
        ));
    }
    field_items.push(SnapshotFieldItem::new(
        "Created".to_string(),
        snapshot.created_at.to_string(),
    ));
    field_items
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::aggregator::MockAggregatorHandler;
    use mithril_aggregator::fake_data;

    #[tokio::test]
    async fn test_list_snapshots_ok() {
        let network = "testnet".to_string();
        let fake_snapshots = fake_data::snapshots(5);
        let mut mock_aggregator_handler = MockAggregatorHandler::new();
        mock_aggregator_handler
            .expect_list_snapshots()
            .return_const(Ok(fake_snapshots.clone()))
            .once();
        let mut client = Client::new(network.clone());
        client.with_aggregator_handler(mock_aggregator_handler);
        let snapshot_list_items = client.list_snapshots().await;
        snapshot_list_items.as_ref().expect("unexpected error");
        let snapshot_list_items_expected = fake_snapshots
            .iter()
            .map(|snapshot| convert_to_list_item(snapshot, network.clone()))
            .collect::<Vec<SnapshotListItem>>();
        assert_eq!(snapshot_list_items.unwrap(), snapshot_list_items_expected);
    }

    #[tokio::test]
    async fn test_list_snapshots_ko() {
        let mut mock_aggregator_handler = MockAggregatorHandler::new();
        mock_aggregator_handler
            .expect_list_snapshots()
            .return_const(Err("error occurred".to_string()))
            .once();
        let mut client = Client::new("testnet".to_string());
        client.with_aggregator_handler(mock_aggregator_handler);
        let snapshot_list_items = client.list_snapshots().await;
        assert!(
            snapshot_list_items.is_err(),
            "an error should have occurred"
        );
    }

    #[tokio::test]
    async fn test_show_snapshot_ok() {
        let digest = "digest123";
        let fake_snapshot = fake_data::snapshots(1).first().unwrap().to_owned();
        let mut mock_aggregator_handler = MockAggregatorHandler::new();
        mock_aggregator_handler
            .expect_get_snapshot_details()
            .return_const(Ok(fake_snapshot.clone()))
            .once();
        let mut client = Client::new("testnet".to_string());
        client.with_aggregator_handler(mock_aggregator_handler);
        let snapshot_item = client.show_snapshot(digest).await;
        snapshot_item.as_ref().expect("unexpected error");
        let snapshot_item_expected = convert_to_field_items(&fake_snapshot, "testnet".to_string());
        assert_eq!(snapshot_item.unwrap(), snapshot_item_expected);
    }

    #[tokio::test]
    async fn test_show_snapshot_ko() {
        let digest = "digest123";
        let mut mock_aggregator_handler = MockAggregatorHandler::new();
        mock_aggregator_handler
            .expect_get_snapshot_details()
            .return_const(Err("error occurred".to_string()))
            .once();
        let mut client = Client::new("testnet".to_string());
        client.with_aggregator_handler(mock_aggregator_handler);
        let snapshot_item = client.show_snapshot(digest).await;
        assert!(snapshot_item.is_err(), "an error should have occurred");
    }
}

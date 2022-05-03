use log::debug;
use mithril_aggregator::fake_data;
use std::str;

use crate::aggregator::AggregatorHandler;
use crate::entities::*;
use crate::verifier::Verifier;

pub const MISSING_AGGREGATOR_HANDLER: &str = "missing aggregator handler";
pub const MISSING_VERIFIER: &str = "missing verifier";

/// AggregatorHandlerWrapper wraps an AggregatorHandler
pub type AggregatorHandlerWrapper = Box<dyn AggregatorHandler>;

/// VerifierWrapper wraps a Verifier
pub type VerifierWrapper = Box<dyn Verifier>;

/// Mithril client wrapper
pub struct Client {
    pub network: String,
    pub aggregator_handler: Option<AggregatorHandlerWrapper>,
    pub verifier: Option<VerifierWrapper>,
}

impl Client {
    /// Client factory
    pub fn new(network: String) -> Self {
        Self {
            network,
            aggregator_handler: None,
            verifier: None,
        }
    }

    /// With AggregatorHandler
    pub fn with_aggregator_handler(
        &mut self,
        aggregator_handler: AggregatorHandlerWrapper,
    ) -> &mut Self {
        self.aggregator_handler = Some(aggregator_handler);
        self
    }

    /// With Verifier
    pub fn with_verifier(&mut self, verifier: VerifierWrapper) -> &mut Self {
        self.verifier = Some(verifier);
        self
    }

    /// List snapshots
    pub async fn list_snapshots(&self) -> Result<Vec<SnapshotListItem>, String> {
        debug!("List snapshots");
        let aggregator_handler = &self
            .aggregator_handler
            .as_ref()
            .ok_or_else(|| MISSING_AGGREGATOR_HANDLER.to_string())?;
        Ok(aggregator_handler
            .list_snapshots()
            .await?
            .iter()
            .map(|snapshot| convert_to_list_item(snapshot, self.network.clone()))
            .collect::<Vec<SnapshotListItem>>())
    }

    /// Show a snapshot
    pub async fn show_snapshot(&self, digest: &str) -> Result<Vec<SnapshotFieldItem>, String> {
        debug!("Show snapshot {}", digest);
        let aggregator_handler = &self
            .aggregator_handler
            .as_ref()
            .ok_or_else(|| MISSING_AGGREGATOR_HANDLER.to_string())?;
        Ok(convert_to_field_items(
            &aggregator_handler.get_snapshot_details(digest).await?,
            self.network.clone(),
        ))
    }

    /// Download a snapshot by digest
    pub async fn download_snapshot(
        &self,
        digest: &str,
        location_index: isize,
    ) -> Result<(String, String), String> {
        debug!("Download snapshot {}", digest);
        let aggregator_handler = &self
            .aggregator_handler
            .as_ref()
            .ok_or_else(|| MISSING_AGGREGATOR_HANDLER.to_string())?;
        let snapshot = aggregator_handler.get_snapshot_details(digest).await?;
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

    /// Restore a snapshot by digest
    pub async fn restore_snapshot(&self, digest: &str) -> Result<String, String> {
        debug!("Restore snapshot {}", digest);
        let aggregator_handler = &self
            .aggregator_handler
            .as_ref()
            .ok_or_else(|| MISSING_AGGREGATOR_HANDLER.to_string())?;
        let verifier = &self
            .verifier
            .as_ref()
            .ok_or_else(|| MISSING_VERIFIER.to_string())?;
        let fake_digest = fake_data::digest();
        debug!("Fake digest {:?}", fake_digest);
        let certificate_hash = str::from_utf8(&fake_digest).map_err(|e| e.to_string())?;
        let certificate_details = aggregator_handler
            .get_certificate_details(certificate_hash)
            .await?;
        verifier.verify_multi_signature(
            &fake_digest,
            certificate_details.multisignature.as_ref(),
            &certificate_details.participants,
            &certificate_details.protocol_parameters,
        )?;
        aggregator_handler.unpack_snapshot(digest).await
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
    use crate::verifier::MockVerifier;
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
        client.with_aggregator_handler(Box::new(mock_aggregator_handler));
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
        client.with_aggregator_handler(Box::new(mock_aggregator_handler));
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
        client.with_aggregator_handler(Box::new(mock_aggregator_handler));
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
        client.with_aggregator_handler(Box::new(mock_aggregator_handler));
        let snapshot_item = client.show_snapshot(digest).await;
        assert!(snapshot_item.is_err(), "an error should have occurred");
    }

    #[tokio::test]
    async fn test_restore_snapshot_ok() {
        let certificate_hash = "certhash123";
        let fake_certificate = fake_data::certificate(certificate_hash.to_string());
        let mut mock_aggregator_handler = MockAggregatorHandler::new();
        let mut mock_verifier = MockVerifier::new();
        mock_aggregator_handler
            .expect_get_certificate_details()
            .return_const(Ok(fake_certificate.clone()))
            .once();
        mock_aggregator_handler
            .expect_unpack_snapshot()
            .return_const(Ok("".to_string()))
            .once();
        mock_verifier
            .expect_verify_multi_signature()
            .return_const(Ok(()))
            .once();
        let mut client = Client::new("testnet".to_string());
        client
            .with_aggregator_handler(Box::new(mock_aggregator_handler))
            .with_verifier(Box::new(mock_verifier));
        let restore = client.restore_snapshot(certificate_hash).await;
        restore.expect("unexpected error");
    }

    #[tokio::test]
    async fn test_restore_snapshot_ko_get_certificate_details() {
        let certificate_hash = "certhash123";
        let mut mock_aggregator_handler = MockAggregatorHandler::new();
        let mock_verifier = MockVerifier::new();
        mock_aggregator_handler
            .expect_get_certificate_details()
            .return_const(Err("an error".to_string()))
            .once();
        let mut client = Client::new("testnet".to_string());
        client
            .with_aggregator_handler(Box::new(mock_aggregator_handler))
            .with_verifier(Box::new(mock_verifier));
        let restore = client.restore_snapshot(certificate_hash).await;
        assert!(restore.is_err(), "an error should have occurred");
    }

    #[tokio::test]
    async fn test_restore_snapshot_ko_verify_multi_signature() {
        let certificate_hash = "certhash123";
        let fake_certificate = fake_data::certificate(certificate_hash.to_string());
        let mut mock_aggregator_handler = MockAggregatorHandler::new();
        let mut mock_verifier = MockVerifier::new();
        mock_aggregator_handler
            .expect_get_certificate_details()
            .return_const(Ok(fake_certificate.clone()))
            .once();

        mock_verifier
            .expect_verify_multi_signature()
            .return_const(Err("an error".to_string()))
            .once();
        let mut client = Client::new("testnet".to_string());
        client
            .with_aggregator_handler(Box::new(mock_aggregator_handler))
            .with_verifier(Box::new(mock_verifier));
        let restore = client.restore_snapshot(certificate_hash).await;
        assert!(restore.is_err(), "an error should have occurred");
    }

    #[tokio::test]
    async fn test_restore_unpack_snapshot() {
        let certificate_hash = "certhash123";
        let fake_certificate = fake_data::certificate(certificate_hash.to_string());
        let mut mock_aggregator_handler = MockAggregatorHandler::new();
        let mut mock_verifier = MockVerifier::new();
        mock_aggregator_handler
            .expect_get_certificate_details()
            .return_const(Ok(fake_certificate.clone()))
            .once();
        mock_aggregator_handler
            .expect_unpack_snapshot()
            .return_const(Err("an error".to_string()))
            .once();
        mock_verifier
            .expect_verify_multi_signature()
            .return_const(Ok(()))
            .once();
        let mut client = Client::new("testnet".to_string());
        client
            .with_aggregator_handler(Box::new(mock_aggregator_handler))
            .with_verifier(Box::new(mock_verifier));
        let restore = client.restore_snapshot(certificate_hash).await;
        assert!(restore.is_err(), "an error should have occurred");
    }
}

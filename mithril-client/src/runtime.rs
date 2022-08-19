use log::debug;
use std::path::Path;
use std::str;
use std::sync::Arc;
use thiserror::Error;

use crate::aggregator::{AggregatorHandler, AggregatorHandlerError};
use crate::entities::*;
use crate::verifier::{ProtocolError, Verifier};

use mithril_common::digesters::{
    CardanoImmutableDigester, ImmutableDigester, ImmutableDigesterError,
};
use mithril_common::entities::{ProtocolMessagePartKey, Snapshot};

/// AggregatorHandlerWrapper wraps an AggregatorHandler
pub type AggregatorHandlerWrapper = Arc<dyn AggregatorHandler>;

/// VerifierWrapper wraps a Verifier
pub type VerifierWrapper = Box<dyn Verifier>;

/// DigesterWrapper wraps a Digester
pub type DigesterWrapper = Box<dyn ImmutableDigester>;

/// [Runtime] related errors.
#[derive(Error, Debug)]
pub enum RuntimeError {
    /// Error raised when accessing a missing dependency.
    #[error("a dependency is missing: '{0}'")]
    MissingDependency(String),

    /// Error raised when the user provided an invalid input.
    #[error("an input is invalid: '{0}'")]
    InvalidInput(String),

    /// Error raised when an [AggregatorHandlerError] is caught when querying the aggregator using
    /// a [AggregatorHandler].
    #[error("aggregator handler error: '{0}'")]
    AggregatorHandler(#[from] AggregatorHandlerError),

    /// Error raised when the digest computation fails.
    #[error("immutale digester error: '{0}'")]
    ImmutableDigester(#[from] ImmutableDigesterError),

    /// Error raised when the digest stored in the signed message doesn't match the
    /// [certificate](https://mithril.network/mithril-common/doc/mithril_common/entities/struct.Certificate.html)
    /// hash.
    #[error("digest unmatch error: '{0}'")]
    DigestUnmatch(String),

    /// Error raised when verification fails.
    #[error("verification error: '{0}'")]
    Protocol(#[from] ProtocolError),
}

/// Mithril client wrapper
pub struct Runtime {
    /// Cardano network
    pub network: String,

    /// Aggregator handler dependency that interacts with an aggregator
    aggregator_handler: Option<AggregatorHandlerWrapper>,

    /// Verifier dependency that verifies certificates and their multi signatures
    verifier: Option<VerifierWrapper>,

    /// Digester dependency that computes the digest used as the message ot be signed and embedded in the multisignature
    digester: Option<DigesterWrapper>,
}

impl Runtime {
    /// Runtime factory
    pub fn new(network: String) -> Self {
        Self {
            network,
            aggregator_handler: None,
            verifier: None,
            digester: None,
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

    /// With Digester
    pub fn with_digester(&mut self, digester: DigesterWrapper) -> &mut Self {
        self.digester = Some(digester);
        self
    }

    /// Get AggregatorHandler
    fn get_aggregator_handler(&self) -> Result<&AggregatorHandlerWrapper, RuntimeError> {
        self.aggregator_handler
            .as_ref()
            .ok_or_else(|| RuntimeError::MissingDependency("aggregator handler".to_string()))
    }

    /// Get Verifier
    fn get_verifier(&self) -> Result<&VerifierWrapper, RuntimeError> {
        self.verifier
            .as_ref()
            .ok_or_else(|| RuntimeError::MissingDependency("verifier".to_string()))
    }

    /// Get Digester
    fn get_digester(&self) -> Result<&DigesterWrapper, RuntimeError> {
        self.digester
            .as_ref()
            .ok_or_else(|| RuntimeError::MissingDependency("digester".to_string()))
    }

    /// List snapshots
    pub async fn list_snapshots(&self) -> Result<Vec<SnapshotListItem>, RuntimeError> {
        debug!("List snapshots");
        Ok(self
            .get_aggregator_handler()?
            .list_snapshots()
            .await
            .map_err(RuntimeError::AggregatorHandler)?
            .iter()
            .map(|snapshot| convert_to_list_item(snapshot, self.network.clone()))
            .collect::<Vec<SnapshotListItem>>())
    }

    /// Show a snapshot
    pub async fn show_snapshot(
        &self,
        digest: &str,
    ) -> Result<Vec<SnapshotFieldItem>, RuntimeError> {
        debug!("Show snapshot {}", digest);
        Ok(convert_to_field_items(
            &self
                .get_aggregator_handler()?
                .get_snapshot_details(digest)
                .await
                .map_err(RuntimeError::AggregatorHandler)?,
            self.network.clone(),
        ))
    }

    /// Download a snapshot by digest
    pub async fn download_snapshot(
        &self,
        digest: &str,
        location_index: isize,
    ) -> Result<(String, String), RuntimeError> {
        debug!("Download snapshot {}", digest);
        let snapshot = self
            .get_aggregator_handler()?
            .get_snapshot_details(digest)
            .await
            .map_err(RuntimeError::AggregatorHandler)?;
        let from = snapshot
            .locations
            .get((location_index - 1) as usize)
            .ok_or_else(|| RuntimeError::InvalidInput("invalid location index".to_string()))?
            .to_owned();
        match self
            .get_aggregator_handler()?
            .download_snapshot(digest, &from)
            .await
        {
            Ok(to) => Ok((from, to)),
            Err(err) => Err(RuntimeError::AggregatorHandler(err)),
        }
    }

    /// Restore a snapshot by digest
    pub async fn restore_snapshot(&mut self, digest: &str) -> Result<String, RuntimeError> {
        debug!("Restore snapshot {}", digest);
        let snapshot = &self
            .get_aggregator_handler()?
            .get_snapshot_details(digest)
            .await
            .map_err(RuntimeError::AggregatorHandler)?;
        let certificate = self
            .get_aggregator_handler()?
            .get_certificate_details(&snapshot.certificate_hash)
            .await
            .map_err(RuntimeError::AggregatorHandler)?;
        let unpacked_path = &self
            .get_aggregator_handler()?
            .unpack_snapshot(digest)
            .await
            .map_err(RuntimeError::AggregatorHandler)?;
        if self.get_digester().is_err() {
            self.with_digester(Box::new(CardanoImmutableDigester::new(
                Path::new(unpacked_path).into(),
                slog_scope::logger(),
            )));
        }
        let unpacked_snapshot_digest = self
            .get_digester()?
            .compute_digest(&certificate.beacon)
            .await
            .map_err(RuntimeError::ImmutableDigester)?;
        let mut protocol_message = certificate.protocol_message.clone();
        protocol_message.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            unpacked_snapshot_digest.clone(),
        );
        if protocol_message.compute_hash() != certificate.signed_message {
            return Err(RuntimeError::DigestUnmatch(unpacked_snapshot_digest));
        }
        self.get_verifier()?
            .verify_certificate_chain(
                certificate,
                self.get_aggregator_handler()?.as_certificate_retriever(),
            )
            .await?;
        Ok(unpacked_path.to_owned())
    }
}

/// Convert Snapshot to SnapshotListItem routine
pub(crate) fn convert_to_list_item(snapshot: &Snapshot, network: String) -> SnapshotListItem {
    SnapshotListItem::new(
        network,
        snapshot.beacon.epoch,
        snapshot.beacon.immutable_file_number,
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
        SnapshotFieldItem::new("Epoch".to_string(), format!("{}", snapshot.beacon.epoch)),
        SnapshotFieldItem::new(
            "Immutable File Number".to_string(),
            format!("{}", snapshot.beacon.immutable_file_number),
        ),
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
    use async_trait::async_trait;

    use mockall::mock;

    use crate::aggregator::AggregatorHandlerError;
    use crate::verifier::{CertificateRetriever, MockVerifier, ProtocolError};
    use mithril_common::digesters::{ImmutableDigester, ImmutableDigesterError};
    use mithril_common::entities::{Beacon, Certificate};
    use mithril_common::fake_data;

    mock! {
        pub DigesterImpl { }

        #[async_trait]
        impl ImmutableDigester for DigesterImpl {
            async fn compute_digest(
                &self,
                beacon: &Beacon,
            ) -> Result<String, ImmutableDigesterError>;
        }
    }

    mock! {
        pub AggregatorHandlerImpl { }

        #[async_trait]
        impl CertificateRetriever for AggregatorHandlerImpl {

            async fn get_certificate_details(
                &self,
                certificate_hash: &str,
            ) -> Result<Certificate, AggregatorHandlerError>;
        }

        #[async_trait]
        impl AggregatorHandler for AggregatorHandlerImpl {
            async fn list_snapshots(&self) -> Result<Vec<Snapshot>, AggregatorHandlerError>;

            async fn get_snapshot_details(&self, digest: &str) -> Result<Snapshot, AggregatorHandlerError>;

            async fn download_snapshot(
                &self,
                digest: &str,
                location: &str,
            ) -> Result<String, AggregatorHandlerError>;

            async fn unpack_snapshot(&self, digest: &str) -> Result<String, AggregatorHandlerError>;

            fn as_certificate_retriever(&self) -> Arc<dyn CertificateRetriever>;
        }
    }

    #[tokio::test]
    async fn test_list_snapshots_ok() {
        let network = "testnet".to_string();
        let fake_snapshots = fake_data::snapshots(5);
        let snapshot_list_items_expected = fake_snapshots
            .iter()
            .map(|snapshot| convert_to_list_item(snapshot, network.clone()))
            .collect::<Vec<SnapshotListItem>>();
        let mut mock_aggregator_handler = MockAggregatorHandlerImpl::new();
        mock_aggregator_handler
            .expect_list_snapshots()
            .return_once(move || Ok(fake_snapshots));
        let mut client = Runtime::new(network.clone());
        client.with_aggregator_handler(Arc::new(mock_aggregator_handler));
        let snapshot_list_items = client.list_snapshots().await;
        snapshot_list_items.as_ref().expect("unexpected error");
        assert_eq!(
            snapshot_list_items.unwrap(),
            snapshot_list_items_expected.to_owned()
        );
    }

    #[tokio::test]
    async fn test_list_snapshots_ko() {
        let mut mock_aggregator_handler = MockAggregatorHandlerImpl::new();
        mock_aggregator_handler
            .expect_list_snapshots()
            .return_once(move || {
                Err(AggregatorHandlerError::RemoteServerTechnical(
                    "error occurred".to_string(),
                ))
            });
        let mut client = Runtime::new("testnet".to_string());
        client.with_aggregator_handler(Arc::new(mock_aggregator_handler));
        let snapshot_list_items = client.list_snapshots().await;
        assert!(
            matches!(snapshot_list_items, Err(RuntimeError::AggregatorHandler(_))),
            "unexpected error type: {:?}",
            snapshot_list_items
        );
    }

    #[tokio::test]
    async fn test_show_snapshot_ok() {
        let digest = "digest123";
        let fake_snapshot = fake_data::snapshots(1).first().unwrap().to_owned();
        let snapshot_item_expected = convert_to_field_items(&fake_snapshot, "testnet".to_string());
        let mut mock_aggregator_handler = MockAggregatorHandlerImpl::new();
        mock_aggregator_handler
            .expect_get_snapshot_details()
            .return_once(move |_| Ok(fake_snapshot));
        let mut client = Runtime::new("testnet".to_string());
        client.with_aggregator_handler(Arc::new(mock_aggregator_handler));
        let snapshot_item = client.show_snapshot(digest).await;
        snapshot_item.as_ref().expect("unexpected error");
        assert_eq!(snapshot_item.unwrap(), snapshot_item_expected);
    }

    #[tokio::test]
    async fn test_show_snapshot_ko() {
        let digest = "digest123";
        let mut mock_aggregator_handler = MockAggregatorHandlerImpl::new();
        mock_aggregator_handler
            .expect_get_snapshot_details()
            .return_once(move |_| {
                Err(AggregatorHandlerError::RemoteServerTechnical(
                    "error occurred".to_string(),
                ))
            });
        let mut client = Runtime::new("testnet".to_string());
        client.with_aggregator_handler(Arc::new(mock_aggregator_handler));
        let snapshot_item = client.show_snapshot(digest).await;
        assert!(
            matches!(snapshot_item, Err(RuntimeError::AggregatorHandler(_))),
            "unexpected error type: {:?}",
            snapshot_item
        );
    }

    #[tokio::test]
    async fn test_restore_snapshot_ok() {
        let fake_certificate = fake_data::certificate("cert-hash-123".to_string());
        let digest_compute = fake_certificate
            .protocol_message
            .get_message_part(&ProtocolMessagePartKey::SnapshotDigest)
            .unwrap()
            .to_owned();
        let digest_restore = digest_compute.clone();
        let fake_snapshot = fake_data::snapshots(1).first().unwrap().to_owned();
        let mut mock_aggregator_handler = MockAggregatorHandlerImpl::new();
        let mut mock_verifier = MockVerifier::new();
        let mut mock_digester = MockDigesterImpl::new();
        mock_aggregator_handler
            .expect_as_certificate_retriever()
            .return_once(move || Arc::new(MockAggregatorHandlerImpl::new()));
        mock_aggregator_handler
            .expect_get_snapshot_details()
            .return_once(move |_| Ok(fake_snapshot));
        mock_aggregator_handler
            .expect_get_certificate_details()
            .returning(move |_| Ok(fake_certificate.clone()))
            .times(1);
        mock_aggregator_handler
            .expect_unpack_snapshot()
            .return_once(move |_| Ok("./target-dir".to_string()));
        mock_verifier
            .expect_verify_certificate_chain()
            .returning(|_, _| Ok(()))
            .times(1);
        mock_digester
            .expect_compute_digest()
            .return_once(move |_| Ok(digest_compute));
        let mut client = Runtime::new("testnet".to_string());
        client
            .with_aggregator_handler(Arc::new(mock_aggregator_handler))
            .with_verifier(Box::new(mock_verifier))
            .with_digester(Box::new(mock_digester));
        let restore = client.restore_snapshot(&digest_restore).await;
        restore.expect("unexpected error");
    }

    #[tokio::test]
    async fn test_restore_snapshot_ko_certificate_chain_fail() {
        let fake_certificate = fake_data::certificate("cert-hash-123".to_string());
        let digest_compute = fake_certificate
            .protocol_message
            .get_message_part(&ProtocolMessagePartKey::SnapshotDigest)
            .unwrap()
            .to_owned();
        let digest_restore = digest_compute.clone();
        let fake_snapshot = fake_data::snapshots(1).first().unwrap().to_owned();
        let mut mock_aggregator_handler = MockAggregatorHandlerImpl::new();
        let mut mock_verifier = MockVerifier::new();
        let mut mock_digester = MockDigesterImpl::new();
        mock_aggregator_handler
            .expect_as_certificate_retriever()
            .return_once(move || Arc::new(MockAggregatorHandlerImpl::new()));
        mock_aggregator_handler
            .expect_get_snapshot_details()
            .return_once(move |_| Ok(fake_snapshot));
        mock_aggregator_handler
            .expect_get_certificate_details()
            .returning(move |_| Ok(fake_certificate.clone()))
            .times(1);
        mock_aggregator_handler
            .expect_unpack_snapshot()
            .return_once(move |_| Ok("./target-dir".to_string()));
        mock_verifier
            .expect_verify_certificate_chain()
            .returning(|_, _| Err(ProtocolError::CertificateChainAVKUnmatch))
            .times(1);
        mock_digester
            .expect_compute_digest()
            .return_once(move |_| Ok(digest_compute));
        let mut client = Runtime::new("testnet".to_string());
        client
            .with_aggregator_handler(Arc::new(mock_aggregator_handler))
            .with_verifier(Box::new(mock_verifier))
            .with_digester(Box::new(mock_digester));
        let restore = client.restore_snapshot(&digest_restore).await;
        assert!(
            matches!(restore, Err(RuntimeError::Protocol(_))),
            "unexpected error type: {:?}",
            restore
        );
    }

    #[tokio::test]
    async fn test_restore_snapshot_ko_digester_error() {
        let fake_certificate = fake_data::certificate("cert-hash-123".to_string());
        let digest_compute = fake_certificate
            .protocol_message
            .get_message_part(&ProtocolMessagePartKey::SnapshotDigest)
            .unwrap()
            .to_owned();
        let digest_restore = digest_compute.clone();
        let mut fake_certificate1 = fake_certificate.clone();
        fake_certificate1.hash = "another-hash".to_string();
        let fake_snapshot = fake_data::snapshots(1).first().unwrap().to_owned();
        let mut mock_aggregator_handler = MockAggregatorHandlerImpl::new();
        let mock_verifier = MockVerifier::new();
        let mut mock_digester = MockDigesterImpl::new();
        mock_aggregator_handler
            .expect_get_snapshot_details()
            .return_once(move |_| Ok(fake_snapshot));
        mock_aggregator_handler
            .expect_get_certificate_details()
            .return_once(move |_| Ok(fake_certificate1));
        mock_aggregator_handler
            .expect_unpack_snapshot()
            .return_once(move |_| Ok("./target-dir".to_string()));
        mock_digester.expect_compute_digest().return_once(|_| {
            Err(ImmutableDigesterError::NotEnoughImmutable {
                found_number: None,
                expected_number: 3,
            })
        });
        let mut client = Runtime::new("testnet".to_string());
        client
            .with_aggregator_handler(Arc::new(mock_aggregator_handler))
            .with_verifier(Box::new(mock_verifier))
            .with_digester(Box::new(mock_digester));
        let restore = client.restore_snapshot(&digest_restore).await;
        assert!(
            matches!(restore, Err(RuntimeError::ImmutableDigester(_))),
            "unexpected error type: {:?}",
            restore
        );
    }

    #[tokio::test]
    async fn test_restore_snapshot_ko_get_snapshot_details() {
        let digest = "digest123";
        let mut mock_aggregator_handler = MockAggregatorHandlerImpl::new();
        let mock_verifier = MockVerifier::new();
        mock_aggregator_handler
            .expect_get_snapshot_details()
            .return_once(move |_| {
                Err(AggregatorHandlerError::RemoteServerTechnical(
                    "error occurred".to_string(),
                ))
            });

        let mut client = Runtime::new("testnet".to_string());
        client
            .with_aggregator_handler(Arc::new(mock_aggregator_handler))
            .with_verifier(Box::new(mock_verifier));
        let restore = client.restore_snapshot(digest).await;
        assert!(
            matches!(restore, Err(RuntimeError::AggregatorHandler(_))),
            "unexpected error type: {:?}",
            restore
        );
    }

    #[tokio::test]
    async fn test_restore_snapshot_ko_get_certificate_details() {
        let digest = "digest123";
        let fake_snapshot = fake_data::snapshots(1).first().unwrap().to_owned();
        let mut mock_aggregator_handler = MockAggregatorHandlerImpl::new();
        let mock_verifier = MockVerifier::new();
        mock_aggregator_handler
            .expect_get_snapshot_details()
            .return_once(move |_| Ok(fake_snapshot));
        mock_aggregator_handler
            .expect_get_certificate_details()
            .return_once(move |_| {
                Err(AggregatorHandlerError::RemoteServerTechnical(
                    "error occurred".to_string(),
                ))
            });

        let mut client = Runtime::new("testnet".to_string());
        client
            .with_aggregator_handler(Arc::new(mock_aggregator_handler))
            .with_verifier(Box::new(mock_verifier));
        let restore = client.restore_snapshot(digest).await;
        assert!(
            matches!(restore, Err(RuntimeError::AggregatorHandler(_))),
            "unexpected error type: {:?}",
            restore
        );
    }

    #[tokio::test]
    async fn test_restore_snapshot_ko_restore_unpack_snapshot() {
        let digest = "digest123";
        let certificate_hash = "certhash123";
        let fake_certificate = fake_data::certificate(certificate_hash.to_string());
        let fake_snapshot = fake_data::snapshots(1).first().unwrap().to_owned();
        let mut mock_aggregator_handler = MockAggregatorHandlerImpl::new();
        let mut mock_verifier = MockVerifier::new();
        mock_aggregator_handler
            .expect_get_snapshot_details()
            .return_once(move |_| Ok(fake_snapshot));
        mock_aggregator_handler
            .expect_get_certificate_details()
            .return_once(move |_| Ok(fake_certificate));
        mock_aggregator_handler
            .expect_unpack_snapshot()
            .return_once(move |_| {
                Err(AggregatorHandlerError::RemoteServerTechnical(
                    "error occurred".to_string(),
                ))
            });
        mock_verifier
            .expect_verify_multi_signature()
            .return_once(|_, _, _, _| Ok(()));
        let mut client = Runtime::new("testnet".to_string());
        client
            .with_aggregator_handler(Arc::new(mock_aggregator_handler))
            .with_verifier(Box::new(mock_verifier));
        let restore = client.restore_snapshot(digest).await;
        assert!(
            matches!(restore, Err(RuntimeError::AggregatorHandler(_))),
            "unexpected error type: {:?}",
            restore
        );
    }
}

use slog_scope::debug;
use std::str;
use std::sync::Arc;
use thiserror::Error;

use crate::aggregator::{AggregatorHandler, AggregatorHandlerError};
use crate::entities::*;

use mithril_common::certificate_chain::{
    CertificateRetrieverError, CertificateVerifier, CertificateVerifierError,
};
use mithril_common::crypto_helper::ProtocolGenesisVerifier;
use mithril_common::digesters::{ImmutableDigester, ImmutableDigesterError};
use mithril_common::entities::{ProtocolMessagePartKey, Snapshot};

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

    /// Error raised when a CertificateRetrieverError tries to retrieve a
    /// [certificate](https://mithril.network/mithril-common/doc/mithril_common/entities/struct.Certificate.html)
    #[error("certificate retriever error: '{0}'")]
    CertificateRetriever(#[from] CertificateRetrieverError),

    /// Error raised when the digest computation fails.
    #[error("immutable digester error: '{0}'")]
    ImmutableDigester(#[from] ImmutableDigesterError),

    /// Error raised when the digest stored in the signed message doesn't match the
    /// [certificate](https://mithril.network/mithril-common/doc/mithril_common/entities/struct.Certificate.html)
    /// hash.
    #[error("digest doesn't match error: '{0}'")]
    DigestDoesntMatch(String),

    /// Error raised when verification fails.
    #[error("verification error: '{0}'")]
    Protocol(#[from] CertificateVerifierError),
}

/// Mithril client runtime
pub struct Runtime {
    /// Cardano network
    pub network: String,
}

impl Runtime {
    /// Runtime factory
    pub fn new(network: String) -> Self {
        Self { network }
    }

    /// List snapshots
    pub async fn list_snapshots(
        &self,
        aggregator_handler: Arc<dyn AggregatorHandler>,
    ) -> Result<Vec<SnapshotListItem>, RuntimeError> {
        debug!("List snapshots");
        Ok(aggregator_handler
            .list_snapshots()
            .await?
            .iter()
            .map(|snapshot| convert_to_list_item(snapshot, self.network.clone()))
            .collect::<Vec<SnapshotListItem>>())
    }

    /// Show a snapshot
    pub async fn show_snapshot<'a>(
        &self,
        aggregator_handler: Arc<dyn AggregatorHandler + 'a>,
        digest: &str,
    ) -> Result<Snapshot, RuntimeError> {
        debug!("Show snapshot {}", digest);
        let snapshot_details = aggregator_handler.get_snapshot_details(digest).await?;

        Ok(snapshot_details)
    }

    /// Download a snapshot by digest
    pub async fn download_snapshot<'a>(
        &self,
        aggregator_handler: Arc<dyn AggregatorHandler + 'a>,
        digest: &str,
        location_index: isize,
    ) -> Result<(String, String), RuntimeError> {
        debug!("Download snapshot {}", digest);
        let snapshot = aggregator_handler.get_snapshot_details(digest).await?;
        let from = snapshot
            .locations
            .get((location_index - 1) as usize)
            .ok_or_else(|| RuntimeError::InvalidInput("invalid location index".to_string()))?
            .to_owned();
        match aggregator_handler.download_snapshot(digest, &from).await {
            Ok(to) => Ok((from, to)),
            Err(err) => Err(RuntimeError::AggregatorHandler(err)),
        }
    }

    /// Restore a snapshot by digest
    pub async fn restore_snapshot<'a>(
        &mut self,
        aggregator_handler: Arc<dyn AggregatorHandler + 'a>,
        digester: Box<dyn ImmutableDigester + 'a>,
        certificate_verifier: Box<dyn CertificateVerifier + 'a>,
        genesis_verifier: ProtocolGenesisVerifier,
        digest: &str,
    ) -> Result<String, RuntimeError> {
        debug!("Restore snapshot {}", digest);
        let snapshot = aggregator_handler.get_snapshot_details(digest).await?;
        let certificate = aggregator_handler
            .get_certificate_details(&snapshot.certificate_hash)
            .await?;
        let unpacked_path = aggregator_handler.unpack_snapshot(digest).await?;
        let unpacked_snapshot_digest = digester.compute_digest(&certificate.beacon).await?;
        let mut protocol_message = certificate.protocol_message.clone();
        protocol_message.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            unpacked_snapshot_digest.clone(),
        );
        if protocol_message.compute_hash() != certificate.signed_message {
            return Err(RuntimeError::DigestDoesntMatch(unpacked_snapshot_digest));
        }
        certificate_verifier
            .verify_certificate_chain(
                certificate,
                aggregator_handler.as_certificate_retriever(),
                &genesis_verifier,
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
pub fn convert_to_field_items(snapshot: &Snapshot, network: String) -> Vec<SnapshotFieldItem> {
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

    use mithril_common::crypto_helper::{ProtocolGenesisSigner, ProtocolGenesisVerifier};
    use mockall::mock;

    use crate::aggregator::AggregatorHandlerError;
    use mithril_common::certificate_chain::{
        CertificateRetriever, CertificateRetrieverError, CertificateVerifierError,
    };
    use mithril_common::digesters::{ImmutableDigester, ImmutableDigesterError};
    use mithril_common::entities::{Beacon, Certificate, ProtocolParameters};
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
        pub CertificateVerifierImpl { }

        #[async_trait]
        impl CertificateVerifier for CertificateVerifierImpl {
            fn verify_multi_signature(
                &self,
                message: &[u8],
                multi_signature: &str,
                aggregate_verification_key: &str,
                protocol_parameters: &ProtocolParameters,
            ) -> Result<(), CertificateVerifierError>;

            async fn verify_genesis_certificate(
                &self,
                certificate: &Certificate,
                genesis_verifier: &ProtocolGenesisVerifier,
            ) -> Result<(), CertificateVerifierError>;

            async fn verify_standard_certificate(
                &self,
                certificate: &Certificate,
                certificate_retriever: Arc<dyn CertificateRetriever>,
            ) -> Result<Option<Certificate>, CertificateVerifierError>;

            async fn verify_certificate(
                &self,
                certificate: &Certificate,
                certificate_retriever: Arc<dyn CertificateRetriever>,
                genesis_verifier: &ProtocolGenesisVerifier,
            ) -> Result<Option<Certificate>, CertificateVerifierError>;

            async fn verify_certificate_chain(
                &self,
                certificate: Certificate,
                certificate_retriever: Arc<dyn CertificateRetriever>,
                genesis_verifier: &ProtocolGenesisVerifier,
            ) -> Result<(), CertificateVerifierError>;
        }
    }

    mock! {
        pub AggregatorHandlerImpl { }

        #[async_trait]
        impl CertificateRetriever for AggregatorHandlerImpl {
            async fn get_certificate_details(
                &self,
                certificate_hash: &str,
            ) -> Result<Certificate, CertificateRetrieverError>;
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

    fn get_dependencies() -> (
        MockAggregatorHandlerImpl,
        MockCertificateVerifierImpl,
        MockDigesterImpl,
        ProtocolGenesisVerifier,
    ) {
        let mock_aggregator_handler = MockAggregatorHandlerImpl::new();
        let mock_verifier = MockCertificateVerifierImpl::new();
        let mock_digester = MockDigesterImpl::new();
        let genesis_verifier =
            ProtocolGenesisSigner::create_deterministic_genesis_signer().create_genesis_verifier();

        (
            mock_aggregator_handler,
            mock_verifier,
            mock_digester,
            genesis_verifier,
        )
    }

    #[tokio::test]
    async fn test_list_snapshots_ok() {
        let network = "testnet".to_string();
        let fake_snapshots = fake_data::snapshots(5);
        let snapshot_list_items_expected = fake_snapshots
            .iter()
            .map(|snapshot| convert_to_list_item(snapshot, network.clone()))
            .collect::<Vec<SnapshotListItem>>();
        let (mut mock_aggregator_handler, _mock_verifier, _mock_digester, _genesis_verifier) =
            get_dependencies();
        mock_aggregator_handler
            .expect_list_snapshots()
            .return_once(move || Ok(fake_snapshots));
        let client = Runtime::new(network.clone());
        let snapshot_list_items = client
            .list_snapshots(Arc::new(mock_aggregator_handler))
            .await;
        snapshot_list_items.as_ref().expect("unexpected error");
        assert_eq!(
            snapshot_list_items.unwrap(),
            snapshot_list_items_expected.to_owned()
        );
    }

    #[tokio::test]
    async fn test_list_snapshots_ko() {
        let (mut mock_aggregator_handler, _mock_verifier, _mock_digester, _genesis_verifier) =
            get_dependencies();
        mock_aggregator_handler
            .expect_list_snapshots()
            .return_once(move || {
                Err(AggregatorHandlerError::RemoteServerTechnical(
                    "error occurred".to_string(),
                ))
            });
        let client = Runtime::new("testnet".to_string());
        let snapshot_list_items = client
            .list_snapshots(Arc::new(mock_aggregator_handler))
            .await;
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
        let snapshot_item_expected = fake_snapshot.clone();
        let (mut mock_aggregator_handler, _mock_verifier, _mock_digester, _genesis_verifier) =
            get_dependencies();
        mock_aggregator_handler
            .expect_get_snapshot_details()
            .return_once(move |_| Ok(fake_snapshot));
        let client = Runtime::new("testnet".to_string());
        let snapshot_item = client
            .show_snapshot(Arc::new(mock_aggregator_handler), digest)
            .await;
        snapshot_item.as_ref().expect("unexpected error");
        assert_eq!(snapshot_item.unwrap(), snapshot_item_expected);
    }

    #[tokio::test]
    async fn test_show_snapshot_ko() {
        let digest = "digest123";
        let (mut mock_aggregator_handler, _mock_verifier, _mock_digester, _genesis_verifier) =
            get_dependencies();
        mock_aggregator_handler
            .expect_get_snapshot_details()
            .return_once(move |_| {
                Err(AggregatorHandlerError::RemoteServerTechnical(
                    "error occurred".to_string(),
                ))
            });
        let client = Runtime::new("testnet".to_string());
        let snapshot_item = client
            .show_snapshot(Arc::new(mock_aggregator_handler), digest)
            .await;
        assert!(
            matches!(snapshot_item, Err(RuntimeError::AggregatorHandler(_))),
            "unexpected error type: {:?}",
            snapshot_item
        );
    }

    #[tokio::test]
    async fn test_restore_snapshot_ok() {
        let fake_certificate = fake_data::certificate("cert-hash-123".to_string());
        let (mut mock_aggregator_handler, mut mock_verifier, mut mock_digester, genesis_verifier) =
            get_dependencies();
        let digest_compute = fake_certificate
            .protocol_message
            .get_message_part(&ProtocolMessagePartKey::SnapshotDigest)
            .unwrap()
            .to_owned();
        let digest_restore = digest_compute.clone();
        let fake_snapshot = fake_data::snapshots(1).first().unwrap().to_owned();
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
            .returning(|_, _, _| Ok(()))
            .times(1);
        mock_digester
            .expect_compute_digest()
            .return_once(move |_| Ok(digest_compute));
        let mut client = Runtime::new("testnet".to_string());
        let restore = client
            .restore_snapshot(
                Arc::new(mock_aggregator_handler),
                Box::new(mock_digester),
                Box::new(mock_verifier),
                genesis_verifier,
                &digest_restore,
            )
            .await;
        restore.expect("unexpected error");
    }

    #[tokio::test]
    async fn test_restore_snapshot_ko_certificate_chain_fail() {
        let fake_certificate = fake_data::certificate("cert-hash-123".to_string());
        let (mut mock_aggregator_handler, mut mock_verifier, mut mock_digester, genesis_verifier) =
            get_dependencies();
        let digest_compute = fake_certificate
            .protocol_message
            .get_message_part(&ProtocolMessagePartKey::SnapshotDigest)
            .unwrap()
            .to_owned();
        let digest_restore = digest_compute.clone();
        let fake_snapshot = fake_data::snapshots(1).first().unwrap().to_owned();
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
            .returning(|_, _, _| Err(CertificateVerifierError::CertificateChainAVKUnmatch))
            .times(1);
        mock_digester
            .expect_compute_digest()
            .return_once(move |_| Ok(digest_compute));
        let mut client = Runtime::new("testnet".to_string());
        let restore = client
            .restore_snapshot(
                Arc::new(mock_aggregator_handler),
                Box::new(mock_digester),
                Box::new(mock_verifier),
                genesis_verifier,
                &digest_restore,
            )
            .await;
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
        let (mut mock_aggregator_handler, mock_verifier, mut mock_digester, genesis_verifier) =
            get_dependencies();
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
        let restore = client
            .restore_snapshot(
                Arc::new(mock_aggregator_handler),
                Box::new(mock_digester),
                Box::new(mock_verifier),
                genesis_verifier,
                &digest_restore,
            )
            .await;
        assert!(
            matches!(restore, Err(RuntimeError::ImmutableDigester(_))),
            "unexpected error type: {:?}",
            restore
        );
    }

    #[tokio::test]
    async fn test_restore_snapshot_ko_get_snapshot_details() {
        let digest = "digest123";
        let (mut mock_aggregator_handler, mock_verifier, mock_digester, genesis_verifier) =
            get_dependencies();
        mock_aggregator_handler
            .expect_get_snapshot_details()
            .return_once(move |_| {
                Err(AggregatorHandlerError::RemoteServerTechnical(
                    "error occurred".to_string(),
                ))
            });

        let mut client = Runtime::new("testnet".to_string());
        let restore = client
            .restore_snapshot(
                Arc::new(mock_aggregator_handler),
                Box::new(mock_digester),
                Box::new(mock_verifier),
                genesis_verifier,
                digest,
            )
            .await;
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
        let (mut mock_aggregator_handler, mock_verifier, mock_digester, genesis_verifier) =
            get_dependencies();
        mock_aggregator_handler
            .expect_get_snapshot_details()
            .return_once(move |_| Ok(fake_snapshot));
        mock_aggregator_handler
            .expect_get_certificate_details()
            .return_once(move |_| {
                Err(CertificateRetrieverError::General(
                    AggregatorHandlerError::RemoteServerTechnical("error occurred".to_string())
                        .to_string(),
                ))
            });

        let mut client = Runtime::new("testnet".to_string());
        let restore = client
            .restore_snapshot(
                Arc::new(mock_aggregator_handler),
                Box::new(mock_digester),
                Box::new(mock_verifier),
                genesis_verifier,
                digest,
            )
            .await;
        assert!(
            matches!(restore, Err(RuntimeError::CertificateRetriever(_))),
            "unexpected error type: {:?}",
            restore
        );
    }

    #[tokio::test]
    async fn test_restore_snapshot_ko_restore_unpack_snapshot() {
        let digest = "digest123";
        let certificate_hash = "cert_hash123";
        let fake_certificate = fake_data::certificate(certificate_hash.to_string());
        let fake_snapshot = fake_data::snapshots(1).first().unwrap().to_owned();
        let (mut mock_aggregator_handler, mut mock_verifier, mock_digester, genesis_verifier) =
            get_dependencies();
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
        let restore = client
            .restore_snapshot(
                Arc::new(mock_aggregator_handler),
                Box::new(mock_digester),
                Box::new(mock_verifier),
                genesis_verifier,
                digest,
            )
            .await;
        assert!(
            matches!(restore, Err(RuntimeError::AggregatorHandler(_))),
            "unexpected error type: {:?}",
            restore
        );
    }
}

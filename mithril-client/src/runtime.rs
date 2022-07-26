use log::debug;
use std::path::Path;
use std::str;
use thiserror::Error;

use crate::aggregator::{AggregatorHandler, AggregatorHandlerError};
use crate::entities::*;
use crate::verifier::{ProtocolError, Verifier};

use mithril_common::digesters::{Digester, DigesterError, ImmutableDigester};
use mithril_common::entities::{Certificate, ProtocolMessagePartKey, Snapshot};

/// AggregatorHandlerWrapper wraps an AggregatorHandler
pub type AggregatorHandlerWrapper = Box<dyn AggregatorHandler>;

/// VerifierWrapper wraps a Verifier
pub type VerifierWrapper = Box<dyn Verifier>;

/// DigesterWrapper wraps a Digester
pub type DigesterWrapper = Box<dyn Digester>;

#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("a dependency is missing: '{0}'")]
    MissingDependency(String),

    #[error("an input is invalid: '{0}'")]
    InvalidInput(String),

    #[error("aggregator handler error: '{0}'")]
    AggregatorHandler(#[from] AggregatorHandlerError),

    #[error("verifier error: '{0}'")]
    Verifier(#[from] ProtocolError),

    #[error("immutale digester error: '{0}'")]
    ImmutableDigester(#[from] DigesterError),

    #[error("digest unmatch error: '{0}'")]
    DigestUnmatch(String),

    #[error("certificate hash unmatch error")]
    CertificateHashUnmatch,

    #[error("certificate chain previous hash unmatch error")]
    CertificateChainPreviousHashUnmatch,

    #[error("certificate chain AVK unmatch error")]
    CertificateChainAVKUnmatch,

    #[error("certificate chain infinite loop error")]
    CertificateChainInfiniteLoop,
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
        let mut certificate = self
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
            self.with_digester(Box::new(ImmutableDigester::new(
                Path::new(unpacked_path).into(),
                slog_scope::logger(),
            )));
        }
        let unpacked_snapshot_digest = self
            .get_digester()?
            .compute_digest()
            .await
            .map_err(RuntimeError::ImmutableDigester)?
            .digest;
        let mut protocol_message = certificate.protocol_message.clone();
        protocol_message.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            unpacked_snapshot_digest.clone(),
        );
        if protocol_message.compute_hash() != certificate.signed_message {
            return Err(RuntimeError::DigestUnmatch(unpacked_snapshot_digest));
        }
        while let Some(previous_certificate) = self.verify_certificate(&certificate).await? {
            certificate = previous_certificate;
        }
        Ok(unpacked_path.to_owned())
    }

    /// Verify if a Certificate is valid and returns the previous Certificate in the chain if exists
    /// Step 1: Check if the hash is valid (i.e. the Certificate has not been tampered by modifying its content)
    /// Step 2: Check that the multi signature is valid if it is a Standard Certificate (i.e verifiction of the Mithril multi signature)
    /// Step 3: Check that the aggregate verification key of the Certificate is registered in the previous Certificate in the chain
    pub async fn verify_certificate(
        &self,
        certificate: &Certificate,
    ) -> Result<Option<Certificate>, RuntimeError> {
        debug!("Verify certificate {:#?}", certificate);
        println!(
            "Verify certificate #{} @ epoch #{}",
            certificate.hash, certificate.beacon.epoch
        );
        certificate
            .hash
            .eq(&certificate.compute_hash())
            .then(|| certificate.hash.clone())
            .ok_or(RuntimeError::CertificateHashUnmatch)?;
        self.get_verifier()?
            .verify_multi_signature(
                &certificate.signed_message.as_bytes().to_vec(),
                &certificate.multi_signature,
                &certificate.aggregate_verification_key,
                &certificate.metadata.protocol_parameters,
            )
            .map_err(RuntimeError::Verifier)?;
        match certificate.previous_hash.as_str() {
            "" => {
                // Genesis Certificate
                // TODO: Verify the validity of the genesis certificate
                println!("Genesis certificate found and automatically considered as valid");
                Ok(None)
            }
            previous_hash if previous_hash == certificate.hash => {
                Err(RuntimeError::CertificateChainInfiniteLoop)
            }
            _ => {
                // Standard Certificate
                let previous_certificate = &self
                    .get_aggregator_handler()?
                    .get_certificate_details(&certificate.previous_hash)
                    .await
                    .map_err(RuntimeError::AggregatorHandler)?;
                match &previous_certificate.hash {
                    previous_hash if previous_hash != &certificate.previous_hash => {
                        Err(RuntimeError::CertificateChainPreviousHashUnmatch)
                    }
                    _ => {
                        match &previous_certificate
                            .protocol_message
                            .get_message_part(&ProtocolMessagePartKey::NextAggregateVerificationKey)
                        {
                            Some(next_aggregate_verification_key)
                                if *next_aggregate_verification_key
                                    == &certificate.aggregate_verification_key
                                    && previous_certificate.beacon.epoch
                                        != certificate.beacon.epoch =>
                            {
                                // Certificate has not the same epoch as previous certificate
                                Ok(Some(previous_certificate.to_owned()))
                            }
                            Some(_)
                                if previous_certificate.aggregate_verification_key
                                    == certificate.aggregate_verification_key
                                    && previous_certificate.beacon.epoch
                                        == certificate.beacon.epoch =>
                            {
                                // Certificate has the same epoch as previous certificate
                                Ok(Some(previous_certificate.to_owned()))
                            }
                            None => Ok(None),
                            _ => {
                                debug!("Previous certificate {:#?}", previous_certificate);
                                Err(RuntimeError::CertificateChainAVKUnmatch)
                            }
                        }
                    }
                }
            }
        }
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

    use crate::aggregator::{AggregatorHandlerError, MockAggregatorHandler};
    use crate::verifier::{MockVerifier, ProtocolError};
    use mithril_common::digesters::{Digester, DigesterError, DigesterResult};
    use mithril_common::fake_data;

    mock! {
       pub DigesterImpl { }

       #[async_trait]
       impl Digester for DigesterImpl {
           async fn compute_digest(&self) -> Result<DigesterResult, DigesterError>;
       }
    }

    fn create_fake_certificate_chain(total_certificates: u64) -> Vec<Certificate> {
        let certificates = (1..total_certificates + 1)
            .into_iter()
            .map(|i| {
                let digest = format!("digest{}", i);
                let certificate_hash = format!("certificate_hash-{}", i);
                let avk = format!("next_avk-{}", i);
                let next_avk = format!("next_avk-{}", i + 1);
                let mut fake_certificate = fake_data::certificate(certificate_hash.to_string());
                fake_certificate.beacon.epoch = i;
                fake_certificate.beacon.immutable_file_number = i * 10;
                fake_certificate
                    .protocol_message
                    .set_message_part(ProtocolMessagePartKey::SnapshotDigest, digest.to_string());
                fake_certificate.protocol_message.set_message_part(
                    ProtocolMessagePartKey::NextAggregateVerificationKey,
                    next_avk.to_string(),
                );
                fake_certificate.aggregate_verification_key = avk.to_string();
                fake_certificate.signed_message = fake_certificate.protocol_message.compute_hash();
                fake_certificate.previous_hash = "".to_string();
                match i {
                    1 => {
                        fake_certificate.multi_signature = "".to_string();
                        fake_certificate.genesis_signature = "genesis-signature".to_string()
                    }
                    _ => {
                        fake_certificate.multi_signature = format!("multi-sig-{}", i);
                        fake_certificate.genesis_signature = "".to_string()
                    }
                }
                fake_certificate
            })
            .collect::<Vec<Certificate>>();
        let mut certificates_new: Vec<Certificate> = Vec::new();
        certificates
            .iter()
            .enumerate()
            .for_each(|(i, certificate)| {
                let mut certificate_new = certificate.clone();
                if i > 0 {
                    if let Some(previous_certificate) = certificates_new.get(i - 1) {
                        certificate_new.previous_hash = previous_certificate.compute_hash();
                    }
                }
                certificate_new.hash = certificate_new.compute_hash();
                certificates_new.push(certificate_new.to_owned());
            });
        certificates_new.reverse();
        certificates_new
    }

    #[tokio::test]
    async fn test_list_snapshots_ok() {
        let network = "testnet".to_string();
        let fake_snapshots = fake_data::snapshots(5);
        let snapshot_list_items_expected = fake_snapshots
            .iter()
            .map(|snapshot| convert_to_list_item(snapshot, network.clone()))
            .collect::<Vec<SnapshotListItem>>();
        let mut mock_aggregator_handler = MockAggregatorHandler::new();
        mock_aggregator_handler
            .expect_list_snapshots()
            .return_once(move || Ok(fake_snapshots));
        let mut client = Runtime::new(network.clone());
        client.with_aggregator_handler(Box::new(mock_aggregator_handler));
        let snapshot_list_items = client.list_snapshots().await;
        snapshot_list_items.as_ref().expect("unexpected error");
        assert_eq!(
            snapshot_list_items.unwrap(),
            snapshot_list_items_expected.to_owned()
        );
    }

    #[tokio::test]
    async fn test_list_snapshots_ko() {
        let mut mock_aggregator_handler = MockAggregatorHandler::new();
        mock_aggregator_handler
            .expect_list_snapshots()
            .return_once(move || {
                Err(AggregatorHandlerError::RemoteServerTechnical(
                    "error occurred".to_string(),
                ))
            });
        let mut client = Runtime::new("testnet".to_string());
        client.with_aggregator_handler(Box::new(mock_aggregator_handler));
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
        let mut mock_aggregator_handler = MockAggregatorHandler::new();
        mock_aggregator_handler
            .expect_get_snapshot_details()
            .return_once(move |_| Ok(fake_snapshot));
        let mut client = Runtime::new("testnet".to_string());
        client.with_aggregator_handler(Box::new(mock_aggregator_handler));
        let snapshot_item = client.show_snapshot(digest).await;
        snapshot_item.as_ref().expect("unexpected error");
        assert_eq!(snapshot_item.unwrap(), snapshot_item_expected);
    }

    #[tokio::test]
    async fn test_show_snapshot_ko() {
        let digest = "digest123";
        let mut mock_aggregator_handler = MockAggregatorHandler::new();
        mock_aggregator_handler
            .expect_get_snapshot_details()
            .return_once(move |_| {
                Err(AggregatorHandlerError::RemoteServerTechnical(
                    "error occurred".to_string(),
                ))
            });
        let mut client = Runtime::new("testnet".to_string());
        client.with_aggregator_handler(Box::new(mock_aggregator_handler));
        let snapshot_item = client.show_snapshot(digest).await;
        assert!(
            matches!(snapshot_item, Err(RuntimeError::AggregatorHandler(_))),
            "unexpected error type: {:?}",
            snapshot_item
        );
    }

    #[tokio::test]
    async fn test_restore_snapshot_ok() {
        let total_certificates = 10;
        let fake_certificates = create_fake_certificate_chain(total_certificates);
        let digest_compute = fake_certificates[0]
            .protocol_message
            .get_message_part(&ProtocolMessagePartKey::SnapshotDigest)
            .unwrap()
            .to_owned();
        let digest_restore = digest_compute.clone();
        let fake_snapshot = fake_data::snapshots(1).first().unwrap().to_owned();
        let mut mock_aggregator_handler = MockAggregatorHandler::new();
        let mut mock_verifier = MockVerifier::new();
        let mut mock_digester = MockDigesterImpl::new();
        mock_aggregator_handler
            .expect_get_snapshot_details()
            .return_once(move |_| Ok(fake_snapshot));
        for fake_certificate in fake_certificates {
            mock_aggregator_handler
                .expect_get_certificate_details()
                .returning(move |_| Ok(fake_certificate.clone()))
                .times(1);
        }
        mock_aggregator_handler
            .expect_unpack_snapshot()
            .return_once(move |_| Ok("./target-dir".to_string()));
        mock_verifier
            .expect_verify_multi_signature()
            .returning(|_, _, _, _| Ok(()))
            .times(mockall::TimesRange::from(total_certificates as usize));
        mock_digester.expect_compute_digest().return_once(move || {
            Ok(DigesterResult {
                digest: digest_compute,
                last_immutable_file_number: 0,
            })
        });
        let mut client = Runtime::new("testnet".to_string());
        client
            .with_aggregator_handler(Box::new(mock_aggregator_handler))
            .with_verifier(Box::new(mock_verifier))
            .with_digester(Box::new(mock_digester));
        let restore = client.restore_snapshot(&digest_restore).await;
        restore.expect("unexpected error");
    }

    #[tokio::test]
    async fn test_restore_snapshot_ko_certificate_chain_previous_hash_unmatch() {
        let fake_certificates = create_fake_certificate_chain(3);
        let digest_compute = fake_certificates[0]
            .protocol_message
            .get_message_part(&ProtocolMessagePartKey::SnapshotDigest)
            .unwrap()
            .to_owned();
        let digest_restore = digest_compute.clone();
        let fake_certificate1 = fake_certificates[0].clone();
        let mut fake_certificate2 = fake_certificates[1].clone();
        fake_certificate2.previous_hash = "another-hash".to_string();
        fake_certificate2.hash = fake_certificate2.compute_hash();
        let fake_snapshot = fake_data::snapshots(1).first().unwrap().to_owned();
        let mut mock_aggregator_handler = MockAggregatorHandler::new();
        let mut mock_verifier = MockVerifier::new();
        let mut mock_digester = MockDigesterImpl::new();
        mock_aggregator_handler
            .expect_get_snapshot_details()
            .return_once(move |_| Ok(fake_snapshot));
        mock_aggregator_handler
            .expect_get_certificate_details()
            .returning(move |_| Ok(fake_certificate1.clone()))
            .times(1);
        mock_aggregator_handler
            .expect_get_certificate_details()
            .returning(move |_| Ok(fake_certificate2.clone()))
            .times(1);
        mock_aggregator_handler
            .expect_unpack_snapshot()
            .return_once(move |_| Ok("./target-dir".to_string()));
        mock_verifier
            .expect_verify_multi_signature()
            .returning(|_, _, _, _| Ok(()))
            .times(1);
        mock_digester.expect_compute_digest().return_once(move || {
            Ok(DigesterResult {
                digest: digest_compute,
                last_immutable_file_number: 0,
            })
        });
        let mut client = Runtime::new("testnet".to_string());
        client
            .with_aggregator_handler(Box::new(mock_aggregator_handler))
            .with_verifier(Box::new(mock_verifier))
            .with_digester(Box::new(mock_digester));
        let restore = client.restore_snapshot(&digest_restore).await;
        assert!(
            matches!(
                restore,
                Err(RuntimeError::CertificateChainPreviousHashUnmatch)
            ),
            "unexpected error type: {:?}",
            restore
        );
    }

    #[tokio::test]
    async fn test_restore_snapshot_ko_certificate_chain_avk_unmatch() {
        let fake_certificates = create_fake_certificate_chain(3);
        let digest_compute = fake_certificates[0]
            .protocol_message
            .get_message_part(&ProtocolMessagePartKey::SnapshotDigest)
            .unwrap()
            .to_owned();
        let digest_restore = digest_compute.clone();
        let fake_certificate1 = fake_certificates[0].clone();
        let mut fake_certificate2 = fake_certificates[1].clone();
        fake_certificate2.protocol_message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            "another-avk".to_string(),
        );
        let fake_snapshot = fake_data::snapshots(1).first().unwrap().to_owned();
        let mut mock_aggregator_handler = MockAggregatorHandler::new();
        let mut mock_verifier = MockVerifier::new();
        let mut mock_digester = MockDigesterImpl::new();
        mock_aggregator_handler
            .expect_get_snapshot_details()
            .return_once(move |_| Ok(fake_snapshot));
        mock_aggregator_handler
            .expect_get_certificate_details()
            .returning(move |_| Ok(fake_certificate1.clone()))
            .times(1);
        mock_aggregator_handler
            .expect_get_certificate_details()
            .returning(move |_| Ok(fake_certificate2.clone()))
            .times(1);
        mock_aggregator_handler
            .expect_unpack_snapshot()
            .return_once(move |_| Ok("./target-dir".to_string()));
        mock_verifier
            .expect_verify_multi_signature()
            .returning(|_, _, _, _| Ok(()))
            .times(1);
        mock_digester.expect_compute_digest().return_once(move || {
            Ok(DigesterResult {
                digest: digest_compute,
                last_immutable_file_number: 0,
            })
        });
        let mut client = Runtime::new("testnet".to_string());
        client
            .with_aggregator_handler(Box::new(mock_aggregator_handler))
            .with_verifier(Box::new(mock_verifier))
            .with_digester(Box::new(mock_digester));
        let restore = client.restore_snapshot(&digest_restore).await;
        assert!(
            matches!(restore, Err(RuntimeError::CertificateChainAVKUnmatch)),
            "unexpected error type: {:?}",
            restore
        );
    }

    #[tokio::test]
    async fn test_restore_snapshot_ko_certificate_hash_not_matching() {
        let fake_certificates = create_fake_certificate_chain(1);
        let digest_compute = fake_certificates[0]
            .protocol_message
            .get_message_part(&ProtocolMessagePartKey::SnapshotDigest)
            .unwrap()
            .to_owned();
        let digest_restore = digest_compute.clone();
        let mut fake_certificate1 = fake_certificates[0].clone();
        fake_certificate1.hash = "another-hash".to_string();
        let fake_snapshot = fake_data::snapshots(1).first().unwrap().to_owned();
        let mut mock_aggregator_handler = MockAggregatorHandler::new();
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
        mock_digester.expect_compute_digest().return_once(move || {
            Ok(DigesterResult {
                digest: digest_compute,
                last_immutable_file_number: 0,
            })
        });
        let mut client = Runtime::new("testnet".to_string());
        client
            .with_aggregator_handler(Box::new(mock_aggregator_handler))
            .with_verifier(Box::new(mock_verifier))
            .with_digester(Box::new(mock_digester));
        let restore = client.restore_snapshot(&digest_restore).await;
        assert!(
            matches!(restore, Err(RuntimeError::CertificateHashUnmatch)),
            "unexpected error type: {:?}",
            restore
        );
    }

    #[tokio::test]
    async fn test_restore_snapshot_ko_digests_not_matching() {
        let fake_certificates = create_fake_certificate_chain(1);
        let digest_compute = fake_certificates[0]
            .protocol_message
            .get_message_part(&ProtocolMessagePartKey::SnapshotDigest)
            .unwrap()
            .to_owned();
        let digest_restore = "digest-tampered".to_string();
        let mut fake_certificate1 = fake_certificates[0].clone();
        fake_certificate1.hash = "another-hash".to_string();
        let fake_snapshot = fake_data::snapshots(1).first().unwrap().to_owned();
        let mut mock_aggregator_handler = MockAggregatorHandler::new();
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
        mock_digester.expect_compute_digest().return_once(move || {
            Ok(DigesterResult {
                digest: digest_compute,
                last_immutable_file_number: 0,
            })
        });
        let mut client = Runtime::new("testnet".to_string());
        client
            .with_aggregator_handler(Box::new(mock_aggregator_handler))
            .with_verifier(Box::new(mock_verifier))
            .with_digester(Box::new(mock_digester));
        let restore = client.restore_snapshot(&digest_restore).await;
        assert!(
            matches!(restore, Err(RuntimeError::CertificateHashUnmatch)),
            "unexpected error type: {:?}",
            restore
        );
    }

    #[tokio::test]
    async fn test_restore_snapshot_ko_digester_error() {
        let fake_certificates = create_fake_certificate_chain(1);
        let digest_compute = fake_certificates[0]
            .protocol_message
            .get_message_part(&ProtocolMessagePartKey::SnapshotDigest)
            .unwrap()
            .to_owned();
        let digest_restore = digest_compute.clone();
        let mut fake_certificate1 = fake_certificates[0].clone();
        fake_certificate1.hash = "another-hash".to_string();
        let fake_snapshot = fake_data::snapshots(1).first().unwrap().to_owned();
        let mut mock_aggregator_handler = MockAggregatorHandler::new();
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
        mock_digester
            .expect_compute_digest()
            .return_once(|| Err(DigesterError::NotEnoughImmutable()));
        let mut client = Runtime::new("testnet".to_string());
        client
            .with_aggregator_handler(Box::new(mock_aggregator_handler))
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
        let mut mock_aggregator_handler = MockAggregatorHandler::new();
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
            .with_aggregator_handler(Box::new(mock_aggregator_handler))
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
        let mut mock_aggregator_handler = MockAggregatorHandler::new();
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
            .with_aggregator_handler(Box::new(mock_aggregator_handler))
            .with_verifier(Box::new(mock_verifier));
        let restore = client.restore_snapshot(digest).await;
        assert!(
            matches!(restore, Err(RuntimeError::AggregatorHandler(_))),
            "unexpected error type: {:?}",
            restore
        );
    }

    #[tokio::test]
    async fn test_restore_snapshot_ko_verify_multi_signature() {
        let fake_certificates = create_fake_certificate_chain(1);
        let digest_compute = fake_certificates[0]
            .protocol_message
            .get_message_part(&ProtocolMessagePartKey::SnapshotDigest)
            .unwrap()
            .to_owned();
        let digest_restore = digest_compute.clone();
        let fake_certificate1 = fake_certificates[0].clone();
        let fake_snapshot = fake_data::snapshots(1).first().unwrap().to_owned();
        let mut mock_aggregator_handler = MockAggregatorHandler::new();
        let mut mock_verifier = MockVerifier::new();
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
        mock_digester.expect_compute_digest().return_once(move || {
            Ok(DigesterResult {
                digest: digest_compute,
                last_immutable_file_number: 0,
            })
        });
        mock_verifier
            .expect_verify_multi_signature()
            .return_once(move |_, _, _, _| {
                Err(ProtocolError::VerifyMultiSignature(
                    "error occurred".to_string(),
                ))
            });
        let mut client = Runtime::new("testnet".to_string());
        client
            .with_aggregator_handler(Box::new(mock_aggregator_handler))
            .with_verifier(Box::new(mock_verifier))
            .with_digester(Box::new(mock_digester));
        let restore = client.restore_snapshot(&digest_restore).await;
        assert!(
            matches!(restore, Err(RuntimeError::Verifier(_))),
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
        let mut mock_aggregator_handler = MockAggregatorHandler::new();
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
            .with_aggregator_handler(Box::new(mock_aggregator_handler))
            .with_verifier(Box::new(mock_verifier));
        let restore = client.restore_snapshot(digest).await;
        assert!(
            matches!(restore, Err(RuntimeError::AggregatorHandler(_))),
            "unexpected error type: {:?}",
            restore
        );
    }
}

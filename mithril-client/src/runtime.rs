use log::debug;
use std::path::Path;
use std::str;
use thiserror::Error;

use crate::aggregator::{AggregatorHandler, AggregatorHandlerError};
use crate::entities::*;
use crate::verifier::{ProtocolError, Verifier};

use mithril_common::entities::Snapshot;
use mithril_common::immutable_digester::{ImmutableDigester, ImmutableDigesterError};

/// AggregatorHandlerWrapper wraps an AggregatorHandler
pub type AggregatorHandlerWrapper = Box<dyn AggregatorHandler>;

/// VerifierWrapper wraps a Verifier
pub type VerifierWrapper = Box<dyn Verifier>;

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
    ImmutableDigester(#[from] ImmutableDigesterError),

    #[error("digest error: '{0}'")]
    Digest(String),
}

/// Mithril client wrapper
pub struct Runtime {
    /// Cardano network
    pub network: String,

    /// Aggregator handler dependency that interacts with an aggregator
    aggregator_handler: Option<AggregatorHandlerWrapper>,

    /// Verifier dependency that verifies certificates and their multi signatures
    verifier: Option<VerifierWrapper>,
}

impl Runtime {
    /// Runtime factory
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
    pub async fn restore_snapshot(&self, digest: &str) -> Result<String, RuntimeError> {
        debug!("Restore snapshot {}", digest);
        let snapshot = &self
            .get_aggregator_handler()?
            .get_snapshot_details(digest)
            .await
            .map_err(RuntimeError::AggregatorHandler)?;
        let certificate = &self
            .get_aggregator_handler()?
            .get_certificate_details(&snapshot.certificate_hash)
            .await
            .map_err(RuntimeError::AggregatorHandler)?;
        self.get_verifier()?
            .verify_multi_signature(
                &digest.as_bytes().to_vec(),
                &certificate.multisignature,
                &certificate.aggregate_verification_key,
                &certificate.protocol_parameters,
            )
            .map_err(RuntimeError::Verifier)?;
        let unpacked_path = &self
            .get_aggregator_handler()?
            .unpack_snapshot(digest)
            .await
            .map_err(RuntimeError::AggregatorHandler)?;
        let unpacked_digest =
            ImmutableDigester::new(Path::new(unpacked_path).into(), slog_scope::logger())
                .compute_digest()
                .map_err(RuntimeError::ImmutableDigester)?
                .digest;
        match unpacked_digest == digest {
            true => Ok(unpacked_path.to_owned()),
            false => Err(RuntimeError::Digest(unpacked_digest)),
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

    use crate::aggregator::{AggregatorHandlerError, MockAggregatorHandler};
    use crate::verifier::{MockVerifier, ProtocolError};
    use mithril_common::fake_data;

    // TODO: Dulicated test setup code from 'mithril-common/src/immutable_digester.rs' that maybe should be set in a `tests_setup' module in the 'immutable_digester'. A mock would be even better!
    // Start duplicated code
    use std::fs;
    use std::fs::File;
    use std::io::prelude::*;
    use std::path::PathBuf;
    const ROOT_FOLDER: &str = "tests-work-folder/";

    fn get_test_dir(subdir_name: &str) -> PathBuf {
        let parent_dir = format!("{}{}", ROOT_FOLDER, subdir_name);
        let parent_dir = Path::new(&parent_dir).to_path_buf();

        if parent_dir.exists() {
            fs::remove_dir_all(&parent_dir)
                .expect(&*format!("Could not remove dir {:?}", parent_dir));
        }
        fs::create_dir_all(&parent_dir).expect(&*format!("Could not create dir {:?}", parent_dir));

        parent_dir
    }

    fn create_fake_files(parent_dir: &Path, child_filenames: &[&str]) {
        for filename in child_filenames {
            let file = parent_dir.join(Path::new(filename));
            let mut source_file = File::create(&file).unwrap();
            write!(source_file, "This is a test file named '{}'", filename).unwrap();
        }
    }
    // End duplicated code

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
            snapshot_list_items.is_err(),
            "an error should have occurred"
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
        assert!(snapshot_item.is_err(), "an error should have occurred");
    }

    #[tokio::test]
    async fn test_restore_snapshot_ok() {
        let target_dir = get_test_dir("test_restore_snapshot_ok/immutable");
        let entries = vec![
            "001.chunk",
            "001.primary",
            "001.secondary",
            "002.chunk",
            "002.primary",
            "002.secondary",
        ];
        create_fake_files(&target_dir, &entries);
        let digest = ImmutableDigester::new(target_dir.clone().into(), slog_scope::logger())
            .compute_digest()
            .unwrap()
            .digest;
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
            .return_once(move |_| Ok(target_dir.to_str().unwrap().to_owned()));
        mock_verifier
            .expect_verify_multi_signature()
            .return_once(|_, _, _, _| Ok(()));
        let mut client = Runtime::new("testnet".to_string());
        client
            .with_aggregator_handler(Box::new(mock_aggregator_handler))
            .with_verifier(Box::new(mock_verifier));
        let restore = client.restore_snapshot(&digest).await;
        restore.expect("unexpected error");
    }

    #[tokio::test]
    async fn test_restore_snapshot_ko_digests_not_matching() {
        let target_dir = get_test_dir("test_restore_snapshot_ko_digests_not_matching/immutable");
        let entries = vec![
            "001.chunk",
            "001.primary",
            "001.secondary",
            "002.chunk",
            "002.primary",
            "002.secondary",
        ];
        create_fake_files(&target_dir, &entries);
        let digest = "digest-not-matching";
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
            .return_once(move |_| Ok(target_dir.to_str().unwrap().to_owned()));
        mock_verifier
            .expect_verify_multi_signature()
            .return_once(|_, _, _, _| Ok(()));
        let mut client = Runtime::new("testnet".to_string());
        client
            .with_aggregator_handler(Box::new(mock_aggregator_handler))
            .with_verifier(Box::new(mock_verifier));
        let restore = client.restore_snapshot(digest).await;
        assert!(restore.is_err(), "an error should have occurred");
    }

    #[tokio::test]
    async fn test_restore_snapshot_ko_digester_error() {
        let target_dir = get_test_dir("test_restore_snapshot_ko_digester_error/immutable");
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
            .return_once(move |_| Ok(target_dir.to_str().unwrap().to_owned()));
        mock_verifier
            .expect_verify_multi_signature()
            .return_once(|_, _, _, _| Ok(()));
        let mut client = Runtime::new("testnet".to_string());
        client
            .with_aggregator_handler(Box::new(mock_aggregator_handler))
            .with_verifier(Box::new(mock_verifier));
        let restore = client.restore_snapshot(digest).await;
        assert!(restore.is_err(), "an error should have occurred");
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
        assert!(restore.is_err(), "an error should have occurred");
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
        assert!(restore.is_err(), "an error should have occurred");
    }

    #[tokio::test]
    async fn test_restore_snapshot_ko_verify_multi_signature() {
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
            .with_verifier(Box::new(mock_verifier));
        let restore = client.restore_snapshot(digest).await;
        assert!(restore.is_err(), "an error should have occurred");
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
        assert!(restore.is_err(), "an error should have occurred");
    }
}

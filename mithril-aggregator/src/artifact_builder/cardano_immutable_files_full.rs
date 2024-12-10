use anyhow::Context;
use async_trait::async_trait;
use semver::Version;
use slog::{debug, warn, Logger};
use std::sync::Arc;
use thiserror::Error;

use crate::{
    file_uploaders::FileLocation, snapshotter::OngoingSnapshot, FileUploader, Snapshotter,
};

use super::ArtifactBuilder;
use mithril_common::logging::LoggerExtensions;
use mithril_common::{
    entities::{
        CardanoDbBeacon, Certificate, CompressionAlgorithm, ProtocolMessagePartKey, Snapshot,
    },
    CardanoNetwork, StdResult,
};

/// [CardanoImmutableFilesFullArtifact] error
/// to fail.
#[derive(Debug, Error)]
pub enum CardanoImmutableFilesFullArtifactError {
    /// Protocol message part is missing
    #[error("Missing protocol message for beacon: '{0}'.")]
    MissingProtocolMessage(CardanoDbBeacon),
}

/// A [CardanoImmutableFilesFullArtifact] builder
pub struct CardanoImmutableFilesFullArtifactBuilder {
    cardano_network: CardanoNetwork,
    cardano_node_version: Version,
    snapshotter: Arc<dyn Snapshotter>,
    snapshot_uploader: Arc<dyn FileUploader>,
    compression_algorithm: CompressionAlgorithm,
    logger: Logger,
}

impl CardanoImmutableFilesFullArtifactBuilder {
    /// CardanoImmutableFilesFull artifact builder factory
    pub fn new(
        cardano_network: CardanoNetwork,
        cardano_node_version: &Version,
        snapshotter: Arc<dyn Snapshotter>,
        snapshot_uploader: Arc<dyn FileUploader>,
        compression_algorithm: CompressionAlgorithm,
        logger: Logger,
    ) -> Self {
        Self {
            cardano_network,
            cardano_node_version: cardano_node_version.clone(),
            snapshotter,
            snapshot_uploader,
            compression_algorithm,
            logger: logger.new_with_component_name::<Self>(),
        }
    }

    async fn create_snapshot_archive(
        &self,
        beacon: &CardanoDbBeacon,
        snapshot_digest: &str,
    ) -> StdResult<OngoingSnapshot> {
        debug!(self.logger, ">> create_snapshot_archive");

        let snapshotter = self.snapshotter.clone();
        let snapshot_name = format!(
            "{}-e{}-i{}.{}.{}",
            self.cardano_network,
            *beacon.epoch,
            beacon.immutable_file_number,
            snapshot_digest,
            self.compression_algorithm.tar_file_extension()
        );
        // spawn a separate thread to prevent blocking
        let ongoing_snapshot =
            tokio::task::spawn_blocking(move || -> StdResult<OngoingSnapshot> {
                snapshotter.snapshot(&snapshot_name)
            })
            .await??;

        debug!(self.logger, " > Snapshot created: '{ongoing_snapshot:?}'");

        Ok(ongoing_snapshot)
    }

    async fn upload_snapshot_archive(
        &self,
        ongoing_snapshot: &OngoingSnapshot,
    ) -> StdResult<Vec<FileLocation>> {
        debug!(self.logger, ">> upload_snapshot_archive");
        let location = self
            .snapshot_uploader
            .upload(ongoing_snapshot.get_file_path())
            .await;

        if let Err(error) = tokio::fs::remove_file(ongoing_snapshot.get_file_path()).await {
            warn!(
                self.logger, " > Post upload ongoing snapshot file removal failure";
                "error" => error
            );
        }

        Ok(vec![location?])
    }

    async fn create_snapshot(
        &self,
        beacon: CardanoDbBeacon,
        ongoing_snapshot: &OngoingSnapshot,
        snapshot_digest: String,
        remote_locations: Vec<String>,
    ) -> StdResult<Snapshot> {
        debug!(self.logger, ">> create_snapshot");

        let snapshot = Snapshot::new(
            snapshot_digest,
            self.cardano_network,
            beacon,
            *ongoing_snapshot.get_file_size(),
            remote_locations,
            self.compression_algorithm,
            &self.cardano_node_version,
        );

        Ok(snapshot)
    }
}

#[async_trait]
impl ArtifactBuilder<CardanoDbBeacon, Snapshot> for CardanoImmutableFilesFullArtifactBuilder {
    async fn compute_artifact(
        &self,
        beacon: CardanoDbBeacon,
        certificate: &Certificate,
    ) -> StdResult<Snapshot> {
        let snapshot_digest = certificate
            .protocol_message
            .get_message_part(&ProtocolMessagePartKey::SnapshotDigest)
            .ok_or_else(|| {
                CardanoImmutableFilesFullArtifactError::MissingProtocolMessage(beacon.clone())
            })?
            .to_owned();

        let ongoing_snapshot = self
            .create_snapshot_archive(&beacon, &snapshot_digest)
            .await
            .with_context(|| {
                "Cardano Immutable Files Full Artifact Builder can not create snapshot archive"
            })?;
        let locations = self
            .upload_snapshot_archive(&ongoing_snapshot)
            .await
            .with_context(|| {
                format!("Cardano Immutable Files Full Artifact Builder can not upload snapshot archive to path: '{:?}'", ongoing_snapshot.get_file_path())
            })?;

        let snapshot = self
            .create_snapshot(beacon, &ongoing_snapshot, snapshot_digest, locations)
            .await?;

        Ok(snapshot)
    }
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;
    use std::path::Path;
    use tempfile::NamedTempFile;

    use mithril_common::{entities::CompressionAlgorithm, test_utils::fake_data};

    use crate::{
        file_uploaders::MockFileUploader, test_tools::TestLogger, DumbFileUploader, DumbSnapshotter,
    };

    use super::*;

    #[tokio::test]
    async fn should_compute_valid_artifact() {
        let beacon = fake_data::beacon();
        let certificate = fake_data::certificate("certificate-123".to_string());
        let snapshot_digest = certificate
            .protocol_message
            .get_message_part(&ProtocolMessagePartKey::SnapshotDigest)
            .unwrap();

        let dumb_snapshotter = Arc::new(DumbSnapshotter::new());
        let dumb_snapshot_uploader = Arc::new(DumbFileUploader::new());

        let cardano_immutable_files_full_artifact_builder =
            CardanoImmutableFilesFullArtifactBuilder::new(
                fake_data::network(),
                &Version::parse("1.0.0").unwrap(),
                dumb_snapshotter.clone(),
                dumb_snapshot_uploader.clone(),
                CompressionAlgorithm::Zstandard,
                TestLogger::stdout(),
            );
        let artifact = cardano_immutable_files_full_artifact_builder
            .compute_artifact(beacon.clone(), &certificate)
            .await
            .unwrap();
        let last_ongoing_snapshot = dumb_snapshotter
            .get_last_snapshot()
            .unwrap()
            .expect("A snapshot should have been 'created'");

        let remote_locations = vec![dumb_snapshot_uploader
            .get_last_upload()
            .unwrap()
            .expect("A snapshot should have been 'uploaded'")];
        let artifact_expected = Snapshot::new(
            snapshot_digest.to_owned(),
            fake_data::network(),
            beacon,
            *last_ongoing_snapshot.get_file_size(),
            remote_locations,
            CompressionAlgorithm::Zstandard,
            &Version::parse("1.0.0").unwrap(),
        );
        assert_eq!(artifact_expected, artifact);
    }

    #[tokio::test]
    async fn remove_snapshot_archive_after_upload() {
        let file = NamedTempFile::new().unwrap();
        let file_path = file.path();
        let snapshot = OngoingSnapshot::new(file_path.to_path_buf(), 7331);

        let cardano_immutable_files_full_artifact_builder =
            CardanoImmutableFilesFullArtifactBuilder::new(
                fake_data::network(),
                &Version::parse("1.0.0").unwrap(),
                Arc::new(DumbSnapshotter::new()),
                Arc::new(DumbFileUploader::new()),
                CompressionAlgorithm::default(),
                TestLogger::stdout(),
            );

        cardano_immutable_files_full_artifact_builder
            .upload_snapshot_archive(&snapshot)
            .await
            .expect("Snapshot upload should not fail");

        assert!(
            !file_path.exists(),
            "Ongoing snapshot file should have been removed after upload"
        );
    }

    #[tokio::test]
    async fn snapshot_archive_name_after_beacon_values() {
        let network = fake_data::network();
        let beacon = CardanoDbBeacon::new(20, 145);
        let digest = "test+digest";

        let cardano_immutable_files_full_artifact_builder =
            CardanoImmutableFilesFullArtifactBuilder::new(
                network,
                &Version::parse("1.0.0").unwrap(),
                Arc::new(DumbSnapshotter::new()),
                Arc::new(DumbFileUploader::new()),
                CompressionAlgorithm::Gzip,
                TestLogger::stdout(),
            );

        let ongoing_snapshot = cardano_immutable_files_full_artifact_builder
            .create_snapshot_archive(&beacon, digest)
            .await
            .expect("create_snapshot_archive should not fail");

        assert_eq!(
            Path::new(&format!(
                "{}-e{}-i{}.{digest}.tar.gz",
                network, *beacon.epoch, beacon.immutable_file_number,
            )),
            ongoing_snapshot.get_file_path()
        );
    }

    #[tokio::test]
    async fn snapshot_archive_name_after_compression_algorithm() {
        let mut invalid_result: Vec<CompressionAlgorithm> = vec![];

        for algorithm in CompressionAlgorithm::list() {
            let cardano_immutable_files_full_artifact_builder =
                CardanoImmutableFilesFullArtifactBuilder::new(
                    fake_data::network(),
                    &Version::parse("1.0.0").unwrap(),
                    Arc::new(DumbSnapshotter::new()),
                    Arc::new(DumbFileUploader::new()),
                    algorithm,
                    TestLogger::stdout(),
                );

            let ongoing_snapshot = cardano_immutable_files_full_artifact_builder
                .create_snapshot_archive(&CardanoDbBeacon::default(), "test+digest")
                .await
                .expect("create_snapshot_archive should not fail");
            let file_name = ongoing_snapshot
                .get_file_path()
                .file_name()
                .and_then(|f| f.to_str())
                .unwrap();
            let expected_extension = algorithm.tar_file_extension();

            if !file_name.ends_with(&expected_extension) {
                invalid_result.push(algorithm);
            }
        }

        assert!(
            invalid_result.is_empty(),
            "Archive name did not contain some algorithms extension after snapshot, failing algorithm(s): {invalid_result:?}",
        );
    }

    #[tokio::test]
    async fn remove_snapshot_archive_after_upload_even_if_an_error_occurred() {
        let file = NamedTempFile::new().unwrap();
        let file_path = file.path();
        let snapshot = OngoingSnapshot::new(file_path.to_path_buf(), 7331);
        let mut snapshot_uploader = MockFileUploader::new();
        snapshot_uploader
            .expect_upload()
            .return_once(|_| Err(anyhow!("an error")))
            .once();

        let cardano_immutable_files_full_artifact_builder =
            CardanoImmutableFilesFullArtifactBuilder::new(
                fake_data::network(),
                &Version::parse("1.0.0").unwrap(),
                Arc::new(DumbSnapshotter::new()),
                Arc::new(snapshot_uploader),
                CompressionAlgorithm::default(),
                TestLogger::stdout(),
            );

        cardano_immutable_files_full_artifact_builder
            .upload_snapshot_archive(&snapshot)
            .await
            .expect_err("Snapshot upload should have failed");

        assert!(
            !file_path.exists(),
            "Ongoing snapshot file should have been removed even after upload failure"
        );
    }
}

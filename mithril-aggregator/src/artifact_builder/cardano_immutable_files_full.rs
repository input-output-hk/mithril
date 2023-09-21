use anyhow::Context;
use async_trait::async_trait;
use semver::Version;
use slog_scope::{debug, warn};
use std::sync::Arc;
use thiserror::Error;

use crate::{
    snapshot_uploaders::SnapshotLocation, snapshotter::OngoingSnapshot, SnapshotUploader,
    Snapshotter,
};

use super::ArtifactBuilder;
use mithril_common::{
    entities::{
        Beacon, Certificate, CompressionAlgorithm, ProtocolMessage, ProtocolMessagePartKey,
        Snapshot,
    },
    StdResult,
};

/// [CardanoImmutableFilesFullArtifact] error
/// to fail.
#[derive(Debug, Error)]
pub enum CardanoImmutableFilesFullArtifactError {
    /// Protocol message part is missing
    #[error("Missing protocol message for beacon: '{0}'.")]
    MissingProtocolMessage(Beacon),
}

/// A [CardanoImmutableFilesFullArtifact] builder
pub struct CardanoImmutableFilesFullArtifactBuilder {
    cardano_node_version: Version,
    snapshotter: Arc<dyn Snapshotter>,
    snapshot_uploader: Arc<dyn SnapshotUploader>,
    compression_algorithm: CompressionAlgorithm,
}

impl CardanoImmutableFilesFullArtifactBuilder {
    /// CardanoImmutableFilesFull artifact builder factory
    pub fn new(
        cardano_node_version: &Version,
        snapshotter: Arc<dyn Snapshotter>,
        snapshot_uploader: Arc<dyn SnapshotUploader>,
        compression_algorithm: CompressionAlgorithm,
    ) -> Self {
        Self {
            cardano_node_version: cardano_node_version.clone(),
            snapshotter,
            snapshot_uploader,
            compression_algorithm,
        }
    }

    async fn create_snapshot_archive(
        &self,
        beacon: &Beacon,
        protocol_message: &ProtocolMessage,
    ) -> StdResult<OngoingSnapshot> {
        debug!("CardanoImmutableFilesFullArtifactBuilder: create snapshot archive");

        let snapshotter = self.snapshotter.clone();
        let snapshot_digest = protocol_message
            .get_message_part(&ProtocolMessagePartKey::SnapshotDigest)
            .ok_or_else(|| {
                CardanoImmutableFilesFullArtifactError::MissingProtocolMessage(beacon.clone())
            })?;
        let snapshot_name = format!(
            "{}-e{}-i{}.{}.{}",
            beacon.network,
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

        debug!(" > snapshot created: '{:?}'", ongoing_snapshot);

        Ok(ongoing_snapshot)
    }

    async fn upload_snapshot_archive(
        &self,
        ongoing_snapshot: &OngoingSnapshot,
    ) -> StdResult<Vec<SnapshotLocation>> {
        debug!("CardanoImmutableFilesFullArtifactBuilder: upload snapshot archive");
        let location = self
            .snapshot_uploader
            .upload_snapshot(ongoing_snapshot.get_file_path())
            .await;

        if let Err(error) = tokio::fs::remove_file(ongoing_snapshot.get_file_path()).await {
            warn!(
                " > Post upload ongoing snapshot file removal failure: {}",
                error
            );
        }

        Ok(vec![location?])
    }

    async fn create_snapshot(
        &self,
        certificate: &Certificate,
        ongoing_snapshot: &OngoingSnapshot,
        remote_locations: Vec<String>,
    ) -> StdResult<Snapshot> {
        debug!("CardanoImmutableFilesFullArtifactBuilder: create snapshot");
        let snapshot_digest = certificate
            .protocol_message
            .get_message_part(&ProtocolMessagePartKey::SnapshotDigest)
            .ok_or_else(|| {
                CardanoImmutableFilesFullArtifactError::MissingProtocolMessage(
                    certificate.beacon.clone(),
                )
            })?
            .to_owned();
        let snapshot = Snapshot::new(
            snapshot_digest,
            certificate.beacon.to_owned(),
            *ongoing_snapshot.get_file_size(),
            remote_locations,
            self.compression_algorithm,
            &self.cardano_node_version,
        );

        Ok(snapshot)
    }
}

#[async_trait]
impl ArtifactBuilder<Beacon, Snapshot> for CardanoImmutableFilesFullArtifactBuilder {
    async fn compute_artifact(
        &self,
        beacon: Beacon,
        certificate: &Certificate,
    ) -> StdResult<Snapshot> {
        let ongoing_snapshot = self
            .create_snapshot_archive(&beacon, &certificate.protocol_message)
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
            .create_snapshot(certificate, &ongoing_snapshot, locations)
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

    use super::*;

    use crate::{snapshot_uploaders::MockSnapshotUploader, DumbSnapshotUploader, DumbSnapshotter};

    #[tokio::test]
    async fn should_compute_valid_artifact() {
        let beacon = fake_data::beacon();
        let certificate = fake_data::certificate("certificate-123".to_string());
        let snapshot_digest = certificate
            .protocol_message
            .get_message_part(&ProtocolMessagePartKey::SnapshotDigest)
            .unwrap();

        let dumb_snapshotter = Arc::new(DumbSnapshotter::new());
        let dumb_snapshot_uploader = Arc::new(DumbSnapshotUploader::new());

        let cardano_immutable_files_full_artifact_builder =
            CardanoImmutableFilesFullArtifactBuilder::new(
                &Version::parse("1.0.0").unwrap(),
                dumb_snapshotter.clone(),
                dumb_snapshot_uploader.clone(),
                CompressionAlgorithm::Zstandard,
            );
        let artifact = cardano_immutable_files_full_artifact_builder
            .compute_artifact(beacon, &certificate)
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
            certificate.beacon.to_owned(),
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
                &Version::parse("1.0.0").unwrap(),
                Arc::new(DumbSnapshotter::new()),
                Arc::new(DumbSnapshotUploader::new()),
                CompressionAlgorithm::default(),
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
        let beacon = Beacon::new("network".to_string(), 20, 145);
        let mut message = ProtocolMessage::new();
        message.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            "test+digest".to_string(),
        );

        let cardano_immutable_files_full_artifact_builder =
            CardanoImmutableFilesFullArtifactBuilder::new(
                &Version::parse("1.0.0").unwrap(),
                Arc::new(DumbSnapshotter::new()),
                Arc::new(DumbSnapshotUploader::new()),
                CompressionAlgorithm::Gzip,
            );

        let ongoing_snapshot = cardano_immutable_files_full_artifact_builder
            .create_snapshot_archive(&beacon, &message)
            .await
            .expect("create_snapshot_archive should not fail");

        assert_eq!(
            Path::new(&format!(
                "{}-e{}-i{}.{}.tar.gz",
                beacon.network, *beacon.epoch, beacon.immutable_file_number, "test+digest"
            )),
            ongoing_snapshot.get_file_path()
        );
    }

    #[tokio::test]
    async fn snapshot_archive_name_after_compression_algorithm() {
        let mut message = ProtocolMessage::new();
        message.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            "test+digest".to_string(),
        );

        let mut invalid_result: Vec<CompressionAlgorithm> = vec![];

        for algorithm in CompressionAlgorithm::list() {
            let cardano_immutable_files_full_artifact_builder =
                CardanoImmutableFilesFullArtifactBuilder::new(
                    &Version::parse("1.0.0").unwrap(),
                    Arc::new(DumbSnapshotter::new()),
                    Arc::new(DumbSnapshotUploader::new()),
                    algorithm,
                );

            let ongoing_snapshot = cardano_immutable_files_full_artifact_builder
                .create_snapshot_archive(&Beacon::default(), &message)
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
    async fn remove_snapshot_archive_after_upload_even_if_an_error_occured() {
        let file = NamedTempFile::new().unwrap();
        let file_path = file.path();
        let snapshot = OngoingSnapshot::new(file_path.to_path_buf(), 7331);
        let mut snapshot_uploader = MockSnapshotUploader::new();
        snapshot_uploader
            .expect_upload_snapshot()
            .return_once(|_| Err(anyhow!("an error")))
            .once();

        let cardano_immutable_files_full_artifact_builder =
            CardanoImmutableFilesFullArtifactBuilder::new(
                &Version::parse("1.0.0").unwrap(),
                Arc::new(DumbSnapshotter::new()),
                Arc::new(snapshot_uploader),
                CompressionAlgorithm::default(),
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

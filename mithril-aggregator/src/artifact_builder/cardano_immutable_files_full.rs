use anyhow::Context;
use async_trait::async_trait;
use mithril_common::entities::{FileUri, ImmutableFileNumber};
use semver::Version;
use slog::{Logger, debug, warn};
use std::sync::Arc;
use thiserror::Error;

use mithril_common::logging::LoggerExtensions;
use mithril_common::{
    CardanoNetwork, StdResult,
    entities::{CardanoDbBeacon, Certificate, ProtocolMessagePartKey, Snapshot},
};

use super::ArtifactBuilder;
use crate::{FileUploader, services::Snapshotter, tools::file_archiver::FileArchive};

/// [CardanoImmutableFilesFullArtifact] error
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
    logger: Logger,
}

impl CardanoImmutableFilesFullArtifactBuilder {
    /// CardanoImmutableFilesFull artifact builder factory
    pub fn new(
        cardano_network: CardanoNetwork,
        cardano_node_version: &Version,
        snapshotter: Arc<dyn Snapshotter>,
        snapshot_uploader: Arc<dyn FileUploader>,
        logger: Logger,
    ) -> Self {
        Self {
            cardano_network,
            cardano_node_version: cardano_node_version.clone(),
            snapshotter,
            snapshot_uploader,
            logger: logger.new_with_component_name::<Self>(),
        }
    }

    async fn create_immutables_snapshot_archive(
        &self,
        base_file_name_without_extension: &str,
    ) -> StdResult<FileArchive> {
        debug!(self.logger, ">> create_immutables_snapshot_archive");

        let snapshotter = self.snapshotter.clone();
        let snapshot_name = base_file_name_without_extension.to_owned();
        let ongoing_snapshot =
            snapshotter.snapshot_all_completed_immutables(&snapshot_name).await?;

        debug!(
            self.logger,
            " > Immutables snapshot created: '{ongoing_snapshot:?}'"
        );

        Ok(ongoing_snapshot)
    }

    async fn create_ancillary_snapshot_archive(
        &self,
        immutable_file_number: ImmutableFileNumber,
        base_file_name_without_extension: &str,
    ) -> StdResult<FileArchive> {
        debug!(self.logger, ">> create_ancillary_snapshot_archive");

        let snapshotter = self.snapshotter.clone();
        let snapshot_name = format!("{base_file_name_without_extension}.ancillary");
        let ongoing_snapshot = snapshotter
            .snapshot_ancillary(immutable_file_number, &snapshot_name)
            .await?;

        debug!(
            self.logger,
            " > Ancillary snapshot created: '{ongoing_snapshot:?}'"
        );

        Ok(ongoing_snapshot)
    }

    async fn upload_snapshot_archive(
        &self,
        ongoing_snapshot: &FileArchive,
    ) -> StdResult<Vec<FileUri>> {
        debug!(self.logger, ">> upload_snapshot_archive");
        let location = self.snapshot_uploader.upload(ongoing_snapshot.get_file_path()).await;

        if let Err(error) = tokio::fs::remove_file(ongoing_snapshot.get_file_path()).await {
            warn!(
                self.logger, " > Post upload ongoing snapshot file removal failure";
                "error" => error
            );
        }

        Ok(vec![location?])
    }

    fn create_snapshot(
        &self,
        beacon: CardanoDbBeacon,
        ongoing_immutables_snapshot: &FileArchive,
        ongoing_ancillary_snapshot: &FileArchive,
        snapshot_digest: String,
        immutables_remote_locations: Vec<String>,
        ancillary_remote_locations: Vec<String>,
    ) -> StdResult<Snapshot> {
        debug!(self.logger, ">> create_snapshot");

        let snapshot = Snapshot {
            digest: snapshot_digest,
            network: self.cardano_network.into(),
            beacon,
            size: ongoing_immutables_snapshot.get_archive_size(),
            ancillary_size: Some(ongoing_ancillary_snapshot.get_archive_size()),
            locations: immutables_remote_locations,
            ancillary_locations: Some(ancillary_remote_locations),
            compression_algorithm: ongoing_immutables_snapshot.get_compression_algorithm(),
            cardano_node_version: self.cardano_node_version.to_string(),
        };

        Ok(snapshot)
    }

    fn get_base_file_name_without_extension(
        &self,
        beacon: &CardanoDbBeacon,
        snapshot_digest: &str,
    ) -> String {
        let snapshot_name = format!(
            "{}-e{}-i{}.{}",
            self.cardano_network, *beacon.epoch, beacon.immutable_file_number, snapshot_digest,
        );
        snapshot_name
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

        let base_file_name_without_extension =
            self.get_base_file_name_without_extension(&beacon, &snapshot_digest);
        let ongoing_immutables_snapshot = self
            .create_immutables_snapshot_archive(&base_file_name_without_extension)
            .await
            .with_context(|| {
                "Cardano Immutable Files Full Artifact Builder can not create immutables snapshot archive"
            })?;
        let ongoing_ancillary_snapshot = self
            .create_ancillary_snapshot_archive(beacon.immutable_file_number, &base_file_name_without_extension)
            .await
            .with_context(|| {
                "Cardano Immutable Files Full Artifact Builder can not create ancillary snapshot archive"
            })?;
        let immutables_locations = self
            .upload_snapshot_archive(&ongoing_immutables_snapshot)
            .await
            .with_context(|| {
                format!("Cardano Immutable Files Full Artifact Builder can not upload immutables snapshot archive to path: '{:?}'", ongoing_immutables_snapshot.get_file_path())
            })?;
        let ancillary_locations = self
            .upload_snapshot_archive(&ongoing_ancillary_snapshot)
            .await
            .with_context(|| {
                format!("Cardano Immutable Files Full Artifact Builder can not upload ancillary snapshot archive to path: '{:?}'", ongoing_ancillary_snapshot.get_file_path())
            })?;

        let snapshot = self.create_snapshot(
            beacon,
            &ongoing_immutables_snapshot,
            &ongoing_ancillary_snapshot,
            snapshot_digest,
            immutables_locations.into_iter().map(Into::into).collect(),
            ancillary_locations.into_iter().map(Into::into).collect(),
        )?;

        Ok(snapshot)
    }
}

#[cfg(test)]
mod tests {
    use anyhow::anyhow;
    use tempfile::NamedTempFile;

    use mithril_common::{entities::CompressionAlgorithm, test::double::fake_data};

    use crate::{
        DumbUploader, file_uploaders::MockFileUploader, services::DumbSnapshotter,
        test_tools::TestLogger,
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
        let individual_snapshot_archive_size = 7331;

        let dumb_snapshotter = Arc::new(
            DumbSnapshotter::new(CompressionAlgorithm::Zstandard)
                .with_archive_size(individual_snapshot_archive_size),
        );
        let dumb_snapshot_uploader = Arc::new(DumbUploader::default());

        let cardano_immutable_files_full_artifact_builder =
            CardanoImmutableFilesFullArtifactBuilder::new(
                fake_data::network(),
                &Version::parse("1.0.0").unwrap(),
                dumb_snapshotter.clone(),
                dumb_snapshot_uploader.clone(),
                TestLogger::stdout(),
            );
        let artifact = cardano_immutable_files_full_artifact_builder
            .compute_artifact(beacon.clone(), &certificate)
            .await
            .unwrap();

        let last_uploads = dumb_snapshot_uploader.get_last_n_uploads(2).unwrap();
        let [ancillary_location, immutables_location, ..] = last_uploads.as_slice() else {
            panic!("Two snapshots should have been 'uploaded'");
        };
        let artifact_expected = Snapshot {
            digest: snapshot_digest.to_owned(),
            network: fake_data::network().into(),
            beacon,
            size: individual_snapshot_archive_size,
            ancillary_size: Some(individual_snapshot_archive_size),
            locations: vec![immutables_location.into()],
            ancillary_locations: Some(vec![ancillary_location.into()]),
            compression_algorithm: CompressionAlgorithm::Zstandard,
            cardano_node_version: "1.0.0".to_string(),
        };

        assert_eq!(artifact_expected, artifact);
    }

    #[tokio::test]
    async fn snapshot_archive_name_include_beacon_and_network_values() {
        let network = fake_data::network();
        let beacon = CardanoDbBeacon::new(20, 145);
        let digest = "test+digest";

        let cardano_immutable_files_full_artifact_builder =
            CardanoImmutableFilesFullArtifactBuilder::new(
                network,
                &Version::parse("1.0.0").unwrap(),
                Arc::new(DumbSnapshotter::default()),
                Arc::new(DumbUploader::default()),
                TestLogger::stdout(),
            );

        let name = cardano_immutable_files_full_artifact_builder
            .get_base_file_name_without_extension(&beacon, digest);

        assert_eq!(
            format!(
                "{}-e{}-i{}.{digest}",
                network, *beacon.epoch, beacon.immutable_file_number,
            ),
            name
        );
    }

    #[tokio::test]
    async fn remove_snapshot_archive_after_upload() {
        let file = NamedTempFile::new().unwrap();
        let file_path = file.path();
        let snapshot = FileArchive::new(
            file_path.to_path_buf(),
            7331,
            7331,
            CompressionAlgorithm::default(),
        );

        let cardano_immutable_files_full_artifact_builder =
            CardanoImmutableFilesFullArtifactBuilder::new(
                fake_data::network(),
                &Version::parse("1.0.0").unwrap(),
                Arc::new(DumbSnapshotter::default()),
                Arc::new(DumbUploader::default()),
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
    async fn remove_snapshot_archive_after_upload_even_if_an_error_occurred() {
        let file = NamedTempFile::new().unwrap();
        let file_path = file.path();
        let snapshot = FileArchive::new(
            file_path.to_path_buf(),
            7331,
            7331,
            CompressionAlgorithm::default(),
        );
        let mut snapshot_uploader = MockFileUploader::new();
        snapshot_uploader
            .expect_upload()
            .return_once(|_| Err(anyhow!("an error")))
            .once();

        let cardano_immutable_files_full_artifact_builder =
            CardanoImmutableFilesFullArtifactBuilder::new(
                fake_data::network(),
                &Version::parse("1.0.0").unwrap(),
                Arc::new(DumbSnapshotter::default()),
                Arc::new(snapshot_uploader),
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

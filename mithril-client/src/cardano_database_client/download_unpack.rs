use std::collections::BTreeSet;
use std::ops::RangeInclusive;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tokio::task::JoinSet;

use anyhow::anyhow;

use mithril_common::{
    entities::{AncillaryLocation, CompressionAlgorithm, ImmutableFileNumber, ImmutablesLocation},
    messages::CardanoDatabaseSnapshotMessage,
};

use crate::feedback::{MithrilEvent, MithrilEventCardanoDatabase};
use crate::file_downloader::{DownloadEvent, FileDownloader, FileDownloaderUri};
use crate::MithrilResult;

use super::api::CardanoDatabaseClient;
use super::immutable_file_range::ImmutableFileRange;

/// Options for downloading and unpacking a Cardano database
#[derive(Debug, Default)]
pub struct DownloadUnpackOptions {
    /// Allow overriding the destination directory
    pub allow_override: bool,

    /// Include ancillary files in the download
    pub include_ancillary: bool,
}

impl CardanoDatabaseClient {
    /// The maximum number of parallel downloads and unpacks for immutable files.
    /// This could be a customizable parameter depending on level of concurrency wanted by the user.
    const MAX_PARALLEL_DOWNLOAD_UNPACK: usize = 100;

    /// Download and unpack the given Cardano database parts data by hash.
    pub async fn download_unpack(
        &self,
        cardano_database_snapshot: &CardanoDatabaseSnapshotMessage,
        immutable_file_range: &ImmutableFileRange,
        target_dir: &Path,
        download_unpack_options: DownloadUnpackOptions,
    ) -> MithrilResult<()> {
        let compression_algorithm = cardano_database_snapshot.compression_algorithm;
        let last_immutable_file_number = cardano_database_snapshot.beacon.immutable_file_number;
        let immutable_file_number_range =
            immutable_file_range.to_range_inclusive(last_immutable_file_number)?;
        self.verify_download_options_compatibility(
            &download_unpack_options,
            &immutable_file_number_range,
            last_immutable_file_number,
        )?;
        self.verify_can_write_to_target_directory(target_dir, &download_unpack_options)?;
        let immutable_locations = &cardano_database_snapshot.locations.immutables;
        self.download_unpack_immutable_files(
            immutable_locations,
            immutable_file_number_range,
            &compression_algorithm,
            target_dir,
        )
        .await?;
        if download_unpack_options.include_ancillary {
            let ancillary_locations = &cardano_database_snapshot.locations.ancillary;
            self.download_unpack_ancillary_file(
                ancillary_locations,
                &compression_algorithm,
                target_dir,
            )
            .await?;
        }

        Ok(())
    }

    fn immutable_files_target_dir(target_dir: &Path) -> PathBuf {
        target_dir.join("immutable")
    }

    fn volatile_target_dir(target_dir: &Path) -> PathBuf {
        target_dir.join("volatile")
    }

    fn ledger_target_dir(target_dir: &Path) -> PathBuf {
        target_dir.join("ledger")
    }

    /// Verify if the target directory is writable.
    fn verify_can_write_to_target_directory(
        &self,
        target_dir: &Path,
        download_unpack_options: &DownloadUnpackOptions,
    ) -> MithrilResult<()> {
        let immutable_files_target_dir = Self::immutable_files_target_dir(target_dir);
        let volatile_target_dir = Self::volatile_target_dir(target_dir);
        let ledger_target_dir = Self::ledger_target_dir(target_dir);
        if !download_unpack_options.allow_override {
            if immutable_files_target_dir.exists() {
                return Err(anyhow!(
                    "Immutable files target directory already exists in: {target_dir:?}"
                ));
            }
            if download_unpack_options.include_ancillary {
                if volatile_target_dir.exists() {
                    return Err(anyhow!(
                        "Volatile target directory already exists in: {target_dir:?}"
                    ));
                }
                if ledger_target_dir.exists() {
                    return Err(anyhow!(
                        "Ledger target directory already exists in: {target_dir:?}"
                    ));
                }
            }
        }

        Ok(())
    }

    /// Verify if the download options are compatible with the immutable file range.
    fn verify_download_options_compatibility(
        &self,
        download_options: &DownloadUnpackOptions,
        immutable_file_range: &RangeInclusive<ImmutableFileNumber>,
        last_immutable_file_number: ImmutableFileNumber,
    ) -> MithrilResult<()> {
        if download_options.include_ancillary
            && !immutable_file_range.contains(&last_immutable_file_number)
        {
            return Err(anyhow!(
                "The last immutable file number {last_immutable_file_number} is outside the range: {immutable_file_range:?}"
            ));
        }

        Ok(())
    }

    /// Download and unpack the immutable files of the given range.
    ///
    /// The download is attempted for each location until the full range is downloaded.
    /// An error is returned if not all the files are downloaded.
    async fn download_unpack_immutable_files(
        &self,
        locations: &[ImmutablesLocation],
        range: RangeInclusive<ImmutableFileNumber>,
        compression_algorithm: &CompressionAlgorithm,
        immutable_files_target_dir: &Path,
    ) -> MithrilResult<()> {
        let mut locations_sorted = locations.to_owned();
        locations_sorted.sort();
        let mut immutable_file_numbers_to_download =
            range.clone().map(|n| n.to_owned()).collect::<BTreeSet<_>>();
        for location in locations_sorted {
            let immutable_files_numbers_downloaded = self
                .download_unpack_immutable_files_for_location(
                    &location,
                    &immutable_file_numbers_to_download,
                    compression_algorithm,
                    immutable_files_target_dir,
                )
                .await?;
            for immutable_file_number in immutable_files_numbers_downloaded {
                immutable_file_numbers_to_download.remove(&immutable_file_number);
            }
            if immutable_file_numbers_to_download.is_empty() {
                return Ok(());
            }
        }

        Err(anyhow!(
                "Failed downloading and unpacking immutable files for immutable_file_numbers: {immutable_file_numbers_to_download:?}"
            ))
    }

    /// Download and unpack the immutable files of the given range.
    ///
    /// The download is attempted for each location until the full range is downloaded.
    /// An error is returned if not all the files are downloaded.
    async fn batch_download_unpack_immutable_files(
        &self,
        file_downloader: Arc<dyn FileDownloader>,
        file_downloader_uris_chunk: Vec<(ImmutableFileNumber, FileDownloaderUri)>,
        compression_algorithm: &CompressionAlgorithm,
        immutable_files_target_dir: &Path,
    ) -> MithrilResult<BTreeSet<ImmutableFileNumber>> {
        let mut immutable_file_numbers_downloaded = BTreeSet::new();
        let mut join_set: JoinSet<MithrilResult<ImmutableFileNumber>> = JoinSet::new();
        for (immutable_file_number, file_downloader_uri) in file_downloader_uris_chunk.into_iter() {
            let file_downloader_uri_clone = file_downloader_uri.to_owned();
            let compression_algorithm_clone = compression_algorithm.to_owned();
            let immutable_files_target_dir_clone = immutable_files_target_dir.to_owned();
            let file_downloader_clone = file_downloader.clone();
            let feedback_receiver_clone = self.feedback_sender.clone();
            let logger_clone = self.logger.clone();
            join_set.spawn(async move {
                    let download_id = MithrilEvent::new_snapshot_download_id();
                    feedback_receiver_clone
                        .send_event(MithrilEvent::CardanoDatabase(
                            MithrilEventCardanoDatabase::ImmutableDownloadStarted { immutable_file_number, download_id: download_id.clone()}))
                        .await;
                    let downloaded = file_downloader_clone
                        .download_unpack(
                            &file_downloader_uri_clone,
                            &immutable_files_target_dir_clone,
                            Some(compression_algorithm_clone),
                            DownloadEvent::Immutable{immutable_file_number, download_id: download_id.clone()},
                        )
                        .await;
                    match downloaded {
                        Ok(_) => {
                            feedback_receiver_clone
                                .send_event(MithrilEvent::CardanoDatabase(
                                    MithrilEventCardanoDatabase::ImmutableDownloadCompleted { immutable_file_number, download_id }))
                                .await;

                            Ok(immutable_file_number)
                        }
                        Err(e) => {
                            slog::error!(
                                logger_clone,
                                "Failed downloading and unpacking immutable file {immutable_file_number} for location {file_downloader_uri:?}"; "error" => e.to_string()
                            );
                            Err(e.context(format!("Failed downloading and unpacking immutable file {immutable_file_number} for location {file_downloader_uri:?}")))
                        }
                    }
                });
        }
        while let Some(result) = join_set.join_next().await {
            match result? {
                Ok(immutable_file_number) => {
                    immutable_file_numbers_downloaded.insert(immutable_file_number);
                }
                Err(e) => {
                    slog::error!(
                        self.logger,
                        "Failed downloading and unpacking immutable files"; "error" => e.to_string()
                    );
                }
            }
        }

        Ok(immutable_file_numbers_downloaded)
    }

    async fn download_unpack_immutable_files_for_location(
        &self,
        location: &ImmutablesLocation,
        immutable_file_numbers_to_download: &BTreeSet<ImmutableFileNumber>,
        compression_algorithm: &CompressionAlgorithm,
        immutable_files_target_dir: &Path,
    ) -> MithrilResult<BTreeSet<ImmutableFileNumber>> {
        let mut immutable_file_numbers_downloaded = BTreeSet::new();
        let file_downloader = self
            .immutable_file_downloader_resolver
            .resolve(location)
            .ok_or_else(|| {
                anyhow!("Failed resolving a file downloader for location: {location:?}")
            })?;
        let file_downloader_uris =
            FileDownloaderUri::expand_immutable_files_location_to_file_downloader_uris(
                location,
                immutable_file_numbers_to_download
                    .clone()
                    .into_iter()
                    .collect::<Vec<_>>()
                    .as_slice(),
            )?;
        let file_downloader_uri_chunks = file_downloader_uris
            .chunks(Self::MAX_PARALLEL_DOWNLOAD_UNPACK)
            .map(|x| x.to_vec())
            .collect::<Vec<_>>();
        for file_downloader_uris_chunk in file_downloader_uri_chunks {
            let immutable_file_numbers_downloaded_chunk = self
                .batch_download_unpack_immutable_files(
                    file_downloader.clone(),
                    file_downloader_uris_chunk,
                    compression_algorithm,
                    immutable_files_target_dir,
                )
                .await?;
            immutable_file_numbers_downloaded.extend(immutable_file_numbers_downloaded_chunk);
        }

        Ok(immutable_file_numbers_downloaded)
    }

    /// Download and unpack the ancillary files.
    async fn download_unpack_ancillary_file(
        &self,
        locations: &[AncillaryLocation],
        compression_algorithm: &CompressionAlgorithm,
        ancillary_file_target_dir: &Path,
    ) -> MithrilResult<()> {
        let mut locations_sorted = locations.to_owned();
        locations_sorted.sort();
        for location in locations_sorted {
            let download_id = MithrilEvent::new_ancillary_download_id();
            self.feedback_sender
                .send_event(MithrilEvent::CardanoDatabase(
                    MithrilEventCardanoDatabase::AncillaryDownloadStarted {
                        download_id: download_id.clone(),
                    },
                ))
                .await;
            let file_downloader = self
                .ancillary_file_downloader_resolver
                .resolve(&location)
                .ok_or_else(|| {
                    anyhow!("Failed resolving a file downloader for location: {location:?}")
                })?;
            let file_downloader_uri: FileDownloaderUri = location.into();
            let downloaded = file_downloader
                .download_unpack(
                    &file_downloader_uri,
                    ancillary_file_target_dir,
                    Some(compression_algorithm.to_owned()),
                    DownloadEvent::Ancillary {
                        download_id: download_id.clone(),
                    },
                )
                .await;
            match downloaded {
                Ok(_) => {
                    self.feedback_sender
                        .send_event(MithrilEvent::CardanoDatabase(
                            MithrilEventCardanoDatabase::AncillaryDownloadCompleted { download_id },
                        ))
                        .await;
                    return Ok(());
                }
                Err(e) => {
                    slog::error!(
                        self.logger,
                        "Failed downloading and unpacking ancillaries for location {file_downloader_uri:?}"; "error" => e.to_string()
                    );
                }
            }
        }

        Err(anyhow!(
            "Failed downloading and unpacking ancillaries for all locations"
        ))
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;
    use std::{fs, sync::Arc};

    use mithril_common::{
        entities::{
            AncillaryLocationDiscriminants, CardanoDbBeacon, Epoch,
            ImmutablesLocationDiscriminants, MultiFilesUri, TemplateUri,
        },
        messages::{
            ArtifactsLocationsMessagePart,
            CardanoDatabaseSnapshotMessage as CardanoDatabaseSnapshot,
        },
        test_utils::TempDir,
    };

    use crate::cardano_database_client::CardanoDatabaseClientDependencyInjector;
    use crate::feedback::StackFeedbackReceiver;
    use crate::file_downloader::MockFileDownloaderBuilder;

    use super::*;

    mod download_unpack {

        use super::*;

        #[tokio::test]
        async fn download_unpack_fails_with_invalid_immutable_file_range() {
            let immutable_file_range = ImmutableFileRange::Range(1, 0);
            let download_unpack_options = DownloadUnpackOptions::default();
            let cardano_db_snapshot = CardanoDatabaseSnapshot {
                hash: "hash-123".to_string(),
                ..CardanoDatabaseSnapshot::dummy()
            };
            let target_dir = Path::new(".");
            let client =
                CardanoDatabaseClientDependencyInjector::new().build_cardano_database_client();

            client
                .download_unpack(
                    &cardano_db_snapshot,
                    &immutable_file_range,
                    target_dir,
                    download_unpack_options,
                )
                .await
                .expect_err("download_unpack should fail");
        }

        #[tokio::test]
        async fn download_unpack_fails_when_immutable_files_download_fail() {
            let total_immutable_files = 10;
            let immutable_file_range = ImmutableFileRange::Range(1, total_immutable_files);
            let download_unpack_options = DownloadUnpackOptions::default();
            let cardano_db_snapshot = CardanoDatabaseSnapshot {
                hash: "hash-123".to_string(),
                locations: ArtifactsLocationsMessagePart {
                    immutables: vec![ImmutablesLocation::CloudStorage {
                        uri: MultiFilesUri::Template(TemplateUri(
                            "http://whatever/{immutable_file_number}.tar.gz".to_string(),
                        )),
                    }],
                    ..ArtifactsLocationsMessagePart::default()
                },
                ..CardanoDatabaseSnapshot::dummy()
            };
            let target_dir = TempDir::new(
                "cardano_database_client",
                "download_unpack_fails_when_immutable_files_download_fail",
            )
            .build();
            let client = CardanoDatabaseClientDependencyInjector::new()
                .with_immutable_file_downloaders(vec![(
                    ImmutablesLocationDiscriminants::CloudStorage,
                    Arc::new({
                        MockFileDownloaderBuilder::default()
                            .with_times(total_immutable_files as usize)
                            .with_failure()
                            .build()
                    }),
                )])
                .build_cardano_database_client();

            client
                .download_unpack(
                    &cardano_db_snapshot,
                    &immutable_file_range,
                    &target_dir,
                    download_unpack_options,
                )
                .await
                .expect_err("download_unpack should fail");
        }

        #[tokio::test]
        async fn download_unpack_fails_when_target_target_dir_would_be_overwritten_without_allow_override(
        ) {
            let immutable_file_range = ImmutableFileRange::Range(1, 10);
            let download_unpack_options = DownloadUnpackOptions::default();
            let cardano_db_snapshot = CardanoDatabaseSnapshot {
                hash: "hash-123".to_string(),
                ..CardanoDatabaseSnapshot::dummy()
            };
            let target_dir = &TempDir::new(
                                "cardano_database_client",
                                "download_unpack_fails_when_target_target_dir_would_be_overwritten_without_allow_override",
                            )
                            .build();
            fs::create_dir_all(target_dir.join("immutable")).unwrap();
            let client =
                CardanoDatabaseClientDependencyInjector::new().build_cardano_database_client();

            client
                .download_unpack(
                    &cardano_db_snapshot,
                    &immutable_file_range,
                    target_dir,
                    download_unpack_options,
                )
                .await
                .expect_err("download_unpack should fail");
        }

        #[tokio::test]
        async fn download_unpack_succeeds_with_valid_range() {
            let immutable_file_range = ImmutableFileRange::Range(1, 2);
            let download_unpack_options = DownloadUnpackOptions {
                include_ancillary: true,
                ..DownloadUnpackOptions::default()
            };
            let cardano_db_snapshot = CardanoDatabaseSnapshot {
                hash: "hash-123".to_string(),
                beacon: CardanoDbBeacon {
                    immutable_file_number: 2,
                    epoch: Epoch(123),
                },
                locations: ArtifactsLocationsMessagePart {
                    immutables: vec![ImmutablesLocation::CloudStorage {
                        uri: MultiFilesUri::Template(TemplateUri(
                            "http://whatever/{immutable_file_number}.tar.gz".to_string(),
                        )),
                    }],
                    ancillary: vec![AncillaryLocation::CloudStorage {
                        uri: "http://whatever/ancillary.tar.gz".to_string(),
                    }],
                    digests: vec![],
                },
                ..CardanoDatabaseSnapshot::dummy()
            };
            let target_dir = TempDir::new(
                "cardano_database_client",
                "download_unpack_succeeds_with_valid_range",
            )
            .build();
            let client = CardanoDatabaseClientDependencyInjector::new()
                .with_immutable_file_downloaders(vec![(
                    ImmutablesLocationDiscriminants::CloudStorage,
                    Arc::new({
                        let mock_file_downloader = MockFileDownloaderBuilder::default()
                            .with_file_uri("http://whatever/00001.tar.gz")
                            .with_target_dir(target_dir.clone())
                            .with_success()
                            .build();

                        MockFileDownloaderBuilder::from_mock(mock_file_downloader)
                            .with_file_uri("http://whatever/00002.tar.gz")
                            .with_target_dir(target_dir.clone())
                            .with_success()
                            .build()
                    }),
                )])
                .with_ancillary_file_downloaders(vec![(
                    AncillaryLocationDiscriminants::CloudStorage,
                    Arc::new(
                        MockFileDownloaderBuilder::default()
                            .with_file_uri("http://whatever/ancillary.tar.gz")
                            .with_target_dir(target_dir.clone())
                            .with_compression(Some(CompressionAlgorithm::default()))
                            .with_success()
                            .build(),
                    ),
                )])
                .build_cardano_database_client();

            client
                .download_unpack(
                    &cardano_db_snapshot,
                    &immutable_file_range,
                    &target_dir,
                    download_unpack_options,
                )
                .await
                .unwrap();
        }
    }

    mod verify_download_options_compatibility {

        use super::*;

        #[test]
        fn verify_download_options_compatibility_succeeds_if_without_ancillary_download() {
            let client =
                CardanoDatabaseClientDependencyInjector::new().build_cardano_database_client();
            let download_options = DownloadUnpackOptions {
                include_ancillary: false,
                ..DownloadUnpackOptions::default()
            };
            let immutable_file_range = ImmutableFileRange::Range(1, 10);
            let last_immutable_file_number = 10;

            client
                .verify_download_options_compatibility(
                    &download_options,
                    &immutable_file_range
                        .to_range_inclusive(last_immutable_file_number)
                        .unwrap(),
                    last_immutable_file_number,
                )
                .unwrap();
        }

        #[test]
        fn verify_download_options_compatibility_succeeds_if_with_ancillary_download_and_compatible_range(
        ) {
            let client =
                CardanoDatabaseClientDependencyInjector::new().build_cardano_database_client();
            let download_options = DownloadUnpackOptions {
                include_ancillary: true,
                ..DownloadUnpackOptions::default()
            };
            let immutable_file_range = ImmutableFileRange::Range(7, 10);
            let last_immutable_file_number = 10;

            client
                .verify_download_options_compatibility(
                    &download_options,
                    &immutable_file_range
                        .to_range_inclusive(last_immutable_file_number)
                        .unwrap(),
                    last_immutable_file_number,
                )
                .unwrap();
        }

        #[test]
        fn verify_download_options_compatibility_fails_if_with_ancillary_download_and_incompatible_range(
        ) {
            let client =
                CardanoDatabaseClientDependencyInjector::new().build_cardano_database_client();
            let download_options = DownloadUnpackOptions {
                include_ancillary: true,
                ..DownloadUnpackOptions::default()
            };
            let immutable_file_range = ImmutableFileRange::Range(7, 10);
            let last_immutable_file_number = 123;

            client
                .verify_download_options_compatibility(
                    &download_options,
                    &immutable_file_range
                        .to_range_inclusive(last_immutable_file_number)
                        .unwrap(),
                    last_immutable_file_number,
                )
                .expect_err("verify_download_options_compatibility should fail as the last immutable file number is outside the range");
        }
    }

    mod verify_can_write_to_target_dir {

        use super::*;

        #[test]
        fn verify_can_write_to_target_dir_always_succeeds_with_allow_overwrite() {
            let target_dir = TempDir::new(
                "cardano_database_client",
                "verify_can_write_to_target_dir_always_succeeds_with_allow_overwrite",
            )
            .build();
            let client =
                CardanoDatabaseClientDependencyInjector::new().build_cardano_database_client();

            client
                .verify_can_write_to_target_directory(
                    &target_dir,
                    &DownloadUnpackOptions {
                        allow_override: true,
                        include_ancillary: false,
                    },
                )
                .unwrap();

            fs::create_dir_all(CardanoDatabaseClient::immutable_files_target_dir(
                &target_dir,
            ))
            .unwrap();
            fs::create_dir_all(CardanoDatabaseClient::volatile_target_dir(&target_dir)).unwrap();
            fs::create_dir_all(CardanoDatabaseClient::ledger_target_dir(&target_dir)).unwrap();
            client
                .verify_can_write_to_target_directory(
                    &target_dir,
                    &DownloadUnpackOptions {
                        allow_override: true,
                        include_ancillary: false,
                    },
                )
                .unwrap();
            client
                .verify_can_write_to_target_directory(
                    &target_dir,
                    &DownloadUnpackOptions {
                        allow_override: true,
                        include_ancillary: true,
                    },
                )
                .unwrap();
        }

        #[test]
        fn verify_can_write_to_target_dir_fails_without_allow_overwrite_and_non_empty_immutable_target_dir(
        ) {
            let target_dir = TempDir::new("cardano_database_client", "verify_can_write_to_target_dir_fails_without_allow_overwrite_and_non_empty_immutable_target_dir").build();
            fs::create_dir_all(CardanoDatabaseClient::immutable_files_target_dir(
                &target_dir,
            ))
            .unwrap();
            let client =
                CardanoDatabaseClientDependencyInjector::new().build_cardano_database_client();

            client
                .verify_can_write_to_target_directory(
                    &target_dir,
                    &DownloadUnpackOptions {
                        allow_override: false,
                        include_ancillary: false,
                    },
                )
                .expect_err("verify_can_write_to_target_dir should fail");

            client
                .verify_can_write_to_target_directory(
                    &target_dir,
                    &DownloadUnpackOptions {
                        allow_override: false,
                        include_ancillary: true,
                    },
                )
                .expect_err("verify_can_write_to_target_dir should fail");
        }

        #[test]
        fn verify_can_write_to_target_dir_fails_without_allow_overwrite_and_non_empty_ledger_target_dir(
        ) {
            let target_dir = TempDir::new("cardano_database_client", "verify_can_write_to_target_dir_fails_without_allow_overwrite_and_non_empty_ledger_target_dir").build();
            fs::create_dir_all(CardanoDatabaseClient::ledger_target_dir(&target_dir)).unwrap();
            let client =
                CardanoDatabaseClientDependencyInjector::new().build_cardano_database_client();

            client
                .verify_can_write_to_target_directory(
                    &target_dir,
                    &DownloadUnpackOptions {
                        allow_override: false,
                        include_ancillary: true,
                    },
                )
                .expect_err("verify_can_write_to_target_dir should fail");

            client
                .verify_can_write_to_target_directory(
                    &target_dir,
                    &DownloadUnpackOptions {
                        allow_override: false,
                        include_ancillary: false,
                    },
                )
                .unwrap();
        }

        #[test]
        fn verify_can_write_to_target_dir_fails_without_allow_overwrite_and_non_empty_volatile_target_dir(
        ) {
            let target_dir = TempDir::new("cardano_database_client", "verify_can_write_to_target_dir_fails_without_allow_overwrite_and_non_empty_volatile_target_dir").build();
            fs::create_dir_all(CardanoDatabaseClient::volatile_target_dir(&target_dir)).unwrap();
            let client =
                CardanoDatabaseClientDependencyInjector::new().build_cardano_database_client();

            client
                .verify_can_write_to_target_directory(
                    &target_dir,
                    &DownloadUnpackOptions {
                        allow_override: false,
                        include_ancillary: true,
                    },
                )
                .expect_err("verify_can_write_to_target_dir should fail");

            client
                .verify_can_write_to_target_directory(
                    &target_dir,
                    &DownloadUnpackOptions {
                        allow_override: false,
                        include_ancillary: false,
                    },
                )
                .unwrap();
        }
    }

    mod download_unpack_immutable_files {

        use super::*;

        #[tokio::test]
        async fn download_unpack_immutable_files_fails_if_one_is_not_retrieved() {
            let total_immutable_files = 2;
            let immutable_file_range = ImmutableFileRange::Range(1, total_immutable_files);
            let target_dir = TempDir::new(
                "cardano_database_client",
                "download_unpack_immutable_files_succeeds",
            )
            .build();
            let client = CardanoDatabaseClientDependencyInjector::new()
                .with_immutable_file_downloaders(vec![(
                    ImmutablesLocationDiscriminants::CloudStorage,
                    Arc::new({
                        let mock_file_downloader =
                            MockFileDownloaderBuilder::default().with_failure().build();

                        MockFileDownloaderBuilder::from_mock(mock_file_downloader)
                            .with_success()
                            .build()
                    }),
                )])
                .build_cardano_database_client();

            client
                .download_unpack_immutable_files(
                    &[ImmutablesLocation::CloudStorage {
                        uri: MultiFilesUri::Template(TemplateUri(
                            "http://whatever/{immutable_file_number}.tar.gz".to_string(),
                        )),
                    }],
                    immutable_file_range
                        .to_range_inclusive(total_immutable_files)
                        .unwrap(),
                    &CompressionAlgorithm::default(),
                    &target_dir,
                )
                .await
                .expect_err("download_unpack_immutable_files should fail");
        }

        #[tokio::test]
        async fn download_unpack_immutable_files_succeeds_if_all_are_retrieved_with_same_location()
        {
            let total_immutable_files = 2;
            let immutable_file_range = ImmutableFileRange::Range(1, total_immutable_files);
            let target_dir = TempDir::new(
                "cardano_database_client",
                "download_unpack_immutable_files_succeeds",
            )
            .build();
            let client = CardanoDatabaseClientDependencyInjector::new()
                .with_immutable_file_downloaders(vec![(
                    ImmutablesLocationDiscriminants::CloudStorage,
                    Arc::new(
                        MockFileDownloaderBuilder::default()
                            .with_times(2)
                            .with_success()
                            .build(),
                    ),
                )])
                .build_cardano_database_client();

            client
                .download_unpack_immutable_files(
                    &[ImmutablesLocation::CloudStorage {
                        uri: MultiFilesUri::Template(TemplateUri(
                            "http://whatever-1/{immutable_file_number}.tar.gz".to_string(),
                        )),
                    }],
                    immutable_file_range
                        .to_range_inclusive(total_immutable_files)
                        .unwrap(),
                    &CompressionAlgorithm::default(),
                    &target_dir,
                )
                .await
                .unwrap();
        }

        #[tokio::test]
        async fn download_unpack_immutable_files_succeeds_if_all_are_retrieved_with_different_locations(
        ) {
            let total_immutable_files = 2;
            let immutable_file_range = ImmutableFileRange::Range(1, total_immutable_files);
            let target_dir = TempDir::new(
                "cardano_database_client",
                "download_unpack_immutable_files_succeeds",
            )
            .build();
            let client = CardanoDatabaseClientDependencyInjector::new()
                .with_immutable_file_downloaders(vec![(
                    ImmutablesLocationDiscriminants::CloudStorage,
                    Arc::new({
                        let mock_file_downloader = MockFileDownloaderBuilder::default()
                            .with_file_uri("http://whatever-1/00001.tar.gz")
                            .with_target_dir(target_dir.clone())
                            .with_failure()
                            .build();
                        let mock_file_downloader =
                            MockFileDownloaderBuilder::from_mock(mock_file_downloader)
                                .with_file_uri("http://whatever-1/00002.tar.gz")
                                .with_target_dir(target_dir.clone())
                                .with_success()
                                .build();

                        MockFileDownloaderBuilder::from_mock(mock_file_downloader)
                            .with_file_uri("http://whatever-2/00001.tar.gz")
                            .with_target_dir(target_dir.clone())
                            .with_success()
                            .build()
                    }),
                )])
                .build_cardano_database_client();

            client
                .download_unpack_immutable_files(
                    &[
                        ImmutablesLocation::CloudStorage {
                            uri: MultiFilesUri::Template(TemplateUri(
                                "http://whatever-1/{immutable_file_number}.tar.gz".to_string(),
                            )),
                        },
                        ImmutablesLocation::CloudStorage {
                            uri: MultiFilesUri::Template(TemplateUri(
                                "http://whatever-2/{immutable_file_number}.tar.gz".to_string(),
                            )),
                        },
                    ],
                    immutable_file_range
                        .to_range_inclusive(total_immutable_files)
                        .unwrap(),
                    &CompressionAlgorithm::default(),
                    &target_dir,
                )
                .await
                .unwrap();
        }

        #[tokio::test]
        async fn download_unpack_immutable_files_sends_feedbacks() {
            let total_immutable_files = 1;
            let immutable_file_range = ImmutableFileRange::Range(1, total_immutable_files);
            let target_dir = Path::new(".");
            let feedback_receiver = Arc::new(StackFeedbackReceiver::new());
            let client = CardanoDatabaseClientDependencyInjector::new()
                .with_immutable_file_downloaders(vec![(
                    ImmutablesLocationDiscriminants::CloudStorage,
                    Arc::new(MockFileDownloaderBuilder::default().with_success().build()),
                )])
                .with_feedback_receivers(&[feedback_receiver.clone()])
                .build_cardano_database_client();

            client
                .download_unpack_immutable_files(
                    &[ImmutablesLocation::CloudStorage {
                        uri: MultiFilesUri::Template(TemplateUri(
                            "http://whatever/{immutable_file_number}.tar.gz".to_string(),
                        )),
                    }],
                    immutable_file_range
                        .to_range_inclusive(total_immutable_files)
                        .unwrap(),
                    &CompressionAlgorithm::default(),
                    target_dir,
                )
                .await
                .unwrap();

            let sent_events = feedback_receiver.stacked_events();
            let id = sent_events[0].event_id();
            let expected_events = vec![
                MithrilEvent::CardanoDatabase(
                    MithrilEventCardanoDatabase::ImmutableDownloadStarted {
                        immutable_file_number: 1,
                        download_id: id.to_string(),
                    },
                ),
                MithrilEvent::CardanoDatabase(
                    MithrilEventCardanoDatabase::ImmutableDownloadCompleted {
                        immutable_file_number: 1,
                        download_id: id.to_string(),
                    },
                ),
            ];
            assert_eq!(expected_events, sent_events);
        }
    }

    mod download_unpack_ancillary_file {

        use super::*;

        #[tokio::test]
        async fn download_unpack_ancillary_file_fails_if_no_location_is_retrieved() {
            let target_dir = Path::new(".");
            let client = CardanoDatabaseClientDependencyInjector::new()
                .with_ancillary_file_downloaders(vec![(
                    AncillaryLocationDiscriminants::CloudStorage,
                    Arc::new(MockFileDownloaderBuilder::default().with_failure().build()),
                )])
                .build_cardano_database_client();

            client
                .download_unpack_ancillary_file(
                    &[AncillaryLocation::CloudStorage {
                        uri: "http://whatever-1/ancillary.tar.gz".to_string(),
                    }],
                    &CompressionAlgorithm::default(),
                    target_dir,
                )
                .await
                .expect_err("download_unpack_ancillary_file should fail");
        }

        #[tokio::test]
        async fn download_unpack_ancillary_file_succeeds_if_at_least_one_location_is_retrieved() {
            let target_dir = Path::new(".");
            let client = CardanoDatabaseClientDependencyInjector::new()
                .with_ancillary_file_downloaders(vec![(
                    AncillaryLocationDiscriminants::CloudStorage,
                    Arc::new({
                        let mock_file_downloader = MockFileDownloaderBuilder::default()
                            .with_file_uri("http://whatever-1/ancillary.tar.gz")
                            .with_target_dir(target_dir.to_path_buf())
                            .with_failure()
                            .build();

                        MockFileDownloaderBuilder::from_mock(mock_file_downloader)
                            .with_file_uri("http://whatever-2/ancillary.tar.gz")
                            .with_target_dir(target_dir.to_path_buf())
                            .with_success()
                            .build()
                    }),
                )])
                .build_cardano_database_client();

            client
                .download_unpack_ancillary_file(
                    &[
                        AncillaryLocation::CloudStorage {
                            uri: "http://whatever-1/ancillary.tar.gz".to_string(),
                        },
                        AncillaryLocation::CloudStorage {
                            uri: "http://whatever-2/ancillary.tar.gz".to_string(),
                        },
                    ],
                    &CompressionAlgorithm::default(),
                    target_dir,
                )
                .await
                .unwrap();
        }

        #[tokio::test]
        async fn download_unpack_ancillary_file_succeeds_when_first_location_is_retrieved() {
            let target_dir = Path::new(".");
            let client = CardanoDatabaseClientDependencyInjector::new()
                .with_ancillary_file_downloaders(vec![(
                    AncillaryLocationDiscriminants::CloudStorage,
                    Arc::new(
                        MockFileDownloaderBuilder::default()
                            .with_file_uri("http://whatever-1/ancillary.tar.gz")
                            .with_target_dir(target_dir.to_path_buf())
                            .with_success()
                            .build(),
                    ),
                )])
                .build_cardano_database_client();

            client
                .download_unpack_ancillary_file(
                    &[
                        AncillaryLocation::CloudStorage {
                            uri: "http://whatever-1/ancillary.tar.gz".to_string(),
                        },
                        AncillaryLocation::CloudStorage {
                            uri: "http://whatever-2/ancillary.tar.gz".to_string(),
                        },
                    ],
                    &CompressionAlgorithm::default(),
                    target_dir,
                )
                .await
                .unwrap();
        }

        #[tokio::test]
        async fn download_unpack_ancillary_files_sends_feedbacks() {
            let target_dir = Path::new(".");
            let feedback_receiver = Arc::new(StackFeedbackReceiver::new());
            let client = CardanoDatabaseClientDependencyInjector::new()
                .with_ancillary_file_downloaders(vec![(
                    AncillaryLocationDiscriminants::CloudStorage,
                    Arc::new(MockFileDownloaderBuilder::default().with_success().build()),
                )])
                .with_feedback_receivers(&[feedback_receiver.clone()])
                .build_cardano_database_client();

            client
                .download_unpack_ancillary_file(
                    &[AncillaryLocation::CloudStorage {
                        uri: "http://whatever-1/ancillary.tar.gz".to_string(),
                    }],
                    &CompressionAlgorithm::default(),
                    target_dir,
                )
                .await
                .unwrap();

            let sent_events = feedback_receiver.stacked_events();
            let id = sent_events[0].event_id();
            let expected_events = vec![
                MithrilEvent::CardanoDatabase(
                    MithrilEventCardanoDatabase::AncillaryDownloadStarted {
                        download_id: id.to_string(),
                    },
                ),
                MithrilEvent::CardanoDatabase(
                    MithrilEventCardanoDatabase::AncillaryDownloadCompleted {
                        download_id: id.to_string(),
                    },
                ),
            ];
            assert_eq!(expected_events, sent_events);
        }
    }
}

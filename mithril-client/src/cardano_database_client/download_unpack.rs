use std::collections::{BTreeSet, VecDeque};
use std::future::Future;
use std::ops::RangeInclusive;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use std::sync::Arc;

use anyhow::{anyhow, Context};
use slog::Logger;
use tokio::task::JoinSet;

use mithril_common::{
    digesters::{IMMUTABLE_DIR, LEDGER_DIR, VOLATILE_DIR},
    entities::{AncillaryLocation, CompressionAlgorithm, ImmutableFileNumber, ImmutablesLocation},
    messages::{AncillaryMessagePart, CardanoDatabaseSnapshotMessage, ImmutablesMessagePart},
};

use crate::feedback::{FeedbackSender, MithrilEvent, MithrilEventCardanoDatabase};
use crate::file_downloader::{DownloadEvent, FileDownloader, FileDownloaderUri};
use crate::utils::{AncillaryVerifier, VecDequeExtensions};
use crate::MithrilResult;

use super::immutable_file_range::ImmutableFileRange;

/// The future type for downloading a file
type DownloadFuture = dyn Future<Output = MithrilResult<()>> + Send;

/// A task to download and unpack a file
struct DownloadTask {
    kind: DownloadKind,
    locations_to_try: Vec<LocationToDownload>,
    size_uncompressed: u64,
    target_dir: PathBuf,
    download_event: DownloadEvent,
}

impl DownloadTask {
    fn tried_locations(&self) -> String {
        self.locations_to_try
            .iter()
            .map(|l| l.file_downloader_uri.as_str())
            .collect::<Vec<_>>()
            .join(", ")
    }

    fn name(&self) -> String {
        match &self.kind {
            DownloadKind::Immutable(immutable_file_number) => {
                format!("immutable_file_{immutable_file_number:05}")
            }
            DownloadKind::Ancillary { .. } => "ancillary".to_string(),
        }
    }

    async fn download_unpack_file(&self, target_dir: &Path, logger: &Logger) -> MithrilResult<()> {
        let mut download_succeeded = false;

        for location_to_try in &self.locations_to_try {
            let downloaded = location_to_try
                .file_downloader
                .download_unpack(
                    &location_to_try.file_downloader_uri,
                    self.size_uncompressed,
                    target_dir,
                    location_to_try.compression_algorithm,
                    self.download_event.clone(),
                )
                .await;

            if let Err(e) = downloaded {
                slog::error!(
                    logger,
                    "Failed downloading and unpacking {} for location {:?}",
                    self.name(), location_to_try.file_downloader_uri;
                    "error" => ?e
                );
            } else {
                download_succeeded = true;
                break;
            }
        }

        if download_succeeded {
            Ok(())
        } else {
            Err(anyhow!(
                "All locations failed for {}, tried locations: {}",
                self.name(),
                self.tried_locations()
            ))
        }
    }

    async fn download_unpack_verify_ancillary(
        &self,
        ancillary_files_temp_dir: &Path,
        target_dir: &Path,
        ancillary_verifier: &Arc<AncillaryVerifier>,
        logger: &Logger,
    ) -> MithrilResult<()> {
        self.download_unpack_file(ancillary_files_temp_dir, logger)
            .await?;
        let validated_manifest = ancillary_verifier.verify(ancillary_files_temp_dir).await?;
        validated_manifest.move_to_final_location(target_dir).await
    }

    /// Build a future that will download and unpack the file when awaited.
    ///
    /// The download is attempted for each location until the file is downloaded.
    /// If all locations fail, an error is returned.
    fn build_download_future(self, logger: Logger) -> Pin<Box<DownloadFuture>> {
        let download_future = async move {
            match &self.kind {
                DownloadKind::Immutable(..) => {
                    self.download_unpack_file(&self.target_dir, &logger).await
                }
                DownloadKind::Ancillary { verifier } => {
                    let ancillary_files_temp_dir =
                        InternalArtifactDownloader::temp_ancillary_target_dir(
                            &self.target_dir,
                            self.download_event.download_id(),
                        );
                    tokio::fs::create_dir(&ancillary_files_temp_dir)
                        .await
                        .with_context(|| {
                            format!(
                                "can not create download target directory '{}'",
                                self.target_dir.display()
                            )
                        })?;

                    let download_unpack_verify_result = self
                        .download_unpack_verify_ancillary(
                            &ancillary_files_temp_dir,
                            &self.target_dir,
                            verifier,
                            &logger,
                        )
                        .await;

                    if let Err(e) = tokio::fs::remove_dir_all(&ancillary_files_temp_dir).await {
                        slog::warn!(
                            logger, "Failed to remove ancillary unpack directory '{}'", ancillary_files_temp_dir.display();
                            "error" => ?e
                        );
                    }

                    download_unpack_verify_result
                }
            }
        };

        Box::pin(download_future)
    }
}

enum DownloadKind {
    Immutable(ImmutableFileNumber),
    Ancillary { verifier: Arc<AncillaryVerifier> },
}

struct LocationToDownload {
    file_downloader: Arc<dyn FileDownloader>,
    file_downloader_uri: FileDownloaderUri,
    compression_algorithm: Option<CompressionAlgorithm>,
}

/// Options for downloading and unpacking a Cardano database
#[derive(Debug, Copy, Clone)]
pub struct DownloadUnpackOptions {
    /// Allow overriding the destination directory
    pub allow_override: bool,

    /// Include ancillary files in the download
    pub include_ancillary: bool,

    /// Maximum number of parallel downloads
    pub max_parallel_downloads: usize,
}

impl Default for DownloadUnpackOptions {
    fn default() -> Self {
        Self {
            allow_override: false,
            include_ancillary: false,
            max_parallel_downloads: 100,
        }
    }
}

pub struct InternalArtifactDownloader {
    http_file_downloader: Arc<dyn FileDownloader>,
    ancillary_verifier: Option<Arc<AncillaryVerifier>>,
    feedback_sender: FeedbackSender,
    logger: slog::Logger,
}

impl InternalArtifactDownloader {
    /// Constructs a new `InternalArtifactDownloader`.
    pub fn new(
        http_file_downloader: Arc<dyn FileDownloader>,
        ancillary_verifier: Option<Arc<AncillaryVerifier>>,
        feedback_sender: FeedbackSender,
        logger: slog::Logger,
    ) -> Self {
        Self {
            http_file_downloader,
            ancillary_verifier,
            feedback_sender,
            logger,
        }
    }

    /// Download and unpack the given Cardano database parts data by hash.
    pub async fn download_unpack(
        &self,
        cardano_database_snapshot: &CardanoDatabaseSnapshotMessage,
        immutable_file_range: &ImmutableFileRange,
        target_dir: &Path,
        download_unpack_options: DownloadUnpackOptions,
    ) -> MithrilResult<()> {
        let download_id = MithrilEvent::new_snapshot_download_id();
        let last_immutable_file_number = cardano_database_snapshot.beacon.immutable_file_number;
        let immutable_file_number_range =
            immutable_file_range.to_range_inclusive(last_immutable_file_number)?;
        let immutable_file_range_length =
            immutable_file_number_range.end() - immutable_file_number_range.start() + 1;
        self.feedback_sender
            .send_event(MithrilEvent::CardanoDatabase(
                MithrilEventCardanoDatabase::Started {
                    download_id: download_id.clone(),
                    total_immutable_files: immutable_file_range_length,
                    include_ancillary: download_unpack_options.include_ancillary,
                },
            ))
            .await;
        Self::verify_download_options_compatibility(
            &download_unpack_options,
            &immutable_file_number_range,
            last_immutable_file_number,
        )?;
        Self::verify_can_write_to_target_directory(target_dir, &download_unpack_options)?;

        let mut tasks = VecDeque::from(self.build_download_tasks_for_immutables(
            &cardano_database_snapshot.immutables,
            immutable_file_number_range,
            target_dir,
            &download_id,
        )?);
        if download_unpack_options.include_ancillary {
            tasks.push_back(self.new_ancillary_download_task(
                &cardano_database_snapshot.ancillary,
                target_dir,
                &download_id,
            )?);
        }
        self.batch_download_unpack(tasks, download_unpack_options.max_parallel_downloads)
            .await?;

        self.feedback_sender
            .send_event(MithrilEvent::CardanoDatabase(
                MithrilEventCardanoDatabase::Completed {
                    download_id: download_id.clone(),
                },
            ))
            .await;

        Ok(())
    }

    fn immutable_files_target_dir(target_dir: &Path) -> PathBuf {
        target_dir.join(IMMUTABLE_DIR)
    }

    fn volatile_target_dir(target_dir: &Path) -> PathBuf {
        target_dir.join(VOLATILE_DIR)
    }

    fn ledger_target_dir(target_dir: &Path) -> PathBuf {
        target_dir.join(LEDGER_DIR)
    }

    fn temp_ancillary_target_dir(target_dir: &Path, download_id: &str) -> PathBuf {
        target_dir.join(format!("ancillary-{download_id}"))
    }

    /// Verify if the target directory is writable.
    fn verify_can_write_to_target_directory(
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

    fn build_download_tasks_for_immutables(
        &self,
        immutable_locations: &ImmutablesMessagePart,
        immutable_file_number_range: RangeInclusive<ImmutableFileNumber>,
        target_dir: &Path,
        download_id: &str,
    ) -> MithrilResult<Vec<DownloadTask>> {
        let immutable_file_numbers_to_download = immutable_file_number_range
            .map(|n| n.to_owned())
            .collect::<BTreeSet<_>>();

        let mut immutable_tasks = vec![];
        for immutable_file_number in immutable_file_numbers_to_download {
            immutable_tasks.push(self.new_immutable_download_task(
                immutable_locations,
                immutable_file_number,
                target_dir,
                download_id,
            )?);
        }
        Ok(immutable_tasks)
    }

    fn new_immutable_download_task(
        &self,
        immutable_locations: &ImmutablesMessagePart,
        immutable_file_number: ImmutableFileNumber,
        immutable_files_target_dir: &Path,
        download_id: &str,
    ) -> MithrilResult<DownloadTask> {
        let mut locations_to_try = vec![];
        let mut locations_sorted = immutable_locations.sanitized_locations()?;
        locations_sorted.sort();
        for location in locations_sorted {
            let location_to_try = match location {
                ImmutablesLocation::CloudStorage {
                    uri,
                    compression_algorithm,
                } => {
                    let file_downloader = self.http_file_downloader.clone();
                    let file_downloader_uri = FileDownloaderUri::from(
                        uri.expand_for_immutable_file_number(immutable_file_number),
                    );

                    LocationToDownload {
                        file_downloader,
                        file_downloader_uri,
                        compression_algorithm: compression_algorithm.to_owned(),
                    }
                }
                // Note: unknown locations should have been filtered out by `sanitized_locations`
                ImmutablesLocation::Unknown => unreachable!(),
            };

            locations_to_try.push(location_to_try);
        }

        Ok(DownloadTask {
            kind: DownloadKind::Immutable(immutable_file_number),
            locations_to_try,
            target_dir: immutable_files_target_dir.to_path_buf(),
            size_uncompressed: immutable_locations.average_size_uncompressed,
            download_event: DownloadEvent::Immutable {
                download_id: download_id.to_string(),
                immutable_file_number,
            },
        })
    }

    fn new_ancillary_download_task(
        &self,
        locations: &AncillaryMessagePart,
        target_dir: &Path,
        download_id: &str,
    ) -> MithrilResult<DownloadTask> {
        let ancillary_verifier = self.ancillary_verifier.clone().ok_or(anyhow!(
            "ancillary verifier is not set, please use `set_ancillary_verification_key` when creating the client"
        ))?;

        let mut locations_to_try = vec![];
        let mut locations_sorted = locations.sanitized_locations()?;
        locations_sorted.sort();
        for location in locations_sorted {
            let location_to_try = match location {
                AncillaryLocation::CloudStorage {
                    uri,
                    compression_algorithm,
                } => {
                    let file_downloader = self.http_file_downloader.clone();
                    let file_downloader_uri = FileDownloaderUri::from(uri);

                    LocationToDownload {
                        file_downloader,
                        file_downloader_uri,
                        compression_algorithm: compression_algorithm.to_owned(),
                    }
                }
                // Note: unknown locations should have been filtered out by `sanitized_locations`
                AncillaryLocation::Unknown => unreachable!(),
            };

            locations_to_try.push(location_to_try);
        }

        Ok(DownloadTask {
            kind: DownloadKind::Ancillary {
                verifier: ancillary_verifier.clone(),
            },
            locations_to_try,
            target_dir: target_dir.to_path_buf(),
            size_uncompressed: locations.size_uncompressed,
            download_event: DownloadEvent::Ancillary {
                download_id: download_id.to_string(),
            },
        })
    }

    /// Download and unpack the files in parallel.
    async fn batch_download_unpack(
        &self,
        mut tasks: VecDeque<DownloadTask>,
        max_parallel_downloads: usize,
    ) -> MithrilResult<()> {
        let mut join_set: JoinSet<MithrilResult<()>> = JoinSet::new();

        let initial_tasks_chunk = tasks.pop_up_to_n(max_parallel_downloads);
        for task in initial_tasks_chunk {
            join_set.spawn(task.build_download_future(self.logger.clone()));
        }

        while let Some(result) = join_set.join_next().await {
            if let Err(error) = result? {
                join_set.abort_all();
                anyhow::bail!(error);
            }

            if let Some(task) = tasks.pop_front() {
                join_set.spawn(task.build_download_future(self.logger.clone()));
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;
    use std::{fs, sync::Arc};

    use mithril_common::{
        entities::{CardanoDbBeacon, Epoch, MultiFilesUri, TemplateUri},
        messages::CardanoDatabaseSnapshotMessage as CardanoDatabaseSnapshot,
        test_utils::{fake_keys, temp_dir_create, TempDir},
    };

    use crate::cardano_database_client::CardanoDatabaseClientDependencyInjector;
    use crate::file_downloader::{MockFileDownloader, MockFileDownloaderBuilder};
    use crate::test_utils::TestLogger;

    use super::*;

    mod download_unpack {
        use mithril_common::crypto_helper::ManifestSigner;
        use mithril_common::messages::{
            AncillaryMessagePart, DigestsMessagePart, ImmutablesMessagePart,
        };

        use crate::file_downloader::FakeAncillaryFileBuilder;

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
                immutables: ImmutablesMessagePart {
                    average_size_uncompressed: 512,
                    locations: vec![ImmutablesLocation::CloudStorage {
                        uri: MultiFilesUri::Template(TemplateUri(
                            "http://whatever/{immutable_file_number}.tar.gz".to_string(),
                        )),
                        compression_algorithm: Some(CompressionAlgorithm::Gzip),
                    }],
                },

                ..CardanoDatabaseSnapshot::dummy()
            };
            let target_dir = TempDir::new(
                "cardano_database_client",
                "download_unpack_fails_when_immutable_files_download_fail",
            )
            .build();
            let client = CardanoDatabaseClientDependencyInjector::new()
                .with_http_file_downloader(Arc::new({
                    MockFileDownloaderBuilder::default()
                        .with_times(total_immutable_files as usize)
                        .with_failure()
                        .build()
                }))
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
        async fn download_unpack_fails_when_target_dir_would_be_overwritten_without_allow_override()
        {
            let immutable_file_range = ImmutableFileRange::Range(1, 10);
            let download_unpack_options = DownloadUnpackOptions::default();
            let cardano_db_snapshot = CardanoDatabaseSnapshot {
                hash: "hash-123".to_string(),
                ..CardanoDatabaseSnapshot::dummy()
            };
            let target_dir = &TempDir::new(
                "cardano_database_client",
                "download_unpack_fails_when_target_dir_would_be_overwritten_without_allow_override",
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
            let ancillary_signer = ManifestSigner::create_deterministic_signer();
            let cardano_db_snapshot = CardanoDatabaseSnapshot {
                hash: "hash-123".to_string(),
                beacon: CardanoDbBeacon {
                    immutable_file_number: 2,
                    epoch: Epoch(123),
                },
                immutables: ImmutablesMessagePart {
                    average_size_uncompressed: 512,
                    locations: vec![ImmutablesLocation::CloudStorage {
                        uri: MultiFilesUri::Template(TemplateUri(
                            "http://whatever/{immutable_file_number}.tar.gz".to_string(),
                        )),
                        compression_algorithm: Some(CompressionAlgorithm::Gzip),
                    }],
                },
                ancillary: AncillaryMessagePart {
                    size_uncompressed: 2048,
                    locations: vec![AncillaryLocation::CloudStorage {
                        uri: "http://whatever/ancillary.tar.gz".to_string(),
                        compression_algorithm: Some(CompressionAlgorithm::Gzip),
                    }],
                },
                digests: DigestsMessagePart {
                    size_uncompressed: 1024,
                    locations: vec![],
                },
                ..CardanoDatabaseSnapshot::dummy()
            };
            let target_dir = TempDir::new(
                "cardano_database_client",
                "download_unpack_succeeds_with_valid_range",
            )
            .build();
            let client = CardanoDatabaseClientDependencyInjector::new()
                .with_http_file_downloader(Arc::new({
                    MockFileDownloaderBuilder::default()
                        .with_file_uri("http://whatever/00001.tar.gz")
                        .with_target_dir(target_dir.clone())
                        .with_success()
                        .next_call()
                        .with_file_uri("http://whatever/00002.tar.gz")
                        .with_target_dir(target_dir.clone())
                        .with_success()
                        .next_call()
                        .with_file_uri("http://whatever/ancillary.tar.gz")
                        .with_compression(Some(CompressionAlgorithm::Gzip))
                        .with_success_and_create_fake_ancillary_files(
                            FakeAncillaryFileBuilder::builder()
                                .files_in_manifest_to_create(vec!["ledger".to_string()])
                                .sign_manifest(ancillary_signer.clone())
                                .build(),
                        )
                        .build()
                }))
                .with_ancillary_verifier(ancillary_signer.verification_key())
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

        #[tokio::test]
        async fn fail_if_include_ancillary_is_true_and_ancillary_verifier_is_not_set() {
            let download_unpack_options = DownloadUnpackOptions {
                include_ancillary: true,
                ..Default::default()
            };
            let cardano_db_snapshot = CardanoDatabaseSnapshot {
                hash: "hash-123".to_string(),
                ..CardanoDatabaseSnapshot::dummy()
            };
            let target_dir = temp_dir_create!();
            let client =
                CardanoDatabaseClientDependencyInjector::new().build_cardano_database_client();

            let error = client
                .download_unpack(
                    &cardano_db_snapshot,
                    &ImmutableFileRange::Full,
                    &target_dir,
                    download_unpack_options,
                )
                .await
                .unwrap_err();

            let expected_error_text = "ancillary verifier is not set, please use `set_ancillary_verification_key` when creating the client";
            assert!(
                error.to_string().contains(expected_error_text),
                "Expected error message to contain '{expected_error_text}', but got: {error}"
            );
        }
    }

    mod verify_download_options_compatibility {

        use super::*;

        #[test]
        fn verify_download_options_compatibility_succeeds_if_without_ancillary_download() {
            let download_options = DownloadUnpackOptions {
                include_ancillary: false,
                ..DownloadUnpackOptions::default()
            };
            let immutable_file_range = ImmutableFileRange::Range(1, 10);
            let last_immutable_file_number = 10;

            InternalArtifactDownloader::verify_download_options_compatibility(
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
            let download_options = DownloadUnpackOptions {
                include_ancillary: true,
                ..DownloadUnpackOptions::default()
            };
            let immutable_file_range = ImmutableFileRange::Range(7, 10);
            let last_immutable_file_number = 10;

            InternalArtifactDownloader::verify_download_options_compatibility(
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
            let download_options = DownloadUnpackOptions {
                include_ancillary: true,
                ..DownloadUnpackOptions::default()
            };
            let immutable_file_range = ImmutableFileRange::Range(7, 10);
            let last_immutable_file_number = 123;

            InternalArtifactDownloader::verify_download_options_compatibility(
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

            InternalArtifactDownloader::verify_can_write_to_target_directory(
                &target_dir,
                &DownloadUnpackOptions {
                    allow_override: true,
                    include_ancillary: false,
                    ..DownloadUnpackOptions::default()
                },
            )
            .unwrap();

            fs::create_dir_all(InternalArtifactDownloader::immutable_files_target_dir(
                &target_dir,
            ))
            .unwrap();
            fs::create_dir_all(InternalArtifactDownloader::volatile_target_dir(&target_dir))
                .unwrap();
            fs::create_dir_all(InternalArtifactDownloader::ledger_target_dir(&target_dir)).unwrap();
            InternalArtifactDownloader::verify_can_write_to_target_directory(
                &target_dir,
                &DownloadUnpackOptions {
                    allow_override: true,
                    include_ancillary: false,
                    ..DownloadUnpackOptions::default()
                },
            )
            .unwrap();
            InternalArtifactDownloader::verify_can_write_to_target_directory(
                &target_dir,
                &DownloadUnpackOptions {
                    allow_override: true,
                    include_ancillary: true,
                    ..DownloadUnpackOptions::default()
                },
            )
            .unwrap();
        }

        #[test]
        fn verify_can_write_to_target_dir_fails_without_allow_overwrite_and_non_empty_immutable_target_dir(
        ) {
            let target_dir = TempDir::new("cardano_database_client", "verify_can_write_to_target_dir_fails_without_allow_overwrite_and_non_empty_immutable_target_dir").build();
            fs::create_dir_all(InternalArtifactDownloader::immutable_files_target_dir(
                &target_dir,
            ))
            .unwrap();

            InternalArtifactDownloader::verify_can_write_to_target_directory(
                &target_dir,
                &DownloadUnpackOptions {
                    allow_override: false,
                    include_ancillary: false,
                    ..DownloadUnpackOptions::default()
                },
            )
            .expect_err("verify_can_write_to_target_dir should fail");

            InternalArtifactDownloader::verify_can_write_to_target_directory(
                &target_dir,
                &DownloadUnpackOptions {
                    allow_override: false,
                    include_ancillary: true,
                    ..DownloadUnpackOptions::default()
                },
            )
            .expect_err("verify_can_write_to_target_dir should fail");
        }

        #[test]
        fn verify_can_write_to_target_dir_fails_without_allow_overwrite_and_non_empty_ledger_target_dir(
        ) {
            let target_dir = TempDir::new("cardano_database_client", "verify_can_write_to_target_dir_fails_without_allow_overwrite_and_non_empty_ledger_target_dir").build();
            fs::create_dir_all(InternalArtifactDownloader::ledger_target_dir(&target_dir)).unwrap();

            InternalArtifactDownloader::verify_can_write_to_target_directory(
                &target_dir,
                &DownloadUnpackOptions {
                    allow_override: false,
                    include_ancillary: true,
                    ..DownloadUnpackOptions::default()
                },
            )
            .expect_err("verify_can_write_to_target_dir should fail");

            InternalArtifactDownloader::verify_can_write_to_target_directory(
                &target_dir,
                &DownloadUnpackOptions {
                    allow_override: false,
                    include_ancillary: false,
                    ..DownloadUnpackOptions::default()
                },
            )
            .unwrap();
        }

        #[test]
        fn verify_can_write_to_target_dir_fails_without_allow_overwrite_and_non_empty_volatile_target_dir(
        ) {
            let target_dir = TempDir::new("cardano_database_client", "verify_can_write_to_target_dir_fails_without_allow_overwrite_and_non_empty_volatile_target_dir").build();
            fs::create_dir_all(InternalArtifactDownloader::volatile_target_dir(&target_dir))
                .unwrap();

            InternalArtifactDownloader::verify_can_write_to_target_directory(
                &target_dir,
                &DownloadUnpackOptions {
                    allow_override: false,
                    include_ancillary: true,
                    ..DownloadUnpackOptions::default()
                },
            )
            .expect_err("verify_can_write_to_target_dir should fail");

            InternalArtifactDownloader::verify_can_write_to_target_directory(
                &target_dir,
                &DownloadUnpackOptions {
                    allow_override: false,
                    include_ancillary: false,
                    ..DownloadUnpackOptions::default()
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
                "download_unpack_immutable_files_fails_if_one_is_not_retrieved",
            )
            .build();
            let artifact_downloader = InternalArtifactDownloader::new(
                Arc::new(
                    MockFileDownloaderBuilder::default()
                        .with_success()
                        .next_call()
                        .with_failure()
                        .build(),
                ),
                None,
                FeedbackSender::new(&[]),
                TestLogger::stdout(),
            );

            let tasks = artifact_downloader
                .build_download_tasks_for_immutables(
                    &ImmutablesMessagePart {
                        locations: vec![ImmutablesLocation::CloudStorage {
                            uri: MultiFilesUri::Template(TemplateUri(
                                "http://whatever/{immutable_file_number}.tar.gz".to_string(),
                            )),
                            compression_algorithm: Some(CompressionAlgorithm::default()),
                        }],
                        average_size_uncompressed: 0,
                    },
                    immutable_file_range
                        .to_range_inclusive(total_immutable_files)
                        .unwrap(),
                    &target_dir,
                    "download_id",
                )
                .unwrap();

            artifact_downloader
                .batch_download_unpack(tasks.into(), 1)
                .await
                .expect_err("batch_download_unpack of the immutable files should fail");
        }

        #[tokio::test]
        async fn building_immutables_download_tasks_fails_if_all_locations_are_unknown() {
            let total_immutable_files = 2;
            let immutable_file_range = ImmutableFileRange::Range(1, total_immutable_files);
            let target_dir = TempDir::new("cardano_database_client", "download_unpack").build();
            let artifact_downloader = InternalArtifactDownloader::new(
                Arc::new(MockFileDownloader::new()),
                None,
                FeedbackSender::new(&[]),
                TestLogger::stdout(),
            );

            let build_tasks_result = artifact_downloader.build_download_tasks_for_immutables(
                &ImmutablesMessagePart {
                    locations: vec![ImmutablesLocation::Unknown {}],
                    average_size_uncompressed: 0,
                },
                immutable_file_range
                    .to_range_inclusive(total_immutable_files)
                    .unwrap(),
                &target_dir,
                "download_id",
            );

            assert!(
                build_tasks_result.is_err(),
                "building tasks should fail if all location are unknown"
            );
        }

        #[tokio::test]
        async fn download_unpack_immutable_files_succeeds_if_all_are_retrieved_with_same_location()
        {
            let total_immutable_files = 2;
            let immutable_file_range = ImmutableFileRange::Range(1, total_immutable_files);
            let target_dir = TempDir::new(
                "cardano_database_client",
                "download_unpack_immutable_files_succeeds_if_all_are_retrieved_with_same_location",
            )
            .build();
            let artifact_downloader = InternalArtifactDownloader::new(
                Arc::new(
                    MockFileDownloaderBuilder::default()
                        .with_times(2)
                        .with_success()
                        .build(),
                ),
                None,
                FeedbackSender::new(&[]),
                TestLogger::stdout(),
            );

            let tasks = artifact_downloader
                .build_download_tasks_for_immutables(
                    &ImmutablesMessagePart {
                        locations: vec![ImmutablesLocation::CloudStorage {
                            uri: MultiFilesUri::Template(TemplateUri(
                                "http://whatever-1/{immutable_file_number}.tar.gz".to_string(),
                            )),
                            compression_algorithm: Some(CompressionAlgorithm::default()),
                        }],
                        average_size_uncompressed: 0,
                    },
                    immutable_file_range
                        .to_range_inclusive(total_immutable_files)
                        .unwrap(),
                    &target_dir,
                    "download_id",
                )
                .unwrap();

            artifact_downloader
                .batch_download_unpack(tasks.into(), 1)
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
                "download_unpack_immutable_files_succeeds_if_all_are_retrieved_with_different_locations",
            )
            .build();
            let artifact_downloader = InternalArtifactDownloader::new(
                Arc::new(
                    MockFileDownloaderBuilder::default()
                        .with_file_uri("http://whatever-1/00001.tar.gz")
                        .with_target_dir(target_dir.clone())
                        .with_failure()
                        .next_call()
                        .with_file_uri("http://whatever-1/00002.tar.gz")
                        .with_target_dir(target_dir.clone())
                        .with_success()
                        .next_call()
                        .with_file_uri("http://whatever-2/00001.tar.gz")
                        .with_target_dir(target_dir.clone())
                        .with_success()
                        .build(),
                ),
                None,
                FeedbackSender::new(&[]),
                TestLogger::stdout(),
            );

            let tasks = artifact_downloader
                .build_download_tasks_for_immutables(
                    &ImmutablesMessagePart {
                        locations: vec![
                            ImmutablesLocation::CloudStorage {
                                uri: MultiFilesUri::Template(TemplateUri(
                                    "http://whatever-1/{immutable_file_number}.tar.gz".to_string(),
                                )),
                                compression_algorithm: Some(CompressionAlgorithm::default()),
                            },
                            ImmutablesLocation::CloudStorage {
                                uri: MultiFilesUri::Template(TemplateUri(
                                    "http://whatever-2/{immutable_file_number}.tar.gz".to_string(),
                                )),
                                compression_algorithm: Some(CompressionAlgorithm::default()),
                            },
                        ],
                        average_size_uncompressed: 0,
                    },
                    immutable_file_range
                        .to_range_inclusive(total_immutable_files)
                        .unwrap(),
                    &target_dir,
                    "download_id",
                )
                .unwrap();

            artifact_downloader
                .batch_download_unpack(tasks.into(), 1)
                .await
                .unwrap();
        }
    }

    mod download_unpack_ancillary_file {
        use mithril_common::crypto_helper::ManifestSigner;

        use crate::file_downloader::FakeAncillaryFileBuilder;

        use super::*;

        fn fake_ancillary_verifier() -> AncillaryVerifier {
            AncillaryVerifier::new(
                fake_keys::manifest_verification_key()[0]
                    .try_into()
                    .unwrap(),
            )
        }

        #[tokio::test]
        async fn fails_and_delete_temp_ancillary_download_dir_if_no_location_is_retrieved() {
            let target_dir = temp_dir_create!();
            let artifact_downloader = InternalArtifactDownloader::new(
                Arc::new(MockFileDownloaderBuilder::default().with_failure().build()),
                Some(Arc::new(fake_ancillary_verifier())),
                FeedbackSender::new(&[]),
                TestLogger::stdout(),
            );

            let task = artifact_downloader
                .new_ancillary_download_task(
                    &AncillaryMessagePart {
                        locations: vec![AncillaryLocation::CloudStorage {
                            uri: "http://whatever-1/ancillary.tar.gz".to_string(),
                            compression_algorithm: Some(CompressionAlgorithm::default()),
                        }],
                        size_uncompressed: 0,
                    },
                    &target_dir,
                    "download_id",
                )
                .unwrap();

            artifact_downloader
                .batch_download_unpack(vec![task].into(), 1)
                .await
                .expect_err("batch_download_unpack of ancillary file should fail");

            assert!(!InternalArtifactDownloader::temp_ancillary_target_dir(
                &target_dir,
                "download_id"
            )
            .exists());
        }

        #[tokio::test]
        async fn building_ancillary_download_tasks_fails_if_all_locations_are_unknown() {
            let target_dir = temp_dir_create!();
            let artifact_downloader = InternalArtifactDownloader::new(
                Arc::new(MockFileDownloader::new()),
                Some(Arc::new(fake_ancillary_verifier())),
                FeedbackSender::new(&[]),
                TestLogger::stdout(),
            );

            let build_tasks_result = artifact_downloader.new_ancillary_download_task(
                &AncillaryMessagePart {
                    locations: vec![AncillaryLocation::Unknown {}],
                    size_uncompressed: 0,
                },
                &target_dir,
                "download_id",
            );

            assert!(
                build_tasks_result.is_err(),
                "building tasks should fail if all location are unknown"
            );
        }

        #[tokio::test]
        async fn succeeds_if_at_least_one_location_is_retrieved() {
            let target_dir = temp_dir_create!();
            let ancillary_signer = ManifestSigner::create_deterministic_signer();
            let artifact_downloader = InternalArtifactDownloader::new(
                Arc::new(
                    MockFileDownloaderBuilder::default()
                        .with_file_uri("http://whatever-1/ancillary.tar.gz")
                        .with_target_dir(InternalArtifactDownloader::temp_ancillary_target_dir(
                            &target_dir,
                            "download_id",
                        ))
                        .with_failure()
                        .next_call()
                        .with_file_uri("http://whatever-2/ancillary.tar.gz")
                        .with_target_dir(InternalArtifactDownloader::temp_ancillary_target_dir(
                            &target_dir,
                            "download_id",
                        ))
                        .with_success_and_create_fake_ancillary_files(
                            FakeAncillaryFileBuilder::builder()
                                .files_in_manifest_to_create(vec!["ledger".to_string()])
                                .sign_manifest(ancillary_signer.clone())
                                .build(),
                        )
                        .build(),
                ),
                Some(Arc::new(AncillaryVerifier::new(
                    ancillary_signer.verification_key(),
                ))),
                FeedbackSender::new(&[]),
                TestLogger::stdout(),
            );

            let task = artifact_downloader
                .new_ancillary_download_task(
                    &AncillaryMessagePart {
                        locations: vec![
                            AncillaryLocation::CloudStorage {
                                uri: "http://whatever-1/ancillary.tar.gz".to_string(),
                                compression_algorithm: Some(CompressionAlgorithm::default()),
                            },
                            AncillaryLocation::CloudStorage {
                                uri: "http://whatever-2/ancillary.tar.gz".to_string(),
                                compression_algorithm: Some(CompressionAlgorithm::default()),
                            },
                        ],
                        size_uncompressed: 0,
                    },
                    &target_dir,
                    "download_id",
                )
                .unwrap();

            artifact_downloader
                .batch_download_unpack(vec![task].into(), 1)
                .await
                .unwrap();
        }

        #[tokio::test]
        async fn succeeds_when_first_location_is_retrieved() {
            let target_dir = temp_dir_create!();
            let ancillary_signer = ManifestSigner::create_deterministic_signer();
            let artifact_downloader = InternalArtifactDownloader::new(
                Arc::new(
                    MockFileDownloaderBuilder::default()
                        .with_file_uri("http://whatever-1/ancillary.tar.gz")
                        .with_target_dir(InternalArtifactDownloader::temp_ancillary_target_dir(
                            &target_dir,
                            "download_id",
                        ))
                        .with_success_and_create_fake_ancillary_files(
                            FakeAncillaryFileBuilder::builder()
                                .files_in_manifest_to_create(vec!["ledger".to_string()])
                                .sign_manifest(ancillary_signer.clone())
                                .build(),
                        )
                        .build(),
                ),
                Some(Arc::new(AncillaryVerifier::new(
                    ancillary_signer.verification_key(),
                ))),
                FeedbackSender::new(&[]),
                TestLogger::stdout(),
            );

            let task = artifact_downloader
                .new_ancillary_download_task(
                    &AncillaryMessagePart {
                        locations: vec![
                            AncillaryLocation::CloudStorage {
                                uri: "http://whatever-1/ancillary.tar.gz".to_string(),
                                compression_algorithm: Some(CompressionAlgorithm::default()),
                            },
                            AncillaryLocation::CloudStorage {
                                uri: "http://whatever-2/ancillary.tar.gz".to_string(),
                                compression_algorithm: Some(CompressionAlgorithm::default()),
                            },
                        ],
                        size_uncompressed: 0,
                    },
                    &target_dir,
                    "download_id",
                )
                .unwrap();

            artifact_downloader
                .batch_download_unpack(vec![task].into(), 1)
                .await
                .unwrap();
        }

        #[tokio::test]
        async fn fail_and_delete_temp_ancillary_download_dir_if_verify_fail() {
            let target_dir = temp_dir_create!();
            // The verifier will fail because the manifest is not signed
            let artifact_downloader = InternalArtifactDownloader::new(
                Arc::new(
                    MockFileDownloaderBuilder::default()
                        .with_file_uri("http://whatever-1/ancillary.tar.gz")
                        .with_success_and_create_fake_ancillary_files(
                            FakeAncillaryFileBuilder::builder()
                                .files_in_manifest_to_create(vec!["ledger".to_string()])
                                .build(),
                        )
                        .build(),
                ),
                Some(Arc::new(fake_ancillary_verifier())),
                FeedbackSender::new(&[]),
                TestLogger::stdout(),
            );

            let task = artifact_downloader
                .new_ancillary_download_task(
                    &AncillaryMessagePart {
                        locations: vec![AncillaryLocation::CloudStorage {
                            uri: "http://whatever-1/ancillary.tar.gz".to_string(),
                            compression_algorithm: Some(CompressionAlgorithm::default()),
                        }],
                        size_uncompressed: 0,
                    },
                    &target_dir,
                    "download_id",
                )
                .unwrap();

            artifact_downloader
                .batch_download_unpack(vec![task].into(), 1)
                .await
                .unwrap_err();

            assert!(!InternalArtifactDownloader::temp_ancillary_target_dir(
                &target_dir,
                "download_id"
            )
            .exists());
        }

        #[tokio::test]
        async fn move_file_in_manifest_then_delete_temporary_unpack_subfolder_if_verify_succeed() {
            let target_dir = temp_dir_create!();
            let ancillary_signer = ManifestSigner::create_deterministic_signer();
            let verification_key = ancillary_signer.verification_key();
            let artifact_downloader = InternalArtifactDownloader::new(
                Arc::new(
                    MockFileDownloaderBuilder::default()
                        .with_file_uri("http://whatever/ancillary.tar.gz")
                        .with_target_dir(InternalArtifactDownloader::temp_ancillary_target_dir(
                            &target_dir,
                            "download_id",
                        ))
                        .with_success_and_create_fake_ancillary_files(
                            FakeAncillaryFileBuilder::builder()
                                .files_in_manifest_to_create(vec!["dummy_ledger".to_string()])
                                .files_not_in_manifest_to_create(vec![
                                    "not_in_ancillary".to_string()
                                ])
                                .sign_manifest(ancillary_signer)
                                .build(),
                        )
                        .build(),
                ),
                Some(Arc::new(AncillaryVerifier::new(verification_key))),
                FeedbackSender::new(&[]),
                TestLogger::stdout(),
            );

            let task = artifact_downloader
                .new_ancillary_download_task(
                    &AncillaryMessagePart {
                        locations: vec![AncillaryLocation::CloudStorage {
                            uri: "http://whatever/ancillary.tar.gz".to_string(),
                            compression_algorithm: Some(CompressionAlgorithm::default()),
                        }],
                        size_uncompressed: 0,
                    },
                    &target_dir,
                    "download_id",
                )
                .unwrap();

            artifact_downloader
                .batch_download_unpack(vec![task].into(), 1)
                .await
                .unwrap();

            assert!(target_dir.join("dummy_ledger").exists());
            assert!(!target_dir.join("not_in_ancillary").exists());
            assert!(!InternalArtifactDownloader::temp_ancillary_target_dir(
                &target_dir,
                "download-id"
            )
            .exists());
        }
    }
}

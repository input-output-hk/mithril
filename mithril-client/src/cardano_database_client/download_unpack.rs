use std::collections::{BTreeSet, VecDeque};
use std::future::Future;
use std::ops::RangeInclusive;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use std::sync::Arc;
use tokio::task::JoinSet;

use anyhow::anyhow;

use mithril_common::{
    digesters::{IMMUTABLE_DIR, LEDGER_DIR, VOLATILE_DIR},
    entities::{AncillaryLocation, CompressionAlgorithm, ImmutableFileNumber, ImmutablesLocation},
    messages::CardanoDatabaseSnapshotMessage,
};

use crate::feedback::{FeedbackSender, MithrilEvent, MithrilEventCardanoDatabase};
use crate::file_downloader::{DownloadEvent, FileDownloader, FileDownloaderUri};
use crate::utils::VecDequeExtensions;
use crate::MithrilResult;

use super::immutable_file_range::ImmutableFileRange;

/// The future type for downloading a file
type DownloadFuture = dyn Future<Output = MithrilResult<()>> + Send;

/// A task to download and unpack a file
struct DownloadTask {
    name: String,
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
    feedback_sender: FeedbackSender,
    logger: slog::Logger,
}

impl InternalArtifactDownloader {
    /// Constructs a new `InternalArtifactDownloader`.
    pub fn new(
        http_file_downloader: Arc<dyn FileDownloader>,
        feedback_sender: FeedbackSender,
        logger: slog::Logger,
    ) -> Self {
        Self {
            http_file_downloader,
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
        let immutable_locations = &cardano_database_snapshot.immutables.locations;
        let average_size_uncompressed = cardano_database_snapshot
            .immutables
            .average_size_uncompressed;

        let tasks = VecDeque::from(self.build_download_tasks_for_immutables(
            immutable_locations,
            immutable_file_number_range,
            target_dir,
            average_size_uncompressed,
            &download_id,
        )?);
        if download_unpack_options.include_ancillary {
            let ancillary = &cardano_database_snapshot.ancillary;
            self.download_unpack_ancillary_file(
                &ancillary.locations,
                ancillary.size_uncompressed,
                target_dir,
                &download_id,
            )
            .await?;
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
        immutable_locations: &[ImmutablesLocation],
        immutable_file_number_range: RangeInclusive<ImmutableFileNumber>,
        target_dir: &Path,
        average_size_uncompressed: u64,
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
                average_size_uncompressed,
                download_id,
            )?);
        }
        Ok(immutable_tasks)
    }

    fn new_immutable_download_task(
        &self,
        locations: &[ImmutablesLocation],
        immutable_file_number: ImmutableFileNumber,
        immutable_files_target_dir: &Path,
        average_size_uncompressed: u64,
        download_id: &str,
    ) -> MithrilResult<DownloadTask> {
        let mut locations_to_try = vec![];
        let mut locations_sorted = locations.to_owned();
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
                ImmutablesLocation::Unknown => {
                    return Err(anyhow!("Unknown location type to download immutable"));
                }
            };

            locations_to_try.push(location_to_try);
        }

        Ok(DownloadTask {
            name: format!("immutable_file_{:05}", immutable_file_number),
            locations_to_try,
            target_dir: immutable_files_target_dir.to_path_buf(),
            size_uncompressed: average_size_uncompressed,
            download_event: DownloadEvent::Immutable {
                download_id: download_id.to_string(),
                immutable_file_number,
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

        while !tasks.is_empty() {
            let tasks_chunk = tasks.pop_up_to_n(max_parallel_downloads);

            for task in tasks_chunk {
                join_set.spawn(self.spawn_download_future(task));
            }
            while let Some(result) = join_set.join_next().await {
                if let Err(error) = result? {
                    join_set.abort_all();
                    anyhow::bail!(error);
                }
            }
        }

        Ok(())
    }

    /// Spawn a download future that can be added to a join set.
    ///
    /// The download is attempted for each location until the file is downloaded.
    /// If all locations fail, an error is returned.
    fn spawn_download_future(&self, task: DownloadTask) -> Pin<Box<DownloadFuture>> {
        let logger_clone = self.logger.clone();
        let download_future = async move {
            let tried_locations = task.tried_locations();
            for location_to_try in task.locations_to_try {
                let downloaded = location_to_try
                    .file_downloader
                    .download_unpack(
                        &location_to_try.file_downloader_uri,
                        task.size_uncompressed,
                        &task.target_dir,
                        location_to_try.compression_algorithm,
                        task.download_event.clone(),
                    )
                    .await;

                if let Err(e) = downloaded {
                    slog::error!(
                        logger_clone,
                        "Failed downloading and unpacking {} for location {:?}",
                        task.name, location_to_try.file_downloader_uri;
                        "error" => ?e
                    );
                } else {
                    return Ok(());
                }
            }

            Err(anyhow!(
                "All locations failed for {}, tried locations: {tried_locations}",
                task.name,
            ))
        };

        Box::pin(download_future)
    }

    /// Download and unpack the ancillary files.
    pub(crate) async fn download_unpack_ancillary_file(
        &self,
        locations: &[AncillaryLocation],
        file_size: u64,
        ancillary_file_target_dir: &Path,
        download_id: &str,
    ) -> MithrilResult<()> {
        let mut locations_sorted = locations.to_owned();
        locations_sorted.sort();
        for location in locations_sorted {
            let (file_downloader, compression_algorithm) = match &location {
                AncillaryLocation::CloudStorage {
                    uri: _,
                    compression_algorithm,
                } => (self.http_file_downloader.clone(), *compression_algorithm),
                AncillaryLocation::Unknown => {
                    continue;
                }
            };
            let file_downloader_uri = location.try_into()?;
            let downloaded = file_downloader
                .download_unpack(
                    &file_downloader_uri,
                    file_size,
                    ancillary_file_target_dir,
                    compression_algorithm,
                    DownloadEvent::Ancillary {
                        download_id: download_id.to_string(),
                    },
                )
                .await;
            match downloaded {
                Ok(_) => {
                    return Ok(());
                }
                Err(e) => {
                    slog::error!(
                        self.logger,
                        "Failed downloading and unpacking ancillaries for location {file_downloader_uri:?}"; "error" => ?e
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
        entities::{CardanoDbBeacon, Epoch, MultiFilesUri, TemplateUri},
        messages::CardanoDatabaseSnapshotMessage as CardanoDatabaseSnapshot,
        test_utils::TempDir,
    };

    use crate::cardano_database_client::CardanoDatabaseClientDependencyInjector;
    use crate::file_downloader::{MockFileDownloader, MockFileDownloaderBuilder};
    use crate::test_utils;

    use super::*;

    mod download_unpack {

        use mithril_common::messages::{
            AncillaryMessagePart, DigestsMessagePart, ImmutablesMessagePart,
        };

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
                        .with_target_dir(target_dir.clone())
                        .with_compression(Some(CompressionAlgorithm::Gzip))
                        .with_success()
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
                .unwrap();
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
                FeedbackSender::new(&[]),
                test_utils::test_logger(),
            );

            let tasks = artifact_downloader
                .build_download_tasks_for_immutables(
                    &[ImmutablesLocation::CloudStorage {
                        uri: MultiFilesUri::Template(TemplateUri(
                            "http://whatever/{immutable_file_number}.tar.gz".to_string(),
                        )),
                        compression_algorithm: Some(CompressionAlgorithm::default()),
                    }],
                    immutable_file_range
                        .to_range_inclusive(total_immutable_files)
                        .unwrap(),
                    &target_dir,
                    0,
                    "download_id",
                )
                .unwrap();

            artifact_downloader
                .batch_download_unpack(tasks.into(), 1)
                .await
                .expect_err("batch_download_unpack of the immutable files should fail");
        }

        #[tokio::test]
        async fn building_immutables_download_tasks_fails_if_location_is_unknown() {
            let total_immutable_files = 2;
            let immutable_file_range = ImmutableFileRange::Range(1, total_immutable_files);
            let target_dir = TempDir::new("cardano_database_client", "download_unpack").build();
            let artifact_downloader = InternalArtifactDownloader::new(
                Arc::new(MockFileDownloader::new()),
                FeedbackSender::new(&[]),
                test_utils::test_logger(),
            );

            let build_tasks_result = artifact_downloader.build_download_tasks_for_immutables(
                &[ImmutablesLocation::Unknown {}],
                immutable_file_range
                    .to_range_inclusive(total_immutable_files)
                    .unwrap(),
                &target_dir,
                0,
                "download_id",
            );

            assert!(
                build_tasks_result.is_err(),
                "building tasks should fail if a location is unknown"
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
                FeedbackSender::new(&[]),
                test_utils::test_logger(),
            );

            let tasks = artifact_downloader
                .build_download_tasks_for_immutables(
                    &[ImmutablesLocation::CloudStorage {
                        uri: MultiFilesUri::Template(TemplateUri(
                            "http://whatever-1/{immutable_file_number}.tar.gz".to_string(),
                        )),
                        compression_algorithm: Some(CompressionAlgorithm::default()),
                    }],
                    immutable_file_range
                        .to_range_inclusive(total_immutable_files)
                        .unwrap(),
                    &target_dir,
                    0,
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
                FeedbackSender::new(&[]),
                test_utils::test_logger(),
            );

            let tasks = artifact_downloader
                .build_download_tasks_for_immutables(
                    &[
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
                    immutable_file_range
                        .to_range_inclusive(total_immutable_files)
                        .unwrap(),
                    &target_dir,
                    0,
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

        use super::*;

        #[tokio::test]
        async fn download_unpack_ancillary_file_fails_if_no_location_is_retrieved() {
            let target_dir = Path::new(".");
            let artifact_downloader = InternalArtifactDownloader::new(
                Arc::new(MockFileDownloaderBuilder::default().with_failure().build()),
                FeedbackSender::new(&[]),
                test_utils::test_logger(),
            );

            artifact_downloader
                .download_unpack_ancillary_file(
                    &[AncillaryLocation::CloudStorage {
                        uri: "http://whatever-1/ancillary.tar.gz".to_string(),
                        compression_algorithm: Some(CompressionAlgorithm::default()),
                    }],
                    0,
                    target_dir,
                    "download_id",
                )
                .await
                .expect_err("download_unpack_ancillary_file should fail");
        }

        #[tokio::test]
        async fn download_unpack_ancillary_files_fails_if_location_is_unknown() {
            let target_dir = Path::new(".");
            let artifact_downloader = InternalArtifactDownloader::new(
                Arc::new(MockFileDownloader::new()),
                FeedbackSender::new(&[]),
                test_utils::test_logger(),
            );

            artifact_downloader
                .download_unpack_ancillary_file(
                    &[AncillaryLocation::Unknown {}],
                    0,
                    target_dir,
                    "download_id",
                )
                .await
                .expect_err("download_unpack_ancillary_file should fail");
        }

        #[tokio::test]
        async fn download_unpack_ancillary_file_succeeds_if_at_least_one_location_is_retrieved() {
            let target_dir = Path::new(".");
            let artifact_downloader = InternalArtifactDownloader::new(
                Arc::new(
                    MockFileDownloaderBuilder::default()
                        .with_file_uri("http://whatever-1/ancillary.tar.gz")
                        .with_target_dir(target_dir.to_path_buf())
                        .with_failure()
                        .next_call()
                        .with_file_uri("http://whatever-2/ancillary.tar.gz")
                        .with_target_dir(target_dir.to_path_buf())
                        .with_success()
                        .build(),
                ),
                FeedbackSender::new(&[]),
                test_utils::test_logger(),
            );

            artifact_downloader
                .download_unpack_ancillary_file(
                    &[
                        AncillaryLocation::CloudStorage {
                            uri: "http://whatever-1/ancillary.tar.gz".to_string(),
                            compression_algorithm: Some(CompressionAlgorithm::default()),
                        },
                        AncillaryLocation::CloudStorage {
                            uri: "http://whatever-2/ancillary.tar.gz".to_string(),
                            compression_algorithm: Some(CompressionAlgorithm::default()),
                        },
                    ],
                    0,
                    target_dir,
                    "download_id",
                )
                .await
                .unwrap();
        }

        #[tokio::test]
        async fn download_unpack_ancillary_file_succeeds_when_first_location_is_retrieved() {
            let target_dir = Path::new(".");
            let artifact_downloader = InternalArtifactDownloader::new(
                Arc::new(
                    MockFileDownloaderBuilder::default()
                        .with_file_uri("http://whatever-1/ancillary.tar.gz")
                        .with_target_dir(target_dir.to_path_buf())
                        .with_success()
                        .build(),
                ),
                FeedbackSender::new(&[]),
                test_utils::test_logger(),
            );

            artifact_downloader
                .download_unpack_ancillary_file(
                    &[
                        AncillaryLocation::CloudStorage {
                            uri: "http://whatever-1/ancillary.tar.gz".to_string(),
                            compression_algorithm: Some(CompressionAlgorithm::default()),
                        },
                        AncillaryLocation::CloudStorage {
                            uri: "http://whatever-2/ancillary.tar.gz".to_string(),
                            compression_algorithm: Some(CompressionAlgorithm::default()),
                        },
                    ],
                    0,
                    target_dir,
                    "download_id",
                )
                .await
                .unwrap();
        }
    }
}

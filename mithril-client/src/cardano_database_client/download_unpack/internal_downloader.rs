use std::collections::{BTreeSet, VecDeque};
use std::ops::RangeInclusive;
use std::path::Path;
use std::sync::Arc;

use anyhow::anyhow;
use tokio::task::JoinSet;

use mithril_common::entities::{AncillaryLocation, ImmutableFileNumber, ImmutablesLocation};
use mithril_common::messages::{
    AncillaryMessagePart, CardanoDatabaseSnapshotMessage, ImmutablesMessagePart,
};

use crate::cardano_database_client::ImmutableFileRange;
use crate::feedback::{FeedbackSender, MithrilEvent, MithrilEventCardanoDatabase};
use crate::file_downloader::{DownloadEvent, FileDownloader, FileDownloaderUri};
use crate::utils::{create_bootstrap_node_files, AncillaryVerifier, VecDequeExtensions};
use crate::MithrilResult;

use super::download_task::{DownloadKind, DownloadTask, LocationToDownload};
use super::DownloadUnpackOptions;

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
        download_unpack_options
            .verify_compatibility(&immutable_file_number_range, last_immutable_file_number)?;
        download_unpack_options.verify_can_write_to_target_directory(target_dir)?;

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

        create_bootstrap_node_files(&self.logger, target_dir, &cardano_database_snapshot.network)?;

        self.feedback_sender
            .send_event(MithrilEvent::CardanoDatabase(
                MithrilEventCardanoDatabase::Completed {
                    download_id: download_id.clone(),
                },
            ))
            .await;

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
        test_utils::temp_dir_create,
    };

    use crate::cardano_database_client::CardanoDatabaseClientDependencyInjector;
    use crate::file_downloader::MockFileDownloaderBuilder;
    use crate::test_utils::TestLogger;

    use super::*;

    mod download_unpack {
        use mithril_common::crypto_helper::ManifestSigner;
        use mithril_common::entities::CompressionAlgorithm;
        use mithril_common::messages::DigestsMessagePart;

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
            let target_dir = temp_dir_create!();
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
            let target_dir = temp_dir_create!();
            fs::create_dir_all(target_dir.join("immutable")).unwrap();
            let client =
                CardanoDatabaseClientDependencyInjector::new().build_cardano_database_client();

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
            let target_dir = temp_dir_create!();
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

    mod building_download_tasks {
        use mithril_common::test_utils::fake_keys;

        use crate::file_downloader::MockFileDownloader;

        use super::*;

        fn fake_ancillary_verifier() -> AncillaryVerifier {
            AncillaryVerifier::new(
                fake_keys::manifest_verification_key()[0]
                    .try_into()
                    .unwrap(),
            )
        }

        #[tokio::test]
        async fn building_immutables_download_tasks_fails_if_all_locations_are_unknown() {
            let total_immutable_files = 2;
            let immutable_file_range = ImmutableFileRange::Range(1, total_immutable_files);
            let target_dir = temp_dir_create!();
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
    }
}

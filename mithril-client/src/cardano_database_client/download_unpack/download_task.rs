use std::future::Future;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use std::sync::Arc;

use anyhow::{Context, anyhow};
use slog::Logger;

use mithril_common::entities::{CompressionAlgorithm, ImmutableFileNumber};

use crate::{
    MithrilResult,
    file_downloader::{DownloadEvent, FileDownloader, FileDownloaderUri},
    utils::AncillaryVerifier,
};

/// The future type for downloading a file
type DownloadFuture = dyn Future<Output = MithrilResult<()>> + Send;

/// A task to download and unpack a file
pub struct DownloadTask {
    pub kind: DownloadKind,
    pub locations_to_try: Vec<LocationToDownload>,
    pub size_uncompressed: u64,
    pub target_dir: PathBuf,
    pub download_event: DownloadEvent,
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
        self.download_unpack_file(ancillary_files_temp_dir, logger).await?;
        let validated_manifest = ancillary_verifier.verify(ancillary_files_temp_dir).await?;
        validated_manifest.move_to_final_location(target_dir).await
    }

    /// Build a future that will download and unpack the file when awaited.
    ///
    /// The download is attempted for each location until the file is downloaded.
    /// If all locations fail, an error is returned.
    pub fn build_download_future(self, logger: Logger) -> Pin<Box<DownloadFuture>> {
        let download_future = async move {
            match &self.kind {
                DownloadKind::Immutable(..) => {
                    self.download_unpack_file(&self.target_dir, &logger).await
                }
                DownloadKind::Ancillary { verifier } => {
                    let ancillary_files_temp_dir = temp_ancillary_target_dir(
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

fn temp_ancillary_target_dir(target_dir: &Path, download_id: &str) -> PathBuf {
    target_dir.join(format!("ancillary-{download_id}"))
}

pub enum DownloadKind {
    Immutable(ImmutableFileNumber),
    Ancillary { verifier: Arc<AncillaryVerifier> },
}

pub struct LocationToDownload {
    pub file_downloader: Arc<dyn FileDownloader>,
    pub file_downloader_uri: FileDownloaderUri,
    pub compression_algorithm: Option<CompressionAlgorithm>,
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::FileUri;
    use mithril_common::test_utils::{assert_dir_eq, double::fake_keys, temp_dir_create};

    use crate::file_downloader::MockFileDownloaderBuilder;
    use crate::test_utils::TestLogger;

    use super::*;

    fn create_locations_to_download<U>(
        file_downloader: Arc<dyn FileDownloader>,
        uris: U,
    ) -> Vec<LocationToDownload>
    where
        U: IntoIterator,
        U::Item: AsRef<str>,
    {
        uris.into_iter()
            .map(|uri| LocationToDownload {
                file_downloader: file_downloader.clone(),
                file_downloader_uri: FileDownloaderUri::FileUri(FileUri(uri.as_ref().to_string())),
                compression_algorithm: Some(CompressionAlgorithm::default()),
            })
            .collect()
    }

    mod download_unpack_immutable_files {
        use super::*;

        #[tokio::test]
        async fn fails_if_no_location_is_retrieved() {
            let target_dir = temp_dir_create!();
            let logger = TestLogger::stdout();
            let file_downloader = Arc::new(
                MockFileDownloaderBuilder::default()
                    .with_times(2)
                    .with_failure()
                    .build(),
            );

            let download_task = DownloadTask {
                kind: DownloadKind::Immutable(1),
                locations_to_try: create_locations_to_download(
                    file_downloader,
                    ["http://whatever-1/00001.tar.gz", "http://whatever-2/00001.tar.gz"],
                ),
                size_uncompressed: 0,
                target_dir: target_dir.clone(),
                download_event: DownloadEvent::Immutable {
                    download_id: "download_id".to_string(),
                    immutable_file_number: 1,
                },
            };

            download_task
                .build_download_future(logger)
                .await
                .expect_err("downloading should fail when all locations fail");
        }

        #[tokio::test]
        async fn succeeds_if_at_least_one_location_is_retrieved() {
            let target_dir = temp_dir_create!();
            let logger = TestLogger::stdout();
            let file_downloader = Arc::new(
                MockFileDownloaderBuilder::default()
                    .with_file_uri("http://whatever-1/00001.tar.gz")
                    .with_target_dir(target_dir.clone())
                    .with_failure()
                    .next_call()
                    .with_file_uri("http://whatever-2/00001.tar.gz")
                    .with_target_dir(target_dir.clone())
                    .with_success()
                    .build(),
            );

            let download_task = DownloadTask {
                kind: DownloadKind::Immutable(1),
                locations_to_try: create_locations_to_download(
                    file_downloader,
                    ["http://whatever-1/00001.tar.gz", "http://whatever-2/00001.tar.gz"],
                ),
                size_uncompressed: 0,
                target_dir: target_dir.clone(),
                download_event: DownloadEvent::Immutable {
                    download_id: "download_id".to_string(),
                    immutable_file_number: 1,
                },
            };

            download_task.build_download_future(logger).await.unwrap();
        }
    }

    mod download_unpack_ancillary_file {
        use mithril_common::crypto_helper::ManifestSigner;

        use crate::file_downloader::FakeAncillaryFileBuilder;

        use super::*;

        fn fake_ancillary_verifier() -> AncillaryVerifier {
            AncillaryVerifier::new(fake_keys::manifest_verification_key()[0].try_into().unwrap())
        }

        #[tokio::test]
        async fn fails_and_delete_temp_ancillary_download_dir_if_no_location_is_retrieved() {
            let target_dir = temp_dir_create!();
            let file_downloader =
                Arc::new(MockFileDownloaderBuilder::default().with_failure().build());

            let download_task = DownloadTask {
                kind: DownloadKind::Ancillary {
                    verifier: Arc::new(fake_ancillary_verifier()),
                },
                locations_to_try: create_locations_to_download(
                    file_downloader,
                    ["http://whatever/ancillary.tar.gz"],
                ),
                size_uncompressed: 0,
                target_dir: target_dir.clone(),
                download_event: DownloadEvent::Ancillary {
                    download_id: "download_id".to_string(),
                },
            };

            download_task
                .build_download_future(TestLogger::stdout())
                .await
                .expect_err("batch_download_unpack of ancillary file should fail");

            assert!(!temp_ancillary_target_dir(&target_dir, "download_id").exists());
        }

        #[tokio::test]
        async fn succeeds_if_at_least_one_location_is_retrieved() {
            let target_dir = temp_dir_create!();
            let ancillary_signer = ManifestSigner::create_deterministic_signer();
            let file_downloader = Arc::new(
                MockFileDownloaderBuilder::default()
                    .with_file_uri("http://whatever-1/ancillary.tar.gz")
                    .with_target_dir(temp_ancillary_target_dir(&target_dir, "download_id"))
                    .with_failure()
                    .next_call()
                    .with_file_uri("http://whatever-2/ancillary.tar.gz")
                    .with_target_dir(temp_ancillary_target_dir(&target_dir, "download_id"))
                    .with_success_and_create_fake_ancillary_files(
                        FakeAncillaryFileBuilder::builder()
                            .files_in_manifest_to_create(vec!["ledger".to_string()])
                            .sign_manifest(ancillary_signer.clone())
                            .build(),
                    )
                    .build(),
            );

            let download_task = DownloadTask {
                kind: DownloadKind::Ancillary {
                    verifier: Arc::new(AncillaryVerifier::new(ancillary_signer.verification_key())),
                },
                locations_to_try: create_locations_to_download(
                    file_downloader,
                    [
                        "http://whatever-1/ancillary.tar.gz",
                        "http://whatever-2/ancillary.tar.gz",
                    ],
                ),
                size_uncompressed: 0,
                target_dir: target_dir.clone(),
                download_event: DownloadEvent::Ancillary {
                    download_id: "download_id".to_string(),
                },
            };

            download_task
                .build_download_future(TestLogger::stdout())
                .await
                .unwrap();
        }

        #[tokio::test]
        async fn succeeds_when_first_location_is_retrieved() {
            let target_dir = temp_dir_create!();
            let ancillary_signer = ManifestSigner::create_deterministic_signer();
            let file_downloader = Arc::new(
                MockFileDownloaderBuilder::default()
                    .with_file_uri("http://whatever-1/ancillary.tar.gz")
                    .with_target_dir(temp_ancillary_target_dir(&target_dir, "download_id"))
                    .with_success_and_create_fake_ancillary_files(
                        FakeAncillaryFileBuilder::builder()
                            .files_in_manifest_to_create(vec!["ledger".to_string()])
                            .sign_manifest(ancillary_signer.clone())
                            .build(),
                    )
                    .build(),
            );

            let download_task = DownloadTask {
                kind: DownloadKind::Ancillary {
                    verifier: Arc::new(AncillaryVerifier::new(ancillary_signer.verification_key())),
                },
                locations_to_try: create_locations_to_download(
                    file_downloader,
                    [
                        "http://whatever-1/ancillary.tar.gz",
                        "http://whatever-2/ancillary.tar.gz",
                    ],
                ),
                size_uncompressed: 0,
                target_dir: target_dir.clone(),
                download_event: DownloadEvent::Ancillary {
                    download_id: "download_id".to_string(),
                },
            };

            download_task
                .build_download_future(TestLogger::stdout())
                .await
                .unwrap();
        }

        #[tokio::test]
        async fn fail_and_delete_temp_ancillary_download_dir_if_verify_fail() {
            let target_dir = temp_dir_create!();
            // The verifier will fail because the manifest is not signed
            let file_downloader = Arc::new(
                MockFileDownloaderBuilder::default()
                    .with_file_uri("http://whatever/ancillary.tar.gz")
                    .with_success_and_create_fake_ancillary_files(
                        FakeAncillaryFileBuilder::builder()
                            .files_in_manifest_to_create(vec!["ledger".to_string()])
                            .build(),
                    )
                    .build(),
            );

            let download_task = DownloadTask {
                kind: DownloadKind::Ancillary {
                    verifier: Arc::new(fake_ancillary_verifier()),
                },
                locations_to_try: create_locations_to_download(
                    file_downloader,
                    ["http://whatever/ancillary.tar.gz"],
                ),
                size_uncompressed: 0,
                target_dir: target_dir.clone(),
                download_event: DownloadEvent::Ancillary {
                    download_id: "download_id".to_string(),
                },
            };

            download_task
                .build_download_future(TestLogger::stdout())
                .await
                .unwrap_err();

            assert!(!temp_ancillary_target_dir(&target_dir, "download_id").exists());
        }

        #[tokio::test]
        async fn move_file_in_manifest_then_delete_temporary_unpack_subfolder_if_verify_succeed() {
            let target_dir = temp_dir_create!();
            let ancillary_signer = ManifestSigner::create_deterministic_signer();
            let file_downloader = Arc::new(
                MockFileDownloaderBuilder::default()
                    .with_file_uri("http://whatever/ancillary.tar.gz")
                    .with_target_dir(temp_ancillary_target_dir(&target_dir, "download_id"))
                    .with_success_and_create_fake_ancillary_files(
                        FakeAncillaryFileBuilder::builder()
                            .files_in_manifest_to_create(vec!["dummy_ledger".to_string()])
                            .files_not_in_manifest_to_create(vec!["not_in_ancillary".to_string()])
                            .sign_manifest(ancillary_signer.clone())
                            .build(),
                    )
                    .build(),
            );

            let download_task = DownloadTask {
                kind: DownloadKind::Ancillary {
                    verifier: Arc::new(AncillaryVerifier::new(ancillary_signer.verification_key())),
                },
                locations_to_try: create_locations_to_download(
                    file_downloader,
                    ["http://whatever/ancillary.tar.gz"],
                ),
                size_uncompressed: 0,
                target_dir: target_dir.clone(),
                download_event: DownloadEvent::Ancillary {
                    download_id: "download_id".to_string(),
                },
            };

            download_task
                .build_download_future(TestLogger::stdout())
                .await
                .unwrap();

            assert_dir_eq!(&target_dir, "* dummy_ledger");
        }
    }
}

use std::{
    collections::BTreeMap,
    fs,
    path::{Path, PathBuf},
};

use anyhow::{anyhow, Context};

use mithril_common::{
    crypto_helper::{MKProof, MKTree, MKTreeNode, MKTreeStoreInMemory},
    digesters::{CardanoImmutableDigester, ImmutableDigester, ImmutableFile},
    entities::{DigestLocation, HexEncodedDigest, ImmutableFileName},
    messages::{
        CardanoDatabaseDigestListItemMessage, CardanoDatabaseSnapshotMessage, CertificateMessage,
    },
};

use crate::{feedback::MithrilEvent, file_downloader::FileDownloaderUri, MithrilResult};

use super::api::CardanoDatabaseClient;
use super::immutable_file_range::ImmutableFileRange;

impl CardanoDatabaseClient {
    /// Compute the Merkle proof of membership for the given immutable file range.
    pub async fn compute_merkle_proof(
        &self,
        certificate: &CertificateMessage,
        cardano_database_snapshot: &CardanoDatabaseSnapshotMessage,
        immutable_file_range: &ImmutableFileRange,
        database_dir: &Path,
    ) -> MithrilResult<MKProof> {
        let digest_locations = &cardano_database_snapshot.locations.digests;
        self.download_unpack_digest_file(digest_locations, &Self::digest_target_dir(database_dir))
            .await?;
        let network = certificate.metadata.network.clone();
        let last_immutable_file_number = cardano_database_snapshot.beacon.immutable_file_number;
        let immutable_file_number_range =
            immutable_file_range.to_range_inclusive(last_immutable_file_number)?;
        let downloaded_digests = self.read_digest_file(&Self::digest_target_dir(database_dir))?;
        let downloaded_digests_values = downloaded_digests
            .into_iter()
            .filter(|(immutable_file_name, _)| {
                match ImmutableFile::new(Path::new(immutable_file_name).to_path_buf()) {
                    Ok(immutable_file) => immutable_file.number <= last_immutable_file_number,
                    Err(_) => false,
                }
            })
            .map(|(_immutable_file_name, digest)| digest)
            .collect::<Vec<_>>();
        let merkle_tree: MKTree<MKTreeStoreInMemory> = MKTree::new(&downloaded_digests_values)?;
        let immutable_digester = CardanoImmutableDigester::new(network, None, self.logger.clone());
        let computed_digests = immutable_digester
            .compute_digests_for_range(database_dir, &immutable_file_number_range)
            .await?
            .entries
            .values()
            .map(MKTreeNode::from)
            .collect::<Vec<_>>();
        Self::delete_directory(&Self::digest_target_dir(database_dir))?;

        merkle_tree.compute_proof(&computed_digests)
    }

    async fn download_unpack_digest_file(
        &self,
        locations: &[DigestLocation],
        digest_file_target_dir: &Path,
    ) -> MithrilResult<()> {
        Self::create_directory_if_not_exists(digest_file_target_dir)?;
        let mut locations_sorted = locations.to_owned();
        locations_sorted.sort();
        for location in locations_sorted {
            let download_id = MithrilEvent::new_digest_download_id();
            self.feedback_sender
                .send_event(MithrilEvent::DigestDownloadStarted {
                    download_id: download_id.clone(),
                })
                .await;
            let file_downloader = self
                .digest_file_downloader_resolver
                .resolve(&location)
                .ok_or_else(|| {
                    anyhow!("Failed resolving a file downloader for location: {location:?}")
                })?;
            let file_downloader_uri: FileDownloaderUri = location.into();
            let downloaded = file_downloader
                .download_unpack(
                    &file_downloader_uri,
                    digest_file_target_dir,
                    None,
                    &download_id,
                    Self::feedback_event_builder_digest_download,
                )
                .await;
            match downloaded {
                Ok(_) => {
                    self.feedback_sender
                        .send_event(MithrilEvent::DigestDownloadCompleted { download_id })
                        .await;
                    return Ok(());
                }
                Err(e) => {
                    slog::error!(
                        self.logger,
                        "Failed downloading and unpacking digest for location {file_downloader_uri:?}"; "error" => e.to_string()
                    );
                }
            }
        }

        Err(anyhow!(
            "Failed downloading and unpacking digests for all locations"
        ))
    }

    fn read_digest_file(
        &self,
        digest_file_target_dir: &Path,
    ) -> MithrilResult<BTreeMap<ImmutableFileName, HexEncodedDigest>> {
        let digest_files = Self::read_files_in_directory(digest_file_target_dir)?;
        if digest_files.len() > 1 {
            return Err(anyhow!(
                "Multiple digest files found in directory: {digest_file_target_dir:?}"
            ));
        }
        if digest_files.is_empty() {
            return Err(anyhow!(
                "No digest file found in directory: {digest_file_target_dir:?}"
            ));
        }

        let digest_file = &digest_files[0];
        let content = fs::read_to_string(digest_file)
            .with_context(|| format!("Failed reading digest file: {digest_file:?}"))?;
        let digest_messages: Vec<CardanoDatabaseDigestListItemMessage> =
            serde_json::from_str(&content)
                .with_context(|| format!("Failed deserializing digest file: {digest_file:?}"))?;
        let digest_map = digest_messages
            .into_iter()
            .map(|message| (message.immutable_file_name, message.digest))
            .collect::<BTreeMap<_, _>>();

        Ok(digest_map)
    }

    fn feedback_event_builder_digest_download(
        download_id: String,
        downloaded_bytes: u64,
        size: u64,
    ) -> Option<MithrilEvent> {
        Some(MithrilEvent::DigestDownloadProgress {
            download_id,
            downloaded_bytes,
            size,
        })
    }

    fn digest_target_dir(target_dir: &Path) -> PathBuf {
        target_dir.join("digest")
    }

    fn create_directory_if_not_exists(dir: &Path) -> MithrilResult<()> {
        if dir.exists() {
            return Ok(());
        }

        fs::create_dir_all(dir).map_err(|e| anyhow!("Failed creating directory: {e}"))
    }

    fn delete_directory(dir: &Path) -> MithrilResult<()> {
        if dir.exists() {
            fs::remove_dir_all(dir).map_err(|e| anyhow!("Failed deleting directory: {e}"))?;
        }

        Ok(())
    }

    fn read_files_in_directory(dir: &Path) -> MithrilResult<Vec<PathBuf>> {
        let mut files = vec![];
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_file() {
                files.push(path);
            }
        }

        Ok(files)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;
    use std::fs;
    use std::io::Write;
    use std::path::Path;
    use std::sync::Arc;

    use mithril_common::{
        digesters::{DummyCardanoDbBuilder, ImmutableDigester, ImmutableFile},
        entities::{CardanoDbBeacon, DigestLocationDiscriminants, Epoch, HexEncodedDigest},
        messages::{ArtifactsLocationsMessagePart, CardanoDatabaseDigestListItemMessage},
        test_utils::TempDir,
    };

    use crate::{
        cardano_database_client::CardanoDatabaseClientDependencyInjector,
        feedback::StackFeedbackReceiver,
        file_downloader::{MockFileDownloader, MockFileDownloaderBuilder},
        test_utils::test_logger,
    };

    use super::*;

    mod compute_merkle_proof {

        use std::ops::RangeInclusive;

        use mithril_common::entities::ImmutableFileNumber;

        use super::*;

        async fn create_fake_digest_artifact(
            dir_name: &str,
            beacon: &CardanoDbBeacon,
            immutable_file_range: &RangeInclusive<ImmutableFileNumber>,
            digests_offset: usize,
        ) -> (
            PathBuf,
            CardanoDatabaseSnapshotMessage,
            CertificateMessage,
            MKTree<MKTreeStoreInMemory>,
        ) {
            let cardano_database_snapshot = CardanoDatabaseSnapshotMessage {
                hash: "hash-123".to_string(),
                beacon: beacon.clone(),
                locations: ArtifactsLocationsMessagePart {
                    digests: vec![DigestLocation::CloudStorage {
                        uri: "http://whatever/digests.json".to_string(),
                    }],
                    ..ArtifactsLocationsMessagePart::default()
                },
                ..CardanoDatabaseSnapshotMessage::dummy()
            };
            let certificate = CertificateMessage {
                hash: "cert-hash-123".to_string(),
                ..CertificateMessage::dummy()
            };
            let cardano_db = DummyCardanoDbBuilder::new(dir_name)
                .with_immutables(&immutable_file_range.clone().collect::<Vec<_>>())
                .append_immutable_trio()
                .build();
            let database_dir = cardano_db.get_dir();
            let immutable_digester = CardanoImmutableDigester::new(
                certificate.metadata.network.to_string(),
                None,
                test_logger(),
            );
            let computed_digests = immutable_digester
                .compute_digests_for_range(database_dir, immutable_file_range)
                .await
                .unwrap();
            write_digest_file(&database_dir.join("digest"), &computed_digests.entries).await;

            // We remove the last digests_offset digests to simulate receiving
            // a digest file with more immutable files than downloaded
            for (immutable_file, _digest) in
                computed_digests.entries.iter().rev().take(digests_offset)
            {
                fs::remove_file(
                    database_dir.join(
                        database_dir
                            .join("immutable")
                            .join(immutable_file.filename.clone()),
                    ),
                )
                .unwrap();
            }

            let merkle_tree = immutable_digester
                .compute_merkle_tree(database_dir, beacon)
                .await
                .unwrap();

            (
                database_dir.to_owned(),
                cardano_database_snapshot,
                certificate,
                merkle_tree,
            )
        }

        async fn write_digest_file(
            digest_dir: &Path,
            digests: &BTreeMap<ImmutableFile, HexEncodedDigest>,
        ) {
            let digest_file_path = digest_dir.join("digests.json");
            if !digest_dir.exists() {
                fs::create_dir_all(digest_dir).unwrap();
            }

            let immutable_digest_messages = digests
                .iter()
                .map(
                    |(immutable_file, digest)| CardanoDatabaseDigestListItemMessage {
                        immutable_file_name: immutable_file.filename.clone(),
                        digest: digest.to_string(),
                    },
                )
                .collect::<Vec<_>>();
            serde_json::to_writer(
                fs::File::create(digest_file_path).unwrap(),
                &immutable_digest_messages,
            )
            .unwrap();
        }

        #[tokio::test]
        async fn compute_merkle_proof_succeeds() {
            let beacon = CardanoDbBeacon {
                epoch: Epoch(123),
                immutable_file_number: 10,
            };
            let immutable_file_range = 1..=15;
            let immutable_file_range_to_prove = ImmutableFileRange::Range(2, 4);
            let digests_offset = 3;
            let (database_dir, cardano_database_snapshot, certificate, merkle_tree) =
                create_fake_digest_artifact(
                    "compute_merkle_proof_succeeds",
                    &beacon,
                    &immutable_file_range,
                    digests_offset,
                )
                .await;
            let expected_merkle_root = merkle_tree.compute_root().unwrap();
            let client = CardanoDatabaseClientDependencyInjector::new()
                .with_digest_file_downloaders(vec![(
                    DigestLocationDiscriminants::CloudStorage,
                    Arc::new({
                        MockFileDownloaderBuilder::default()
                            .with_file_uri("http://whatever/digests.json")
                            .with_target_dir(database_dir.join("digest"))
                            .with_compression(None)
                            .with_success()
                            .build()
                    }),
                )])
                .build_cardano_database_client();

            let merkle_proof = client
                .compute_merkle_proof(
                    &certificate,
                    &cardano_database_snapshot,
                    &immutable_file_range_to_prove,
                    &database_dir,
                )
                .await
                .unwrap();
            merkle_proof.verify().unwrap();

            let merkle_proof_root = merkle_proof.root().to_owned();
            assert_eq!(expected_merkle_root, merkle_proof_root);

            assert!(!database_dir.join("digest").exists());
        }
    }

    mod download_unpack_digest_file {

        use super::*;

        #[tokio::test]
        async fn download_unpack_digest_file_fails_if_no_location_is_retrieved() {
            let target_dir = Path::new(".");
            let client = CardanoDatabaseClientDependencyInjector::new()
                .with_digest_file_downloaders(vec![
                    (
                        DigestLocationDiscriminants::CloudStorage,
                        Arc::new(
                            MockFileDownloaderBuilder::default()
                                .with_compression(None)
                                .with_failure()
                                .build(),
                        ),
                    ),
                    (
                        DigestLocationDiscriminants::Aggregator,
                        Arc::new(
                            MockFileDownloaderBuilder::default()
                                .with_compression(None)
                                .with_failure()
                                .build(),
                        ),
                    ),
                ])
                .build_cardano_database_client();

            client
                .download_unpack_digest_file(
                    &[
                        DigestLocation::CloudStorage {
                            uri: "http://whatever-1/digests.json".to_string(),
                        },
                        DigestLocation::Aggregator {
                            uri: "http://whatever-2/digest".to_string(),
                        },
                    ],
                    target_dir,
                )
                .await
                .expect_err("download_unpack_digest_file should fail");
        }

        #[tokio::test]
        async fn download_unpack_digest_file_succeeds_if_at_least_one_location_is_retrieved() {
            let target_dir = Path::new(".");
            let client = CardanoDatabaseClientDependencyInjector::new()
                .with_digest_file_downloaders(vec![
                    (
                        DigestLocationDiscriminants::CloudStorage,
                        Arc::new(
                            MockFileDownloaderBuilder::default()
                                .with_compression(None)
                                .with_failure()
                                .build(),
                        ),
                    ),
                    (
                        DigestLocationDiscriminants::Aggregator,
                        Arc::new(
                            MockFileDownloaderBuilder::default()
                                .with_compression(None)
                                .with_success()
                                .build(),
                        ),
                    ),
                ])
                .build_cardano_database_client();

            client
                .download_unpack_digest_file(
                    &[
                        DigestLocation::CloudStorage {
                            uri: "http://whatever-1/digests.json".to_string(),
                        },
                        DigestLocation::Aggregator {
                            uri: "http://whatever-2/digest".to_string(),
                        },
                    ],
                    target_dir,
                )
                .await
                .unwrap();
        }

        #[tokio::test]
        async fn download_unpack_digest_file_succeeds_when_first_location_is_retrieved() {
            let target_dir = Path::new(".");
            let client = CardanoDatabaseClientDependencyInjector::new()
                .with_digest_file_downloaders(vec![
                    (
                        DigestLocationDiscriminants::CloudStorage,
                        Arc::new(
                            MockFileDownloaderBuilder::default()
                                .with_compression(None)
                                .with_success()
                                .build(),
                        ),
                    ),
                    (
                        DigestLocationDiscriminants::Aggregator,
                        Arc::new(MockFileDownloader::new()),
                    ),
                ])
                .build_cardano_database_client();

            client
                .download_unpack_digest_file(
                    &[
                        DigestLocation::CloudStorage {
                            uri: "http://whatever-1/digests.json".to_string(),
                        },
                        DigestLocation::Aggregator {
                            uri: "http://whatever-2/digest".to_string(),
                        },
                    ],
                    target_dir,
                )
                .await
                .unwrap();
        }

        #[tokio::test]
        async fn download_unpack_digest_file_sends_feedbacks() {
            let target_dir = Path::new(".");
            let feedback_receiver = Arc::new(StackFeedbackReceiver::new());
            let client = CardanoDatabaseClientDependencyInjector::new()
                .with_digest_file_downloaders(vec![(
                    DigestLocationDiscriminants::CloudStorage,
                    Arc::new(
                        MockFileDownloaderBuilder::default()
                            .with_compression(None)
                            .with_success()
                            .build(),
                    ),
                )])
                .with_feedback_receivers(&[feedback_receiver.clone()])
                .build_cardano_database_client();

            client
                .download_unpack_digest_file(
                    &[DigestLocation::CloudStorage {
                        uri: "http://whatever-1/digests.json".to_string(),
                    }],
                    target_dir,
                )
                .await
                .unwrap();

            let sent_events = feedback_receiver.stacked_events();
            let id = sent_events[0].event_id();
            let expected_events = vec![
                MithrilEvent::DigestDownloadStarted {
                    download_id: id.to_string(),
                },
                MithrilEvent::DigestDownloadCompleted {
                    download_id: id.to_string(),
                },
            ];
            assert_eq!(expected_events, sent_events);
        }
    }

    mod read_digest_file {

        use super::*;

        fn create_valid_fake_digest_file(
            file_path: &Path,
            digest_messages: &[CardanoDatabaseDigestListItemMessage],
        ) {
            let mut file = fs::File::create(file_path).unwrap();
            let digest_json = serde_json::to_string(&digest_messages).unwrap();
            file.write_all(digest_json.as_bytes()).unwrap();
        }

        fn create_invalid_fake_digest_file(file_path: &Path) {
            let mut file = fs::File::create(file_path).unwrap();
            file.write_all(b"incorrect-digest").unwrap();
        }

        #[test]
        fn read_digest_file_fails_when_no_digest_file() {
            let target_dir = TempDir::new(
                "cardano_database_client",
                "read_digest_file_fails_when_no_digest_file",
            )
            .build();
            let client =
                CardanoDatabaseClientDependencyInjector::new().build_cardano_database_client();

            client
                .read_digest_file(&target_dir)
                .expect_err("read_digest_file should fail");
        }

        #[test]
        fn read_digest_file_fails_when_multiple_digest_files() {
            let target_dir = TempDir::new(
                "cardano_database_client",
                "read_digest_file_fails_when_multiple_digest_files",
            )
            .build();
            create_valid_fake_digest_file(&target_dir.join("digests.json"), &[]);
            create_valid_fake_digest_file(&target_dir.join("digests-2.json"), &[]);
            let client =
                CardanoDatabaseClientDependencyInjector::new().build_cardano_database_client();

            client
                .read_digest_file(&target_dir)
                .expect_err("read_digest_file should fail");
        }

        #[test]
        fn read_digest_file_fails_when_invalid_unique_digest_file() {
            let target_dir = TempDir::new(
                "cardano_database_client",
                "read_digest_file_fails_when_invalid_unique_digest_file",
            )
            .build();
            create_invalid_fake_digest_file(&target_dir.join("digests.json"));
            let client =
                CardanoDatabaseClientDependencyInjector::new().build_cardano_database_client();

            client
                .read_digest_file(&target_dir)
                .expect_err("read_digest_file should fail");
        }

        #[test]
        fn read_digest_file_succeeds_when_valid_unique_digest_file() {
            let target_dir = TempDir::new(
                "cardano_database_client",
                "read_digest_file_succeeds_when_valid_unique_digest_file",
            )
            .build();
            let digest_messages = vec![
                CardanoDatabaseDigestListItemMessage {
                    immutable_file_name: "00001.chunk".to_string(),
                    digest: "digest-1".to_string(),
                },
                CardanoDatabaseDigestListItemMessage {
                    immutable_file_name: "00002.chunk".to_string(),
                    digest: "digest-2".to_string(),
                },
            ];
            create_valid_fake_digest_file(&target_dir.join("digests.json"), &digest_messages);
            let client =
                CardanoDatabaseClientDependencyInjector::new().build_cardano_database_client();

            let digests = client.read_digest_file(&target_dir).unwrap();
            assert_eq!(
                BTreeMap::from([
                    ("00001.chunk".to_string(), "digest-1".to_string()),
                    ("00002.chunk".to_string(), "digest-2".to_string())
                ]),
                digests
            )
        }
    }
}

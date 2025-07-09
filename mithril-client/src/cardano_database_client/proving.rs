use std::{
    collections::BTreeMap,
    fs,
    path::{Path, PathBuf},
    sync::Arc,
};

use anyhow::{Context, anyhow};

use mithril_cardano_node_internal_database::entities::ImmutableFile;
use mithril_common::{
    crypto_helper::{MKTree, MKTreeNode, MKTreeStoreInMemory},
    entities::{DigestLocation, HexEncodedDigest, ImmutableFileName, ProtocolMessagePartKey},
    messages::{
        CardanoDatabaseDigestListItemMessage, CardanoDatabaseSnapshotMessage, CertificateMessage,
        DigestsMessagePart,
    },
};

use crate::{
    MithrilResult,
    feedback::MithrilEvent,
    file_downloader::{DownloadEvent, FileDownloader, FileDownloaderUri},
    utils::{create_directory_if_not_exists, delete_directory, read_files_in_directory},
};

/// Represents the verified digests and the Merkle tree built from them.
pub struct VerifiedDigests {
    /// A map of immutable file names to their corresponding verified digests.
    pub digests: BTreeMap<ImmutableFileName, HexEncodedDigest>,
    /// The Merkle tree built from the digests.
    pub merkle_tree: MKTree<MKTreeStoreInMemory>,
}

pub struct InternalArtifactProver {
    http_file_downloader: Arc<dyn FileDownloader>,
    logger: slog::Logger,
}

impl InternalArtifactProver {
    /// Constructs a new `InternalArtifactProver`.
    pub fn new(http_file_downloader: Arc<dyn FileDownloader>, logger: slog::Logger) -> Self {
        Self {
            http_file_downloader,
            logger,
        }
    }

    fn check_merkle_root_is_signed_by_certificate(
        certificate: &CertificateMessage,
        merkle_root: &MKTreeNode,
    ) -> MithrilResult<()> {
        let mut message = certificate.protocol_message.clone();
        message.set_message_part(
            ProtocolMessagePartKey::CardanoDatabaseMerkleRoot,
            merkle_root.to_hex(),
        );

        if !certificate.match_message(&message) {
            return Err(anyhow!(
                "Certificate message does not match the computed message for certificate {}",
                certificate.hash
            ));
        }

        Ok(())
    }

    ///Download digests and verify its authenticity against the certificate.
    pub async fn download_and_verify_digests(
        &self,
        certificate: &CertificateMessage,
        cardano_database_snapshot: &CardanoDatabaseSnapshotMessage,
    ) -> MithrilResult<VerifiedDigests> {
        let digest_target_dir = Self::digest_target_dir();
        delete_directory(&digest_target_dir)?;
        self.download_unpack_digest_file(&cardano_database_snapshot.digests, &digest_target_dir)
            .await?;
        let last_immutable_file_number = cardano_database_snapshot.beacon.immutable_file_number;

        let downloaded_digests = self.read_digest_file(&digest_target_dir)?;
        delete_directory(&digest_target_dir)?;

        let filtered_digests = downloaded_digests
            .clone()
            .into_iter()
            .filter(|(immutable_file_name, _)| {
                match ImmutableFile::new(Path::new(immutable_file_name).to_path_buf()) {
                    Ok(immutable_file) => immutable_file.number <= last_immutable_file_number,
                    Err(_) => false,
                }
            })
            .collect::<BTreeMap<_, _>>();

        let filtered_digests_values = filtered_digests.values().collect::<Vec<_>>();
        let merkle_tree: MKTree<MKTreeStoreInMemory> = MKTree::new(&filtered_digests_values)?;

        Self::check_merkle_root_is_signed_by_certificate(
            certificate,
            &merkle_tree.compute_root()?,
        )?;

        Ok(VerifiedDigests {
            digests: filtered_digests,
            merkle_tree,
        })
    }

    async fn download_unpack_digest_file(
        &self,
        digests_locations: &DigestsMessagePart,
        digests_file_target_dir: &Path,
    ) -> MithrilResult<()> {
        create_directory_if_not_exists(digests_file_target_dir)?;
        let mut locations_sorted = digests_locations.sanitized_locations()?;
        locations_sorted.sort();
        for location in locations_sorted {
            let download_id = MithrilEvent::new_cardano_database_download_id();
            let (file_downloader, compression_algorithm) = match &location {
                DigestLocation::CloudStorage {
                    uri: _,
                    compression_algorithm,
                } => (self.http_file_downloader.clone(), *compression_algorithm),
                DigestLocation::Aggregator { .. } => (self.http_file_downloader.clone(), None),
                // Note: unknown locations should have been filtered out by `sanitized_locations`
                DigestLocation::Unknown => unreachable!(),
            };
            let file_downloader_uri: FileDownloaderUri = location.try_into()?;
            let downloaded = file_downloader
                .download_unpack(
                    &file_downloader_uri,
                    digests_locations.size_uncompressed,
                    digests_file_target_dir,
                    compression_algorithm,
                    DownloadEvent::Digest {
                        download_id: download_id.clone(),
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
                        "Failed downloading and unpacking digest for location {file_downloader_uri:?}"; "error" => ?e
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
        let digest_files = read_files_in_directory(digest_file_target_dir)?;
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

    fn digest_target_dir() -> PathBuf {
        std::env::temp_dir().join("mithril_digest")
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
        entities::{CardanoDbBeacon, Epoch, HexEncodedDigest},
        messages::CardanoDatabaseDigestListItemMessage,
        test::{TempDir, double::Dummy},
    };

    use crate::{
        cardano_database_client::CardanoDatabaseClientDependencyInjector,
        file_downloader::MockFileDownloaderBuilder, test_utils::TestLogger,
    };

    use super::*;

    mod download_and_verify_digests {
        use mithril_common::{
            StdResult,
            entities::{ProtocolMessage, ProtocolMessagePartKey},
            messages::DigestsMessagePart,
        };

        use super::*;

        fn write_digest_file(
            digest_dir: &Path,
            digests: &BTreeMap<ImmutableFile, HexEncodedDigest>,
        ) -> StdResult<()> {
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
            )?;

            Ok(())
        }

        fn build_digests_map(size: usize) -> BTreeMap<ImmutableFile, HexEncodedDigest> {
            let mut digests = BTreeMap::new();
            for i in 1..=size {
                for name in ["chunk", "primary", "secondary"] {
                    let immutable_file_name = format!("{i:05}.{name}");
                    let immutable_file =
                        ImmutableFile::new(PathBuf::from(immutable_file_name)).unwrap();
                    let digest = format!("digest-{i}-{name}");
                    digests.insert(immutable_file, digest);
                }
            }

            digests
        }

        #[tokio::test]
        async fn download_and_verify_digest_should_return_digest_map_acording_to_beacon() {
            let beacon = CardanoDbBeacon {
                epoch: Epoch(123),
                immutable_file_number: 42,
            };
            let hightest_immutable_number_in_digest_file =
                123 + beacon.immutable_file_number as usize;
            let digests_in_certificate_map =
                build_digests_map(beacon.immutable_file_number as usize);
            let protocol_message_merkle_root = {
                let digests_in_certificate_values =
                    digests_in_certificate_map.values().cloned().collect::<Vec<_>>();
                let certificate_merkle_tree: MKTree<MKTreeStoreInMemory> =
                    MKTree::new(&digests_in_certificate_values).unwrap();

                certificate_merkle_tree.compute_root().unwrap().to_hex()
            };
            let mut protocol_message = ProtocolMessage::new();
            protocol_message.set_message_part(
                ProtocolMessagePartKey::CardanoDatabaseMerkleRoot,
                protocol_message_merkle_root,
            );
            let certificate = CertificateMessage {
                protocol_message: protocol_message.clone(),
                signed_message: protocol_message.compute_hash(),
                ..CertificateMessage::dummy()
            };

            let digests_location = "http://whatever/digests.json";
            let cardano_database_snapshot = CardanoDatabaseSnapshotMessage {
                beacon,
                digests: DigestsMessagePart {
                    size_uncompressed: 1024,
                    locations: vec![DigestLocation::CloudStorage {
                        uri: digests_location.to_string(),
                        compression_algorithm: None,
                    }],
                },
                ..CardanoDatabaseSnapshotMessage::dummy()
            };
            let client = CardanoDatabaseClientDependencyInjector::new()
                .with_http_file_downloader(Arc::new(
                    MockFileDownloaderBuilder::default()
                        .with_file_uri(digests_location)
                        .with_target_dir(InternalArtifactProver::digest_target_dir())
                        .with_compression(None)
                        .with_returning(Box::new(move |_, _, _, _, _| {
                            write_digest_file(
                                &InternalArtifactProver::digest_target_dir(),
                                &build_digests_map(hightest_immutable_number_in_digest_file),
                            )?;

                            Ok(())
                        }))
                        .build(),
                ))
                .build_cardano_database_client();

            let verified_digests = client
                .download_and_verify_digests(&certificate, &cardano_database_snapshot)
                .await
                .unwrap();

            let expected_digests_in_certificate = digests_in_certificate_map
                .iter()
                .map(|(immutable_file, digest)| {
                    (immutable_file.filename.clone(), digest.to_string())
                })
                .collect();
            assert_eq!(verified_digests.digests, expected_digests_in_certificate);

            assert!(!InternalArtifactProver::digest_target_dir().exists());
        }
    }

    mod download_unpack_digest_file {

        use mithril_common::entities::CompressionAlgorithm;

        use crate::file_downloader::MockFileDownloader;

        use super::*;

        #[tokio::test]
        async fn fails_if_no_location_is_retrieved() {
            let target_dir = Path::new(".");
            let artifact_prover = InternalArtifactProver::new(
                Arc::new(
                    MockFileDownloaderBuilder::default()
                        .with_compression(None)
                        .with_failure()
                        .with_times(2)
                        .build(),
                ),
                TestLogger::stdout(),
            );

            artifact_prover
                .download_unpack_digest_file(
                    &DigestsMessagePart {
                        locations: vec![
                            DigestLocation::CloudStorage {
                                uri: "http://whatever-1/digests.json".to_string(),
                                compression_algorithm: None,
                            },
                            DigestLocation::Aggregator {
                                uri: "http://whatever-2/digest".to_string(),
                            },
                        ],
                        size_uncompressed: 0,
                    },
                    target_dir,
                )
                .await
                .expect_err("download_unpack_digest_file should fail");
        }

        #[tokio::test]
        async fn fails_if_all_locations_are_unknown() {
            let target_dir = Path::new(".");
            let artifact_prover = InternalArtifactProver::new(
                Arc::new(MockFileDownloader::new()),
                TestLogger::stdout(),
            );

            artifact_prover
                .download_unpack_digest_file(
                    &DigestsMessagePart {
                        locations: vec![DigestLocation::Unknown],
                        size_uncompressed: 0,
                    },
                    target_dir,
                )
                .await
                .expect_err("download_unpack_digest_file should fail");
        }

        #[tokio::test]
        async fn succeeds_if_at_least_one_location_is_retrieved() {
            let target_dir = Path::new(".");
            let artifact_prover = InternalArtifactProver::new(
                Arc::new(
                    MockFileDownloaderBuilder::default()
                        .with_compression(None)
                        .with_failure()
                        .next_call()
                        .with_compression(None)
                        .with_success()
                        .build(),
                ),
                TestLogger::stdout(),
            );

            artifact_prover
                .download_unpack_digest_file(
                    &DigestsMessagePart {
                        locations: vec![
                            DigestLocation::CloudStorage {
                                uri: "http://whatever-1/digests.json".to_string(),
                                compression_algorithm: None,
                            },
                            DigestLocation::Aggregator {
                                uri: "http://whatever-2/digest".to_string(),
                            },
                        ],
                        size_uncompressed: 0,
                    },
                    target_dir,
                )
                .await
                .unwrap();
        }

        #[tokio::test]
        async fn succeeds_when_first_location_is_retrieved() {
            let target_dir = Path::new(".");
            let artifact_prover = InternalArtifactProver::new(
                Arc::new(
                    MockFileDownloaderBuilder::default()
                        .with_compression(None)
                        .with_times(1)
                        .with_success()
                        .build(),
                ),
                TestLogger::stdout(),
            );

            artifact_prover
                .download_unpack_digest_file(
                    &DigestsMessagePart {
                        locations: vec![
                            DigestLocation::CloudStorage {
                                uri: "http://whatever-1/digests.json".to_string(),
                                compression_algorithm: None,
                            },
                            DigestLocation::Aggregator {
                                uri: "http://whatever-2/digest".to_string(),
                            },
                        ],
                        size_uncompressed: 0,
                    },
                    target_dir,
                )
                .await
                .unwrap();
        }

        #[tokio::test]
        async fn should_call_download_with_compression_algorithm() {
            let target_dir = Path::new(".");
            let artifact_prover = InternalArtifactProver::new(
                Arc::new(
                    MockFileDownloaderBuilder::default()
                        .with_compression(Some(CompressionAlgorithm::Gzip))
                        .with_times(1)
                        .with_success()
                        .build(),
                ),
                TestLogger::stdout(),
            );

            artifact_prover
                .download_unpack_digest_file(
                    &DigestsMessagePart {
                        locations: vec![
                            DigestLocation::CloudStorage {
                                uri: "http://whatever-1/digests.tar.gz".to_string(),
                                compression_algorithm: Some(CompressionAlgorithm::Gzip),
                            },
                            DigestLocation::Aggregator {
                                uri: "http://whatever-2/digest".to_string(),
                            },
                        ],
                        size_uncompressed: 0,
                    },
                    target_dir,
                )
                .await
                .unwrap();
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
            let artifact_prover = InternalArtifactProver::new(
                Arc::new(
                    MockFileDownloaderBuilder::default()
                        .with_times(0)
                        .with_success()
                        .build(),
                ),
                TestLogger::stdout(),
            );
            artifact_prover
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
            let artifact_prover = InternalArtifactProver::new(
                Arc::new(
                    MockFileDownloaderBuilder::default()
                        .with_times(0)
                        .with_success()
                        .build(),
                ),
                TestLogger::stdout(),
            );
            artifact_prover
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
            let artifact_prover = InternalArtifactProver::new(
                Arc::new(
                    MockFileDownloaderBuilder::default()
                        .with_times(0)
                        .with_success()
                        .build(),
                ),
                TestLogger::stdout(),
            );
            artifact_prover
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
            let artifact_prover = InternalArtifactProver::new(
                Arc::new(
                    MockFileDownloaderBuilder::default()
                        .with_times(0)
                        .with_success()
                        .build(),
                ),
                TestLogger::stdout(),
            );

            let digests = artifact_prover.read_digest_file(&target_dir).unwrap();
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

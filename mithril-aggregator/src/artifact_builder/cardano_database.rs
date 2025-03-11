use std::{path::PathBuf, sync::Arc};

use anyhow::{anyhow, Context};
use async_trait::async_trait;
use semver::Version;

use mithril_common::{
    entities::{
        AncillaryLocations, CardanoDatabaseSnapshot, CardanoDatabaseSnapshotArtifactData,
        CardanoDbBeacon, Certificate, DigestsLocations, ImmutablesLocations,
        ProtocolMessagePartKey, SignedEntityType,
    },
    CardanoNetwork, StdResult,
};

use crate::artifact_builder::{
    utils::compute_uncompressed_database_size, AncillaryArtifactBuilder, ArtifactBuilder,
};

use super::{DigestArtifactBuilder, ImmutableArtifactBuilder};

pub struct CardanoDatabaseArtifactBuilder {
    network: CardanoNetwork,
    db_directory: PathBuf,
    cardano_node_version: Version,
    ancillary_builder: Arc<AncillaryArtifactBuilder>,
    immutable_builder: Arc<ImmutableArtifactBuilder>,
    digest_builder: Arc<DigestArtifactBuilder>,
}

impl CardanoDatabaseArtifactBuilder {
    pub fn new(
        network: CardanoNetwork,
        db_directory: PathBuf,
        cardano_node_version: &Version,
        ancillary_builder: Arc<AncillaryArtifactBuilder>,
        immutable_builder: Arc<ImmutableArtifactBuilder>,
        digest_builder: Arc<DigestArtifactBuilder>,
    ) -> Self {
        Self {
            network,
            db_directory,
            cardano_node_version: cardano_node_version.clone(),
            ancillary_builder,
            immutable_builder,
            digest_builder,
        }
    }
}

#[async_trait]
impl ArtifactBuilder<CardanoDbBeacon, CardanoDatabaseSnapshot> for CardanoDatabaseArtifactBuilder {
    async fn compute_artifact(
        &self,
        beacon: CardanoDbBeacon,
        certificate: &Certificate,
    ) -> StdResult<CardanoDatabaseSnapshot> {
        let merkle_root = certificate
            .protocol_message
            .get_message_part(&ProtocolMessagePartKey::CardanoDatabaseMerkleRoot)
            .ok_or(anyhow!(
                "Can not find CardanoDatabaseMerkleRoot protocol message part in certificate"
            ))
            .with_context(|| {
                format!(
                    "Can not compute CardanoDatabase artifact for signed_entity: {:?}",
                    SignedEntityType::CardanoDatabase(beacon.clone())
                )
            })?;
        let total_db_size_uncompressed = {
            let db_directory = self.db_directory.clone();
            tokio::task::spawn_blocking(move || -> StdResult<u64> {
                compute_uncompressed_database_size(&db_directory)
            })
            .await??
        };

        let ancillary_locations = self.ancillary_builder.upload(&beacon).await?;
        let ancillary_builder = self.ancillary_builder.clone();
        let ancillary_size = {
            let db_directory = self.db_directory.clone();
            let beacon = beacon.clone();
            tokio::task::spawn_blocking(move || -> StdResult<u64> {
                ancillary_builder.compute_uncompressed_size(&db_directory, &beacon)
            })
            .await??
        };

        let immutables_locations = self
            .immutable_builder
            .upload(beacon.immutable_file_number)
            .await?;
        let immutable_average_size = {
            let db_directory = self.db_directory.clone();
            let immutable_file_number = beacon.immutable_file_number;
            let immutable_builder = self.immutable_builder.clone();
            tokio::task::spawn_blocking(move || -> StdResult<u64> {
                immutable_builder
                    .compute_average_uncompressed_size(&db_directory, immutable_file_number)
            })
            .await??
        };

        let digest_upload = self.digest_builder.upload(&beacon).await?;

        let content = CardanoDatabaseSnapshotArtifactData {
            total_db_size_uncompressed,
            digests: DigestsLocations {
                size_uncompressed: digest_upload.size,
                locations: digest_upload.locations,
            },
            immutables: ImmutablesLocations {
                average_size_uncompressed: immutable_average_size,
                locations: immutables_locations,
            },
            ancillary: AncillaryLocations {
                size_uncompressed: ancillary_size,
                locations: ancillary_locations,
            },
        };

        let cardano_database = CardanoDatabaseSnapshot::new(
            merkle_root.to_string(),
            self.network,
            beacon,
            content,
            &self.cardano_node_version,
        );

        Ok(cardano_database)
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::BTreeMap, path::PathBuf};

    use mithril_common::{
        digesters::DummyCardanoDbBuilder,
        entities::{
            AncillaryLocation, CompressionAlgorithm, DigestLocation, ImmutablesLocation,
            MultiFilesUri, ProtocolMessage, ProtocolMessagePartKey, TemplateUri,
        },
        test_utils::{fake_data, TempDir},
        CardanoNetwork,
    };
    use mockall::{predicate, Predicate};

    use crate::{
        artifact_builder::{MockAncillaryFileUploader, MockImmutableFilesUploader},
        immutable_file_digest_mapper::MockImmutableFileDigestMapper,
        services::{DumbSnapshotter, FakeSnapshotter},
        test_tools::TestLogger,
        tools::url_sanitizer::SanitizedUrlWithTrailingSlash,
    };

    use super::*;

    fn get_test_directory(dir_name: &str) -> PathBuf {
        TempDir::create("cardano_database", dir_name)
    }

    #[test]
    fn should_compute_the_size_of_the_uncompressed_database_only_immutable_ledger_and_volatile() {
        let test_dir = get_test_directory("should_compute_the_size_of_the_uncompressed_database_only_immutable_ledger_and_volatile");

        let immutable_trio_file_size = 777;
        let ledger_file_size = 6666;
        let volatile_file_size = 99;
        DummyCardanoDbBuilder::new(test_dir.as_os_str().to_str().unwrap())
            .with_immutables(&[1, 2])
            .set_immutable_trio_file_size(immutable_trio_file_size)
            .with_ledger_files(&["blocks-0.dat", "blocks-1.dat", "blocks-2.dat"])
            .set_ledger_file_size(ledger_file_size)
            .with_volatile_files(&["437", "537", "637", "737"])
            .set_volatile_file_size(volatile_file_size)
            .build();
        let expected_total_size =
            (2 * immutable_trio_file_size) + (3 * ledger_file_size) + (4 * volatile_file_size);

        let total_size = compute_uncompressed_database_size(&test_dir).unwrap();

        assert_eq!(expected_total_size, total_size);
    }

    #[tokio::test]
    async fn should_compute_valid_artifact() {
        let test_dir = get_test_directory("should_compute_valid_artifact");

        let beacon = CardanoDbBeacon::new(123, 3);
        let network = CardanoNetwork::DevNet(123);
        let immutable_trio_file_size = 777;
        let ledger_file_size = 6666;
        let volatile_file_size = 99;
        let cardano_db = DummyCardanoDbBuilder::new("cdb-should_compute_valid_artifact")
            .with_immutables(&[1, 2, 3])
            .append_immutable_trio()
            .set_immutable_trio_file_size(immutable_trio_file_size)
            .with_ledger_files(&["blocks-0.dat"])
            .set_ledger_file_size(ledger_file_size)
            .with_volatile_files(&["437"])
            .set_volatile_file_size(volatile_file_size)
            .build();
        let expected_average_immutable_size = immutable_trio_file_size;
        let expected_ancillary_size =
            immutable_trio_file_size + ledger_file_size + volatile_file_size;
        let expected_total_size =
            4 * immutable_trio_file_size + ledger_file_size + volatile_file_size;

        let snapshotter = Arc::new(FakeSnapshotter::new(test_dir.join("fake_snapshots")));

        let ancillary_artifact_builder = {
            let mut ancillary_uploader = MockAncillaryFileUploader::new();
            ancillary_uploader.expect_upload().return_once(|_, _| {
                Ok(AncillaryLocation::CloudStorage {
                    uri: "ancillary_uri".to_string(),
                    compression_algorithm: Some(CompressionAlgorithm::Gzip),
                })
            });

            AncillaryArtifactBuilder::new(
                vec![Arc::new(ancillary_uploader)],
                snapshotter.clone(),
                network,
                CompressionAlgorithm::Gzip,
                TestLogger::stdout(),
            )
            .unwrap()
        };

        fn predicate_length(length: u64) -> impl Predicate<[PathBuf]> {
            predicate::function(move |p: &[_]| p.len() == length as usize)
        }

        let immutable_artifact_builder = {
            let number_of_immutable_file_loaded = beacon.immutable_file_number;
            let mut immutable_uploader = MockImmutableFilesUploader::new();
            immutable_uploader
                .expect_batch_upload()
                .with(
                    predicate_length(number_of_immutable_file_loaded),
                    predicate::eq(Some(CompressionAlgorithm::Gzip)),
                )
                .return_once(|_, _| {
                    Ok(ImmutablesLocation::CloudStorage {
                        uri: MultiFilesUri::Template(TemplateUri(
                            "immutable_template_uri".to_string(),
                        )),
                        compression_algorithm: Some(CompressionAlgorithm::Zstandard),
                    })
                });

            ImmutableArtifactBuilder::new(
                test_dir.join("immutable"),
                vec![Arc::new(immutable_uploader)],
                snapshotter,
                CompressionAlgorithm::Gzip,
                TestLogger::stdout(),
            )
            .unwrap()
        };

        let digest_artifact_builder = {
            let mut immutable_file_digest_mapper = MockImmutableFileDigestMapper::new();

            immutable_file_digest_mapper
                .expect_get_immutable_file_digest_map()
                .returning(|| Ok(BTreeMap::new()));

            DigestArtifactBuilder::new(
                SanitizedUrlWithTrailingSlash::parse("http://aggregator_uri").unwrap(),
                vec![],
                Arc::new(DumbSnapshotter::new()),
                CompressionAlgorithm::Gzip,
                network,
                test_dir.join("digests"),
                Arc::new(immutable_file_digest_mapper),
                TestLogger::stdout(),
            )
            .unwrap()
        };

        let cardano_database_artifact_builder = CardanoDatabaseArtifactBuilder::new(
            network,
            cardano_db.get_dir().to_owned(),
            &Version::parse("1.0.0").unwrap(),
            Arc::new(ancillary_artifact_builder),
            Arc::new(immutable_artifact_builder),
            Arc::new(digest_artifact_builder),
        );

        let certificate_with_merkle_root = {
            let mut protocol_message = ProtocolMessage::new();
            protocol_message.set_message_part(
                ProtocolMessagePartKey::CardanoDatabaseMerkleRoot,
                "merkleroot".to_string(),
            );
            Certificate {
                protocol_message,
                ..fake_data::certificate("certificate-123".to_string())
            }
        };

        let artifact = cardano_database_artifact_builder
            .compute_artifact(beacon.clone(), &certificate_with_merkle_root)
            .await
            .unwrap();

        let expected_ancillary_locations = vec![AncillaryLocation::CloudStorage {
            uri: "ancillary_uri".to_string(),
            compression_algorithm: Some(CompressionAlgorithm::Gzip),
        }];

        let expected_immutables_locations = vec![ImmutablesLocation::CloudStorage {
            uri: MultiFilesUri::Template(TemplateUri("immutable_template_uri".to_string())),
            compression_algorithm: Some(CompressionAlgorithm::Zstandard),
        }];

        let expected_digest_locations = vec![DigestLocation::Aggregator {
            uri: "http://aggregator_uri/artifact/cardano-database/digests".to_string(),
        }];

        let artifact_expected = CardanoDatabaseSnapshot::new(
            "merkleroot".to_string(),
            network,
            beacon,
            CardanoDatabaseSnapshotArtifactData {
                total_db_size_uncompressed: expected_total_size,
                digests: DigestsLocations {
                    size_uncompressed: artifact.digests.size_uncompressed,
                    locations: expected_digest_locations,
                },
                immutables: ImmutablesLocations {
                    average_size_uncompressed: expected_average_immutable_size,
                    locations: expected_immutables_locations,
                },
                ancillary: AncillaryLocations {
                    size_uncompressed: expected_ancillary_size,
                    locations: expected_ancillary_locations,
                },
            },
            &Version::parse("1.0.0").unwrap(),
        );

        assert!(artifact.digests.size_uncompressed > 0);
        assert_eq!(artifact_expected, artifact);
    }
}

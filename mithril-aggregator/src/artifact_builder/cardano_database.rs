use std::path::{Path, PathBuf};

use anyhow::{anyhow, Context};
use async_trait::async_trait;
use semver::Version;

use mithril_common::{
    entities::{
        ArtifactsLocations, CardanoDatabase, CardanoDbBeacon, Certificate, CompressionAlgorithm,
        ProtocolMessagePartKey, SignedEntityType,
    },
    StdResult,
};

use crate::artifact_builder::ArtifactBuilder;

pub struct CardanoDatabaseArtifactBuilder {
    db_directory: PathBuf, // TODO: temporary, will be accessed through another dependency instead of direct path.
    cardano_node_version: Version,
    compression_algorithm: CompressionAlgorithm,
}

impl CardanoDatabaseArtifactBuilder {
    pub fn new(
        db_directory: PathBuf,
        cardano_node_version: &Version,
        compression_algorithm: CompressionAlgorithm,
    ) -> Self {
        Self {
            db_directory,
            cardano_node_version: cardano_node_version.clone(),
            compression_algorithm,
        }
    }
}

#[async_trait]
impl ArtifactBuilder<CardanoDbBeacon, CardanoDatabase> for CardanoDatabaseArtifactBuilder {
    async fn compute_artifact(
        &self,
        beacon: CardanoDbBeacon,
        certificate: &Certificate,
    ) -> StdResult<CardanoDatabase> {
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
        let total_db_size_uncompressed = compute_uncompressed_database_size(&self.db_directory)?;

        let cardano_database = CardanoDatabase::new(
            merkle_root.to_string(),
            beacon,
            total_db_size_uncompressed,
            ArtifactsLocations::default(), // TODO: temporary default locations, will be injected in next PR.
            self.compression_algorithm,
            &self.cardano_node_version,
        );

        Ok(cardano_database)
    }
}

// Return the sum of the files size contained in the subdirectories 'immutable', 'ledger' and 'volatile'.
fn compute_uncompressed_database_size(db_directory: &Path) -> StdResult<u64> {
    let subdirs = ["immutable", "ledger", "volatile"];

    let mut total_db_size_uncompressed = 0;
    for subdir in subdirs {
        let dir_path = db_directory.join(subdir);

        total_db_size_uncompressed += get_directory_size(&dir_path)
            .with_context(|| format!("Failed to read metadata for directory: {:?}", dir_path))?;
    }

    Ok(total_db_size_uncompressed)
}

fn get_directory_size(path: &Path) -> StdResult<u64> {
    let entries =
        std::fs::read_dir(path).with_context(|| format!("Failed to read directory: {:?}", path))?;

    let mut directory_size = 0;
    for entry in entries {
        let path = entry
            .with_context(|| format!("Failed to read directory entry in {:?}", path))?
            .path();

        if path.is_file() {
            let metadata = std::fs::metadata(&path)
                .with_context(|| format!("Failed to read metadata for file: {:?}", path))?;
            directory_size += metadata.len();
        } else if path.is_dir() {
            directory_size += get_directory_size(&path)?;
        }
    }

    Ok(directory_size)
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use mithril_common::{
        digesters::DummyImmutablesDbBuilder,
        entities::{ProtocolMessage, ProtocolMessagePartKey},
        test_utils::{fake_data, TempDir},
    };

    use super::*;

    fn get_test_directory(dir_name: &str) -> PathBuf {
        TempDir::create("cardano_database", dir_name)
    }

    #[test]
    fn should_compute_the_size_of_the_uncompressed_database_only_immutable_ledger_and_volatile() {
        let test_dir = get_test_directory("should_compute_the_size_of_the_uncompressed_database_only_immutable_ledger_and_volatile");

        DummyImmutablesDbBuilder::new(test_dir.as_os_str().to_str().unwrap())
            .with_immutables(&[1, 2])
            .set_immutable_file_size(1000)
            .with_ledger_files(vec!["blocks-0.dat".to_string()])
            .set_ledger_file_size(5000)
            .with_volatile_files(vec!["437".to_string(), "537".to_string()])
            .set_volatile_file_size(2000)
            .build();
        // Number of immutable files = 2 × 3 ('chunk', 'primary' and 'secondary').
        let expected_total_size = 2 * 3 * 1000 + 5000 + 2000 * 2;

        std::fs::write(test_dir.join("non_computed_file.txt"), "file inside root").unwrap();
        let non_computed_dir = test_dir.join("non_computed_dir");
        std::fs::create_dir(&non_computed_dir).unwrap();
        std::fs::write(
            non_computed_dir.join("another_non_computed_file.txt"),
            "file inside a non computed directory",
        )
        .unwrap();

        let total_size = compute_uncompressed_database_size(&test_dir).unwrap();

        assert_eq!(expected_total_size, total_size);
    }

    #[tokio::test]
    async fn should_compute_valid_artifact() {
        let test_dir = get_test_directory("should_compute_valid_artifact");

        DummyImmutablesDbBuilder::new(test_dir.as_os_str().to_str().unwrap())
            .with_immutables(&[1, 2])
            .set_immutable_file_size(1000)
            .with_ledger_files(vec!["blocks-0.dat".to_string()])
            .set_ledger_file_size(5000)
            .with_volatile_files(vec!["437".to_string(), "537".to_string()])
            .set_volatile_file_size(2000)
            .build();
        // Number of immutable files = 2 × 3 ('chunk', 'primary' and 'secondary').
        let expected_total_size = 2 * 3 * 1000 + 5000 + 2000 * 2;

        let cardano_database_artifact_builder = CardanoDatabaseArtifactBuilder::new(
            test_dir,
            &Version::parse("1.0.0").unwrap(),
            CompressionAlgorithm::Zstandard,
        );

        let beacon = fake_data::beacon();
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

        let artifact_expected = CardanoDatabase::new(
            "merkleroot".to_string(),
            beacon,
            expected_total_size,
            ArtifactsLocations {
                digests: vec![],
                immutables: vec![],
                ancillary: vec![],
            },
            CompressionAlgorithm::Zstandard,
            &Version::parse("1.0.0").unwrap(),
        );

        assert_eq!(artifact_expected, artifact);
    }
}

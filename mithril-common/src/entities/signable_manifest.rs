use std::collections::BTreeMap;
use std::path::PathBuf;

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use thiserror::Error;

use crate::crypto_helper::ManifestSignature;
use crate::StdError;

/// Stores a map of files and their hashes, with an optional signature to verify the integrity of the
/// signed data.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SignableManifest<TKey: Ord, TValue> {
    /// The data stored in the manifest
    pub data: BTreeMap<TKey, TValue>,
    /// The signature of the manifest
    #[serde(skip_serializing_if = "Option::is_none")]
    pub signature: Option<ManifestSignature>,
}

/// Alias of [SignableManifest] for Ancillary files
pub type AncillaryFilesManifest = SignableManifest<PathBuf, String>;

/// Errors that can occur when verifying the integrity of the data in the manifest
#[derive(Error, Debug)]
pub enum AncillaryFilesManifestVerifyError {
    /// The hash of a file does not match the hash in the manifest
    #[error("File `{file_path}` hash does not match expected hash, expected: '{expected_hash}', actual: '{actual_hash}'")]
    FileHashMismatch {
        /// Path of the file that has a hash mismatch
        file_path: PathBuf,
        /// Expected hash of the file according to the manifest
        expected_hash: String,
        /// Actual hash of the file
        actual_hash: String,
    },
    /// An error occurred while computing the hash of a file
    #[error("Failed to compute hash for file `{file_path}`")]
    HashCompute {
        /// Path of the file
        file_path: PathBuf,
        /// Source of the error
        source: StdError,
    },
}

impl AncillaryFilesManifest {
    /// The common name used to serialize and deserialize `AncillaryFilesManifest` files in JSON format
    pub const ANCILLARY_MANIFEST_FILE_NAME: &str = "ancillary_manifest.json";

    cfg_fs! {
        /// Creates a new manifest, without signature, from the files in the provided paths
        ///
        /// The hash of each file will be computed and stored in the manifest
        pub async fn from_paths(
            base_directory: &std::path::Path,
            paths: Vec<PathBuf>,
        ) -> crate::StdResult<Self> {
            use anyhow::Context;
            let mut data = BTreeMap::new();

            for path in paths {
                let file_path = base_directory.join(&path);
                let hash = Self::compute_file_hash(&file_path).await.with_context(|| {
                    format!("Failed to compute hash for file `{}`", file_path.display())
                })?;
                data.insert(path, hash);
            }

            Ok(Self {
                data,
                signature: None,
            })
        }

        /// Verifies the integrity of the data in the manifest
        ///
        /// Checks if the files in the manifest are present in the base directory and have the same hash
        pub async fn verify_data(
            &self,
            base_directory: &std::path::Path,
        ) -> Result<(), AncillaryFilesManifestVerifyError> {
            for (file_path, expected_hash) in &self.data {
                let file_path = base_directory.join(file_path);
                let actual_hash = Self::compute_file_hash(&file_path)
                    .await
                    .map_err(|source| AncillaryFilesManifestVerifyError::HashCompute {
                        file_path: file_path.clone(),
                        source,
                    })?;

                if actual_hash != *expected_hash {
                    return Err(AncillaryFilesManifestVerifyError::FileHashMismatch {
                        file_path,
                        expected_hash: expected_hash.clone(),
                        actual_hash,
                    });
                }
            }

            Ok(())
        }

        async fn compute_file_hash(file_path: &std::path::Path) -> crate::StdResult<String> {
            use tokio::io::AsyncReadExt;

            let mut file = tokio::fs::File::open(&file_path).await?;
            let mut hasher = Sha256::new();

            let mut data = vec![0; 64 * 1024];
            loop {
                let len = file.read(&mut data).await?;
                // No more data to read
                if len == 0 {
                    break;
                }

                hasher.update(&data[..len]);
            }

            Ok(hex::encode(hasher.finalize()))
        }
    }

    /// Aggregates the hashes of all the keys and values of the manifest
    pub fn compute_hash(&self) -> Vec<u8> {
        let mut hasher = Sha256::new();
        for (key, value) in &self.data {
            hasher.update(key.to_string_lossy().as_bytes());
            hasher.update(value.as_bytes());
        }
        hasher.finalize().to_vec()
    }
}

#[cfg(test)]
mod tests {
    #[cfg(feature = "fs")]
    use std::{fs::File, io::Write};

    #[cfg(feature = "fs")]
    use crate::test_utils::temp_dir_create;

    use super::*;

    #[cfg(feature = "fs")]
    fn compute_sha256_hash(data: impl AsRef<[u8]>) -> String {
        hex::encode(Sha256::digest(data))
    }

    mod ancillary_files_manifest {
        use super::*;

        #[test]
        fn compute_hash_when_data_is_empty() {
            let manifest = AncillaryFilesManifest {
                data: BTreeMap::new(),
                signature: None,
            };
            let hash_of_empty_data = Sha256::digest(Vec::<u8>::new()).to_vec();

            assert_eq!(hash_of_empty_data, manifest.compute_hash());
        }

        #[test]
        fn compute_hash() {
            let expected: Vec<u8> = vec![
                123, 150, 146, 219, 108, 23, 117, 210, 8, 3, 126, 211, 68, 93, 169, 200, 177, 115,
                169, 219, 87, 2, 238, 52, 209, 37, 214, 207, 21, 188, 246, 127,
            ];

            let manifest = AncillaryFilesManifest {
                data: BTreeMap::from([
                    (PathBuf::from("file1"), "hash1".to_string()),
                    (PathBuf::from("file2"), "hash2".to_string()),
                ]),
                signature: None,
            };

            assert_eq!(expected, manifest.compute_hash());
            // Order does not matter
            assert_eq!(
                expected,
                AncillaryFilesManifest {
                    data: BTreeMap::from([
                        (PathBuf::from("file2"), "hash2".to_string()),
                        (PathBuf::from("file1"), "hash1".to_string()),
                    ]),
                    ..manifest.clone()
                }
                .compute_hash()
            );
            assert_ne!(
                expected,
                AncillaryFilesManifest {
                    data: BTreeMap::from([
                        (PathBuf::from("file1"), "hash1".to_string()),
                        (PathBuf::from("file3"), "hash3".to_string()),
                    ]),
                    ..manifest.clone()
                }
                .compute_hash()
            );
            assert_ne!(
                expected,
                AncillaryFilesManifest {
                    data: BTreeMap::from([(PathBuf::from("file1"), "hash1".to_string()),]),
                    ..manifest.clone()
                }
                .compute_hash()
            );
        }

        #[test]
        fn signature_is_not_included_in_compute_hash() {
            const TEST_SIGNATURE: &str =
                "b5690fe641ee240248d1335092392fefe2399fb11a4bfaddffc790676f4d48a9c34ec648699a3e3b0ba0de8c8bcde5855f16b88eb644d12a9ba1044b5ba91b07";
            let manifest = AncillaryFilesManifest {
                data: BTreeMap::from([
                    (PathBuf::from("file1"), "hash1".to_string()),
                    (PathBuf::from("file2"), "hash2".to_string()),
                ]),
                signature: Some(TEST_SIGNATURE.try_into().unwrap()),
            };

            assert_eq!(
                AncillaryFilesManifest {
                    signature: None,
                    ..manifest.clone()
                }
                .compute_hash(),
                manifest.compute_hash()
            );
        }

        #[cfg(feature = "fs")]
        #[tokio::test]
        async fn from_paths() {
            let test_dir = temp_dir_create!();
            std::fs::create_dir(test_dir.join("sub_folder")).unwrap();

            let file1_path = PathBuf::from("file1.txt");
            let file2_path = PathBuf::from("sub_folder/file1.txt");

            let mut file1 = File::create(test_dir.join(&file1_path)).unwrap();
            write!(&mut file1, "file1 content").unwrap();

            let mut file2 = File::create(test_dir.join(&file2_path)).unwrap();
            write!(&mut file2, "file2 content").unwrap();

            let manifest = AncillaryFilesManifest::from_paths(
                &test_dir,
                vec![file1_path.clone(), file2_path.clone()],
            )
            .await
            .expect("Manifest creation should succeed");

            assert_eq!(
                AncillaryFilesManifest {
                    data: BTreeMap::from([
                        (file1_path, compute_sha256_hash("file1 content".as_bytes()),),
                        (file2_path, compute_sha256_hash("file2 content".as_bytes()),),
                    ]),
                    signature: None,
                },
                manifest
            );
        }

        #[cfg(feature = "fs")]
        mod verify_data {
            use super::*;

            #[tokio::test]
            async fn verify_data_succeed_when_files_hashes_in_target_directory_match() {
                let test_dir = temp_dir_create!();
                std::fs::create_dir(test_dir.join("sub_folder")).unwrap();

                let file1_path = PathBuf::from("file1.txt");
                let file2_path = PathBuf::from("sub_folder/file1.txt");

                // File not included in the manifest should not be considered
                File::create(test_dir.join("random_not_included_file.txt")).unwrap();

                let mut file1 = File::create(test_dir.join(&file1_path)).unwrap();
                write!(&mut file1, "file1 content").unwrap();

                let mut file2 = File::create(test_dir.join(&file2_path)).unwrap();
                write!(&mut file2, "file2 content").unwrap();

                let manifest = AncillaryFilesManifest {
                    data: BTreeMap::from([
                        (file1_path, compute_sha256_hash("file1 content".as_bytes())),
                        (file2_path, compute_sha256_hash("file2 content".as_bytes())),
                    ]),
                    signature: None,
                };

                manifest
                    .verify_data(&test_dir)
                    .await
                    .expect("Verification should succeed when files exists and hashes match");
            }

            #[tokio::test]
            async fn verify_data_fail_when_a_file_in_missing_in_target_directory() {
                let test_dir = temp_dir_create!();
                let file_path = PathBuf::from("file1.txt");

                let manifest = AncillaryFilesManifest {
                    data: BTreeMap::from([(
                        file_path.clone(),
                        compute_sha256_hash("non existent file content".as_bytes()),
                    )]),
                    signature: None,
                };

                let result = manifest.verify_data(&test_dir).await;
                assert!(
                    matches!(
                        result,
                        Err(AncillaryFilesManifestVerifyError::HashCompute { .. }),
                    ),
                    "Expected HashCompute error, got: {result:?}",
                );
            }

            #[tokio::test]
            async fn verify_data_fail_when_a_file_hash_does_not_match_in_target_directory() {
                let test_dir = temp_dir_create!();

                let file_path = PathBuf::from("file1.txt");

                let mut file = File::create(test_dir.join(&file_path)).unwrap();
                write!(&mut file, "file content").unwrap();

                let manifest = AncillaryFilesManifest {
                    data: BTreeMap::from([(
                        file_path.clone(),
                        "This is not the file content hash".to_string(),
                    )]),
                    signature: None,
                };

                let result = manifest.verify_data(&test_dir).await;
                assert!(
                    matches!(
                        result,
                        Err(AncillaryFilesManifestVerifyError::FileHashMismatch { .. }),
                    ),
                    "Expected FileHashMismatch error, got: {result:?}",
                );
            }
        }
    }
}

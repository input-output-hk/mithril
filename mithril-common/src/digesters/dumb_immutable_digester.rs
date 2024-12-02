use std::path::Path;

use crate::{
    crypto_helper::{MKTree, MKTreeStoreInMemory},
    digesters::{ImmutableDigester, ImmutableDigesterError},
    entities::CardanoDbBeacon,
};
use async_trait::async_trait;
use tokio::sync::RwLock;

/// A [ImmutableDigester] returning configurable result for testing purpose.
pub struct DumbImmutableDigester {
    digest: RwLock<String>,
    mktree_leaves: RwLock<Vec<String>>,
    is_success: bool,
}

impl DumbImmutableDigester {
    /// Set the digest returned by [compute_digest][DumbImmutableDigester::compute_digest]
    pub fn with_digest(mut self, new_digest: &str) -> Self {
        self.digest = RwLock::new(new_digest.to_string());
        self
    }

    /// Set the leaves used to construct the merkle tree returned by [compute_merkle_tree][DumbImmutableDigester::compute_merkle_tree]
    pub fn with_merkle_tree(mut self, leaves: Vec<String>) -> Self {
        self.mktree_leaves = RwLock::new(leaves);
        self
    }

    /// Update digest returned by [compute_digest][DumbImmutableDigester::compute_digest]
    pub async fn update_digest(&self, new_digest: String) {
        let mut digest = self.digest.write().await;
        *digest = new_digest;
    }

    /// Update the leaves used to construct the merkle tree returned by [compute_merkle_tree][DumbImmutableDigester::compute_merkle_tree]
    pub async fn update_merkle_tree(&self, leaves: Vec<String>) {
        let mut mktree_leaves = self.mktree_leaves.write().await;
        *mktree_leaves = leaves;
    }
}

impl Default for DumbImmutableDigester {
    fn default() -> Self {
        Self {
            digest: RwLock::new(String::from("1234")),
            mktree_leaves: RwLock::new(vec!["1".to_string(), "2".to_string(), "3".to_string()]),
            is_success: true,
        }
    }
}

#[async_trait]
impl ImmutableDigester for DumbImmutableDigester {
    async fn compute_digest(
        &self,
        dirpath: &Path,
        beacon: &CardanoDbBeacon,
    ) -> Result<String, ImmutableDigesterError> {
        if self.is_success {
            Ok(self.digest.read().await.clone())
        } else {
            Err(ImmutableDigesterError::NotEnoughImmutable {
                expected_number: beacon.immutable_file_number,
                found_number: None,
                db_dir: dirpath.to_owned(),
            })
        }
    }

    async fn compute_merkle_tree(
        &self,
        dirpath: &Path,
        beacon: &CardanoDbBeacon,
    ) -> Result<MKTree<MKTreeStoreInMemory>, ImmutableDigesterError> {
        if self.is_success {
            let leaves = self.mktree_leaves.read().await;
            Ok(MKTree::new(&leaves).unwrap())
        } else {
            Err(ImmutableDigesterError::NotEnoughImmutable {
                expected_number: beacon.immutable_file_number,
                found_number: None,
                db_dir: dirpath.to_owned(),
            })
        }
    }
}

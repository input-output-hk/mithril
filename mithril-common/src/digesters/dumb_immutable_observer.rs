use std::path::PathBuf;

use crate::{
    digesters::{ImmutableDigester, ImmutableDigesterError},
    entities::Beacon,
};
use async_trait::async_trait;
use tokio::sync::RwLock;

/// A [ImmutableDigester] returning configurable result for testing purpose.
pub struct DumbImmutableDigester {
    digest: RwLock<String>,
    is_success: bool,
}

impl DumbImmutableDigester {
    /// DumbDigester factory
    pub fn new(digest: &str, is_success: bool) -> Self {
        let digest = RwLock::new(String::from(digest));

        Self { digest, is_success }
    }

    /// Update digest returned by [compute_digest][DumbImmutableDigester::compute_digest]
    pub async fn update_digest(&self, new_digest: String) {
        let mut digest = self.digest.write().await;
        *digest = new_digest;
    }
}

impl Default for DumbImmutableDigester {
    fn default() -> Self {
        Self::new("1234", true)
    }
}

#[async_trait]
impl ImmutableDigester for DumbImmutableDigester {
    async fn compute_digest(&self, beacon: &Beacon) -> Result<String, ImmutableDigesterError> {
        if self.is_success {
            Ok(self.digest.read().await.clone())
        } else {
            Err(ImmutableDigesterError::NotEnoughImmutable {
                expected_number: beacon.immutable_file_number,
                found_number: None,
                db_dir: PathBuf::new(),
            })
        }
    }
}

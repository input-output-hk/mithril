use std::path::PathBuf;

use crate::DependencyManager;
use async_trait::async_trait;
use mithril_common::digesters::DigesterError;
use mithril_common::entities::{Beacon, CertificatePending};
use std::path::Path;
use std::sync::Arc;

#[cfg(test)]
use mockall::automock;
pub struct AggregatorConfig {
    /// Interval between each snapshot, in seconds
    pub interval: u32,

    /// Cardano network
    pub network: String,

    /// DB directory to snapshot
    pub db_directory: PathBuf,

    /// Directory to store snapshot
    pub snapshot_directory: PathBuf,

    pub dependencies: Arc<DependencyManager>,
}

impl AggregatorConfig {
    pub fn new(
        interval: u32,
        network: &str,
        db_directory: &Path,
        snapshot_directory: &Path,
        dependencies: Arc<DependencyManager>,
    ) -> Self {
        Self {
            interval,
            network: network.to_string(),
            db_directory: db_directory.to_path_buf(),
            snapshot_directory: snapshot_directory.to_path_buf(),
            dependencies,
        }
    }
}

#[async_trait]
pub trait AggregatorRunnerTrait: Sync + Send {
    /// Return the current beacon if it is newer than the given one.
    fn is_new_beacon(&self, beacon: Option<&Beacon>) -> Option<Beacon>;
    async fn compute_digest(&self, new_beacon: &Beacon) -> Result<String, DigesterError>;
    async fn create_pending_certificate(&self, message: &str)
        -> Result<CertificatePending, String>;
}

pub struct AggregatorRunner {
    config: AggregatorConfig,
}

impl AggregatorRunner {
    pub fn new(config: AggregatorConfig) -> Self {
        Self { config }
    }
}

#[cfg_attr(test, automock)]
#[async_trait]
impl AggregatorRunnerTrait for AggregatorRunner {
    fn is_new_beacon<'a>(&self, beacon: Option<&'a Beacon>) -> Option<Beacon> {
        info!("checking if there is a new beacon");
        let current_beacon = mithril_common::fake_data::beacon();

        if beacon.is_none() || current_beacon > *beacon.unwrap() {
            Some(current_beacon)
        } else {
            None
        }
    }

    async fn compute_digest(&self, new_beacon: &Beacon) -> Result<String, DigesterError> {
        /*
        let snapshotter =
            Snapshotter::new(self.db_directory.clone(), self.snapshot_directory.clone());
        let digester = ImmutableDigester::new(self.db_directory.clone(), slog_scope::logger());

        info!("Computing digest"; "db_directory" => self.db_directory.display());
        let digest_result = tokio::task::spawn_blocking(move || digester.compute_digest())
            .await
            .map_err(|e| RuntimeError::General(e.to_string()))?;
        */
        todo!()
    }

    async fn create_pending_certificate(
        &self,
        message: &str,
    ) -> Result<CertificatePending, String> {
        todo!()
    }
}

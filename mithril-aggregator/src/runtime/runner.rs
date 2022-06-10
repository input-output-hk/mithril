use std::path::PathBuf;

use crate::DependencyManager;
use async_trait::async_trait;
use mithril_common::digesters::{Digester, DigesterResult, ImmutableDigester};
use mithril_common::entities::{Beacon, CertificatePending};

#[allow(unused_imports)]
use slog_scope::{debug, error, info, trace, warn};
use std::path::Path;
use std::sync::Arc;

#[cfg(test)]
use mockall::automock;

use super::RuntimeError;
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
    async fn is_new_beacon(&self, beacon: Option<Beacon>) -> Result<Option<Beacon>, RuntimeError>;
    async fn compute_digest(&self, new_beacon: &Beacon) -> Result<DigesterResult, RuntimeError>;
    async fn update_message_in_multisigner(
        &self,
        digest_result: DigesterResult,
    ) -> Result<(), RuntimeError>;
    async fn create_new_pending_certificate_from_multisigner(
        &self,
        beacon: Beacon,
    ) -> Result<CertificatePending, RuntimeError>;
    async fn save_pending_certificate(
        &self,
        pending_certificate: CertificatePending,
    ) -> Result<(), RuntimeError>;
    async fn drop_pending_certificate(&self) -> Result<(), RuntimeError>;
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
    /// Is there a new beacon?
    /// returns a new beacon if there is one more recent than the given one
    async fn is_new_beacon(
        &self,
        maybe_beacon: Option<Beacon>,
    ) -> Result<Option<Beacon>, RuntimeError> {
        info!("checking if there is a new beacon");
        warn!("using fake data for the new beacon");
        let current_beacon = mithril_common::fake_data::beacon();

        match maybe_beacon {
            Some(beacon) if current_beacon > beacon => Ok(Some(current_beacon)),
            _ => Ok(None),
        }
    }

    async fn compute_digest(&self, new_beacon: &Beacon) -> Result<DigesterResult, RuntimeError> {
        trace!("running runner::compute_digester");
        let digester =
            ImmutableDigester::new(self.config.db_directory.clone(), slog_scope::logger());
        info!("Computing digest"; "db_directory" => self.config.db_directory.display());

        // digest is done in a separate thread because it is blocking the whole task
        debug!("launching digester thread");
        let digest_result = tokio::task::spawn_blocking(move || digester.compute_digest())
            .await
            .map_err(|e| RuntimeError::General(e.to_string()))??;

        debug!(
            "last immutable file number: {}",
            digest_result.last_immutable_file_number
        );
        if digest_result.last_immutable_file_number != new_beacon.immutable_file_number {
            error!("digest beacon is different than the given beacon");
            Err(RuntimeError::General(
                format!("The digest has been computed for a different immutable ({}) file than the one given in the beacon ({}).", digest_result.last_immutable_file_number, new_beacon.immutable_file_number)
            ))
        } else {
            trace!("digest last immutable file number and new beacon file number are consistent");
            Ok(digest_result)
        }
    }

    async fn create_new_pending_certificate_from_multisigner(
        &self,
        beacon: Beacon,
    ) -> Result<CertificatePending, RuntimeError> {
        trace!("running runner::create_pending_certificate");
        let multi_signer = self
            .config
            .dependencies
            .multi_signer
            .as_ref()
            .ok_or(RuntimeError::General(format!("no multisigner registered")))?
            .read()
            .await;

        debug!("creating certificate pending using multisigner");
        warn!("pending certificate's previous hash is fake");
        let pending_certificate = CertificatePending::new(
            beacon,
            multi_signer
                .get_protocol_parameters()
                .await
                .ok_or_else(|| RuntimeError::General(format!("no protocol parameters")))?
                .into(),
            "123".to_string(),
            multi_signer.get_signers().await?,
        );

        Ok(pending_certificate)
    }

    async fn save_pending_certificate(
        &self,
        pending_certificate: CertificatePending,
    ) -> Result<(), RuntimeError> {
        self.config
            .dependencies
            .certificate_pending_store
            .as_ref()
            .ok_or(RuntimeError::General(format!("no multisigner registered")))?
            .write()
            .await
            .save(pending_certificate)
            .await
            .map_err(|e| e.into())
    }

    async fn update_message_in_multisigner(
        &self,
        digest_result: DigesterResult,
    ) -> Result<(), RuntimeError> {
        self.config
            .dependencies
            .multi_signer
            .as_ref()
            .ok_or(RuntimeError::General(format!("no multisigner registered")))?
            .write()
            .await
            .update_current_message(digest_result.digest.into_bytes())
            .await
            .map_err(|e| RuntimeError::MultiSigner(e))
    }

    async fn drop_pending_certificate(&self) -> Result<(), RuntimeError> {
        todo!()
    }
}

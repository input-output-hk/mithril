use async_trait::async_trait;
use chrono::Utc;
use slog_scope::{debug, error, info, trace};
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;

use mithril_common::crypto_helper::ProtocolStakeDistribution;
use mithril_common::digesters::DigesterResult;
use mithril_common::entities::{
    Beacon, Certificate, CertificatePending, ProtocolMessage, ProtocolMessagePartKey, Snapshot,
};
use mithril_common::CardanoNetwork;

use crate::snapshot_uploaders::SnapshotLocation;
use crate::snapshotter::OngoingSnapshot;
use crate::{DependencyManager, ProtocolError, SnapshotError};

#[cfg(test)]
use mockall::automock;

use super::RuntimeError;
#[derive(Debug, Clone)]
pub struct AggregatorConfig {
    /// Interval between each snapshot, in ms
    pub interval: u64,

    /// Cardano network
    pub network: CardanoNetwork,

    /// DB directory to snapshot
    pub db_directory: PathBuf,

    /// Directory to store snapshot
    pub snapshot_directory: PathBuf,
}

impl AggregatorConfig {
    pub fn new(
        interval: u64,
        network: CardanoNetwork,
        db_directory: &Path,
        snapshot_directory: &Path,
    ) -> Self {
        Self {
            interval,
            network,
            db_directory: db_directory.to_path_buf(),
            snapshot_directory: snapshot_directory.to_path_buf(),
        }
    }
}

#[async_trait]
pub trait AggregatorRunnerTrait: Sync + Send {
    /// Return the current beacon if it is newer than the given one.
    async fn is_new_beacon(&self, beacon: Option<Beacon>) -> Result<Option<Beacon>, RuntimeError>;

    async fn compute_digest(&self, new_beacon: &Beacon) -> Result<DigesterResult, RuntimeError>;

    async fn update_beacon(&self, new_beacon: &Beacon) -> Result<(), RuntimeError>;

    async fn update_stake_distribution(&self, new_beacon: &Beacon) -> Result<(), RuntimeError>;

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

    async fn drop_pending_certificate(&self) -> Result<CertificatePending, RuntimeError>;

    async fn is_multisig_created(&self) -> Result<bool, RuntimeError>;

    /// Create an archive of the cardano node db directory.
    ///
    /// Returns the path of the created archive and the archive size as byte.
    async fn create_snapshot_archive(&self) -> Result<OngoingSnapshot, RuntimeError>;

    /// Upload the snapshot at the given location using the configured uploader(s).
    ///
    /// Important: the snapshot is removed after the upload succeeded.
    async fn upload_snapshot_archive(
        &self,
        ongoing_snapshot: &OngoingSnapshot,
    ) -> Result<Vec<SnapshotLocation>, RuntimeError>;

    async fn create_and_save_certificate(
        &self,
        beacon: &Beacon,
    ) -> Result<Certificate, RuntimeError>;

    async fn create_and_save_snapshot(
        &self,
        certificate: Certificate,
        ongoing_snapshot: &OngoingSnapshot,
        remote_locations: Vec<String>,
    ) -> Result<Snapshot, RuntimeError>;
}

pub struct AggregatorRunner {
    config: AggregatorConfig,
    dependencies: Arc<DependencyManager>,
}

impl AggregatorRunner {
    pub fn new(config: AggregatorConfig, dependencies: Arc<DependencyManager>) -> Self {
        Self {
            config,
            dependencies,
        }
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
        let current_beacon = self
            .dependencies
            .beacon_provider
            .as_ref()
            .ok_or_else(|| {
                RuntimeError::General("no beacon provider registered".to_string().into())
            })?
            .read()
            .await
            .get_current_beacon()
            .await
            .map_err(RuntimeError::General)?;

        debug!("checking if there is a new beacon: {:?}", current_beacon);

        match maybe_beacon {
            Some(beacon) if current_beacon > beacon => Ok(Some(current_beacon)),
            None => Ok(Some(current_beacon)),
            _ => Ok(None),
        }
    }

    /// Is a multisignature ready?
    /// Can we create a multisignature.
    async fn is_multisig_created(&self) -> Result<bool, RuntimeError> {
        info!("check if we can create a multisignature");
        let has_multisig = self
            .dependencies
            .multi_signer
            .as_ref()
            .ok_or_else(|| RuntimeError::General("no multisigner registered".to_string().into()))?
            .write()
            .await
            .create_multi_signature()
            .await?
            .is_some();

        if has_multisig {
            debug!("new MULTISIG created");
        } else {
            info!("no multisig created");
        }
        Ok(has_multisig)
    }

    async fn compute_digest(&self, new_beacon: &Beacon) -> Result<DigesterResult, RuntimeError> {
        info!("running runner::compute_digester");
        let digester = self
            .dependencies
            .digester
            .as_ref()
            .ok_or_else(|| RuntimeError::General("no digester registered".to_string().into()))?
            .clone();

        debug!("computing digest"; "db_directory" => self.config.db_directory.display());

        // digest is done in a separate thread because it is blocking the whole task
        debug!("launching digester thread");
        let digest_result = digester
            .compute_digest()
            .await
            .map_err(|e| RuntimeError::General(e.into()))?;
        debug!(
            "last immutable file number: {}",
            digest_result.last_immutable_file_number
        );

        if digest_result.last_immutable_file_number != new_beacon.immutable_file_number {
            error!("digest beacon is different than the given beacon");
            Err(RuntimeError::General(
                format!("The digest has been computed for a different immutable ({}) file than the one given in the beacon ({}).", digest_result.last_immutable_file_number, new_beacon.immutable_file_number).into()
            ))
        } else {
            trace!("digest last immutable file number and new beacon file number are consistent");
            Ok(digest_result)
        }
    }

    async fn update_beacon(&self, new_beacon: &Beacon) -> Result<(), RuntimeError> {
        info!("update beacon"; "beacon" => #?new_beacon);
        let _ = self
            .dependencies
            .beacon_store
            .as_ref()
            .ok_or_else(|| RuntimeError::General("no beacon store registered".to_string().into()))?
            .write()
            .await
            .set_current_beacon(new_beacon.to_owned())
            .await?;
        Ok(())
    }

    async fn update_stake_distribution(&self, new_beacon: &Beacon) -> Result<(), RuntimeError> {
        info!("update stake distribution"; "beacon" => #?new_beacon);
        let stake_distribution = self
            .dependencies
            .chain_observer
            .as_ref()
            .ok_or_else(|| {
                RuntimeError::General("no chain observer registered".to_string().into())
            })?
            .read()
            .await
            .get_current_stake_distribution()
            .await?
            .ok_or_else(|| RuntimeError::General("no epoch was returned".to_string().into()))?;
        let stake_distribution = stake_distribution
            .iter()
            .map(|(party_id, stake)| (party_id.to_owned(), *stake))
            .collect::<ProtocolStakeDistribution>();

        Ok(self
            .dependencies
            .multi_signer
            .as_ref()
            .ok_or_else(|| RuntimeError::General("no multisigner registered".to_string().into()))?
            .write()
            .await
            .update_stake_distribution(&stake_distribution)
            .await?)
    }

    async fn create_new_pending_certificate_from_multisigner(
        &self,
        beacon: Beacon,
    ) -> Result<CertificatePending, RuntimeError> {
        info!("running runner::create_pending_certificate");
        let multi_signer = self
            .dependencies
            .multi_signer
            .as_ref()
            .ok_or_else(|| RuntimeError::General("no multisigner registered".to_string().into()))?
            .read()
            .await;

        debug!("creating certificate pending using multisigner");
        let signers = match multi_signer.get_signers().await {
            Ok(signers) => signers,
            Err(ProtocolError::Beacon(_)) => vec![],
            Err(e) => return Err(e.into()),
        };
        let pending_certificate = CertificatePending::new(
            beacon,
            multi_signer
                .get_protocol_parameters()
                .await
                .ok_or_else(|| RuntimeError::General("no protocol parameters".to_string().into()))?
                .into(),
            signers,
        );

        Ok(pending_certificate)
    }

    async fn save_pending_certificate(
        &self,
        pending_certificate: CertificatePending,
    ) -> Result<(), RuntimeError> {
        info!("saving pending certificate");

        self.dependencies
            .certificate_pending_store
            .as_ref()
            .ok_or_else(|| {
                RuntimeError::General("no certificate pending store registered".to_string().into())
            })?
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
        info!("update message in multisigner");
        let mut multi_signer = self
            .dependencies
            .multi_signer
            .as_ref()
            .ok_or_else(|| RuntimeError::General("no multisigner registered".to_string().into()))?
            .write()
            .await;
        let mut protocol_message = ProtocolMessage::new();
        protocol_message
            .set_message_part(ProtocolMessagePartKey::SnapshotDigest, digest_result.digest);
        protocol_message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            multi_signer
                .compute_next_stake_distribution_aggregate_verification_key()
                .await
                .map_err(RuntimeError::MultiSigner)?
                .unwrap_or_default(),
        );
        multi_signer
            .update_current_message(protocol_message)
            .await
            .map_err(RuntimeError::MultiSigner)
    }

    async fn drop_pending_certificate(&self) -> Result<CertificatePending, RuntimeError> {
        info!("drop pending certificate");

        let certificate_pending = self
            .dependencies
            .certificate_pending_store
            .as_ref()
            .ok_or_else(|| {
                RuntimeError::General("no certificate pending store registered".to_string().into())
            })?
            .write()
            .await
            .remove()
            .await?
            .ok_or_else(|| {
                RuntimeError::General(
                    "no certificate pending for the given beacon"
                        .to_string()
                        .into(),
                )
            })?;

        Ok(certificate_pending)
    }

    async fn create_snapshot_archive(&self) -> Result<OngoingSnapshot, RuntimeError> {
        info!("create snapshot archive");

        let snapshotter =
            self.dependencies.snapshotter.clone().ok_or_else(|| {
                RuntimeError::General("no snapshotter registered".to_string().into())
            })?;
        let protocol_message = self
            .dependencies
            .multi_signer
            .as_ref()
            .ok_or_else(|| RuntimeError::General("no multisigner registered".to_string().into()))?
            .read()
            .await
            .get_current_message()
            .await
            .ok_or_else(|| RuntimeError::General("no message found".to_string().into()))?;
        let snapshot_digest = protocol_message
            .get_message_part(&ProtocolMessagePartKey::SnapshotDigest)
            .ok_or_else(|| {
                RuntimeError::General("no snapshot digest message part found".to_string().into())
            })?;
        let snapshot_name = format!("{}.{}.tar.gz", self.config.network, snapshot_digest);
        // spawn a separate thread to prevent blocking
        let ongoing_snapshot =
            tokio::task::spawn_blocking(move || -> Result<OngoingSnapshot, SnapshotError> {
                snapshotter.snapshot(&snapshot_name)
            })
            .await
            .map_err(|e| RuntimeError::General(e.into()))??;

        debug!("snapshot created: '{:?}'", ongoing_snapshot);

        Ok(ongoing_snapshot)
    }

    async fn create_and_save_certificate(
        &self,
        beacon: &Beacon,
    ) -> Result<Certificate, RuntimeError> {
        info!("create and save certificate");
        let mut certificate_store = self
            .dependencies
            .certificate_store
            .as_ref()
            .ok_or_else(|| {
                RuntimeError::General("no certificate store registered".to_string().into())
            })?
            .write()
            .await;
        let latest_certificates = certificate_store.get_list(2).await?;
        let last_certificate = latest_certificates.get(0);
        let penultimate_certificate = latest_certificates.get(1);
        let previous_hash = match (penultimate_certificate, last_certificate) {
            (Some(penultimate_certificate), Some(last_certificate)) => {
                // Check if last certificate is first certificate of its epoch
                if penultimate_certificate.beacon.epoch != last_certificate.beacon.epoch {
                    &last_certificate.hash
                } else {
                    &last_certificate.previous_hash
                }
            }
            (None, Some(last_certificate)) => &last_certificate.hash,
            _ => "",
        };
        let multisigner = self
            .dependencies
            .multi_signer
            .as_ref()
            .ok_or_else(|| RuntimeError::General("no multisigner registered".to_string().into()))?
            .read()
            .await;
        let certificate = multisigner
            .create_certificate(beacon.clone(), previous_hash.to_owned())
            .await?
            .ok_or_else(|| RuntimeError::General("no certificate generated".to_string().into()))?;
        let _ = certificate_store.save(certificate.clone()).await?;

        Ok(certificate)
    }

    async fn upload_snapshot_archive(
        &self,
        ongoing_snapshot: &OngoingSnapshot,
    ) -> Result<Vec<SnapshotLocation>, RuntimeError> {
        info!("upload snapshot archive");
        let location = self
            .dependencies
            .snapshot_uploader
            .as_ref()
            .ok_or_else(|| {
                RuntimeError::SnapshotUploader("no snapshot uploader registered".to_string())
            })?
            .read()
            .await
            .upload_snapshot(ongoing_snapshot.get_file_path())
            .await
            .map_err(RuntimeError::SnapshotUploader)?;

        tokio::fs::remove_file(ongoing_snapshot.get_file_path())
            .await
            .map_err(|e| {
                RuntimeError::General(
                    format!("Post upload local snapshot removal failure: {}", e).into(),
                )
            })?;

        Ok(vec![location])
    }

    async fn create_and_save_snapshot(
        &self,
        certificate: Certificate,
        ongoing_snapshot: &OngoingSnapshot,
        remote_locations: Vec<String>,
    ) -> Result<Snapshot, RuntimeError> {
        let snapshot_digest = certificate
            .protocol_message
            .get_message_part(&ProtocolMessagePartKey::SnapshotDigest)
            .ok_or_else(|| RuntimeError::General("message part not found".to_string().into()))?
            .to_owned();
        let snapshot = Snapshot::new(
            snapshot_digest,
            certificate.beacon,
            certificate.hash,
            *ongoing_snapshot.get_file_size(),
            format!("{:?}", Utc::now()),
            remote_locations,
        );

        self.dependencies
            .snapshot_store
            .as_ref()
            .ok_or_else(|| {
                RuntimeError::General("no snapshot store registered".to_string().into())
            })?
            .write()
            .await
            .add_snapshot(snapshot.clone())
            .await?;

        Ok(snapshot)
    }
}

#[cfg(test)]
pub mod tests {
    use crate::{
        initialize_dependencies,
        runtime::{AggregatorRunner, AggregatorRunnerTrait},
    };
    use mithril_common::{digesters::DigesterResult, entities::Beacon};
    use mithril_common::{entities::ProtocolMessagePartKey, store::stake_store::StakeStorer};

    #[tokio::test]
    async fn test_is_new_beacon() {
        let (dependencies, config) = initialize_dependencies().await;
        let runner = AggregatorRunner::new(config, dependencies);

        // No beacon means the current beacon is newer
        let res = runner.is_new_beacon(None).await;
        assert!(res.unwrap().is_some());

        // old beacon means the current beacon is newer
        let beacon = Beacon {
            network: "testnet".to_string(),
            epoch: 0,
            immutable_file_number: 0,
        };
        let res = runner.is_new_beacon(Some(beacon)).await;
        assert!(res.unwrap().is_some());

        // new beacon mens the current beacon is not new
        let beacon = Beacon {
            network: "whatever".to_string(),
            epoch: 9206230,
            immutable_file_number: 10000,
        };
        let res = runner.is_new_beacon(Some(beacon)).await;
        assert!(res.unwrap().is_none());
    }

    #[tokio::test]
    async fn test_update_beacon() {
        let (deps, config) = initialize_dependencies().await;
        let runner = AggregatorRunner::new(config, deps.clone());
        let beacon = runner.is_new_beacon(None).await.unwrap().unwrap();
        let res = runner.update_beacon(&beacon).await;

        assert!(res.is_ok());
        let stored_beacon = deps
            .beacon_store
            .as_ref()
            .unwrap()
            .read()
            .await
            .get_current_beacon()
            .await
            .unwrap()
            .unwrap();

        assert_eq!(beacon, stored_beacon);
    }
    #[tokio::test]
    async fn test_update_stake_distribution() {
        let (deps, config) = initialize_dependencies().await;
        let runner = AggregatorRunner::new(config, deps.clone());
        let beacon = runner.is_new_beacon(None).await.unwrap().unwrap();
        let _res = runner
            .update_beacon(&beacon)
            .await
            .expect("setting the beacon should not fail");
        let _res = runner
            .update_stake_distribution(&beacon)
            .await
            .expect("updating stake distribution should not return an error");

        let current_stake_distribution = deps
            .chain_observer
            .as_ref()
            .unwrap()
            .read()
            .await
            .get_current_stake_distribution()
            .await
            .unwrap()
            .expect("The stake distribution should not be None.");

        // TODO: check why to fetch EPOCH+1
        let saved_stake_distribution = deps
            .stake_store
            .as_ref()
            .unwrap()
            .read()
            .await
            .get_stakes(beacon.epoch + 1)
            .await
            .unwrap()
            .expect(
                format!(
                    "I should have a stake distribution for the epoch {:?}",
                    beacon.epoch
                )
                .as_str(),
            );

        assert_eq!(
            current_stake_distribution.len(),
            saved_stake_distribution.len()
        );
        for (party_id, stake) in current_stake_distribution.iter() {
            let signer_with_stake = saved_stake_distribution.get(party_id).unwrap();
            assert_eq!(stake, &signer_with_stake.stake);
        }
    }

    #[tokio::test]
    async fn test_create_new_pending_certificate_from_multisigner() {
        let (deps, config) = initialize_dependencies().await;
        let runner = AggregatorRunner::new(config, deps);
        let beacon = runner.is_new_beacon(None).await.unwrap().unwrap();
        runner.update_beacon(&beacon).await.unwrap();
        let certificate = runner
            .create_new_pending_certificate_from_multisigner(beacon.clone())
            .await
            .unwrap();

        assert_eq!(beacon, certificate.beacon);
    }

    #[tokio::test]
    async fn test_update_message_in_multisigner() {
        let (deps, config) = initialize_dependencies().await;
        let runner = AggregatorRunner::new(config, deps.clone());
        let beacon = runner.is_new_beacon(None).await.unwrap().unwrap();
        let digester_result = DigesterResult {
            digest: "1+2+3+4=10".to_string(),
            last_immutable_file_number: beacon.immutable_file_number,
        };
        runner.update_beacon(&beacon).await.unwrap();

        assert!(runner
            .update_message_in_multisigner(digester_result)
            .await
            .is_ok());
        let message = deps
            .multi_signer
            .as_ref()
            .unwrap()
            .read()
            .await
            .get_current_message()
            .await
            .unwrap();

        assert_eq!(
            "1+2+3+4=10",
            message
                .get_message_part(&ProtocolMessagePartKey::SnapshotDigest)
                .unwrap()
        );
    }

    #[tokio::test]
    async fn test_save_pending_certificate() {
        let (deps, config) = initialize_dependencies().await;
        let runner = AggregatorRunner::new(config, deps.clone());
        let beacon = runner.is_new_beacon(None).await.unwrap().unwrap();
        runner.update_beacon(&beacon).await.unwrap();
        let pending_certificate = runner
            .create_new_pending_certificate_from_multisigner(beacon.clone())
            .await
            .unwrap();
        let res = runner
            .save_pending_certificate(pending_certificate.clone())
            .await;
        assert!(res.is_ok());

        let saved_cert = deps
            .certificate_pending_store
            .as_ref()
            .unwrap()
            .read()
            .await
            .get()
            .await
            .unwrap()
            .unwrap();

        assert_eq!(pending_certificate, saved_cert);
    }

    #[tokio::test]
    async fn test_drop_pending_certificate() {
        let (deps, config) = initialize_dependencies().await;
        let runner = AggregatorRunner::new(config, deps.clone());
        let beacon = runner.is_new_beacon(None).await.unwrap().unwrap();
        runner.update_beacon(&beacon).await.unwrap();
        let pending_certificate = runner
            .create_new_pending_certificate_from_multisigner(beacon.clone())
            .await
            .unwrap();
        runner
            .save_pending_certificate(pending_certificate.clone())
            .await
            .unwrap();
        let cert = runner.drop_pending_certificate().await.unwrap();
        assert_eq!(pending_certificate, cert);
        let maybe_saved_cert = deps
            .certificate_pending_store
            .as_ref()
            .unwrap()
            .read()
            .await
            .get()
            .await
            .unwrap();
        assert!(maybe_saved_cert.is_none());
    }

    #[tokio::test]
    async fn test_drop_pending_no_certificate() {
        let (deps, config) = initialize_dependencies().await;
        let runner = AggregatorRunner::new(config, deps.clone());
        let beacon = runner.is_new_beacon(None).await.unwrap().unwrap();
        runner.update_beacon(&beacon).await.unwrap();
        let res = runner.drop_pending_certificate().await;
        assert!(res.is_err());
        let err = res.unwrap_err();
        assert_eq!(
            "general error: no certificate pending for the given beacon".to_string(),
            err.to_string()
        );
    }
}

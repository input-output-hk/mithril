use async_trait::async_trait;
use chrono::Utc;
use mithril_common::entities::Epoch;
use slog_scope::{debug, info, warn};
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;

use mithril_common::crypto_helper::ProtocolStakeDistribution;
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
}

impl AggregatorConfig {
    pub fn new(interval: u64, network: CardanoNetwork, db_directory: &Path) -> Self {
        Self {
            interval,
            network,
            db_directory: db_directory.to_path_buf(),
        }
    }
}

#[async_trait]
pub trait AggregatorRunnerTrait: Sync + Send {
    /// Return the current beacon if it is newer than the given one.
    async fn is_new_beacon(&self, beacon: Option<Beacon>) -> Result<Option<Beacon>, RuntimeError>;

    async fn compute_digest(&self, new_beacon: &Beacon) -> Result<String, RuntimeError>;

    async fn update_beacon(&self, new_beacon: &Beacon) -> Result<(), RuntimeError>;

    async fn update_stake_distribution(&self, new_beacon: &Beacon) -> Result<(), RuntimeError>;

    async fn update_protocol_parameters_in_multisigner(
        &self,
        new_beacon: &Beacon,
    ) -> Result<(), RuntimeError>;

    async fn update_message_in_multisigner(&self, digest: String) -> Result<(), RuntimeError>;

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
            .get_current_beacon()
            .await?;

        debug!("checking if there is a new beacon: {:?}", current_beacon);

        match maybe_beacon {
            Some(beacon) if current_beacon > beacon => Ok(Some(current_beacon)),
            None => Ok(Some(current_beacon)),
            _ => Ok(None),
        }
    }

    async fn compute_digest(&self, new_beacon: &Beacon) -> Result<String, RuntimeError> {
        info!("running runner::compute_digest");
        let digester = self.dependencies.digester.clone();

        debug!("computing digest"; "db_directory" => self.config.db_directory.display());

        debug!("launching digester thread");
        let digest = digester
            .compute_digest(new_beacon.immutable_file_number)
            .await
            .map_err(|e| RuntimeError::General(e.into()))?;
        debug!("computed digest: {}", digest);

        Ok(digest)
    }

    async fn update_beacon(&self, new_beacon: &Beacon) -> Result<(), RuntimeError> {
        info!("update beacon"; "beacon" => #?new_beacon);
        self.dependencies
            .multi_signer
            .write()
            .await
            .update_current_beacon(new_beacon.to_owned())
            .await?;
        Ok(())
    }

    async fn update_stake_distribution(&self, new_beacon: &Beacon) -> Result<(), RuntimeError> {
        info!("update stake distribution"; "beacon" => #?new_beacon);
        let stake_distribution = self
            .dependencies
            .chain_observer
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
            .write()
            .await
            .update_stake_distribution(&stake_distribution)
            .await?)
    }

    async fn update_protocol_parameters_in_multisigner(
        &self,
        new_beacon: &Beacon,
    ) -> Result<(), RuntimeError> {
        info!("update protocol parameters"; "beacon" => #?new_beacon);
        let protocol_parameters = self.dependencies.config.protocol_parameters.clone();
        Ok(self
            .dependencies
            .multi_signer
            .write()
            .await
            .update_protocol_parameters(&protocol_parameters.into())
            .await?)
    }

    async fn update_message_in_multisigner(&self, digest: String) -> Result<(), RuntimeError> {
        info!("update message in multisigner");
        let mut multi_signer = self.dependencies.multi_signer.write().await;
        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(ProtocolMessagePartKey::SnapshotDigest, digest);
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

    async fn create_new_pending_certificate_from_multisigner(
        &self,
        beacon: Beacon,
    ) -> Result<CertificatePending, RuntimeError> {
        info!("running runner::create_pending_certificate");
        let multi_signer = self.dependencies.multi_signer.read().await;

        debug!("creating certificate pending using multisigner");
        let signers = match multi_signer.get_signers().await {
            Ok(signers) => signers,
            Err(ProtocolError::Beacon(_)) => vec![],
            Err(e) => return Err(e.into()),
        };
        let next_signer = match multi_signer.get_next_signers_with_stake().await {
            Ok(signers) => signers,
            Err(ProtocolError::Beacon(_)) => vec![],
            Err(e) => return Err(e.into()),
        };

        let protocol_parameters = multi_signer
            .get_protocol_parameters()
            .await?
            .ok_or_else(|| RuntimeError::General("no protocol parameters".to_string().into()))?;
        let next_protocol_parameters = multi_signer
            .get_next_protocol_parameters()
            .await?
            .ok_or_else(|| {
                RuntimeError::General("no next protocol parameters".to_string().into())
            })?;

        let pending_certificate = CertificatePending::new(
            beacon,
            protocol_parameters.into(),
            next_protocol_parameters.into(),
            signers,
            next_signer.into_iter().map(|s| s.into()).collect(),
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
            .save(pending_certificate)
            .await
            .map_err(|e| e.into())
    }

    async fn drop_pending_certificate(&self) -> Result<CertificatePending, RuntimeError> {
        info!("drop pending certificate");

        let certificate_pending = self
            .dependencies
            .certificate_pending_store
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

    /// Is a multisignature ready?
    /// Can we create a multisignature.
    async fn is_multisig_created(&self) -> Result<bool, RuntimeError> {
        info!("check if we can create a multisignature");
        let has_multisig = self
            .dependencies
            .multi_signer
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

    async fn create_snapshot_archive(&self) -> Result<OngoingSnapshot, RuntimeError> {
        info!("create snapshot archive");

        let snapshotter = self.dependencies.snapshotter.clone();
        let protocol_message = self
            .dependencies
            .multi_signer
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

    async fn upload_snapshot_archive(
        &self,
        ongoing_snapshot: &OngoingSnapshot,
    ) -> Result<Vec<SnapshotLocation>, RuntimeError> {
        info!("upload snapshot archive");
        let location = self
            .dependencies
            .snapshot_uploader
            .upload_snapshot(ongoing_snapshot.get_file_path())
            .await
            .map_err(RuntimeError::SnapshotUploader)?;

        if let Err(error) = tokio::fs::remove_file(ongoing_snapshot.get_file_path()).await {
            warn!(
                "Post upload ongoing snapshot file removal failure: {}",
                error
            );
        }

        Ok(vec![location])
    }

    async fn create_and_save_certificate(
        &self,
        beacon: &Beacon,
    ) -> Result<Certificate, RuntimeError> {
        info!("create and save certificate");
        let certificate_store = self.dependencies.certificate_store.clone();
        let latest_certificates = certificate_store.get_list(2).await?;
        let last_certificate = latest_certificates.get(0);
        let penultimate_certificate = latest_certificates.get(1);
        let previous_hash = match (penultimate_certificate, last_certificate) {
            (Some(penultimate_certificate), Some(last_certificate)) => {
                // Check if last certificate is exactly at most one epoch before current epoch
                if beacon.epoch - last_certificate.beacon.epoch > Epoch(1) {
                    return Err(RuntimeError::CertificateChainEpochGap(
                        beacon.epoch,
                        last_certificate.beacon.epoch,
                    ));
                }
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
        let multisigner = self.dependencies.multi_signer.read().await;
        let certificate = multisigner
            .create_certificate(beacon.clone(), previous_hash.to_owned())
            .await?
            .ok_or_else(|| RuntimeError::General("no certificate generated".to_string().into()))?;
        let _ = certificate_store.save(certificate.clone()).await?;

        Ok(certificate)
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
            .add_snapshot(snapshot.clone())
            .await?;

        Ok(snapshot)
    }
}

#[cfg(test)]
pub mod tests {
    use crate::dependency::{
        ProtocolParametersStoreWrapper, StakeStoreWrapper, VerificationKeyStoreWrapper,
    };
    use crate::multi_signer::MockMultiSigner;
    use crate::runtime::RuntimeError;
    use crate::snapshotter::OngoingSnapshot;
    use crate::ProtocolParametersStorer;
    use crate::{
        initialize_dependencies,
        runtime::{AggregatorRunner, AggregatorRunnerTrait},
        VerificationKeyStorer,
    };
    use mithril_common::crypto_helper::tests_setup::setup_certificate_chain;
    use mithril_common::entities::{
        Beacon, CertificatePending, Epoch, SignerWithStake, StakeDistribution,
    };
    use mithril_common::{
        entities::ProtocolMessagePartKey, fake_data, store::StakeStorer,
        NEXT_SIGNER_EPOCH_RETRIEVAL_OFFSET, SIGNER_EPOCH_RETRIEVAL_OFFSET,
    };
    use std::sync::Arc;
    use tempfile::NamedTempFile;
    use tokio::sync::RwLock;

    async fn save_verification_keys_and_stake_distribution_and_protocol_parameters(
        signers: Vec<SignerWithStake>,
        beacon: &Beacon,
        epoch_offset: i64,
        key_store: VerificationKeyStoreWrapper,
        stake_store: StakeStoreWrapper,
        protocol_parameters_store: ProtocolParametersStoreWrapper,
    ) {
        let epoch = beacon
            .epoch
            .offset_by(epoch_offset)
            .expect("epoch.offset_by should not fail");
        for signer in signers.iter().map(|s| s.into()).collect::<Vec<_>>() {
            key_store
                .save_verification_key(epoch, signer)
                .await
                .expect("save_verification_key should not fail");
        }
        stake_store
            .save_stakes(
                epoch,
                signers
                    .into_iter()
                    .map(|s| s.into())
                    .collect::<StakeDistribution>(),
            )
            .await
            .expect("save_stakes should not fail");
        let protocol_parameters = fake_data::protocol_parameters();
        protocol_parameters_store
            .save_protocol_parameters(epoch, protocol_parameters)
            .await
            .expect("save_protocol_parameters should not fail");
    }

    #[tokio::test]
    async fn test_is_new_beacon() {
        let (dependencies, config) = initialize_dependencies().await;
        let runner = AggregatorRunner::new(config, Arc::new(dependencies));

        // No beacon means the current beacon is newer
        let res = runner.is_new_beacon(None).await;
        assert!(res.unwrap().is_some());

        // old beacon means the current beacon is newer
        let beacon = Beacon {
            network: "testnet".to_string(),
            epoch: Epoch(0),
            immutable_file_number: 0,
        };
        let res = runner.is_new_beacon(Some(beacon)).await;
        assert!(res.unwrap().is_some());

        // new beacon mens the current beacon is not new
        let beacon = Beacon {
            network: "whatever".to_string(),
            epoch: Epoch(9206230),
            immutable_file_number: 10000,
        };
        let res = runner.is_new_beacon(Some(beacon)).await;
        assert!(res.unwrap().is_none());
    }

    #[tokio::test]
    async fn test_update_beacon() {
        let (deps, config) = initialize_dependencies().await;
        let deps = Arc::new(deps);
        let runner = AggregatorRunner::new(config, deps.clone());
        let beacon = runner.is_new_beacon(None).await.unwrap().unwrap();
        let res = runner.update_beacon(&beacon).await;

        assert!(res.is_ok());
        let stored_beacon = deps
            .multi_signer
            .read()
            .await
            .get_current_beacon()
            .await
            .unwrap();

        assert_eq!(beacon, stored_beacon);
    }

    #[tokio::test]
    async fn test_update_stake_distribution() {
        let (deps, config) = initialize_dependencies().await;
        let deps = Arc::new(deps);
        let runner = AggregatorRunner::new(config, deps.clone());
        let beacon = runner.is_new_beacon(None).await.unwrap().unwrap();
        runner
            .update_beacon(&beacon)
            .await
            .expect("setting the beacon should not fail");
        runner
            .update_stake_distribution(&beacon)
            .await
            .expect("updating stake distribution should not return an error");

        let current_stake_distribution = deps
            .chain_observer
            .get_current_stake_distribution()
            .await
            .unwrap()
            .expect("The stake distribution should not be None.");

        let saved_stake_distribution = deps
            .stake_store
            .get_stakes(
                beacon
                    .epoch
                    .offset_to_recording_epoch()
                    .expect("offset_to_recording_epoch should not fail"),
            )
            .await
            .unwrap()
            .unwrap_or_else(|| {
                panic!(
                    "I should have a stake distribution for the epoch {:?}",
                    beacon.epoch
                )
            });

        assert_eq!(
            current_stake_distribution.len(),
            saved_stake_distribution.len()
        );
        for (party_id, stake) in current_stake_distribution.iter() {
            assert_eq!(stake, saved_stake_distribution.get(party_id).unwrap());
        }
    }

    #[tokio::test]
    async fn test_update_protocol_parameters_in_multisigner() {
        let (deps, config) = initialize_dependencies().await;
        let deps = Arc::new(deps);
        let runner = AggregatorRunner::new(config, deps.clone());
        let beacon = runner.is_new_beacon(None).await.unwrap().unwrap();
        runner
            .update_beacon(&beacon)
            .await
            .expect("setting the beacon should not fail");
        runner
            .update_protocol_parameters_in_multisigner(&beacon)
            .await
            .expect("updating protocol parameters should not return an error");

        let current_protocol_parameters = deps.config.protocol_parameters.clone();

        let saved_protocol_parameters = deps
            .protocol_parameters_store
            .get_protocol_parameters(beacon.epoch + 1)
            .await
            .unwrap()
            .unwrap_or_else(|| {
                panic!(
                    "should have protocol parameters for the epoch {:?}",
                    beacon.epoch
                )
            });

        assert_eq!(current_protocol_parameters, saved_protocol_parameters);
    }

    #[tokio::test]
    async fn test_create_new_pending_certificate_from_multisigner() {
        let (deps, config) = initialize_dependencies().await;
        let deps = Arc::new(deps);
        let runner = AggregatorRunner::new(config, deps.clone());
        let beacon = runner.is_new_beacon(None).await.unwrap().unwrap();
        runner.update_beacon(&beacon).await.unwrap();

        let signers = fake_data::signers_with_stakes(5);
        let current_signers = signers[1..3].to_vec();
        let next_signers = signers[2..5].to_vec();
        save_verification_keys_and_stake_distribution_and_protocol_parameters(
            current_signers.clone(),
            &beacon,
            SIGNER_EPOCH_RETRIEVAL_OFFSET,
            deps.verification_key_store.clone(),
            deps.stake_store.clone(),
            deps.protocol_parameters_store.clone(),
        )
        .await;
        save_verification_keys_and_stake_distribution_and_protocol_parameters(
            next_signers.clone(),
            &beacon,
            NEXT_SIGNER_EPOCH_RETRIEVAL_OFFSET,
            deps.verification_key_store.clone(),
            deps.stake_store.clone(),
            deps.protocol_parameters_store.clone(),
        )
        .await;

        let mut certificate = runner
            .create_new_pending_certificate_from_multisigner(beacon.clone())
            .await
            .unwrap();
        certificate.signers.sort_by_key(|s| s.party_id.clone());
        certificate.next_signers.sort_by_key(|s| s.party_id.clone());
        let mut expected = CertificatePending::new(
            beacon,
            fake_data::protocol_parameters(),
            fake_data::protocol_parameters(),
            current_signers.into_iter().map(|s| s.into()).collect(),
            next_signers.into_iter().map(|s| s.into()).collect(),
        );
        expected.signers.sort_by_key(|s| s.party_id.clone());
        expected.next_signers.sort_by_key(|s| s.party_id.clone());

        assert_eq!(expected, certificate);
    }

    #[tokio::test]
    async fn test_update_message_in_multisigner() {
        let (deps, config) = initialize_dependencies().await;
        let deps = Arc::new(deps);
        let runner = AggregatorRunner::new(config, deps.clone());
        let beacon = runner.is_new_beacon(None).await.unwrap().unwrap();
        let digest = "1+2+3+4=10".to_string();
        runner.update_beacon(&beacon).await.unwrap();

        assert!(runner.update_message_in_multisigner(digest).await.is_ok());
        let message = deps
            .multi_signer
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
        let deps = Arc::new(deps);
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

        let saved_cert = deps.certificate_pending_store.get().await.unwrap().unwrap();

        assert_eq!(pending_certificate, saved_cert);
    }

    #[tokio::test]
    async fn test_drop_pending_certificate() {
        let (deps, config) = initialize_dependencies().await;
        let deps = Arc::new(deps);
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
        let maybe_saved_cert = deps.certificate_pending_store.get().await.unwrap();
        assert!(maybe_saved_cert.is_none());
    }

    #[tokio::test]
    async fn test_drop_pending_no_certificate() {
        let (deps, config) = initialize_dependencies().await;
        let runner = AggregatorRunner::new(config, Arc::new(deps));
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

    #[tokio::test]
    async fn test_create_and_save_certificate_ok() {
        let certificate_chain = setup_certificate_chain(5, 1);
        let first_certificate = certificate_chain[0].clone();
        let beacon = first_certificate.beacon.clone();
        let (mut deps, config) = initialize_dependencies().await;
        let mut mock_multi_signer = MockMultiSigner::new();
        mock_multi_signer
            .expect_create_certificate()
            .return_once(move |_, _| Ok(Some(first_certificate.to_owned())));
        deps.multi_signer = Arc::new(RwLock::new(mock_multi_signer));
        let certificate_store = deps.certificate_store.clone();
        let runner = AggregatorRunner::new(config, Arc::new(deps));
        for certificate in certificate_chain[1..].iter().rev() {
            certificate_store
                .as_ref()
                .save(certificate.to_owned())
                .await
                .expect("save certificate to store should not fail");
        }

        let certificate = runner.create_and_save_certificate(&beacon).await;
        certificate.expect("a certificate should have been created and saved");
    }

    #[tokio::test]
    async fn test_create_and_save_certificate_ko_epoch_gap() {
        let (deps, config) = initialize_dependencies().await;
        let certificate_store = deps.certificate_store.clone();
        let runner = AggregatorRunner::new(config, Arc::new(deps));
        let total_certificates = 5;
        let certificate_chain = setup_certificate_chain(5, 1);
        let mut beacon = certificate_chain.first().unwrap().beacon.clone();
        beacon.epoch = beacon.epoch + 2;
        for certificate in certificate_chain.into_iter().rev() {
            certificate_store
                .as_ref()
                .save(certificate)
                .await
                .expect("save certificate to store should not fail");
        }

        let certificate = runner.create_and_save_certificate(&beacon).await;
        assert!(certificate.is_err());
        let err = certificate.unwrap_err();
        assert_eq!(
            RuntimeError::CertificateChainEpochGap(beacon.epoch, Epoch(total_certificates))
                .to_string(),
            err.to_string()
        );
    }

    #[tokio::test]
    async fn test_remove_snapshot_archive_after_upload() {
        let (deps, config) = initialize_dependencies().await;
        let runner = AggregatorRunner::new(config, Arc::new(deps));
        let file = NamedTempFile::new().unwrap();
        let file_path = file.path();
        let snapshot = OngoingSnapshot::new(file_path.to_path_buf(), 7331);

        runner
            .upload_snapshot_archive(&snapshot)
            .await
            .expect("Snapshot upload should not fail");

        assert!(
            !file_path.exists(),
            "Ongoing snapshot file should have been removed after upload"
        );
    }
}

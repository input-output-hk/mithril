use async_trait::async_trait;
use mithril_common::entities::Epoch;
use mithril_common::entities::SignedEntityType;
use mithril_common::store::StakeStorer;
use slog_scope::{debug, warn};

use mithril_common::crypto_helper::ProtocolStakeDistribution;
use mithril_common::entities::{
    Beacon, Certificate, CertificatePending, ProtocolMessage, ProtocolMessagePartKey,
};
use mithril_common::CardanoNetwork;

use std::error::Error as StdError;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;

use crate::entities::OpenMessage;
use crate::RuntimeError;
use crate::{DependencyManager, ProtocolError};

#[cfg(test)]
use mockall::automock;

use super::error::RunnerError;

/// Configuration structure dedicated to the AggregatorRuntime.
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
    /// Create a new instance of AggregatorConfig.
    pub fn new(interval: u64, network: CardanoNetwork, db_directory: &Path) -> Self {
        Self {
            interval,
            network,
            db_directory: db_directory.to_path_buf(),
        }
    }
}

/// This trait is intended to allow mocking the AggregatorRunner in tests.
/// It exposes all the methods needed by the state machine.
#[async_trait]
pub trait AggregatorRunnerTrait: Sync + Send {
    /// Return the current beacon from the chain
    async fn get_beacon_from_chain(&self) -> Result<Beacon, Box<dyn StdError + Sync + Send>>;

    /// Check if a certificate already have been issued for a given beacon.
    async fn does_certificate_exist_for_beacon(
        &self,
        beacon: &Beacon,
    ) -> Result<bool, Box<dyn StdError + Sync + Send>>;

    /// Check if a certificate chain is valid.
    async fn is_certificate_chain_valid(&self) -> Result<bool, Box<dyn StdError + Sync + Send>>;

    /// Update the multisigner with the given beacon.
    async fn update_beacon(
        &self,
        new_beacon: &Beacon,
    ) -> Result<(), Box<dyn StdError + Sync + Send>>;

    /// Read the stake distribution from the blockchain and store it.
    async fn update_stake_distribution(
        &self,
        new_beacon: &Beacon,
    ) -> Result<(), Box<dyn StdError + Sync + Send>>;

    /// Open the signer registration round of an epoch.
    async fn open_signer_registration_round(
        &self,
        new_beacon: &Beacon,
    ) -> Result<(), Box<dyn StdError + Sync + Send>>;

    /// Close the signer registration round of an epoch.
    async fn close_signer_registration_round(&self) -> Result<(), Box<dyn StdError + Sync + Send>>;

    /// Update the multisigner with the protocol parameters from configuration.
    async fn update_protocol_parameters_in_multisigner(
        &self,
        new_beacon: &Beacon,
    ) -> Result<(), Box<dyn StdError + Sync + Send>>;

    /// Compute the protocol message
    async fn compute_protocol_message(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> Result<ProtocolMessage, Box<dyn StdError + Sync + Send>>;

    /// Return the actual pending certificate from the multisigner.
    async fn create_new_pending_certificate_from_multisigner(
        &self,
        beacon: Beacon,
        signed_entity_type: &SignedEntityType,
    ) -> Result<CertificatePending, Box<dyn StdError + Sync + Send>>;

    /// Store the given pending certificate.
    async fn save_pending_certificate(
        &self,
        pending_certificate: CertificatePending,
    ) -> Result<(), Box<dyn StdError + Sync + Send>>;

    /// Drop the multisigner's actual pending certificate.
    async fn drop_pending_certificate(
        &self,
    ) -> Result<Option<CertificatePending>, Box<dyn StdError + Sync + Send>>;

    /// Create multi-signature.
    async fn create_certificate(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> Result<Option<Certificate>, Box<dyn StdError + Sync + Send>>;

    /// Create an artifact and persist it.
    async fn create_artifact(
        &self,
        signed_entity_type: &SignedEntityType,
        certificate: &Certificate,
    ) -> Result<(), Box<dyn StdError + Sync + Send>>;

    /// Update the EraChecker with EraReader information.
    async fn update_era_checker(
        &self,
        beacon: &Beacon,
    ) -> Result<(), Box<dyn StdError + Sync + Send>>;

    /// Certifier inform new epoch
    async fn certifier_inform_new_epoch(
        &self,
        epoch: &Epoch,
    ) -> Result<(), Box<dyn StdError + Sync + Send>>;

    /// Create new open message
    async fn create_open_message(
        &self,
        signed_entity_type: &SignedEntityType,
        protocol_message: &ProtocolMessage,
    ) -> Result<OpenMessage, Box<dyn StdError + Sync + Send>>;
}

/// The runner responsibility is to expose a code API for the state machine. It
/// holds services and configuration.
pub struct AggregatorRunner {
    dependencies: Arc<DependencyManager>,
}

impl AggregatorRunner {
    /// Create a new instance of the Aggrergator Runner.
    pub fn new(dependencies: Arc<DependencyManager>) -> Self {
        Self { dependencies }
    }
}

#[cfg_attr(test, automock)]
#[async_trait]
impl AggregatorRunnerTrait for AggregatorRunner {
    /// Return the current beacon from the chain
    async fn get_beacon_from_chain(&self) -> Result<Beacon, Box<dyn StdError + Sync + Send>> {
        debug!("RUNNER: get beacon from chain");
        let beacon = self
            .dependencies
            .beacon_provider
            .get_current_beacon()
            .await?;

        Ok(beacon)
    }

    async fn does_certificate_exist_for_beacon(
        &self,
        beacon: &Beacon,
    ) -> Result<bool, Box<dyn StdError + Sync + Send>> {
        debug!("RUNNER: does_certificate_exist_for_beacon");
        let certificate_exist = self
            .dependencies
            .certificate_store
            .get_from_beacon(beacon)
            .await?
            .is_some();
        Ok(certificate_exist)
    }

    async fn is_certificate_chain_valid(&self) -> Result<bool, Box<dyn StdError + Sync + Send>> {
        debug!("RUNNER: is_certificate_chain_valid");
        let certificate_store = self.dependencies.certificate_store.clone();
        let latest_certificates = certificate_store.get_list(1).await?;
        let latest_certificate = latest_certificates.first();
        if latest_certificate.is_none() {
            return Ok(false);
        }

        match self
            .dependencies
            .certificate_verifier
            .verify_certificate_chain(
                latest_certificate.unwrap().to_owned(),
                certificate_store.clone(),
                &self.dependencies.genesis_verifier,
            )
            .await
        {
            Ok(()) => Ok(true),
            Err(error) => {
                warn!(" > invalid certificate chain"; "error" => ?error);
                Ok(false)
            }
        }
    }

    async fn update_beacon(
        &self,
        new_beacon: &Beacon,
    ) -> Result<(), Box<dyn StdError + Sync + Send>> {
        debug!("RUNNER: update beacon"; "beacon" => #?new_beacon);
        self.dependencies
            .multi_signer
            .write()
            .await
            .update_current_beacon(new_beacon.to_owned())
            .await?;
        Ok(())
    }

    async fn update_stake_distribution(
        &self,
        new_beacon: &Beacon,
    ) -> Result<(), Box<dyn StdError + Sync + Send>> {
        debug!("RUNNER: update stake distribution"; "beacon" => #?new_beacon);
        let exists_stake_distribution = !self
            .dependencies
            .stake_store
            .get_stakes(
                self.dependencies
                    .multi_signer
                    .read()
                    .await
                    .get_current_beacon()
                    .await
                    .ok_or_else(|| {
                        RuntimeError::keep_state("Current beacon is not available", None)
                    })?
                    .epoch
                    .offset_to_recording_epoch(),
            )
            .await?
            .unwrap_or_default()
            .is_empty();
        if exists_stake_distribution {
            return Ok(());
        }

        let stake_distribution = self
            .dependencies
            .chain_observer
            .get_current_stake_distribution()
            .await?
            .ok_or_else(|| {
                RunnerError::MissingStakeDistribution(format!(
                    "Chain observer: no stake distribution for beacon {new_beacon:?}."
                ))
            })?;
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

    async fn open_signer_registration_round(
        &self,
        new_beacon: &Beacon,
    ) -> Result<(), Box<dyn StdError + Sync + Send>> {
        debug!("RUNNER: open signer registration round"; "beacon" => #?new_beacon);
        let registration_epoch = new_beacon.epoch.offset_to_recording_epoch();

        let stakes = self
            .dependencies
            .stake_store
            .get_stakes(registration_epoch)
            .await?
            .unwrap_or_default();

        self.dependencies
            .signer_registration_round_opener
            .open_registration_round(registration_epoch, stakes)
            .await
            .map_err(|e| e.into())
    }

    async fn close_signer_registration_round(&self) -> Result<(), Box<dyn StdError + Sync + Send>> {
        debug!("RUNNER: close signer registration round");

        self.dependencies
            .signer_registration_round_opener
            .close_registration_round()
            .await
            .map_err(|e| e.into())
    }

    async fn update_protocol_parameters_in_multisigner(
        &self,
        new_beacon: &Beacon,
    ) -> Result<(), Box<dyn StdError + Sync + Send>> {
        debug!("RUNNER: update protocol parameters"; "beacon" => #?new_beacon);
        let protocol_parameters = self.dependencies.config.protocol_parameters.clone();

        self.dependencies
            .multi_signer
            .write()
            .await
            .update_protocol_parameters(&protocol_parameters.into())
            .await
            .map_err(|e| e.into())
    }

    async fn compute_protocol_message(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> Result<ProtocolMessage, Box<dyn StdError + Sync + Send>> {
        debug!("RUNNER: compute protocol message");
        let mut protocol_message = self
            .dependencies
            .signable_builder_service
            .compute_protocol_message(signed_entity_type.to_owned())
            .await?;

        let multi_signer = self.dependencies.multi_signer.write().await;
        protocol_message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            multi_signer
                .compute_next_stake_distribution_aggregate_verification_key()
                .await?
                .unwrap_or_default(),
        );

        Ok(protocol_message)
    }

    async fn create_new_pending_certificate_from_multisigner(
        &self,
        beacon: Beacon,
        signed_entity_type: &SignedEntityType,
    ) -> Result<CertificatePending, Box<dyn StdError + Sync + Send>> {
        debug!("RUNNER: create new pending certificate from multisigner");
        let multi_signer = self.dependencies.multi_signer.read().await;

        let signers = match multi_signer.get_signers().await {
            Ok(signers) => signers,
            Err(ProtocolError::Beacon(_)) => vec![],
            Err(e) => return Err(e.into()),
        };
        let next_signers = match multi_signer.get_next_signers_with_stake().await {
            Ok(signers) => signers,
            Err(ProtocolError::Beacon(_)) => vec![],
            Err(e) => return Err(e.into()),
        };

        let protocol_parameters =
            multi_signer
                .get_protocol_parameters()
                .await?
                .ok_or_else(|| {
                    RunnerError::MissingProtocolParameters(format!(
                        "no current protocol parameters found for beacon {beacon:?}"
                    ))
                })?;

        let next_protocol_parameters = multi_signer
            .get_next_protocol_parameters()
            .await?
            .ok_or_else(|| {
                RunnerError::MissingProtocolParameters(format!(
                    "no next protocol parameters found for beacon {beacon:?}"
                ))
            })?;

        let pending_certificate = CertificatePending::new(
            beacon,
            signed_entity_type.to_owned(),
            protocol_parameters.into(),
            next_protocol_parameters.into(),
            signers,
            next_signers.into_iter().map(|s| s.into()).collect(),
        );

        Ok(pending_certificate)
    }

    async fn save_pending_certificate(
        &self,
        pending_certificate: CertificatePending,
    ) -> Result<(), Box<dyn StdError + Sync + Send>> {
        debug!("RUNNER: saving pending certificate");

        self.dependencies
            .certificate_pending_store
            .save(pending_certificate)
            .await
            .map_err(|e| e.into())
    }

    async fn drop_pending_certificate(
        &self,
    ) -> Result<Option<CertificatePending>, Box<dyn StdError + Sync + Send>> {
        debug!("RUNNER: drop pending certificate");

        let certificate_pending = self.dependencies.certificate_pending_store.remove().await?;
        if certificate_pending.is_none() {
            warn!(" > drop_pending_certificate::no certificate pending in store, did the previous loop crashed ?");
        }

        Ok(certificate_pending)
    }

    async fn create_certificate(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> Result<Option<Certificate>, Box<dyn StdError + Sync + Send>> {
        debug!("RUNNER: create multi-signature");

        self.dependencies
            .certifier_service
            .create_certificate(signed_entity_type)
            .await
    }

    async fn create_artifact(
        &self,
        signed_entity_type: &SignedEntityType,
        certificate: &Certificate,
    ) -> Result<(), Box<dyn StdError + Sync + Send>> {
        debug!("RUNNER: create artifact");
        self.dependencies
            .artifact_builder_service
            .create_artifact(signed_entity_type.to_owned(), certificate)
            .await?;

        Ok(())
    }

    async fn update_era_checker(
        &self,
        beacon: &Beacon,
    ) -> Result<(), Box<dyn StdError + Sync + Send>> {
        let token = self
            .dependencies
            .era_reader
            .read_era_epoch_token(beacon.epoch)
            .await?;

        let current_era = token.get_current_supported_era()?;
        self.dependencies
            .era_checker
            .change_era(current_era, token.get_current_epoch());
        debug!(
            "Current Era is {} (Epoch {}).",
            current_era,
            token.get_current_epoch()
        );

        if token.get_next_supported_era().is_err() {
            let era_name = &token.get_next_era_marker().unwrap().name;
            warn!("Upcoming Era '{era_name}' is not supported by this version of the software. Please update!");
        }

        Ok(())
    }

    async fn certifier_inform_new_epoch(
        &self,
        epoch: &Epoch,
    ) -> Result<(), Box<dyn StdError + Sync + Send>> {
        self.dependencies
            .certifier_service
            .inform_epoch(*epoch)
            .await?;

        Ok(())
    }

    async fn create_open_message(
        &self,
        signed_entity_type: &SignedEntityType,
        protocol_message: &ProtocolMessage,
    ) -> Result<OpenMessage, Box<dyn StdError + Sync + Send>> {
        self.dependencies
            .certifier_service
            .create_open_message(signed_entity_type, protocol_message)
            .await
    }
}

#[cfg(test)]
pub mod tests {
    use crate::certifier_service::MockCertifierService;
    use crate::{
        initialize_dependencies,
        runtime::{AggregatorRunner, AggregatorRunnerTrait},
    };
    use crate::{MithrilSignerRegisterer, ProtocolParametersStorer, SignerRegistrationRound};
    use mithril_common::chain_observer::FakeObserver;
    use mithril_common::digesters::DumbImmutableFileObserver;
    use mithril_common::entities::{
        Beacon, CertificatePending, Epoch, SignedEntityType, StakeDistribution,
    };
    use mithril_common::store::StakeStorer;
    use mithril_common::test_utils::fake_data;
    use mithril_common::test_utils::MithrilFixtureBuilder;
    use mithril_common::{BeaconProviderImpl, CardanoNetwork};
    use std::sync::Arc;

    #[tokio::test]
    async fn test_get_beacon_from_chain() {
        let expected_beacon = Beacon::new("private".to_string(), 2, 17);
        let mut dependencies = initialize_dependencies().await;
        let immutable_file_observer = Arc::new(DumbImmutableFileObserver::default());
        immutable_file_observer
            .shall_return(Some(expected_beacon.immutable_file_number))
            .await;
        let beacon_provider = Arc::new(BeaconProviderImpl::new(
            Arc::new(FakeObserver::new(Some(expected_beacon.clone()))),
            immutable_file_observer,
            CardanoNetwork::TestNet(42),
        ));
        dependencies.beacon_provider = beacon_provider;
        let runner = AggregatorRunner::new(Arc::new(dependencies));

        // Retrieves the expected beacon
        let res = runner.get_beacon_from_chain().await;
        assert_eq!(expected_beacon, res.unwrap());
    }

    #[tokio::test]
    async fn test_does_certificate_exist_for_beacon() {
        let dependencies = initialize_dependencies().await;
        let certificate_store = dependencies.certificate_store.clone();
        let runner = AggregatorRunner::new(Arc::new(dependencies));

        let beacon = fake_data::beacon();
        let mut certificate = fake_data::genesis_certificate("certificate_hash".to_string());
        certificate.beacon = beacon.clone();

        assert!(!runner
            .does_certificate_exist_for_beacon(&beacon)
            .await
            .unwrap());
        certificate_store.save(certificate).await.unwrap();
        assert!(runner
            .does_certificate_exist_for_beacon(&beacon)
            .await
            .unwrap());
    }

    #[tokio::test]
    async fn test_update_beacon() {
        let deps = initialize_dependencies().await;
        let deps = Arc::new(deps);
        let runner = AggregatorRunner::new(deps.clone());
        let beacon = runner.get_beacon_from_chain().await.unwrap();
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
        let mut deps = initialize_dependencies().await;
        let chain_observer = Arc::new(FakeObserver::default());
        deps.chain_observer = chain_observer.clone();
        let deps = Arc::new(deps);
        let runner = AggregatorRunner::new(deps.clone());
        let beacon = runner.get_beacon_from_chain().await.unwrap();
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let expected = fixture.stake_distribution();

        chain_observer
            .set_signers(fixture.signers_with_stake())
            .await;
        runner
            .update_beacon(&beacon)
            .await
            .expect("setting the beacon should not fail");
        runner
            .update_stake_distribution(&beacon)
            .await
            .expect("updating stake distribution should not return an error");

        let saved_stake_distribution = deps
            .stake_store
            .get_stakes(beacon.epoch.offset_to_recording_epoch())
            .await
            .unwrap()
            .unwrap_or_else(|| {
                panic!(
                    "I should have a stake distribution for the epoch {:?}",
                    beacon.epoch
                )
            });

        assert_eq!(expected, saved_stake_distribution);
    }

    #[tokio::test]
    async fn test_open_signer_registration_round() {
        let mut deps = initialize_dependencies().await;
        let signer_registration_round_opener = Arc::new(MithrilSignerRegisterer::new(
            deps.chain_observer.clone(),
            deps.verification_key_store.clone(),
            deps.signer_recorder.clone(),
        ));
        deps.signer_registration_round_opener = signer_registration_round_opener.clone();
        let stake_store = deps.stake_store.clone();
        let deps = Arc::new(deps);
        let runner = AggregatorRunner::new(deps.clone());

        let beacon = fake_data::beacon();
        let recording_epoch = beacon.epoch.offset_to_recording_epoch();
        let stake_distribution: StakeDistribution =
            StakeDistribution::from([("a".to_string(), 5), ("b".to_string(), 10)]);

        stake_store
            .save_stakes(recording_epoch, stake_distribution.clone())
            .await
            .expect("Save Stake distribution should not fail");

        runner
            .open_signer_registration_round(&beacon)
            .await
            .expect("opening signer registration should not return an error");

        let saved_current_round = signer_registration_round_opener.get_current_round().await;

        let expected_signer_registration_round =
            SignerRegistrationRound::dummy(recording_epoch, stake_distribution);

        assert_eq!(
            Some(expected_signer_registration_round),
            saved_current_round,
        );
    }

    #[tokio::test]
    async fn test_close_signer_registration_round() {
        let mut deps = initialize_dependencies().await;
        let signer_registration_round_opener = Arc::new(MithrilSignerRegisterer::new(
            deps.chain_observer.clone(),
            deps.verification_key_store.clone(),
            deps.signer_recorder.clone(),
        ));
        deps.signer_registration_round_opener = signer_registration_round_opener.clone();
        let deps = Arc::new(deps);
        let runner = AggregatorRunner::new(deps.clone());

        let beacon = fake_data::beacon();
        runner
            .open_signer_registration_round(&beacon)
            .await
            .expect("opening signer registration should not return an error");

        runner
            .close_signer_registration_round()
            .await
            .expect("closing signer registration should not return an error");

        let saved_current_round = signer_registration_round_opener.get_current_round().await;
        assert!(saved_current_round.is_none());
    }

    #[tokio::test]
    async fn test_update_protocol_parameters_in_multisigner() {
        let deps = initialize_dependencies().await;
        let deps = Arc::new(deps);
        let runner = AggregatorRunner::new(deps.clone());
        let beacon = runner.get_beacon_from_chain().await.unwrap();
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
            .get_protocol_parameters(beacon.epoch.offset_to_protocol_parameters_recording_epoch())
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
        let deps = initialize_dependencies().await;
        let deps = Arc::new(deps);
        let runner = AggregatorRunner::new(deps.clone());
        let beacon = runner.get_beacon_from_chain().await.unwrap();
        runner.update_beacon(&beacon).await.unwrap();
        let signed_entity_type = SignedEntityType::dummy();

        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let current_signers = fixture.signers_with_stake()[1..3].to_vec();
        let next_signers = fixture.signers_with_stake()[2..5].to_vec();
        let protocol_parameters = fake_data::protocol_parameters();
        deps.prepare_for_genesis(
            current_signers.clone(),
            next_signers.clone(),
            &protocol_parameters.clone(),
        )
        .await;

        let mut certificate = runner
            .create_new_pending_certificate_from_multisigner(beacon.clone(), &signed_entity_type)
            .await
            .unwrap();
        certificate.signers.sort_by_key(|s| s.party_id.clone());
        certificate.next_signers.sort_by_key(|s| s.party_id.clone());
        let mut expected = CertificatePending::new(
            beacon,
            signed_entity_type,
            protocol_parameters.clone(),
            protocol_parameters,
            current_signers.into_iter().map(|s| s.into()).collect(),
            next_signers.into_iter().map(|s| s.into()).collect(),
        );
        expected.signers.sort_by_key(|s| s.party_id.clone());
        expected.next_signers.sort_by_key(|s| s.party_id.clone());

        assert_eq!(expected, certificate);
    }

    #[tokio::test]
    async fn test_save_pending_certificate() {
        let deps = initialize_dependencies().await;
        let deps = Arc::new(deps);
        let runner = AggregatorRunner::new(deps.clone());
        let beacon = runner.get_beacon_from_chain().await.unwrap();
        runner.update_beacon(&beacon).await.unwrap();
        let pending_certificate = fake_data::certificate_pending();

        runner
            .save_pending_certificate(pending_certificate.clone())
            .await
            .expect("save_pending_certificate should not fail");

        let saved_cert = deps.certificate_pending_store.get().await.unwrap().unwrap();
        assert_eq!(pending_certificate, saved_cert);
    }

    #[tokio::test]
    async fn test_drop_pending_certificate() {
        let deps = initialize_dependencies().await;
        let deps = Arc::new(deps);
        let runner = AggregatorRunner::new(deps.clone());
        let beacon = runner.get_beacon_from_chain().await.unwrap();
        runner.update_beacon(&beacon).await.unwrap();
        let pending_certificate = fake_data::certificate_pending();
        deps.certificate_pending_store
            .save(pending_certificate.clone())
            .await
            .unwrap();

        let cert = runner.drop_pending_certificate().await.unwrap();
        assert_eq!(Some(pending_certificate), cert);
        let maybe_saved_cert = deps.certificate_pending_store.get().await.unwrap();
        assert_eq!(None, maybe_saved_cert);
    }

    #[tokio::test]
    async fn test_drop_pending_no_certificate() {
        let deps = initialize_dependencies().await;
        let deps = Arc::new(deps);
        let runner = AggregatorRunner::new(deps.clone());
        let beacon = runner.get_beacon_from_chain().await.unwrap();
        runner.update_beacon(&beacon).await.unwrap();

        let cert = runner.drop_pending_certificate().await.unwrap();
        assert_eq!(None, cert);
        let maybe_saved_cert = deps.certificate_pending_store.get().await.unwrap();
        assert_eq!(None, maybe_saved_cert);
    }

    #[tokio::test]
    async fn test_update_era_checker() {
        let deps = initialize_dependencies().await;
        let beacon_provider = deps.beacon_provider.clone();
        let era_checker = deps.era_checker.clone();
        let mut beacon = beacon_provider.get_current_beacon().await.unwrap();

        assert_eq!(beacon.epoch, era_checker.current_epoch());
        let runner = AggregatorRunner::new(Arc::new(deps));
        beacon.epoch += 1;

        runner.update_era_checker(&beacon).await.unwrap();
        assert_eq!(beacon.epoch, era_checker.current_epoch());
    }

    #[tokio::test]
    async fn test_certifier_inform_new_epoch() {
        let mut mock_certifier_service = MockCertifierService::new();
        mock_certifier_service
            .expect_inform_epoch()
            .returning(|_| Ok(()))
            .times(1);

        let mut deps = initialize_dependencies().await;
        deps.certifier_service = Arc::new(mock_certifier_service);

        let runner = AggregatorRunner::new(Arc::new(deps));
        runner.certifier_inform_new_epoch(&Epoch(1)).await.unwrap();
    }
}

use anyhow::{anyhow, Context};
use async_trait::async_trait;
use slog_scope::{debug, warn};
use std::{path::Path, path::PathBuf, sync::Arc};

use mithril_common::entities::{
    Beacon, Certificate, CertificatePending, Epoch, ProtocolMessage, ProtocolMessagePartKey,
    SignedEntityType, Signer,
};
use mithril_common::{CardanoNetwork, StdResult};
use mithril_persistence::store::StakeStorer;

use crate::entities::OpenMessage;
use crate::DependencyContainer;

#[cfg(test)]
use mockall::automock;

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
    async fn get_beacon_from_chain(&self) -> StdResult<Beacon>;

    /// Retrieves the current open message for a given signed entity type.
    async fn get_current_open_message_for_signed_entity_type(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<Option<OpenMessage>>;

    /// Retrieves the current non certified open message.
    async fn get_current_non_certified_open_message(
        &self,
        current_beacon: &Beacon,
    ) -> StdResult<Option<OpenMessage>>;

    /// Check if a certificate chain is valid.
    async fn is_certificate_chain_valid(&self, beacon: &Beacon) -> StdResult<()>;

    /// Read the stake distribution from the blockchain and store it.
    async fn update_stake_distribution(&self, new_beacon: &Beacon) -> StdResult<()>;

    /// Open the signer registration round of an epoch.
    async fn open_signer_registration_round(&self, new_beacon: &Beacon) -> StdResult<()>;

    /// Close the signer registration round of an epoch.
    async fn close_signer_registration_round(&self) -> StdResult<()>;

    /// Ask the EpochService to update the protocol parameters.
    async fn update_protocol_parameters(&self) -> StdResult<()>;

    /// Compute the protocol message
    async fn compute_protocol_message(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<ProtocolMessage>;

    /// Mark expired open message.
    async fn mark_open_message_if_expired(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<Option<OpenMessage>>;

    /// Create a new pending certificate.
    async fn create_new_pending_certificate(
        &self,
        beacon: Beacon,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<CertificatePending>;

    /// Store the given pending certificate.
    async fn save_pending_certificate(
        &self,
        pending_certificate: CertificatePending,
    ) -> StdResult<()>;

    /// Drop the actual pending certificate in the store.
    async fn drop_pending_certificate(&self) -> StdResult<Option<CertificatePending>>;

    /// Tell the certifier to try to create a new certificate.
    async fn create_certificate(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<Option<Certificate>>;

    /// Create an artifact and persist it.
    async fn create_artifact(
        &self,
        signed_entity_type: &SignedEntityType,
        certificate: &Certificate,
    ) -> StdResult<()>;

    /// Update the EraChecker with EraReader information.
    async fn update_era_checker(&self, beacon: &Beacon) -> StdResult<()>;

    /// Ask services to update themselves for the new epoch
    async fn inform_new_epoch(&self, epoch: Epoch) -> StdResult<()>;

    /// Precompute what doesn't change for the actual epoch
    async fn precompute_epoch_data(&self) -> StdResult<()>;

    /// Create new open message
    async fn create_open_message(
        &self,
        signed_entity_type: &SignedEntityType,
        protocol_message: &ProtocolMessage,
    ) -> StdResult<OpenMessage>;
}

/// The runner responsibility is to expose a code API for the state machine. It
/// holds services and configuration.
pub struct AggregatorRunner {
    dependencies: Arc<DependencyContainer>,
}

impl AggregatorRunner {
    /// Create a new instance of the Aggregator Runner.
    pub fn new(dependencies: Arc<DependencyContainer>) -> Self {
        Self { dependencies }
    }
}

#[cfg_attr(test, automock)]
#[async_trait]
impl AggregatorRunnerTrait for AggregatorRunner {
    /// Return the current beacon from the chain
    async fn get_beacon_from_chain(&self) -> StdResult<Beacon> {
        debug!("RUNNER: get beacon from chain");
        let beacon = self
            .dependencies
            .beacon_provider
            .get_current_beacon()
            .await?;

        Ok(beacon)
    }

    async fn get_current_open_message_for_signed_entity_type(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<Option<OpenMessage>> {
        debug!("RUNNER: get_current_open_message_for_signed_entity_type"; "signed_entity_type" => ?signed_entity_type);
        self.mark_open_message_if_expired(signed_entity_type)
            .await?;

        Ok(self
            .dependencies
            .certifier_service
            .get_open_message(signed_entity_type)
            .await
            .with_context(|| format!("CertifierService can not get open message for signed_entity_type: '{signed_entity_type}'"))?)
    }

    async fn get_current_non_certified_open_message(
        &self,
        current_beacon: &Beacon,
    ) -> StdResult<Option<OpenMessage>> {
        debug!("RUNNER: get_current_non_certified_open_message"; "beacon" => #?current_beacon);
        let signed_entity_types = self
            .dependencies
            .config
            .list_allowed_signed_entity_types(current_beacon)
            .with_context(|| {
                "AggregatorRunner can not create the list of allowed signed entity types"
            })?;
        for signed_entity_type in signed_entity_types {
            if let Some(open_message) = self
                .get_current_open_message_for_signed_entity_type(&signed_entity_type)
                .await
                .with_context(|| format!("AggregatorRunner can not get current open message for signed entity type: '{}'", &signed_entity_type))?
            {
                if !open_message.is_certified {
                    return Ok(Some(open_message));
                }
                if open_message.is_certified || open_message.is_expired {
                    continue;
                }
            }
            let protocol_message = self.compute_protocol_message(&signed_entity_type).await.with_context(|| format!("AggregatorRunner can not compute protocol message for signed_entity_type: '{signed_entity_type}'"))?;
            let open_message_new = self.create_open_message(&signed_entity_type, &protocol_message)
            .await
            .with_context(|| format!("AggregatorRunner can not create open message for signed_entity_type: '{signed_entity_type}'"))?;

            return Ok(Some(open_message_new));
        }

        Ok(None)
    }

    async fn is_certificate_chain_valid(&self, beacon: &Beacon) -> StdResult<()> {
        debug!("RUNNER: is_certificate_chain_valid");
        self.dependencies
            .certifier_service
            .verify_certificate_chain(beacon.epoch)
            .await?;

        Ok(())
    }

    async fn update_stake_distribution(&self, new_beacon: &Beacon) -> StdResult<()> {
        debug!("RUNNER: update stake distribution"; "beacon" => #?new_beacon);
        self.dependencies
            .stake_distribution_service
            .update_stake_distribution()
            .await
            .with_context(|| format!("AggregatorRunner could not update stake distribution for beacon: '{new_beacon}'"))
    }

    async fn open_signer_registration_round(&self, new_beacon: &Beacon) -> StdResult<()> {
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
    }

    async fn close_signer_registration_round(&self) -> StdResult<()> {
        debug!("RUNNER: close signer registration round");

        self.dependencies
            .signer_registration_round_opener
            .close_registration_round()
            .await
    }

    async fn update_protocol_parameters(&self) -> StdResult<()> {
        self.dependencies
            .epoch_service
            .write()
            .await
            .update_protocol_parameters()
            .await
    }

    async fn compute_protocol_message(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<ProtocolMessage> {
        debug!("RUNNER: compute protocol message");
        let mut protocol_message = self
            .dependencies
            .signable_builder_service
            .compute_protocol_message(signed_entity_type.to_owned())
            .await
            .with_context(|| format!("Runner can not compute protocol message for signed entity type: '{signed_entity_type}'"))?;

        let epoch_service = self.dependencies.epoch_service.read().await;
        protocol_message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            epoch_service
                .next_aggregate_verification_key()?
                .to_json_hex()
                .with_context(|| "convert next avk to json hex failure")?,
        );

        Ok(protocol_message)
    }

    async fn mark_open_message_if_expired(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<Option<OpenMessage>> {
        debug!("RUNNER: mark expired open message");

        let expired_open_message = self
            .dependencies
            .certifier_service
            .mark_open_message_if_expired(signed_entity_type)
            .await
            .with_context(|| "CertifierService can not mark expired open message")?;

        debug!(
            "RUNNER: marked expired open messages: {:#?}",
            expired_open_message
        );

        Ok(expired_open_message)
    }

    async fn create_new_pending_certificate(
        &self,
        beacon: Beacon,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<CertificatePending> {
        debug!("RUNNER: create new pending certificate");
        let epoch_service = self.dependencies.epoch_service.read().await;

        let signers = epoch_service.current_signers_with_stake()?;
        let next_signers = epoch_service.next_signers_with_stake()?;

        let protocol_parameters =
            epoch_service
                .current_protocol_parameters()
                .with_context(|| {
                    format!("no current protocol parameters found for beacon {beacon:?}")
                })?;
        let next_protocol_parameters = epoch_service
            .next_protocol_parameters()
            .with_context(|| format!("no next protocol parameters found for beacon {beacon:?}"))?;

        let pending_certificate = CertificatePending::new(
            beacon,
            signed_entity_type.to_owned(),
            protocol_parameters.clone(),
            next_protocol_parameters.clone(),
            Signer::vec_from(signers.clone()),
            Signer::vec_from(next_signers.clone()),
        );

        Ok(pending_certificate)
    }

    async fn save_pending_certificate(
        &self,
        pending_certificate: CertificatePending,
    ) -> StdResult<()> {
        debug!("RUNNER: saving pending certificate");

        let signed_entity_type = pending_certificate.signed_entity_type.clone();
        self.dependencies
            .certificate_pending_store
            .save(pending_certificate)
            .await
            .map_err(|e| anyhow!(e))
            .with_context(|| format!("CertificatePendingStore can not save pending certificate with signed_entity_type: '{signed_entity_type}'"))
    }

    async fn drop_pending_certificate(&self) -> StdResult<Option<CertificatePending>> {
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
    ) -> StdResult<Option<Certificate>> {
        debug!("RUNNER: create_certificate");

        self.dependencies
            .certifier_service
            .create_certificate(signed_entity_type)
            .await
            .with_context(|| {
                format!(
                    "CertifierService can not create certificate for signed_entity_type: '{signed_entity_type}'"
                )
            })
    }

    async fn create_artifact(
        &self,
        signed_entity_type: &SignedEntityType,
        certificate: &Certificate,
    ) -> StdResult<()> {
        debug!("RUNNER: create artifact");
        self.dependencies
            .signed_entity_service
            .create_artifact(signed_entity_type.to_owned(), certificate)
            .await
            .with_context(|| {
                format!(
                    "SignedEntityService can not create artifact for signed_entity_type: '{signed_entity_type}' with certificate hash: '{}'",
                    certificate.hash
                )
            })?;

        Ok(())
    }

    async fn update_era_checker(&self, beacon: &Beacon) -> StdResult<()> {
        let token = self
            .dependencies
            .era_reader
            .read_era_epoch_token(beacon.epoch)
            .await
            .with_context(|| {
                format!(
                    "EraReader can not get era epoch token for current epoch: '{}'",
                    beacon.epoch
                )
            })?;

        let current_era = token
            .get_current_supported_era()
            .with_context(|| "EraEpochToken can not get current supported era")?;
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

    async fn precompute_epoch_data(&self) -> StdResult<()> {
        self.dependencies
            .epoch_service
            .write()
            .await
            .precompute_epoch_data()
            .await?;

        Ok(())
    }

    async fn inform_new_epoch(&self, epoch: Epoch) -> StdResult<()> {
        self.dependencies
            .certifier_service
            .inform_epoch(epoch)
            .await?;

        self.dependencies
            .epoch_service
            .write()
            .await
            .inform_epoch(epoch)
            .await?;

        Ok(())
    }

    async fn create_open_message(
        &self,
        signed_entity_type: &SignedEntityType,
        protocol_message: &ProtocolMessage,
    ) -> StdResult<OpenMessage> {
        self.dependencies
            .certifier_service
            .create_open_message(signed_entity_type, protocol_message)
            .await
    }
}

#[cfg(test)]
pub mod tests {
    use crate::services::FakeEpochService;
    use crate::{
        entities::OpenMessage,
        initialize_dependencies,
        runtime::{AggregatorRunner, AggregatorRunnerTrait},
        services::{MithrilStakeDistributionService, MockCertifierService},
        DependencyContainer, MithrilSignerRegisterer, SignerRegistrationRound,
    };
    use async_trait::async_trait;
    use chrono::{DateTime, Utc};
    use mithril_common::{
        chain_observer::FakeObserver,
        digesters::DumbImmutableFileObserver,
        entities::{
            Beacon, CertificatePending, ProtocolMessage, SignedEntityType, Signer,
            StakeDistribution,
        },
        signable_builder::SignableBuilderService,
        test_utils::{fake_data, MithrilFixtureBuilder},
        BeaconProviderImpl, CardanoNetwork, StdResult,
    };
    use mithril_persistence::store::StakeStorer;
    use mockall::{mock, predicate::eq};
    use std::sync::Arc;
    use tokio::sync::RwLock;

    mock! {
        SignableBuilderServiceImpl { }

        #[async_trait]
        impl SignableBuilderService for SignableBuilderServiceImpl
        {

            async fn compute_protocol_message(
                &self,
                signed_entity_type: SignedEntityType,
            ) -> StdResult<ProtocolMessage>;
        }
    }

    async fn build_runner_with_fixture_data(deps: DependencyContainer) -> AggregatorRunner {
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let current_epoch = deps
            .chain_observer
            .get_current_epoch()
            .await
            .unwrap()
            .unwrap();
        deps.init_state_from_fixture(
            &fixture,
            &[
                current_epoch.offset_to_signer_retrieval_epoch().unwrap(),
                current_epoch,
                current_epoch.next(),
            ],
        )
        .await;

        AggregatorRunner::new(Arc::new(deps))
    }

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
    async fn test_update_stake_distribution() {
        let mut deps = initialize_dependencies().await;
        let chain_observer = Arc::new(FakeObserver::default());
        deps.chain_observer = chain_observer.clone();
        deps.stake_distribution_service = Arc::new(MithrilStakeDistributionService::new(
            deps.stake_store.clone(),
            chain_observer.clone(),
        ));
        let deps = Arc::new(deps);
        let runner = AggregatorRunner::new(deps.clone());
        let beacon = runner.get_beacon_from_chain().await.unwrap();
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let expected = fixture.stake_distribution();

        chain_observer
            .set_signers(fixture.signers_with_stake())
            .await;
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
            None,
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
            None,
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
    async fn test_expire_open_message() {
        let pending_certificate = fake_data::certificate_pending();

        let open_message_expected = OpenMessage {
            signed_entity_type: pending_certificate.signed_entity_type.clone(),
            is_certified: false,
            is_expired: false,
            expires_at: Some(
                DateTime::parse_from_rfc3339("2000-01-19T13:43:05.618857482Z")
                    .unwrap()
                    .with_timezone(&Utc),
            ),
            ..OpenMessage::dummy()
        };
        let open_message_clone = open_message_expected.clone();

        let mut mock_certifier_service = MockCertifierService::new();
        mock_certifier_service
            .expect_mark_open_message_if_expired()
            .return_once(|_| Ok(Some(open_message_clone)));

        let mut deps = initialize_dependencies().await;
        deps.certifier_service = Arc::new(mock_certifier_service);

        let runner = build_runner_with_fixture_data(deps).await;
        let open_message_expired = runner
            .mark_open_message_if_expired(&open_message_expected.signed_entity_type)
            .await
            .expect("mark_open_message_if_expired should not fail");

        assert_eq!(Some(open_message_expected), open_message_expired);
    }

    #[tokio::test]
    async fn test_create_new_pending_certificate() {
        let deps = initialize_dependencies().await;
        let deps = Arc::new(deps);
        let runner = AggregatorRunner::new(deps.clone());
        let beacon = runner.get_beacon_from_chain().await.unwrap();
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
        runner.inform_new_epoch(beacon.epoch).await.unwrap();

        let mut certificate = runner
            .create_new_pending_certificate(beacon.clone(), &signed_entity_type)
            .await
            .unwrap();
        certificate.signers.sort_by_key(|s| s.party_id.clone());
        certificate.next_signers.sort_by_key(|s| s.party_id.clone());
        let mut expected = CertificatePending::new(
            beacon,
            signed_entity_type,
            protocol_parameters.clone(),
            protocol_parameters,
            Signer::vec_from(current_signers),
            Signer::vec_from(next_signers),
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
    async fn test_inform_new_epoch() {
        let mut mock_certifier_service = MockCertifierService::new();
        mock_certifier_service
            .expect_inform_epoch()
            .returning(|_| Ok(()))
            .times(1);

        let mut deps = initialize_dependencies().await;
        let current_epoch = deps
            .chain_observer
            .get_current_epoch()
            .await
            .unwrap()
            .unwrap();

        deps.certifier_service = Arc::new(mock_certifier_service);
        deps.epoch_service = Arc::new(RwLock::new(FakeEpochService::from_fixture(
            current_epoch,
            &MithrilFixtureBuilder::default().build(),
        )));

        let runner = AggregatorRunner::new(Arc::new(deps));

        runner.inform_new_epoch(current_epoch).await.unwrap();
    }

    #[tokio::test]
    async fn test_update_protocol_parameters() {
        let mut mock_certifier_service = MockCertifierService::new();
        mock_certifier_service
            .expect_inform_epoch()
            .returning(|_| Ok(()))
            .times(1);

        let mut deps = initialize_dependencies().await;
        deps.certifier_service = Arc::new(mock_certifier_service);
        let protocol_parameters_store = deps.protocol_parameters_store.clone();
        let expected_protocol_parameters = deps.config.protocol_parameters.clone();
        let current_epoch = deps.ticker_service.get_current_epoch().await.unwrap();
        let insert_epoch = current_epoch.offset_to_protocol_parameters_recording_epoch();

        let runner = build_runner_with_fixture_data(deps).await;
        runner.inform_new_epoch(current_epoch).await.unwrap();
        runner
            .update_protocol_parameters()
            .await
            .expect("update_protocol_parameters should not fail");

        let saved_protocol_parameters = protocol_parameters_store
            .get_protocol_parameters(insert_epoch)
            .await
            .unwrap()
            .unwrap_or_else(|| panic!("should have protocol parameters for epoch {insert_epoch}",));

        assert_eq!(expected_protocol_parameters, saved_protocol_parameters);
    }

    #[tokio::test]
    async fn test_precompute_epoch_data() {
        let mut deps = initialize_dependencies().await;
        let current_epoch = deps
            .chain_observer
            .get_current_epoch()
            .await
            .unwrap()
            .unwrap();

        deps.epoch_service = Arc::new(RwLock::new(FakeEpochService::from_fixture(
            current_epoch,
            &MithrilFixtureBuilder::default().build(),
        )));

        let runner = AggregatorRunner::new(Arc::new(deps));

        runner.precompute_epoch_data().await.unwrap();
    }

    #[tokio::test]
    async fn test_get_current_non_certified_open_message_should_create_new_open_message_for_mithril_stake_distribution_if_none_exists(
    ) {
        let beacon = fake_data::beacon();
        let open_message_expected = OpenMessage {
            signed_entity_type: SignedEntityType::MithrilStakeDistribution(beacon.epoch),
            is_certified: false,
            ..OpenMessage::dummy()
        };
        let open_message_clone = open_message_expected.clone();

        let mut mock_certifier_service = MockCertifierService::new();
        mock_certifier_service
            .expect_inform_epoch()
            .return_once(|_| Ok(()));
        mock_certifier_service
            .expect_get_open_message()
            .return_once(|_| Ok(None))
            .times(1);
        mock_certifier_service
            .expect_create_open_message()
            .return_once(|_, _| Ok(open_message_clone))
            .times(1);
        mock_certifier_service
            .expect_mark_open_message_if_expired()
            .return_once(|_| Ok(None))
            .times(1);

        let mut deps = initialize_dependencies().await;
        deps.certifier_service = Arc::new(mock_certifier_service);

        let runner = build_runner_with_fixture_data(deps).await;
        let current_epoch = runner
            .dependencies
            .ticker_service
            .get_current_epoch()
            .await
            .unwrap();
        runner.inform_new_epoch(current_epoch).await.unwrap();
        runner.precompute_epoch_data().await.unwrap();

        let open_message_returned = runner
            .get_current_non_certified_open_message(&beacon)
            .await
            .unwrap();
        assert_eq!(Some(open_message_expected), open_message_returned);
    }

    #[tokio::test]
    async fn test_get_current_non_certified_open_message_should_return_existing_open_message_for_mithril_stake_distribution_if_already_exists_and_not_expired(
    ) {
        let beacon = fake_data::beacon();
        let open_message_expected = OpenMessage {
            signed_entity_type: SignedEntityType::MithrilStakeDistribution(beacon.epoch),
            is_certified: false,
            is_expired: false,
            ..OpenMessage::dummy()
        };
        let open_message_clone = open_message_expected.clone();

        let mut mock_certifier_service = MockCertifierService::new();
        mock_certifier_service
            .expect_get_open_message()
            .return_once(|_| Ok(Some(open_message_clone)))
            .times(1);
        mock_certifier_service
            .expect_mark_open_message_if_expired()
            .return_once(|_| Ok(None))
            .times(1);
        mock_certifier_service.expect_create_open_message().never();

        let mut deps = initialize_dependencies().await;
        deps.certifier_service = Arc::new(mock_certifier_service);

        let runner = build_runner_with_fixture_data(deps).await;

        let open_message_returned = runner
            .get_current_non_certified_open_message(&beacon)
            .await
            .unwrap();
        assert_eq!(Some(open_message_expected), open_message_returned);
    }

    #[tokio::test]
    async fn test_get_current_non_certified_open_message_should_return_existing_open_message_for_cardano_immutables_if_already_exists_and_open_message_mithril_stake_distribution_already_certified(
    ) {
        let beacon = fake_data::beacon();
        let open_message_already_certified = OpenMessage {
            signed_entity_type: SignedEntityType::MithrilStakeDistribution(beacon.epoch),
            is_certified: true,
            is_expired: false,
            ..OpenMessage::dummy()
        };
        let open_message_expected = OpenMessage {
            signed_entity_type: SignedEntityType::CardanoImmutableFilesFull(fake_data::beacon()),
            is_certified: false,
            is_expired: false,
            ..OpenMessage::dummy()
        };
        let open_message_clone = open_message_expected.clone();

        let mut mock_certifier_service = MockCertifierService::new();
        mock_certifier_service
            .expect_get_open_message()
            .return_once(|_| Ok(Some(open_message_already_certified)))
            .times(1);
        mock_certifier_service
            .expect_get_open_message()
            .return_once(|_| Ok(Some(open_message_clone)))
            .times(1);
        mock_certifier_service
            .expect_mark_open_message_if_expired()
            .returning(|_| Ok(None))
            .times(2);
        mock_certifier_service.expect_create_open_message().never();

        let mut deps = initialize_dependencies().await;
        deps.certifier_service = Arc::new(mock_certifier_service);

        let runner = build_runner_with_fixture_data(deps).await;

        let open_message_returned = runner
            .get_current_non_certified_open_message(&beacon)
            .await
            .unwrap();
        assert_eq!(Some(open_message_expected), open_message_returned);
    }

    #[tokio::test]
    async fn test_get_current_non_certified_open_message_should_create_open_message_for_cardano_immutables_if_none_exists_and_open_message_mithril_stake_distribution_already_certified(
    ) {
        let beacon = fake_data::beacon();
        let open_message_already_certified = OpenMessage {
            signed_entity_type: SignedEntityType::MithrilStakeDistribution(beacon.epoch),
            is_certified: true,
            is_expired: false,
            ..OpenMessage::dummy()
        };
        let open_message_expected = OpenMessage {
            signed_entity_type: SignedEntityType::CardanoImmutableFilesFull(fake_data::beacon()),
            is_certified: false,
            is_expired: false,
            ..OpenMessage::dummy()
        };
        let open_message_clone = open_message_expected.clone();

        let mut mock_certifier_service = MockCertifierService::new();
        mock_certifier_service
            .expect_inform_epoch()
            .return_once(|_| Ok(()));
        mock_certifier_service
            .expect_get_open_message()
            .with(eq(open_message_already_certified
                .signed_entity_type
                .clone()))
            .return_once(|_| Ok(Some(open_message_already_certified)))
            .times(1);
        mock_certifier_service
            .expect_get_open_message()
            .return_once(|_| Ok(None))
            .times(1);
        mock_certifier_service
            .expect_create_open_message()
            .return_once(|_, _| Ok(open_message_clone))
            .times(1);
        mock_certifier_service
            .expect_mark_open_message_if_expired()
            .returning(|_| Ok(None))
            .times(2);

        let mut mock_signable_builder_service = MockSignableBuilderServiceImpl::new();
        mock_signable_builder_service
            .expect_compute_protocol_message()
            .return_once(|_| Ok(ProtocolMessage::default()));

        let mut deps = initialize_dependencies().await;
        deps.certifier_service = Arc::new(mock_certifier_service);
        deps.signable_builder_service = Arc::new(mock_signable_builder_service);

        let runner = build_runner_with_fixture_data(deps).await;
        let current_epoch = runner
            .dependencies
            .ticker_service
            .get_current_epoch()
            .await
            .unwrap();
        runner.inform_new_epoch(current_epoch).await.unwrap();
        runner.precompute_epoch_data().await.unwrap();

        let open_message_returned = runner
            .get_current_non_certified_open_message(&beacon)
            .await
            .unwrap();
        assert_eq!(Some(open_message_expected), open_message_returned);
    }

    #[tokio::test]
    async fn test_get_current_non_certified_open_message_should_return_none_if_all_open_message_already_certified(
    ) {
        let beacon = fake_data::beacon();
        let open_message_already_certified_mithril_stake_distribution = OpenMessage {
            signed_entity_type: SignedEntityType::MithrilStakeDistribution(beacon.epoch),
            is_certified: true,
            is_expired: false,
            ..OpenMessage::dummy()
        };
        let open_message_already_certified_cardano_immutable_files = OpenMessage {
            signed_entity_type: SignedEntityType::CardanoImmutableFilesFull(fake_data::beacon()),
            is_certified: true,
            is_expired: false,
            ..OpenMessage::dummy()
        };

        let mut mock_certifier_service = MockCertifierService::new();
        mock_certifier_service
            .expect_get_open_message()
            .return_once(|_| {
                Ok(Some(
                    open_message_already_certified_mithril_stake_distribution,
                ))
            })
            .times(1);
        mock_certifier_service
            .expect_get_open_message()
            .return_once(|_| Ok(Some(open_message_already_certified_cardano_immutable_files)))
            .times(1);
        mock_certifier_service
            .expect_mark_open_message_if_expired()
            .returning(|_| Ok(None))
            .times(2);
        mock_certifier_service.expect_create_open_message().never();

        let mut deps = initialize_dependencies().await;
        deps.certifier_service = Arc::new(mock_certifier_service);

        let runner = build_runner_with_fixture_data(deps).await;

        let open_message_returned = runner
            .get_current_non_certified_open_message(&beacon)
            .await
            .unwrap();
        assert!(open_message_returned.is_none());
    }

    #[tokio::test]
    async fn test_get_current_non_certified_open_message_should_return_existing_open_message_if_all_open_message_already_expired(
    ) {
        let beacon = fake_data::beacon();
        let open_message_expired_cardano_immutable_files = OpenMessage {
            signed_entity_type: SignedEntityType::CardanoImmutableFilesFull(fake_data::beacon()),
            is_certified: false,
            is_expired: true,
            ..OpenMessage::dummy()
        };
        let open_message_expected = open_message_expired_cardano_immutable_files.clone();

        let mut mock_certifier_service = MockCertifierService::new();
        mock_certifier_service
            .expect_get_open_message()
            .return_once(|_| Ok(Some(open_message_expired_cardano_immutable_files)))
            .times(1);
        mock_certifier_service
            .expect_mark_open_message_if_expired()
            .return_once(|_| Ok(None))
            .times(1);
        mock_certifier_service.expect_create_open_message().never();

        let mut deps = initialize_dependencies().await;
        deps.certifier_service = Arc::new(mock_certifier_service);

        let runner = build_runner_with_fixture_data(deps).await;

        let open_message_returned = runner
            .get_current_non_certified_open_message(&beacon)
            .await
            .unwrap();
        assert_eq!(Some(open_message_expected), open_message_returned);
    }
}

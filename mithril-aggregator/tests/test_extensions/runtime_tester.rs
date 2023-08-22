use mithril_aggregator::database::provider::SignedEntityRecord;
use mithril_aggregator::{
    dependency_injection::DependenciesBuilder, event_store::EventMessage, AggregatorRuntime,
    Configuration, DumbSnapshotUploader, DumbSnapshotter, SignerRegisterer,
    SignerRegistrationError,
};
use mithril_common::{
    chain_observer::FakeObserver,
    crypto_helper::ProtocolGenesisSigner,
    digesters::{DumbImmutableDigester, DumbImmutableFileObserver},
    entities::{
        Beacon, Certificate, CertificateSignature, Epoch, ImmutableFileNumber,
        SignedEntityTypeDiscriminants, Snapshot, StakeDistribution,
    },
    era::{adapters::EraReaderDummyAdapter, EraMarker, EraReader, SupportedEra},
    test_utils::{
        MithrilFixture, MithrilFixtureBuilder, SignerFixture, StakeDistributionGenerationMethod,
    },
};
use slog::Drain;
use slog_scope::debug;
use std::sync::Arc;
use tokio::sync::mpsc::UnboundedReceiver;

use crate::test_extensions::{AggregatorObserver, ExpectedCertificate};

#[macro_export]
macro_rules! cycle {
    ( $tester:expr, $expected_state:expr ) => {{
        $tester.cycle().await.unwrap();
        assert_eq!($expected_state, $tester.runtime.get_state());
    }};
}

#[macro_export]
macro_rules! cycle_err {
    ( $tester:expr, $expected_state:expr ) => {{
        $tester
            .cycle()
            .await
            .expect_err("cycle tick should have returned an error");
        assert_eq!($expected_state, $tester.runtime.get_state());
    }};
}

#[macro_export]
macro_rules! assert_last_certificate_eq {
    ( $tester:expr, $expected_certificate:expr ) => {{
        let last_certificate = $tester.get_last_expected_certificate().await.unwrap();
        assert_eq!($expected_certificate, last_certificate);
    }};
}

pub struct RuntimeTester {
    pub snapshot_uploader: Arc<DumbSnapshotUploader>,
    pub chain_observer: Arc<FakeObserver>,
    pub immutable_file_observer: Arc<DumbImmutableFileObserver>,
    pub digester: Arc<DumbImmutableDigester>,
    pub snapshotter: Arc<DumbSnapshotter>,
    pub genesis_signer: Arc<ProtocolGenesisSigner>,
    pub deps_builder: DependenciesBuilder,
    pub runtime: AggregatorRuntime,
    pub receiver: UnboundedReceiver<EventMessage>,
    pub era_reader_adapter: Arc<EraReaderDummyAdapter>,
    pub observer: Arc<AggregatorObserver>,
    _logs_guard: slog_scope::GlobalLoggerGuard,
}

impl RuntimeTester {
    pub async fn build(start_beacon: Beacon, configuration: Configuration) -> Self {
        let snapshot_uploader = Arc::new(DumbSnapshotUploader::new());
        let immutable_file_observer = Arc::new(DumbImmutableFileObserver::new());
        immutable_file_observer
            .shall_return(Some(start_beacon.immutable_file_number))
            .await;
        let chain_observer = Arc::new(FakeObserver::new(Some(start_beacon)));
        let digester = Arc::new(DumbImmutableDigester::default());
        let snapshotter = Arc::new(DumbSnapshotter::new());
        let genesis_signer = Arc::new(ProtocolGenesisSigner::create_deterministic_genesis_signer());
        let era_reader_adapter =
            Arc::new(EraReaderDummyAdapter::from_markers(vec![EraMarker::new(
                &SupportedEra::dummy().to_string(),
                Some(Epoch(0)),
            )]));
        let mut deps_builder = DependenciesBuilder::new(configuration);
        deps_builder.snapshot_uploader = Some(snapshot_uploader.clone());
        deps_builder.chain_observer = Some(chain_observer.clone());
        deps_builder.immutable_file_observer = Some(immutable_file_observer.clone());
        deps_builder.immutable_digester = Some(digester.clone());
        deps_builder.snapshotter = Some(snapshotter.clone());
        deps_builder.era_reader = Some(Arc::new(EraReader::new(era_reader_adapter.clone())));

        let runtime = deps_builder.create_aggregator_runner().await.unwrap();
        let decorator = slog_term::PlainDecorator::new(slog_term::TestStdoutWriter);
        let drain = slog_term::CompactFormat::new(decorator).build().fuse();
        let drain = slog_async::Async::new(drain).build().fuse();
        let log = slog_scope::set_global_logger(slog::Logger::root(Arc::new(drain), slog::o!()));
        let receiver = deps_builder.get_event_transmitter_receiver().await.unwrap();
        let observer = Arc::new(AggregatorObserver::new(&mut deps_builder).await);

        Self {
            snapshot_uploader,
            chain_observer,
            immutable_file_observer,
            digester,
            snapshotter,
            genesis_signer,
            deps_builder,
            runtime,
            receiver,
            era_reader_adapter,
            observer,
            _logs_guard: log,
        }
    }

    /// cycle the runtime once
    pub async fn cycle(&mut self) -> Result<(), String> {
        self.runtime
            .cycle()
            .await
            .map_err(|e| format!("Ticking the state machine should not fail, error: {e:?}"))?;
        Ok(())
    }

    /// Check if a message has been sent.
    pub async fn check_message(&mut self, source: &str, action: &str) -> Result<(), String> {
        let message = self
            .receiver
            .try_recv()
            .map_err(|e| format!("No message has been sent: '{e}'."))?;
        let mut error_message = String::new();

        if source != message.source {
            error_message = format!(
                "The source of the message ({}) is NOT what was expected ({source}).",
                &message.source
            );
        }
        if action != message.action {
            error_message.push_str(&format!(
                "The action of the message ({}) is NOT what was expected ({action}).",
                &message.action
            ));
        }

        if error_message.is_empty() {
            Ok(())
        } else {
            Err(error_message)
        }
    }

    /// Init the aggregator state based on the data in the given fixture
    pub async fn init_state_from_fixture(
        &mut self,
        fixture: &MithrilFixture,
    ) -> Result<(), String> {
        // Tell the chain observer to returns the signers from the fixture when returning stake distribution
        self.chain_observer
            .set_signers(fixture.signers_with_stake())
            .await;

        // Init the stores needed for a genesis certificate
        let dependency_container = self
            .deps_builder
            .build_dependency_container()
            .await
            .map_err(|e| format!("getting the dependency_container should not fail: {e:?}"))?;
        let genesis_epochs = dependency_container.get_genesis_epochs().await;
        dependency_container
            .init_state_from_fixture(fixture, &[genesis_epochs.0, genesis_epochs.1])
            .await;
        Ok(())
    }

    /// Registers the genesis certificate
    pub async fn register_genesis_certificate(
        &mut self,
        fixture: &MithrilFixture,
    ) -> Result<(), String> {
        let beacon = self.observer.current_beacon().await;
        let genesis_certificate = fixture.create_genesis_certificate(&beacon);
        debug!("genesis_certificate: {genesis_certificate:?}");
        self.deps_builder
            .get_certificate_repository()
            .await
            .unwrap()
            .create_certificate(genesis_certificate)
            .await
            .map_err(|e| format!("Saving the genesis certificate should not fail: {e:?}"))?;
        Ok(())
    }

    /// Increase the immutable file number of the beacon, returns the new number.
    pub async fn increase_immutable_number(&mut self) -> Result<ImmutableFileNumber, String> {
        let new_immutable_number = self.immutable_file_observer.increase().await.unwrap();
        self.update_digester_digest().await?;

        let updated_number = self.observer.current_beacon().await.immutable_file_number;

        if new_immutable_number == updated_number {
            Ok(new_immutable_number)
        } else {
            Err(format!(
                "beacon_provider immutable file number should've increased, expected:{new_immutable_number} / actual:{updated_number}"))
        }
    }

    /// Increase the epoch of the beacon, returns the new epoch.
    pub async fn increase_epoch(&mut self) -> Result<Epoch, String> {
        let new_epoch = self
            .chain_observer
            .next_epoch()
            .await
            .ok_or("a new epoch should have been issued")?;
        self.update_digester_digest().await?;
        self.deps_builder
            .get_certifier_service()
            .await
            .unwrap()
            .inform_epoch(new_epoch)
            .await
            .expect("inform_epoch should not fail");

        Ok(new_epoch)
    }

    /// Register the given signers in the registerer
    pub async fn register_signers(&mut self, signers: &[SignerFixture]) -> Result<(), String> {
        let registration_epoch = self
            .chain_observer
            .current_beacon
            .read()
            .await
            .as_ref()
            .unwrap()
            .epoch
            .offset_to_recording_epoch();
        let signer_registerer = self.deps_builder.get_mithril_registerer().await.unwrap();
        for signer_with_stake in signers.iter().map(|f| &f.signer_with_stake) {
            match signer_registerer
                .register_signer(registration_epoch, &signer_with_stake.to_owned().into())
                .await
            {
                Ok(_) => {}
                Err(SignerRegistrationError::ExistingSigner(_)) => {}
                Err(e) => {
                    return Err(format!("Registering a signer should not fail: {e:?}"));
                }
            }
        }

        Ok(())
    }

    /// "Send", actually register, the given single signatures in the multi-signers
    pub async fn send_single_signatures(
        &mut self,
        discriminant: SignedEntityTypeDiscriminants,
        signers: &[SignerFixture],
    ) -> Result<(), String> {
        let certifier_service = self.deps_builder.get_certifier_service().await.unwrap();
        let signed_entity_type = self
            .observer
            .get_current_signed_entity_type(discriminant)
            .await?;
        let message = certifier_service
            .get_open_message(&signed_entity_type)
            .await
            .unwrap()
            .ok_or("There should be a message to be signed.")?
            .protocol_message;

        for signer_fixture in signers {
            if let Some(single_signatures) = signer_fixture.sign(&message) {
                certifier_service
                    .register_single_signature(&signed_entity_type, &single_signatures)
                    .await
                    .map_err(|e| {
                        format!("registering a winning lottery signature should not fail: {e:?}")
                    })?;
            } else {
                panic!(
                    "Signer '{}' could not sign. \
                    This test is based on the assumption that every signer signs everytime. \
                    Possible fix: relax the protocol parameters or give more stakes to this signer.",
                    signer_fixture.signer_with_stake.party_id,
                );
            }
        }

        Ok(())
    }

    /// List the certificates and snapshots from their respective stores.
    pub async fn get_last_certificates_and_snapshots(
        &mut self,
    ) -> Result<(Vec<Certificate>, Vec<Snapshot>), String> {
        let certificates = self
            .deps_builder
            .get_certificate_repository()
            .await
            .unwrap()
            .get_latest_certificates(1000) // Arbitrary high number to get all of them in store
            .await
            .map_err(|e| format!("Querying certificate store should not fail {e:?}"))?;
        let signed_entities = self
            .deps_builder
            .get_signed_entity_service()
            .await
            .unwrap()
            .get_last_signed_snapshots(20)
            .await
            .map_err(|e| format!("Querying snapshot store should not fail {e:?}"))?;
        let snapshots = signed_entities
            .into_iter()
            .map(|record| record.artifact)
            .collect::<Vec<Snapshot>>();

        Ok((certificates, snapshots))
    }

    /// Updates the stake distribution given a vector of signers with stakes
    pub async fn update_stake_distribution(
        &mut self,
        new_stake_distribution: StakeDistribution,
    ) -> Result<MithrilFixture, String> {
        let beacon = self.observer.current_beacon().await;
        let protocol_parameters = self
            .deps_builder
            .get_protocol_parameters_store()
            .await
            .unwrap()
            .get_protocol_parameters(beacon.epoch.offset_to_recording_epoch())
            .await
            .map_err(|e| {
                format!("Querying the recording epoch protocol_parameters should not fail: {e:?}")
            })?
            .ok_or("A protocol parameters for the recording epoch should be available")?;

        let fixture = MithrilFixtureBuilder::default()
            .with_signers(new_stake_distribution.len())
            .with_protocol_parameters(protocol_parameters)
            .with_stake_distribution(StakeDistributionGenerationMethod::Custom(
                new_stake_distribution,
            ))
            .build();

        self.chain_observer
            .set_signers(fixture.signers_with_stake())
            .await;

        Ok(fixture)
    }

    /// Update the digester result using the current beacon
    pub async fn update_digester_digest(&mut self) -> Result<(), String> {
        let beacon = self.observer.current_beacon().await;

        self.digester
            .update_digest(format!(
                "n{}-e{}-i{}",
                beacon.network, beacon.epoch, beacon.immutable_file_number
            ))
            .await;

        Ok(())
    }

    /// Update the Era markers
    pub async fn set_era_markers(&self, markers: Vec<EraMarker>) {
        self.era_reader_adapter.set_markers(markers)
    }

    /// Get the last produced certificate with its signed entity if it's not a genesis certificate
    pub async fn get_last_certificate_with_signed_entity(
        &mut self,
    ) -> Result<(Certificate, Option<SignedEntityRecord>), String> {
        let certificate = self
            .deps_builder
            .get_certifier_service()
            .await
            .unwrap()
            .get_latest_certificates(1)
            .await
            .map_err(|e| format!("Querying last certificate should not fail {e:?}"))?
            .first()
            .ok_or("No certificate have been produced by the aggregator")?
            .clone();

        let signed_entity = match &certificate.signature {
            CertificateSignature::GenesisSignature(_) => None,
            CertificateSignature::MultiSignature(_) => {
                let record = self
                    .deps_builder
                    .get_signed_entity_storer()
                    .await
                    .unwrap()
                    .get_signed_entity_by_certificate_id(&certificate.hash)
                    .await
                    .unwrap()
                    .ok_or("A signed entity must exist for non genesis certificate")?;
                Some(record)
            }
        };

        Ok((certificate, signed_entity))
    }

    /// Get the last produced certificate and transform it to a [ExpectedCertificate]
    pub async fn get_last_expected_certificate(&mut self) -> Result<ExpectedCertificate, String> {
        let (certificate, signed_entity_record) =
            self.get_last_certificate_with_signed_entity().await?;

        let expected_certificate = match signed_entity_record {
            None if certificate.is_genesis() => ExpectedCertificate::new_genesis(
                certificate.beacon,
                certificate.aggregate_verification_key.try_into().unwrap(),
            ),
            None => {
                panic!("A certificate should always have a SignedEntity if it's not a genesis certificate");
            }
            Some(record) => {
                let previous_cert_identifier = self
                    .get_expected_certificate_identifier(&certificate.previous_hash)
                    .await?;

                ExpectedCertificate::new(
                    certificate.beacon,
                    &certificate.metadata.signers,
                    certificate.aggregate_verification_key.try_into().unwrap(),
                    record.signed_entity_type,
                    previous_cert_identifier,
                )
            }
        };

        Ok(expected_certificate)
    }

    /// Get the [ExpectedCertificate] identifier for the given certificate hash.
    async fn get_expected_certificate_identifier(
        &mut self,
        certificate_hash: &str,
    ) -> Result<String, String> {
        let cert_identifier = match self
            .deps_builder
            .get_signed_entity_storer()
            .await
            .unwrap()
            .get_signed_entity_by_certificate_id(certificate_hash)
            .await
            .unwrap()
        {
            Some(record) => ExpectedCertificate::identifier(&record.signed_entity_type),
            None => {
                // Certificate is a genesis certificate
                let genesis_certificate = self
                    .deps_builder
                    .get_certifier_service()
                    .await
                    .unwrap()
                    .get_certificate_by_hash(certificate_hash)
                    .await
                    .map_err(|e| format!("Querying genesis certificate should not fail {e:?}"))?
                    .ok_or(format!(
                        "A genesis certificate should exist with hash {}",
                        certificate_hash
                    ))?;
                ExpectedCertificate::genesis_identifier(&genesis_certificate.beacon)
            }
        };

        Ok(cert_identifier)
    }
}

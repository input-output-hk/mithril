use anyhow::{anyhow, Context};
use chrono::Utc;
use mithril_aggregator::{
    database::{record::SignedEntityRecord, repository::OpenMessageRepository},
    dependency_injection::DependenciesBuilder,
    event_store::EventMessage,
    AggregatorRuntime, Configuration, DependencyContainer, DumbSnapshotUploader, DumbSnapshotter,
    SignerRegistrationError,
};
use mithril_common::{
    cardano_block_scanner::{DumbBlockScanner, ScannedBlock},
    chain_observer::{ChainObserver, FakeObserver},
    crypto_helper::ProtocolGenesisSigner,
    digesters::{DumbImmutableDigester, DumbImmutableFileObserver, ImmutableFileObserver},
    entities::{
        BlockNumber, Certificate, CertificateSignature, ChainPoint, Epoch, ImmutableFileNumber,
        SignedEntityTypeDiscriminants, Snapshot, StakeDistribution, TimePoint,
    },
    era::{adapters::EraReaderDummyAdapter, EraMarker, EraReader, SupportedEra},
    test_utils::{
        MithrilFixture, MithrilFixtureBuilder, SignerFixture, StakeDistributionGenerationMethod,
    },
    StdResult,
};
use slog::Drain;
use slog_scope::debug;
use std::sync::Arc;
use std::time::Duration;
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
    pub network: String,
    pub snapshot_uploader: Arc<DumbSnapshotUploader>,
    pub chain_observer: Arc<FakeObserver>,
    pub immutable_file_observer: Arc<DumbImmutableFileObserver>,
    pub digester: Arc<DumbImmutableDigester>,
    pub snapshotter: Arc<DumbSnapshotter>,
    pub genesis_signer: Arc<ProtocolGenesisSigner>,
    pub dependencies: DependencyContainer,
    pub runtime: AggregatorRuntime,
    pub receiver: UnboundedReceiver<EventMessage>,
    pub era_reader_adapter: Arc<EraReaderDummyAdapter>,
    pub observer: Arc<AggregatorObserver>,
    pub open_message_repository: Arc<OpenMessageRepository>,
    pub block_scanner: Arc<DumbBlockScanner>,
    _logs_guard: slog_scope::GlobalLoggerGuard,
}

fn build_logger() -> slog_scope::GlobalLoggerGuard {
    let decorator = slog_term::PlainDecorator::new(slog_term::TestStdoutWriter);
    let drain = slog_term::CompactFormat::new(decorator).build().fuse();
    let drain = slog_async::Async::new(drain).build().fuse();
    slog_scope::set_global_logger(slog::Logger::root(Arc::new(drain), slog::o!()))
}

impl RuntimeTester {
    pub async fn build(start_time_point: TimePoint, configuration: Configuration) -> Self {
        let logger = build_logger();
        let network = configuration.network.clone();
        let snapshot_uploader = Arc::new(DumbSnapshotUploader::new());
        let immutable_file_observer = Arc::new(DumbImmutableFileObserver::new());
        immutable_file_observer
            .shall_return(Some(start_time_point.immutable_file_number))
            .await;
        let chain_observer = Arc::new(FakeObserver::new(Some(start_time_point)));
        let digester = Arc::new(DumbImmutableDigester::default());
        let snapshotter = Arc::new(DumbSnapshotter::new());
        let genesis_signer = Arc::new(ProtocolGenesisSigner::create_deterministic_genesis_signer());
        let era_reader_adapter =
            Arc::new(EraReaderDummyAdapter::from_markers(vec![EraMarker::new(
                &SupportedEra::dummy().to_string(),
                Some(Epoch(0)),
            )]));
        let block_scanner = Arc::new(DumbBlockScanner::new());
        let mut deps_builder = DependenciesBuilder::new(configuration);
        deps_builder.snapshot_uploader = Some(snapshot_uploader.clone());
        deps_builder.chain_observer = Some(chain_observer.clone());
        deps_builder.immutable_file_observer = Some(immutable_file_observer.clone());
        deps_builder.immutable_digester = Some(digester.clone());
        deps_builder.snapshotter = Some(snapshotter.clone());
        deps_builder.era_reader = Some(Arc::new(EraReader::new(era_reader_adapter.clone())));
        deps_builder.block_scanner = Some(block_scanner.clone());

        let dependencies = deps_builder.build_dependency_container().await.unwrap();
        let runtime = deps_builder.create_aggregator_runner().await.unwrap();
        let receiver = deps_builder.get_event_transmitter_receiver().await.unwrap();
        let observer = Arc::new(AggregatorObserver::new(&mut deps_builder).await);
        let open_message_repository = deps_builder.get_open_message_repository().await.unwrap();

        Self {
            network,
            snapshot_uploader,
            chain_observer,
            immutable_file_observer,
            digester,
            snapshotter,
            genesis_signer,
            dependencies,
            runtime,
            receiver,
            era_reader_adapter,
            observer,
            open_message_repository,
            block_scanner,
            _logs_guard: logger,
        }
    }

    /// cycle the runtime once
    pub async fn cycle(&mut self) -> StdResult<()> {
        self.runtime
            .cycle()
            .await
            .with_context(|| "Ticking the state machine should not fail")
    }

    /// Check if a message has been sent.
    pub async fn check_message(&mut self, source: &str, action: &str) -> StdResult<()> {
        let message = self
            .receiver
            .try_recv()
            .with_context(|| "No message has been sent")?;
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
            Err(anyhow!(error_message))
        }
    }

    /// Init the aggregator state based on the data in the given fixture
    pub async fn init_state_from_fixture(&mut self, fixture: &MithrilFixture) -> StdResult<()> {
        // Tell the chain observer to returns the signers from the fixture when returning stake distribution
        self.chain_observer
            .set_signers(fixture.signers_with_stake())
            .await;

        // Init the stores needed for a genesis certificate
        let genesis_epochs = self.dependencies.get_genesis_epochs().await;
        self.dependencies
            .init_state_from_fixture(fixture, &[genesis_epochs.0, genesis_epochs.1])
            .await;
        Ok(())
    }

    /// Registers the genesis certificate
    pub async fn register_genesis_certificate(
        &mut self,
        fixture: &MithrilFixture,
    ) -> StdResult<()> {
        let time_point = self.observer.current_time_point().await;
        let genesis_certificate = fixture.create_genesis_certificate(
            &self.network,
            time_point.epoch,
            time_point.immutable_file_number,
        );
        debug!("genesis_certificate: {:?}", genesis_certificate);
        self.dependencies
            .certificate_repository
            .create_certificate(genesis_certificate)
            .await
            .with_context(|| {
                format!(
                    "Runtime Tester can not create certificate with fixture: '{:?}'",
                    fixture
                )
            })?;

        Ok(())
    }

    /// Increase the immutable file number of the simulated db, returns the new number.
    pub async fn increase_immutable_number(&mut self) -> StdResult<ImmutableFileNumber> {
        let new_immutable_number = self.immutable_file_observer.increase().await.unwrap();
        self.update_digester_digest().await;

        let updated_number = self
            .observer
            .current_time_point()
            .await
            .immutable_file_number;

        if new_immutable_number == updated_number {
            Ok(new_immutable_number)
        } else {
            Err(anyhow!(
                "immutable file number should've increased, expected:{new_immutable_number} / actual:{updated_number}"))
        }
    }

    /// Increase the epoch of the beacon, returns the new epoch.
    pub async fn increase_epoch(&mut self) -> StdResult<Epoch> {
        let new_epoch = self
            .chain_observer
            .next_epoch()
            .await
            .ok_or(anyhow!("a new epoch should have been issued"))?;
        self.update_digester_digest().await;
        self.dependencies
            .certifier_service
            .inform_epoch(new_epoch)
            .await
            .with_context(|| "inform_epoch should not fail")?;

        Ok(new_epoch)
    }

    /// increase the block number in the fake observer
    pub async fn increase_block_number(&mut self, increment: u64, expected: u64) -> StdResult<()> {
        let new_block_number = self
            .chain_observer
            .increase_block_number(increment)
            .await
            .ok_or_else(|| anyhow!("no block number returned".to_string()))?;

        anyhow::ensure!(
            expected == new_block_number,
            "expected to increase block number up to {expected}, got {new_block_number}",
        );

        // Make the block scanner return new blocks
        let current_immutable = self
            .immutable_file_observer
            .get_last_immutable_number()
            .await?;
        let blocks_to_scan: Vec<ScannedBlock> = ((expected - increment + 1)..=expected)
            .map(|block_number| {
                let block_hash = format!("block_hash-{block_number}");
                let slot_number = 10 * block_number;
                ScannedBlock::new(
                    block_hash,
                    block_number,
                    slot_number,
                    current_immutable,
                    vec![format!("tx_hash-{block_number}-1")],
                )
            })
            .collect();
        self.block_scanner.add_forwards(vec![blocks_to_scan]);

        Ok(())
    }

    pub async fn cardano_chain_send_rollback(
        &mut self,
        rollback_to_block_number: BlockNumber,
    ) -> StdResult<()> {
        let actual_block_number = self
            .chain_observer
            .get_current_chain_point()
            .await?
            .map(|c| c.block_number)
            .ok_or_else(|| anyhow!("no block number returned".to_string()))?;
        let decrement = actual_block_number - rollback_to_block_number;
        let new_block_number = self
            .chain_observer
            .decrease_block_number(decrement)
            .await
            .ok_or_else(|| anyhow!("no block number returned".to_string()))?;

        anyhow::ensure!(
            rollback_to_block_number == new_block_number,
            "expected to increase block number up to {rollback_to_block_number}, got {new_block_number}",
        );

        let chain_point = ChainPoint {
            slot_number: 1,
            block_number: rollback_to_block_number,
            block_hash: format!("block_hash-{rollback_to_block_number}"),
        };
        self.block_scanner.add_backward(chain_point);

        Ok(())
    }

    /// Register the given signers in the registerer
    pub async fn register_signers(&mut self, signers: &[SignerFixture]) -> StdResult<()> {
        let registration_epoch = self
            .chain_observer
            .current_time_point
            .read()
            .await
            .as_ref()
            .unwrap()
            .epoch
            .offset_to_recording_epoch();
        for signer_with_stake in signers.iter().map(|f| &f.signer_with_stake) {
            match self
                .dependencies
                .signer_registerer
                .register_signer(registration_epoch, &signer_with_stake.to_owned().into())
                .await
            {
                Ok(_) | Err(SignerRegistrationError::ExistingSigner(_)) => {}
                error => {
                    error.with_context(|| "Registering a signer should not fail")?;
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
    ) -> StdResult<()> {
        let certifier_service = self.dependencies.certifier_service.clone();
        let signed_entity_type = self
            .observer
            .get_current_signed_entity_type(discriminant)
            .await?;
        let message = certifier_service
            .get_open_message(&signed_entity_type)
            .await
            .with_context(|| {
                format!("An open message should exist for signed_entity_type: {signed_entity_type}")
            })?
            .ok_or(anyhow!("There should be a message to be signed."))?
            .protocol_message;

        for signer_fixture in signers {
            if let Some(single_signatures) = signer_fixture.sign(&message) {
                certifier_service
                    .register_single_signature(&signed_entity_type, &single_signatures)
                    .await
                    .with_context(|| "registering a winning lottery signature should not fail")?;
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
    ) -> StdResult<(Vec<Certificate>, Vec<Snapshot>)> {
        let certificates = self
            .dependencies
            .certificate_repository
            .get_latest_certificates(1000) // Arbitrary high number to get all of them in store
            .await
            .with_context(|| "Querying certificate store should not fail")?;
        let signed_entities = self
            .dependencies
            .signed_entity_service
            .get_last_signed_snapshots(20)
            .await
            .with_context(|| "Querying snapshot store should not fail")?;
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
    ) -> StdResult<MithrilFixture> {
        let epoch = self.observer.current_time_point().await.epoch;
        let protocol_parameters = self
            .dependencies
            .protocol_parameters_store
            .get_protocol_parameters(epoch.offset_to_recording_epoch())
            .await
            .with_context(|| "Querying the recording epoch protocol_parameters should not fail")?
            .ok_or(anyhow!(
                "A protocol parameters for the recording epoch should be available"
            ))?;

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
    pub async fn update_digester_digest(&mut self) {
        let time_point = self.observer.current_time_point().await;

        self.digester
            .update_digest(format!(
                "n{}-e{}-i{}",
                self.network, time_point.epoch, time_point.immutable_file_number
            ))
            .await;
    }

    /// Activate open message expiration
    pub async fn activate_open_message_expiration(
        &self,
        discriminant: SignedEntityTypeDiscriminants,
        timeout: Duration,
    ) -> StdResult<()> {
        let signed_entity_type = self
            .observer
            .get_current_signed_entity_type(discriminant)
            .await?;
        let mut open_message = self
            .open_message_repository
            .get_open_message(&signed_entity_type)
            .await
            .with_context(|| "Querying open message should not fail")?
            .ok_or(anyhow!("An open message should exist"))?;
        open_message.expires_at = Some(Utc::now() + timeout);
        self.open_message_repository
            .update_open_message(&open_message)
            .await
            .with_context(|| "Saving open message should not fail")?;

        Ok(())
    }

    /// Update the Era markers
    pub async fn set_era_markers(&self, markers: Vec<EraMarker>) {
        self.era_reader_adapter.set_markers(markers)
    }

    /// Get the last produced certificate with its signed entity if it's not a genesis certificate
    pub async fn get_last_certificate_with_signed_entity(
        &mut self,
    ) -> StdResult<(Certificate, Option<SignedEntityRecord>)> {
        let certificate = self
            .dependencies
            .certifier_service
            .get_latest_certificates(1)
            .await
            .with_context(|| "Querying last certificate should not fail")?
            .first()
            .ok_or(anyhow!(
                "No certificate have been produced by the aggregator"
            ))?
            .clone();

        let signed_entity = match &certificate.signature {
            CertificateSignature::GenesisSignature(..) => None,
            CertificateSignature::MultiSignature(..) => {
                let record = self
                    .dependencies
                    .signed_entity_storer
                    .get_signed_entity_by_certificate_id(&certificate.hash)
                    .await
                    .with_context(|| "Querying certificate should not fail")?
                    .ok_or(anyhow!(
                        "A signed entity must exist for non genesis certificate"
                    ))?;
                Some(record)
            }
        };

        Ok((certificate, signed_entity))
    }

    /// Get the last produced certificate and transform it to a [ExpectedCertificate]
    pub async fn get_last_expected_certificate(&mut self) -> StdResult<ExpectedCertificate> {
        let (certificate, signed_entity_record) =
            self.get_last_certificate_with_signed_entity().await?;

        let expected_certificate = match signed_entity_record {
            None if certificate.is_genesis() => ExpectedCertificate::new_genesis(
                certificate.as_cardano_db_beacon(),
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
                    certificate.as_cardano_db_beacon(),
                    certificate.metadata.signers.as_slice(),
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
    ) -> StdResult<String> {
        let cert_identifier = match self
            .dependencies
            .signed_entity_storer
            .get_signed_entity_by_certificate_id(certificate_hash)
            .await
            .with_context(|| "Querying signed entity should not fail")?
        {
            Some(record) => ExpectedCertificate::identifier(&record.signed_entity_type),
            None => {
                // Certificate is a genesis certificate
                let genesis_certificate = self
                    .dependencies
                    .certifier_service
                    .get_certificate_by_hash(certificate_hash)
                    .await
                    .with_context(|| "Querying genesis certificate should not fail")?
                    .ok_or(anyhow!(
                        "A genesis certificate should exist with hash {}",
                        certificate_hash
                    ))?;
                ExpectedCertificate::genesis_identifier(&genesis_certificate.as_cardano_db_beacon())
            }
        };

        Ok(cert_identifier)
    }
}

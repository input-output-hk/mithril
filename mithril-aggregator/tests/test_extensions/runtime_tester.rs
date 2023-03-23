use crate::test_extensions::initialize_dependencies;
use mithril_aggregator::event_store::EventMessage;
use mithril_common::certificate_chain::CertificateGenesisProducer;
use mithril_common::era::adapters::EraReaderDummyAdapter;
use mithril_common::era::{EraMarker, SupportedEra};
use mithril_common::test_utils::{
    MithrilFixtureBuilder, SignerFixture, StakeDistributionGenerationMethod,
};
use slog::Drain;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::mpsc::UnboundedReceiver;

use mithril_aggregator::{
    AggregatorRunner, AggregatorRuntime, DependencyManager, DumbSnapshotUploader, DumbSnapshotter,
    ProtocolParametersStorer,
};
use mithril_common::crypto_helper::{key_encode_hex, ProtocolClerk, ProtocolGenesisSigner};
use mithril_common::digesters::DumbImmutableFileObserver;
use mithril_common::entities::{
    Certificate, Epoch, ImmutableFileNumber, ProtocolParameters, SignerWithStake, SingleSignatures,
    Snapshot, StakeDistribution,
};
use mithril_common::{chain_observer::FakeObserver, digesters::DumbImmutableDigester};

#[macro_export]
macro_rules! cycle {
    ( $tester:expr, $expected_state:expr ) => {{
        $tester.cycle().await.unwrap();
        assert_eq!($expected_state, $tester.runtime.get_state());
    }};
}

pub struct RuntimeTester {
    pub snapshot_uploader: Arc<DumbSnapshotUploader>,
    pub chain_observer: Arc<FakeObserver>,
    pub immutable_file_observer: Arc<DumbImmutableFileObserver>,
    pub digester: Arc<DumbImmutableDigester>,
    pub snapshotter: Arc<DumbSnapshotter>,
    pub genesis_signer: Arc<ProtocolGenesisSigner>,
    pub deps: Arc<DependencyManager>,
    pub runtime: AggregatorRuntime,
    pub receiver: UnboundedReceiver<EventMessage>,
    pub era_reader_adapter: Arc<EraReaderDummyAdapter>,
    _logs_guard: slog_scope::GlobalLoggerGuard,
}

impl RuntimeTester {
    pub async fn build(default_protocol_parameters: ProtocolParameters) -> Self {
        let snapshot_uploader = Arc::new(DumbSnapshotUploader::new());
        let chain_observer = Arc::new(FakeObserver::default());
        let immutable_file_observer = Arc::new(DumbImmutableFileObserver::default());
        let digester = Arc::new(DumbImmutableDigester::default());
        let snapshotter = Arc::new(DumbSnapshotter::new());
        let genesis_signer = Arc::new(ProtocolGenesisSigner::create_deterministic_genesis_signer());
        let era_reader_adapter =
            Arc::new(EraReaderDummyAdapter::from_markers(vec![EraMarker::new(
                &SupportedEra::dummy().to_string(),
                Some(Epoch(0)),
            )]));
        let (deps, config, receiver) = initialize_dependencies(
            default_protocol_parameters,
            snapshot_uploader.clone(),
            chain_observer.clone(),
            immutable_file_observer.clone(),
            digester.clone(),
            snapshotter.clone(),
            genesis_signer.clone(),
            era_reader_adapter.clone(),
        )
        .await;
        let runner = Arc::new(AggregatorRunner::new(config.clone(), deps.clone()));
        let runtime =
            AggregatorRuntime::new(Duration::from_millis(config.interval), None, runner.clone())
                .await
                .expect("Instantiating the Runtime should not fail.");

        let decorator = slog_term::PlainDecorator::new(slog_term::TestStdoutWriter);
        let drain = slog_term::CompactFormat::new(decorator).build().fuse();
        let drain = slog_async::Async::new(drain).build().fuse();
        let log = slog_scope::set_global_logger(slog::Logger::root(Arc::new(drain), slog::o!()));

        Self {
            snapshot_uploader,
            chain_observer,
            immutable_file_observer,
            digester,
            snapshotter,
            genesis_signer,
            deps,
            runtime,
            receiver,
            era_reader_adapter,
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

    /// Registers the genesis certificate
    pub async fn register_genesis_certificate(
        &self,
        signers: &[SignerFixture],
    ) -> Result<(), String> {
        let beacon = self
            .deps
            .beacon_provider
            .get_current_beacon()
            .await
            .map_err(|e| format!("Querying the current beacon should not fail: {e:?}"))?;
        let protocol_parameters = self
            .deps
            .protocol_parameters_store
            .get_protocol_parameters(beacon.epoch)
            .await
            .map_err(|e| {
                format!("Querying the recording epoch protocol_parameters should not fail: {e:?}")
            })?
            .ok_or("A protocol parameters for the epoch should be available")?;
        let first_signer = &&signers
            .first()
            .ok_or_else(|| "Signers list should not be empty".to_string())?
            .protocol_signer;
        let clerk = ProtocolClerk::from_signer(first_signer);
        let genesis_avk = clerk.compute_avk();
        let genesis_producer = CertificateGenesisProducer::new(Some(self.genesis_signer.clone()));
        let genesis_protocol_message = CertificateGenesisProducer::create_genesis_protocol_message(
            &genesis_avk,
        )
        .map_err(|e| format!("Creating the genesis protocol message should not fail: {e:?}"))?;
        let genesis_signature = genesis_producer
            .sign_genesis_protocol_message(genesis_protocol_message)
            .map_err(|e| format!("Signing the genesis protocol message should not fail: {e:?}"))?;
        let genesis_certificate = CertificateGenesisProducer::create_genesis_certificate(
            protocol_parameters,
            beacon,
            genesis_avk,
            genesis_signature,
        )
        .map_err(|e| format!("Creating the genesis certificate should not fail: {e:?}"))?;
        self.deps
            .certificate_store
            .save(genesis_certificate)
            .await
            .map_err(|e| format!("Saving the genesis certificate should not fail: {e:?}"))?;
        Ok(())
    }

    /// Increase the immutable file number of the beacon, returns the new number.
    pub async fn increase_immutable_number(&self) -> Result<ImmutableFileNumber, String> {
        let new_immutable_number = self.immutable_file_observer.increase().await.unwrap();
        self.update_digester_digest().await?;

        let updated_number = self
            .deps
            .beacon_provider
            .get_current_beacon()
            .await
            .map_err(|e| format!("Querying the current beacon should not fail: {e:?}"))?
            .immutable_file_number;

        if new_immutable_number == updated_number {
            Ok(new_immutable_number)
        } else {
            Err(format!(
                "beacon_provider immutable file number should've increased, expected:{new_immutable_number} / actual:{updated_number}"))
        }
    }

    /// Increase the epoch of the beacon, returns the new epoch.
    pub async fn increase_epoch(&self) -> Result<Epoch, String> {
        let new_epoch = self
            .chain_observer
            .next_epoch()
            .await
            .ok_or("a new epoch should have been issued")?;
        self.update_digester_digest().await?;

        Ok(new_epoch)
    }

    /// Register the given signers in the registerer
    pub async fn register_signers(&self, signers: &[SignerFixture]) -> Result<(), String> {
        for signer_with_stake in signers.iter().map(|f| &f.signer_with_stake) {
            self.deps
                .signer_registerer
                .register_signer(&signer_with_stake.to_owned().into())
                .await
                .map_err(|e| format!("Registering a signer should not fail: {e:?}"))?;
        }

        Ok(())
    }

    /// "Send", actually register, the given single signatures in the multi-signers
    pub async fn send_single_signatures(&self, signers: &[SignerFixture]) -> Result<(), String> {
        let mut multisigner = self.deps.multi_signer.write().await;
        let message = multisigner
            .get_current_message()
            .await
            .ok_or("There should be a message to be signed.")?;

        for signer_fixture in signers {
            if let Some(signature) = signer_fixture
                .protocol_signer
                .sign(message.compute_hash().as_bytes())
            {
                let single_signatures = SingleSignatures::new(
                    signer_fixture.signer_with_stake.party_id.to_owned(),
                    key_encode_hex(&signature).expect("hex encoding should not fail"),
                    signature.indexes,
                );

                multisigner
                    .register_single_signature(&single_signatures)
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
        &self,
    ) -> Result<(Vec<Certificate>, Vec<Snapshot>), String> {
        let certificates = self
            .deps
            .certificate_store
            .get_list(1000) // Arbitrary high number to get all of them in store
            .await
            .map_err(|e| format!("Querying certificate store should not fail {e:?}"))?;
        let snapshots = self
            .deps
            .snapshot_store
            .list_snapshots()
            .await
            .map_err(|e| format!("Querying snapshot store should not fail {e:?}"))?;

        Ok((certificates, snapshots))
    }

    /// Updates the stake distribution given a vector of signers with stakes
    pub async fn update_stake_distribution(
        &self,
        signers_with_stake: Vec<SignerWithStake>,
    ) -> Result<Vec<SignerFixture>, String> {
        self.chain_observer
            .set_signers(signers_with_stake.clone())
            .await;
        let beacon = self
            .deps
            .beacon_provider
            .get_current_beacon()
            .await
            .map_err(|e| format!("Querying the current beacon should not fail: {e:?}"))?;
        let protocol_parameters = self
            .deps
            .protocol_parameters_store
            .get_protocol_parameters(beacon.epoch.offset_to_recording_epoch())
            .await
            .map_err(|e| {
                format!("Querying the recording epoch protocol_parameters should not fail: {e:?}")
            })?
            .ok_or("A protocol parameters for the recording epoch should be available")?;

        let fixture = MithrilFixtureBuilder::default()
            .with_signers(signers_with_stake.len())
            .with_protocol_parameters(protocol_parameters)
            .with_stake_distribution(StakeDistributionGenerationMethod::Custom(
                StakeDistribution::from_iter(
                    signers_with_stake
                        .into_iter()
                        .map(|s| (s.party_id, s.stake)),
                ),
            ))
            .build();

        Ok(fixture.signers_fixture())
    }

    // Update the digester result using the current beacon
    pub async fn update_digester_digest(&self) -> Result<(), String> {
        let beacon = self
            .deps
            .beacon_provider
            .get_current_beacon()
            .await
            .map_err(|e| format!("Querying the current beacon should not fail: {e:?}"))?;

        self.digester
            .update_digest(format!(
                "n{}-e{}-i{}",
                beacon.network, beacon.epoch, beacon.immutable_file_number
            ))
            .await;

        Ok(())
    }

    // update the Era markers
    pub async fn set_era_markers(&self, markers: Vec<EraMarker>) {
        self.era_reader_adapter.set_markers(markers)
    }
}

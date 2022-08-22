use crate::test_extensions::{initialize_dependencies, TestSigner, TickCounter};
use slog::Drain;
use std::sync::Arc;
use std::time::Duration;

use mithril_aggregator::{
    AggregatorRunner, AggregatorRuntime, DependencyManager, DumbSnapshotUploader, DumbSnapshotter,
};
use mithril_common::crypto_helper::key_encode_hex;
use mithril_common::crypto_helper::tests_setup::setup_signers_from_parties;
use mithril_common::digesters::DumbImmutableFileObserver;
use mithril_common::entities::{
    Certificate, Epoch, ImmutableFileNumber, SignerWithStake, SingleSignatures, Snapshot,
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
    pub deps: Arc<DependencyManager>,
    pub runtime: AggregatorRuntime,
    tick_counter: TickCounter,
    _logs_guard: slog_scope::GlobalLoggerGuard,
}

impl RuntimeTester {
    pub async fn build() -> Self {
        let snapshot_uploader = Arc::new(DumbSnapshotUploader::new());
        let chain_observer = Arc::new(FakeObserver::default());
        let immutable_file_observer = Arc::new(DumbImmutableFileObserver::default());
        let digester = Arc::new(DumbImmutableDigester::default());
        let snapshotter = Arc::new(DumbSnapshotter::new());
        let (deps, config) = initialize_dependencies(
            snapshot_uploader.clone(),
            chain_observer.clone(),
            immutable_file_observer.clone(),
            digester.clone(),
            snapshotter.clone(),
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
            deps,
            runtime,
            tick_counter: TickCounter::new(),
            _logs_guard: log,
        }
    }

    /// cycle the runtime once
    pub async fn cycle(&mut self) -> Result<(), String> {
        self.tick_counter.increase();
        self.runtime
            .cycle()
            .await
            .map_err(|e| format!("{}, error: {:?}", self.tick_counter.get_message(), e))?;
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
            .map_err(|e| format!("Querying the current beacon should not fail: {:?}", e))?
            .immutable_file_number;

        if new_immutable_number == updated_number {
            Ok(new_immutable_number)
        } else {
            Err(format!(
                "beacon_provider immutable file number should've increased, expected:{} / actual:{}",
                new_immutable_number,
                updated_number))
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

    /// Register the given signers in the multi-signers
    pub async fn register_signers(&self, signers: &[TestSigner]) -> Result<(), String> {
        let mut multisigner = self.deps.multi_signer.write().await;

        for (party_id, _stakes, verification_key, _signer, _initializer) in signers {
            multisigner
                .register_signer(party_id.to_owned(), verification_key)
                .await
                .map_err(|e| format!("Registering a signer should not fail: {:?}", e))?;
        }

        Ok(())
    }

    /// "Send", actually register, the given single signatures in the multi-signers
    pub async fn send_single_signatures(&self, signers: &[TestSigner]) -> Result<(), String> {
        let mut multisigner = self.deps.multi_signer.write().await;
        let message = multisigner
            .get_current_message()
            .await
            .ok_or("There should be a message to be signed.")?;

        for (party_id, _stakes, _verification_key, protocol_signer, _initializer) in signers {
            if let Some(signature) = protocol_signer.sign(message.compute_hash().as_bytes()) {
                let single_signatures = SingleSignatures::new(
                    party_id.to_string(),
                    key_encode_hex(&signature).expect("hex encoding should not fail"),
                    signature.indexes,
                );

                multisigner
                    .register_single_signature(&single_signatures)
                    .await
                    .map_err(|e| {
                        format!(
                            "registering a winning lottery signature should not fail: {:?}",
                            e
                        )
                    })?;
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
            .map_err(|e| format!("Querying certificate store should not fail {:?}", e))?;
        let snapshots = self
            .deps
            .snapshot_store
            .list_snapshots()
            .await
            .map_err(|e| format!("Querying snapshot store should not fail {:?}", e))?;

        Ok((certificates, snapshots))
    }

    pub async fn update_stake_distribution(
        &self,
        signers_with_stake: Vec<SignerWithStake>,
    ) -> Vec<TestSigner> {
        self.chain_observer
            .set_signers(signers_with_stake.clone())
            .await;

        setup_signers_from_parties(
            &signers_with_stake
                .clone()
                .into_iter()
                .map(|s| (s.party_id, s.stake))
                .collect::<Vec<_>>(),
        )
    }

    // Update the digester result using the current beacon
    pub async fn update_digester_digest(&self) -> Result<(), String> {
        let beacon = self
            .deps
            .beacon_provider
            .get_current_beacon()
            .await
            .map_err(|e| format!("Querying the current beacon should not fail: {:?}", e))?;

        self.digester
            .update_digest(format!(
                "n{}-e{}-i{}",
                beacon.network, beacon.epoch, beacon.immutable_file_number
            ))
            .await;

        Ok(())
    }
}

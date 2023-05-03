#![allow(dead_code)]
use mithril_common::api_version::APIVersionProvider;
use mithril_common::digesters::ImmutableFileObserver;
use mithril_common::entities::SignerWithStake;
use mithril_common::era::{
    adapters::{EraReaderAdapterType, EraReaderDummyAdapter},
    EraChecker, EraReader,
};
use mithril_common::era::{EraMarker, SupportedEra};
use mithril_common::signable_builder::{
    CardanoImmutableFilesFullSignableBuilder, MithrilStakeDistributionSignableBuilder,
};
use mithril_common::BeaconProvider;
use slog::Drain;
use slog_scope::debug;
use std::error::Error as StdError;
use std::fmt::Debug;
use std::{path::PathBuf, sync::Arc, time::Duration};
use thiserror::Error;

use mithril_common::crypto_helper::tests_setup;
use mithril_common::{
    chain_observer::FakeObserver,
    digesters::{DumbImmutableDigester, DumbImmutableFileObserver},
    entities::{Beacon, Epoch},
    store::{adapter::MemoryAdapter, StakeStore, StakeStorer},
    BeaconProviderImpl,
};
use mithril_signer::{
    CertificateHandler, Configuration, MithrilSingleSigner, ProtocolInitializerStore,
    ProtocolInitializerStorer, RuntimeError, SignableBuilderService, SignerRunner, SignerServices,
    SignerState, StateMachine,
};

use super::FakeAggregator;

type Result<T> = std::result::Result<T, TestError>;

#[derive(Debug, Error)]
pub enum TestError {
    #[error("Assertion failed: {0}")]
    AssertFailed(String),
    #[error("Subsystem error: {0:?}")]
    SubsystemError(#[from] Box<dyn StdError + Sync + Send>),
    #[error("Value error: {0}")]
    ValueError(String),
}

impl From<RuntimeError> for TestError {
    fn from(value: RuntimeError) -> Self {
        Self::SubsystemError(value.into())
    }
}

pub struct StateMachineTester {
    state_machine: StateMachine,
    immutable_observer: Arc<DumbImmutableFileObserver>,
    chain_observer: Arc<FakeObserver>,
    certificate_handler: Arc<FakeAggregator>,
    protocol_initializer_store: Arc<ProtocolInitializerStore>,
    stake_store: Arc<StakeStore>,
    era_checker: Arc<EraChecker>,
    era_reader_adapter: Arc<EraReaderDummyAdapter>,
    comment_no: u32,
    _logs_guard: slog_scope::GlobalLoggerGuard,
}

impl Debug for StateMachineTester {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        debug!("Debug called after comment N°{}.", self.comment_no);
        write!(f, "DEBUG")
    }
}

impl StateMachineTester {
    pub async fn init(signers_with_stake: &[SignerWithStake]) -> Result<Self> {
        let selected_signer_with_stake = signers_with_stake.first().ok_or_else(|| {
            TestError::AssertFailed("there should be at least one signer with stakes".to_string())
        })?;
        let selected_signer_party_id = selected_signer_with_stake.party_id.clone();
        let selected_signer_temp_dir =
            tests_setup::setup_temp_directory_for_signer(&selected_signer_party_id, false);
        let config = Configuration {
            aggregator_endpoint: "http://0.0.0.0:8000".to_string(),
            cardano_cli_path: PathBuf::new(),
            cardano_node_socket_path: PathBuf::new(),
            db_directory: PathBuf::new(),
            network: "devnet".to_string(),
            network_magic: Some(42),
            party_id: Some(selected_signer_party_id),
            run_interval: 5000,
            data_stores_directory: PathBuf::new(),
            store_retention_limit: None,
            kes_secret_key_path: selected_signer_temp_dir
                .as_ref()
                .map(|dir| dir.join("kes.sk")),
            operational_certificate_path: selected_signer_temp_dir
                .as_ref()
                .map(|dir| dir.join("opcert.cert")),
            disable_digests_cache: false,
            reset_digests_cache: false,
            era_reader_adapter_type: EraReaderAdapterType::Bootstrap,
            era_reader_adapter_params: None,
        };

        let decorator = slog_term::PlainDecorator::new(slog_term::TestStdoutWriter);
        let drain = slog_term::CompactFormat::new(decorator).build().fuse();
        let drain = slog_async::Async::new(drain).build().fuse();
        let logs_guard =
            slog_scope::set_global_logger(slog::Logger::root(Arc::new(drain), slog::o!()));

        let immutable_observer = Arc::new(DumbImmutableFileObserver::new());
        immutable_observer.shall_return(Some(1)).await;
        let chain_observer = Arc::new(FakeObserver::new(Some(Beacon {
            epoch: Epoch(1),
            immutable_file_number: 1,
            network: "devnet".to_string(),
        })));
        let beacon_provider = Arc::new(BeaconProviderImpl::new(
            chain_observer.clone(),
            immutable_observer.clone(),
            config.get_network().unwrap(),
        ));
        let certificate_handler = Arc::new(FakeAggregator::new(beacon_provider.clone()));
        let digester = Arc::new(DumbImmutableDigester::new("DIGEST", true));
        let protocol_initializer_store = Arc::new(ProtocolInitializerStore::new(
            Box::new(MemoryAdapter::new(None).unwrap()),
            config.store_retention_limit,
        ));
        let single_signer = Arc::new(MithrilSingleSigner::new(
            config.party_id.to_owned().unwrap_or_default(),
        ));
        let stake_store = Arc::new(StakeStore::new(
            Box::new(MemoryAdapter::new(None).unwrap()),
            config.store_retention_limit,
        ));
        let era_reader_adapter = Arc::new(EraReaderDummyAdapter::from_markers(vec![
            (EraMarker {
                name: SupportedEra::dummy().to_string(),
                epoch: Some(Epoch(0)),
            }),
        ]));
        let era_reader = Arc::new(EraReader::new(era_reader_adapter.clone()));
        let era_epoch_token = era_reader
            .read_era_epoch_token(beacon_provider.get_current_beacon().await.unwrap().epoch)
            .await
            .unwrap();
        let era_checker = Arc::new(EraChecker::new(
            era_epoch_token.get_current_supported_era().unwrap(),
            era_epoch_token.get_current_epoch(),
        ));

        let api_version_provider = Arc::new(APIVersionProvider::new(era_checker.clone()));

        let signable_builder =
            CardanoImmutableFilesFullSignableBuilder::new(digester.clone(), slog_scope::logger());
        let mithril_stake_distribution_signable_builder =
            MithrilStakeDistributionSignableBuilder::default();
        let signable_builder_service = Arc::new(SignableBuilderService::new(
            signable_builder,
            mithril_stake_distribution_signable_builder,
        ));

        let services = SignerServices {
            certificate_handler: certificate_handler.clone(),
            beacon_provider: beacon_provider.clone(),
            chain_observer: chain_observer.clone(),
            digester: digester.clone(),
            protocol_initializer_store: protocol_initializer_store.clone(),
            single_signer: single_signer.clone(),
            stake_store: stake_store.clone(),
            era_checker: era_checker.clone(),
            era_reader,
            api_version_provider,
            signable_builder_service,
        };
        // set up stake distribution
        chain_observer
            .set_signers(signers_with_stake.to_owned())
            .await;

        let runner = Box::new(SignerRunner::new(config, services));

        let state_machine = StateMachine::new(SignerState::Init, runner, Duration::from_secs(5));

        Ok(StateMachineTester {
            state_machine,
            immutable_observer,
            chain_observer,
            certificate_handler,
            protocol_initializer_store,
            stake_store,
            era_checker,
            era_reader_adapter,
            comment_no: 0,
            _logs_guard: logs_guard,
        })
    }

    fn assert(&mut self, condition: bool, description: String) -> Result<&mut Self> {
        if !condition {
            Err(TestError::AssertFailed(description))
        } else {
            Ok(self)
        }
    }
    /// trigger a cycle in the state machine
    async fn cycle(&mut self) -> Result<&mut Self> {
        self.state_machine.cycle().await?;

        Ok(self)
    }

    /// Is the state machine in `Init` state?
    pub fn is_init(&mut self) -> Result<&mut Self> {
        self.assert(
            self.state_machine.get_state().is_init(),
            "state machine shall be in Init state".to_string(),
        )
    }

    /// cycle the state machine and test the resulting state
    pub async fn cycle_registered(&mut self) -> Result<&mut Self> {
        self.cycle().await?;
        self.assert(
            self.state_machine.get_state().is_registered(),
            format!(
                "state machine is in {} state (Registered was expected)",
                self.state_machine.get_state()
            ),
        )
    }

    /// cycle the state machine and test the resulting state
    pub async fn cycle_signed(&mut self) -> Result<&mut Self> {
        self.cycle().await?;

        self.assert(
            self.state_machine.get_state().is_signed(),
            format!(
                "state machine is in {} state (Signed was expected)",
                self.state_machine.get_state()
            ),
        )
    }

    /// cycle the state machine and test the resulting state
    pub async fn cycle_unregistered(&mut self) -> Result<&mut Self> {
        self.cycle().await?;

        self.assert(
            self.state_machine.get_state().is_unregistered(),
            format!(
                "state machine is in {} state (Unregistered was expected)",
                self.state_machine.get_state()
            ),
        )
    }

    /// make the aggregator send the epoch settings from now on
    pub async fn aggregator_send_epoch_settings(&mut self) -> &mut Self {
        self.certificate_handler.release_epoch_settings().await;
        self
    }

    /// check there is a protocol initializer for the given Epoch
    pub async fn check_protocol_initializer(&mut self, epoch: Epoch) -> Result<&mut Self> {
        let maybe_protocol_initializer = self
            .protocol_initializer_store
            .get_protocol_initializer(epoch)
            .await
            .map_err(|e| TestError::SubsystemError(e.into()))?;

        self.assert(maybe_protocol_initializer.is_some(), format!(
                "there should be a protocol intializer in store for Epoch {}, here is the last 3 in store: {:?}",
                epoch,
                self.protocol_initializer_store
                    .get_last_protocol_initializer(2)
                    .await
                    .unwrap(),
            ))
    }

    /// check there is a stake for the given Epoch
    pub async fn check_stake_store(&mut self, epoch: Epoch) -> Result<&mut Self> {
        let maybe_stakes = self
            .stake_store
            .get_stakes(epoch)
            .await
            .map_err(|e| TestError::SubsystemError(e.into()))?;

        self.assert(
            maybe_stakes.is_some(),
            format!("there should be stake distribution saved for {epoch:?}"),
        )
    }

    /// increase the immutable file number in the dumb beacon provider
    pub async fn increase_immutable(&mut self, increment: u64, expected: u64) -> Result<&mut Self> {
        let immutable_number = self
            .immutable_observer
            .get_last_immutable_number()
            .await
            .map_err(|e| TestError::SubsystemError(e.into()))?;
        let new_immutable = immutable_number + increment;
        self.assert(
            expected == new_immutable,
            format!("expected to increase immutable number up to {expected}, got {new_immutable}"),
        )?;
        self.immutable_observer
            .shall_return(Some(new_immutable))
            .await;

        Ok(self)
    }

    /// increase the epoch in the chain observer
    pub async fn increase_epoch(&mut self, expected: u64) -> Result<&mut Self> {
        let new_epoch = self
            .chain_observer
            .next_epoch()
            .await
            .ok_or_else(|| TestError::ValueError("no epoch returned".to_string()))?;

        self.assert(
            expected == new_epoch,
            format!("Epoch increased by 1 to {new_epoch} ({expected} expected)"),
        )
    }

    /// add a comment in the logs
    pub fn comment(&mut self, comment: &str) -> &mut Self {
        self.comment_no += 1;
        debug!("COMMENT {:02} 💬 {}", self.comment_no, comment);

        self
    }

    /// register the signer in the certificate handler
    pub async fn register_signers(
        &mut self,
        signers_with_stake: &[SignerWithStake],
    ) -> Result<&mut Self> {
        let epoch = self
            .chain_observer
            .current_beacon
            .read()
            .await
            .as_ref()
            .unwrap()
            .epoch;
        for signer_with_stake in signers_with_stake {
            self.certificate_handler
                .register_signer(epoch, &signer_with_stake.to_owned().into())
                .await
                .map_err(|e| TestError::SubsystemError(e.into()))?;
        }

        Ok(self)
    }

    /// Check when the era_checker has been updated
    pub async fn check_era_checker_last_updated_at(&mut self, epoch: Epoch) -> Result<&mut Self> {
        self.assert(
            self.era_checker.current_epoch() == epoch,
            format!("The epoch the EraChecker has been the last updated '{}' is different from expected epoch '{}'.", self.era_checker.current_epoch(), epoch)
        )
    }

    /// Set Era markers
    pub fn set_era_markers(&mut self, markers: Vec<EraMarker>) -> &mut Self {
        debug!("Era markers set: {:?}", markers);
        self.era_reader_adapter.set_markers(markers);

        self
    }
}

use anyhow::Context;
use async_trait::async_trait;
use slog_scope::{debug, warn};
use thiserror::Error;
use tokio::sync::RwLockReadGuard;

use mithril_common::crypto_helper::{KESPeriod, OpCert, ProtocolOpCert, SerDeShelleyFileFormat};
use mithril_common::entities::{
    Epoch, PartyId, ProtocolMessage, SignedEntityType, Signer, TimePoint,
};
use mithril_common::StdResult;
use mithril_persistence::store::StakeStorer;

use crate::dependency_injection::SignerDependencyContainer;
use crate::entities::{BeaconToSign, SignerEpochSettings};
use crate::services::{EpochService, MithrilProtocolInitializerBuilder};
use crate::Configuration;

/// This trait is mainly intended for mocking.
#[async_trait]
pub trait Runner: Send + Sync {
    /// Fetch the current epoch settings if any.
    async fn get_epoch_settings(&self) -> StdResult<Option<SignerEpochSettings>>;

    /// Fetch the beacon to sign if any.
    async fn get_beacon_to_sign(&self) -> StdResult<Option<BeaconToSign>>;

    /// Fetch the current time point from the Cardano node.
    async fn get_current_time_point(&self) -> StdResult<TimePoint>;

    /// Register the signer verification key to the aggregator.
    async fn register_signer_to_aggregator(&self) -> StdResult<()>;

    /// Read the stake distribution and store it.
    async fn update_stake_distribution(&self, epoch: Epoch) -> StdResult<()>;

    /// Check if the signer can sign the current epoch.
    async fn can_sign_current_epoch(&self) -> StdResult<bool>;

    /// Register epoch information
    async fn inform_epoch_settings(&self, epoch_settings: SignerEpochSettings) -> StdResult<()>;

    /// Create the message to be signed with the single signature.
    async fn compute_message(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<ProtocolMessage>;

    /// Create the single signature.
    async fn compute_publish_single_signature(
        &self,
        beacon_to_sign: &BeaconToSign,
        message: &ProtocolMessage,
    ) -> StdResult<()>;

    /// Mark the beacon as signed.
    async fn mark_beacon_as_signed(&self, beacon: &BeaconToSign) -> StdResult<()>;

    /// Read the current era and update the EraChecker.
    async fn update_era_checker(&self, epoch: Epoch) -> StdResult<()>;

    /// Perform the upkeep tasks.
    async fn upkeep(&self, current_epoch: Epoch) -> StdResult<()>;
}

/// This type represents the errors thrown from the Runner.
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum RunnerError {
    /// Value was expected from a subsystem but None was returned.
    #[error("No value returned by the subsystem for `{0}`.")]
    NoValueError(String),
    /// Could not associate my node with a stake.
    #[error("No stake associated with myself.")]
    NoStakeForSelf(),
    /// Could not find the stake for one of the signers.
    #[error("No stake associated with this signer, party_id: {0}.")]
    NoStakeForSigner(PartyId),
    /// Parse file error
    #[error("File parse failed: {0}.")]
    FileParse(String),
}

/// Controller methods for the Signer's state machine.
pub struct SignerRunner {
    config: Configuration,
    services: SignerDependencyContainer,
}

impl SignerRunner {
    /// Create a new Runner instance.
    pub fn new(config: Configuration, services: SignerDependencyContainer) -> Self {
        Self { services, config }
    }

    async fn epoch_service_read(&self) -> RwLockReadGuard<'_, dyn EpochService> {
        self.services.epoch_service.read().await
    }
}

#[cfg_attr(test, mockall::automock)]
#[async_trait]
impl Runner for SignerRunner {
    async fn get_epoch_settings(&self) -> StdResult<Option<SignerEpochSettings>> {
        debug!("RUNNER: get_epoch_settings");

        self.services
            .certificate_handler
            .retrieve_epoch_settings()
            .await
            .map_err(|e| e.into())
    }

    async fn get_beacon_to_sign(&self) -> StdResult<Option<BeaconToSign>> {
        debug!("RUNNER: get_beacon_to_sign");

        self.services.certifier.get_beacon_to_sign().await
    }

    async fn get_current_time_point(&self) -> StdResult<TimePoint> {
        debug!("RUNNER: get_current_time_point");

        self.services
            .ticker_service
            .get_current_time_point()
            .await
            .with_context(|| "Runner can not get current time point")
    }

    async fn register_signer_to_aggregator(&self) -> StdResult<()> {
        debug!("RUNNER: register_signer_to_aggregator");

        let (epoch, protocol_parameters) = {
            let epoch_service = self.services.epoch_service.read().await;
            let epoch = epoch_service.epoch_of_current_data()?;
            let protocol_parameters = epoch_service.next_protocol_parameters()?;

            (epoch, protocol_parameters.clone())
        };

        let epoch_offset_to_recording_epoch = epoch.offset_to_recording_epoch();
        let stake_distribution = self
            .services
            .stake_store
            .get_stakes(epoch_offset_to_recording_epoch)
            .await?
            .ok_or_else(|| {
                RunnerError::NoValueError(format!(
                    "stakes at epoch {epoch_offset_to_recording_epoch}"
                ))
            })?;
        let stake = stake_distribution
            .get(&self.services.single_signer.get_party_id())
            .ok_or_else(RunnerError::NoStakeForSelf)?;
        let (operational_certificate, protocol_operational_certificate) = match &self
            .config
            .operational_certificate_path
        {
            Some(operational_certificate_path) => {
                let opcert: OpCert = OpCert::from_file(operational_certificate_path)
                    .map_err(|_| RunnerError::FileParse("operational_certificate_path".to_string()))
                    .with_context(|| {
                        "register_signer_to_aggregator can not decode OpCert from file"
                    })?;
                (Some(opcert.clone()), Some(ProtocolOpCert::new(opcert)))
            }
            _ => (None, None),
        };

        let kes_period = match operational_certificate {
            Some(operational_certificate) => Some(
                self.services
                    .chain_observer
                    .get_current_kes_period(&operational_certificate)
                    .await?
                    .unwrap_or_default()
                    - operational_certificate.start_kes_period as KESPeriod,
            ),
            None => None,
        };
        let protocol_initializer = MithrilProtocolInitializerBuilder::build(
            stake,
            &protocol_parameters,
            self.config.kes_secret_key_path.clone(),
            kes_period,
        )?;
        let signer = Signer::new(
            self.services.single_signer.get_party_id(),
            protocol_initializer.verification_key().into(),
            protocol_initializer.verification_key_signature(),
            protocol_operational_certificate,
            kes_period,
        );
        self.services
            .certificate_handler
            .register_signer(epoch_offset_to_recording_epoch, &signer)
            .await?;
        self.services
            .protocol_initializer_store
            .save_protocol_initializer(epoch_offset_to_recording_epoch, protocol_initializer)
            .await?;

        Ok(())
    }

    async fn update_stake_distribution(&self, epoch: Epoch) -> StdResult<()> {
        debug!("RUNNER: update_stake_distribution");

        let exists_stake_distribution = !self
            .services
            .stake_store
            .get_stakes(epoch.offset_to_recording_epoch())
            .await?
            .unwrap_or_default()
            .is_empty();
        if exists_stake_distribution {
            return Ok(());
        }

        let stake_distribution = self
            .services
            .chain_observer
            .get_current_stake_distribution()
            .await?
            .ok_or_else(|| RunnerError::NoValueError("current_stake_distribution".to_string()))?;
        self.services
            .stake_store
            .save_stakes(epoch.offset_to_recording_epoch(), stake_distribution)
            .await?;

        Ok(())
    }

    async fn can_sign_current_epoch(&self) -> StdResult<bool> {
        let epoch_service = self.epoch_service_read().await;
        epoch_service.can_signer_sign_current_epoch(self.services.single_signer.get_party_id())
    }

    async fn inform_epoch_settings(&self, epoch_settings: SignerEpochSettings) -> StdResult<()> {
        debug!("RUNNER: register_epoch");
        let aggregator_features = self
            .services
            .certificate_handler
            .retrieve_aggregator_features()
            .await?;

        self.services
            .epoch_service
            .write()
            .await
            .inform_epoch_settings(
                epoch_settings,
                aggregator_features.capabilities.signed_entity_types,
            )
            .await
    }

    async fn compute_message(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<ProtocolMessage> {
        debug!("RUNNER: compute_message");

        let protocol_message = self
            .services
            .signable_builder_service
            .compute_protocol_message(signed_entity_type.to_owned())
            .await
            .with_context(|| format!("Runner can not compute protocol message for signed entity type: '{signed_entity_type}'"))?;

        Ok(protocol_message)
    }

    async fn compute_publish_single_signature(
        &self,
        beacon_to_sign: &BeaconToSign,
        message: &ProtocolMessage,
    ) -> StdResult<()> {
        debug!("RUNNER: compute_publish_single_signature");
        self.services
            .certifier
            .compute_publish_single_signature(beacon_to_sign, message)
            .await
    }

    async fn mark_beacon_as_signed(&self, beacon: &BeaconToSign) -> StdResult<()> {
        debug!("RUNNER: mark_beacon_as_signed"; "beacon" => ?beacon);

        self.services.certifier.mark_beacon_as_signed(beacon).await
    }

    async fn update_era_checker(&self, epoch: Epoch) -> StdResult<()> {
        debug!("RUNNER: update_era_checker");

        let era_token = self
            .services
            .era_reader
            .read_era_epoch_token(epoch)
            .await
            .map_err(Box::new)?;
        let current_era = era_token.get_current_supported_era()?;
        self.services
            .era_checker
            .change_era(current_era, era_token.get_current_epoch());
        debug!(
            "Current Era is {} (Epoch {}).",
            current_era,
            era_token.get_current_epoch()
        );

        if era_token.get_next_supported_era().is_err() {
            let era_name = &era_token.get_next_era_marker().unwrap().name;
            warn!("Upcoming Era '{era_name}' is not supported by this version of the software. Please update!");
        }

        Ok(())
    }

    async fn upkeep(&self, current_epoch: Epoch) -> StdResult<()> {
        debug!("RUNNER: upkeep");
        self.services.upkeep_service.run(current_epoch).await?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use mockall::mock;
    use mockall::predicate::eq;
    use std::collections::BTreeSet;
    use std::{path::Path, sync::Arc};
    use tokio::sync::RwLock;

    use mithril_common::{
        api_version::APIVersionProvider,
        cardano_block_scanner::DumbBlockScanner,
        cardano_transactions_preloader::{
            CardanoTransactionsPreloader, CardanoTransactionsPreloaderActivation,
        },
        chain_observer::FakeObserver,
        crypto_helper::{
            MKMap, MKMapNode, MKTreeNode, MKTreeStoreInMemory, MKTreeStorer, ProtocolInitializer,
        },
        digesters::{DumbImmutableDigester, DumbImmutableFileObserver},
        entities::{BlockNumber, BlockRange, Epoch, SignedEntityTypeDiscriminants},
        era::{adapters::EraReaderBootstrapAdapter, EraChecker, EraReader},
        messages::{AggregatorCapabilities, AggregatorFeaturesMessage},
        signable_builder::{
            BlockRangeRootRetriever, CardanoImmutableFilesFullSignableBuilder,
            CardanoStakeDistributionSignableBuilder, CardanoTransactionsSignableBuilder,
            MithrilSignableBuilderService, MithrilStakeDistributionSignableBuilder,
        },
        signed_entity_type_lock::SignedEntityTypeLock,
        test_utils::{fake_data, MithrilFixtureBuilder},
        MithrilTickerService, TickerService,
    };
    use mithril_persistence::store::adapter::{DumbStoreAdapter, MemoryAdapter};
    use mithril_persistence::store::{StakeStore, StakeStorer};

    use crate::database::repository::SignedBeaconRepository;
    use crate::database::test_helper::main_db_connection;
    use crate::metrics::MetricsService;
    use crate::services::{
        CardanoTransactionsImporter, DumbAggregatorClient, MithrilEpochService,
        MithrilSingleSigner, MockTransactionStore, MockUpkeepService, SignerCertifierService,
        SignerSignableSeedBuilder, SignerSignedEntityConfigProvider,
    };
    use crate::store::ProtocolInitializerStore;

    use super::*;

    const DIGESTER_RESULT: &str = "a digest";

    mock! {
        pub FakeTimePointProvider { }

        #[async_trait]
        impl TickerService for FakeTimePointProvider {
            async fn get_current_time_point(&self) -> StdResult<TimePoint>;
        }
    }

    mock! {
        pub BlockRangeRootRetrieverImpl<S: MKTreeStorer> { }

        #[async_trait]
        impl<S: MKTreeStorer> BlockRangeRootRetriever<S> for BlockRangeRootRetrieverImpl<S> {
            async fn retrieve_block_range_roots<'a>(
                &'a self,
                up_to_beacon: BlockNumber,
            ) -> StdResult<Box<dyn Iterator<Item = (BlockRange, MKTreeNode)> + 'a>>;

            async fn compute_merkle_map_from_block_range_roots(
                &self,
                up_to_beacon: BlockNumber,
            ) -> StdResult<MKMap<BlockRange, MKMapNode<BlockRange,S>, S>>;
        }
    }

    async fn init_services() -> SignerDependencyContainer {
        let sqlite_connection = Arc::new(main_db_connection().unwrap());
        let adapter: MemoryAdapter<Epoch, ProtocolInitializer> = MemoryAdapter::new(None).unwrap();
        let stake_distribution_signers = fake_data::signers_with_stakes(2);
        let party_id = stake_distribution_signers[1].party_id.clone();
        let network = fake_data::network();
        let fake_observer = FakeObserver::default();
        fake_observer.set_signers(stake_distribution_signers).await;
        let chain_observer = Arc::new(fake_observer);
        let ticker_service = Arc::new(MithrilTickerService::new(
            chain_observer.clone(),
            Arc::new(DumbImmutableFileObserver::default()),
        ));
        let era_reader = Arc::new(EraReader::new(Arc::new(EraReaderBootstrapAdapter)));
        let era_epoch_token = era_reader
            .read_era_epoch_token(ticker_service.get_current_epoch().await.unwrap())
            .await
            .unwrap();
        let era_checker = Arc::new(EraChecker::new(
            era_epoch_token.get_current_supported_era().unwrap(),
            era_epoch_token.get_current_epoch(),
        ));

        let api_version_provider = Arc::new(APIVersionProvider::new(era_checker.clone()));
        let digester = Arc::new(DumbImmutableDigester::new(DIGESTER_RESULT, true));
        let cardano_immutable_signable_builder =
            Arc::new(CardanoImmutableFilesFullSignableBuilder::new(
                digester.clone(),
                Path::new(""),
                slog_scope::logger(),
            ));
        let mithril_stake_distribution_signable_builder =
            Arc::new(MithrilStakeDistributionSignableBuilder::default());
        let transaction_parser = Arc::new(DumbBlockScanner::new());
        let transaction_store = Arc::new(MockTransactionStore::new());
        let transactions_importer = Arc::new(CardanoTransactionsImporter::new(
            transaction_parser.clone(),
            transaction_store.clone(),
            slog_scope::logger(),
        ));
        let block_range_root_retriever =
            Arc::new(MockBlockRangeRootRetrieverImpl::<MKTreeStoreInMemory>::new());
        let cardano_transactions_builder = Arc::new(CardanoTransactionsSignableBuilder::new(
            transactions_importer.clone(),
            block_range_root_retriever,
            slog_scope::logger(),
        ));
        let stake_store = Arc::new(StakeStore::new(Box::new(DumbStoreAdapter::new()), None));
        let cardano_stake_distribution_builder = Arc::new(
            CardanoStakeDistributionSignableBuilder::new(stake_store.clone()),
        );
        let protocol_initializer_store =
            Arc::new(ProtocolInitializerStore::new(Box::new(adapter), None));
        let epoch_service = Arc::new(RwLock::new(MithrilEpochService::new(
            stake_store.clone(),
            protocol_initializer_store.clone(),
        )));
        let single_signer = Arc::new(MithrilSingleSigner::new(party_id, epoch_service.clone()));
        let signable_seed_builder_service = Arc::new(SignerSignableSeedBuilder::new(
            epoch_service.clone(),
            protocol_initializer_store.clone(),
        ));
        let signable_builder_service = Arc::new(MithrilSignableBuilderService::new(
            era_checker.clone(),
            signable_seed_builder_service,
            mithril_stake_distribution_signable_builder,
            cardano_immutable_signable_builder,
            cardano_transactions_builder,
            cardano_stake_distribution_builder,
        ));
        let metrics_service = Arc::new(MetricsService::new().unwrap());
        let signed_entity_type_lock = Arc::new(SignedEntityTypeLock::default());
        let security_parameter = BlockNumber(0);
        let cardano_transactions_preloader = Arc::new(CardanoTransactionsPreloader::new(
            signed_entity_type_lock.clone(),
            transactions_importer.clone(),
            security_parameter,
            chain_observer.clone(),
            slog_scope::logger(),
            Arc::new(CardanoTransactionsPreloaderActivation::new(true)),
        ));
        let upkeep_service = Arc::new(MockUpkeepService::new());
        let aggregator_client = Arc::new(DumbAggregatorClient::default());
        let certifier = Arc::new(SignerCertifierService::new(
            ticker_service.clone(),
            Arc::new(SignedBeaconRepository::new(sqlite_connection.clone(), None)),
            Arc::new(SignerSignedEntityConfigProvider::new(
                network,
                epoch_service.clone(),
            )),
            signed_entity_type_lock.clone(),
            single_signer.clone(),
            aggregator_client.clone(),
        ));

        SignerDependencyContainer {
            stake_store,
            certificate_handler: aggregator_client,
            chain_observer,
            digester,
            single_signer,
            ticker_service,
            protocol_initializer_store,
            era_checker,
            era_reader,
            api_version_provider,
            signable_builder_service,
            metrics_service,
            signed_entity_type_lock,
            cardano_transactions_preloader,
            upkeep_service,
            epoch_service,
            certifier,
        }
    }

    async fn init_runner(
        maybe_services: Option<SignerDependencyContainer>,
        maybe_config: Option<Configuration>,
    ) -> SignerRunner {
        SignerRunner::new(
            maybe_config.unwrap_or(Configuration::new_sample("1")),
            maybe_services.unwrap_or(init_services().await),
        )
    }

    #[tokio::test]
    async fn test_get_current_time_point() {
        let mut services = init_services().await;
        let expected = TimePoint::dummy();
        let mut ticker_service = MockFakeTimePointProvider::new();
        ticker_service
            .expect_get_current_time_point()
            .once()
            .returning(move || Ok(TimePoint::dummy()));
        services.ticker_service = Arc::new(ticker_service);
        let runner = init_runner(Some(services), None).await;

        assert_eq!(
            expected,
            runner
                .get_current_time_point()
                .await
                .expect("Get current time point should not fail.")
        );
    }

    #[tokio::test]
    async fn test_update_stake_distribution() {
        let services = init_services().await;
        let stake_store = services.stake_store.clone();
        let current_epoch = services
            .chain_observer
            .get_current_epoch()
            .await
            .expect("chain observer should not fail")
            .expect("the observer should return an epoch");
        let runner = init_runner(Some(services), None).await;
        assert!(stake_store
            .get_stakes(current_epoch)
            .await
            .expect("getting stakes from store should not fail")
            .is_none());

        runner
            .update_stake_distribution(current_epoch)
            .await
            .expect("update_stake_distribution should not fail.");

        let stake_distribution = stake_store
            .get_stakes(current_epoch.offset_to_recording_epoch())
            .await
            .expect("getting stakes from store should not fail")
            .expect("there should be stakes for this epoch");

        assert_eq!(2, stake_distribution.len());
    }

    #[tokio::test]
    async fn test_register_signer_to_aggregator() {
        let mut services = init_services().await;
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let certificate_handler = Arc::new(DumbAggregatorClient::default());
        services.certificate_handler = certificate_handler.clone();
        let protocol_initializer_store = services.protocol_initializer_store.clone();
        let current_epoch = services.ticker_service.get_current_epoch().await.unwrap();

        let stakes = services
            .chain_observer
            .get_current_stake_distribution()
            .await
            .unwrap()
            .unwrap();
        services
            .stake_store
            .save_stakes(current_epoch.offset_to_recording_epoch(), stakes)
            .await
            .unwrap();

        let runner = init_runner(Some(services), None).await;
        // inform epoch settings
        let epoch_settings = SignerEpochSettings {
            epoch: current_epoch,
            current_signers: fixture.signers(),
            next_signers: fixture.signers(),
            ..SignerEpochSettings::dummy().clone()
        };
        runner.inform_epoch_settings(epoch_settings).await.unwrap();

        runner
            .register_signer_to_aggregator()
            .await
            .expect("registering a signer to the aggregator should not fail");

        assert!(certificate_handler
            .get_last_registered_signer()
            .await
            .is_some());
        let maybe_protocol_initializer = protocol_initializer_store
            .get_protocol_initializer(current_epoch.offset_to_recording_epoch())
            .await
            .expect("get_protocol_initializer should not fail");
        assert!(
            maybe_protocol_initializer.is_some(),
            "A protocol initializer should have been registered at the 'Recording' epoch"
        );
    }

    #[tokio::test]
    async fn test_update_era_checker() {
        let services = init_services().await;
        let ticker_service = services.ticker_service.clone();
        let era_checker = services.era_checker.clone();
        let mut time_point = ticker_service.get_current_time_point().await.unwrap();

        assert_eq!(time_point.epoch, era_checker.current_epoch());
        let runner = init_runner(Some(services), None).await;
        time_point.epoch += 1;
        runner.update_era_checker(time_point.epoch).await.unwrap();

        assert_eq!(time_point.epoch, era_checker.current_epoch());
    }

    #[tokio::test]
    async fn test_upkeep() {
        let mut services = init_services().await;
        let mut upkeep_service_mock = MockUpkeepService::new();
        upkeep_service_mock
            .expect_run()
            .with(eq(Epoch(17)))
            .returning(|_| Ok(()))
            .once();
        services.upkeep_service = Arc::new(upkeep_service_mock);

        let runner = init_runner(Some(services), None).await;
        runner
            .upkeep(Epoch(17))
            .await
            .expect("upkeep should not fail");
    }

    #[tokio::test]
    async fn test_inform_epoch_setting_pass_allowed_discriminant_to_epoch_service() {
        let mut services = init_services().await;
        let certificate_handler = Arc::new(DumbAggregatorClient::default());
        certificate_handler
            .set_aggregator_features(AggregatorFeaturesMessage {
                capabilities: AggregatorCapabilities {
                    signed_entity_types: BTreeSet::from([
                        SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                        SignedEntityTypeDiscriminants::CardanoTransactions,
                    ]),
                    ..AggregatorFeaturesMessage::dummy().capabilities
                },
                ..AggregatorFeaturesMessage::dummy()
            })
            .await;
        services.certificate_handler = certificate_handler;
        let runner = init_runner(Some(services), None).await;

        let epoch_settings = SignerEpochSettings {
            epoch: Epoch(1),
            ..SignerEpochSettings::dummy()
        };
        runner.inform_epoch_settings(epoch_settings).await.unwrap();

        let epoch_service = runner.services.epoch_service.read().await;
        let recorded_allowed_discriminants = epoch_service.allowed_discriminants().unwrap();

        assert_eq!(
            &BTreeSet::from([
                SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                SignedEntityTypeDiscriminants::CardanoTransactions,
            ]),
            recorded_allowed_discriminants
        );
    }
}

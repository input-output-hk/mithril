use anyhow::Context;
use async_trait::async_trait;
use slog_scope::{debug, info, warn};
use thiserror::Error;

#[cfg(test)]
use mockall::automock;

use mithril_common::crypto_helper::{KESPeriod, OpCert, ProtocolOpCert, SerDeShelleyFileFormat};
use mithril_common::entities::{
    CertificatePending, Epoch, EpochSettings, PartyId, ProtocolMessage, ProtocolMessagePartKey,
    SignedEntityType, Signer, SignerWithStake, SingleSignatures, TimePoint,
};
use mithril_common::StdResult;
use mithril_persistence::store::StakeStorer;

use crate::dependency_injection::SignerDependencyContainer;
use crate::services::MithrilProtocolInitializerBuilder;
use crate::Configuration;

/// This trait is mainly intended for mocking.
#[async_trait]
pub trait Runner: Send + Sync {
    /// Fetch the current epoch settings if any.
    async fn get_epoch_settings(&self) -> StdResult<Option<EpochSettings>>;

    /// Fetch the current pending certificate if any.
    async fn get_pending_certificate(&self) -> StdResult<Option<CertificatePending>>;

    /// Fetch the current time point from the Cardano node.
    async fn get_current_time_point(&self) -> StdResult<TimePoint>;

    /// Get the current signers.
    /// // TODO return a &Vec
    async fn get_current_signers(&self) -> StdResult<Vec<Signer>>;

    /// Get the next signers.
    async fn get_next_signers(&self) -> StdResult<Vec<Signer>>;

    /// Get the next signers with their stake.
    async fn get_current_signers_with_stake(&self) -> StdResult<Vec<SignerWithStake>>;

    /// Get the next signers with their stake.
    async fn get_next_signers_with_stake(&self) -> StdResult<Vec<SignerWithStake>>;

    /// Register the signer verification key to the aggregator.
    async fn register_signer_to_aggregator(&self) -> StdResult<()>;

    /// Read the stake distribution and store it.
    async fn update_stake_distribution(&self, epoch: Epoch) -> StdResult<()>;

    /// Check if all prerequisites for signing are met.
    async fn can_i_sign(&self, pending_certificate: &CertificatePending) -> StdResult<bool>;

    /// Register epoch information
    async fn inform_epoch_settings(&self, epoch_settings: EpochSettings) -> StdResult<()>;

    /// Create the message to be signed with the single signature.
    async fn compute_message(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<ProtocolMessage>;

    /// Create the single signature.
    async fn compute_single_signature(
        &self,
        epoch: Epoch,
        message: &ProtocolMessage,
    ) -> StdResult<Option<SingleSignatures>>;

    /// Send the single signature to the aggregator in order to be aggregated.
    async fn send_single_signature(
        &self,
        signed_entity_type: &SignedEntityType,
        maybe_signature: Option<SingleSignatures>,
    ) -> StdResult<()>;

    /// Read the current era and update the EraChecker.
    async fn update_era_checker(&self, epoch: Epoch) -> StdResult<()>;

    /// Perform the upkeep tasks.
    async fn upkeep(&self) -> StdResult<()>;
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
}

#[cfg_attr(test, automock)]
#[async_trait]
impl Runner for SignerRunner {
    async fn get_epoch_settings(&self) -> StdResult<Option<EpochSettings>> {
        debug!("RUNNER: get_epoch_settings");

        self.services
            .certificate_handler
            .retrieve_epoch_settings()
            .await
            .map_err(|e| e.into())
    }

    async fn get_pending_certificate(&self) -> StdResult<Option<CertificatePending>> {
        debug!("RUNNER: get_pending_certificate");

        self.services
            .certificate_handler
            .retrieve_pending_certificate()
            .await
            .map_err(|e| e.into())
    }

    async fn get_current_time_point(&self) -> StdResult<TimePoint> {
        debug!("RUNNER: get_current_time_point");

        self.services
            .ticker_service
            .get_current_time_point()
            .await
            .with_context(|| "Runner can not get current time point")
    }

    async fn get_current_signers(&self) -> StdResult<Vec<Signer>> {
        debug!("RUNNER: get_current_signers");

        self.services
            .epoch_service
            .read()
            .await
            .current_signers()
            .cloned()
    }

    async fn get_next_signers(&self) -> StdResult<Vec<Signer>> {
        debug!("RUNNER: get_next_signers");

        self.services
            .epoch_service
            .read()
            .await
            .next_signers()
            .cloned()
    }

    /// Get the current signers with their stake.
    async fn get_current_signers_with_stake(&self) -> StdResult<Vec<SignerWithStake>> {
        self.services
            .epoch_service
            .read()
            .await
            .current_signers_with_stake()
            .await
    }

    /// Get the next signers with their stake.
    async fn get_next_signers_with_stake(&self) -> StdResult<Vec<SignerWithStake>> {
        self.services
            .epoch_service
            .read()
            .await
            .next_signers_with_stake()
            .await
    }

    async fn register_signer_to_aggregator(&self) -> StdResult<()> {
        debug!("RUNNER: register_signer_to_aggregator");

        let epoch_service = self.services.epoch_service.read().await;
        let epoch = epoch_service.epoch_of_current_data()?;
        let protocol_parameters = epoch_service.next_protocol_parameters()?;

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
            protocol_parameters,
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

    async fn can_i_sign(&self, pending_certificate: &CertificatePending) -> StdResult<bool> {
        debug!("RUNNER: can_i_sign");
        if self
            .services
            .signed_entity_type_lock
            .is_locked(&pending_certificate.signed_entity_type)
            .await
        {
            debug!(" > signed entity type is locked, can NOT sign");
            return Ok(false);
        }

        if let Some(signer) =
            pending_certificate.get_signer(self.services.single_signer.get_party_id())
        {
            debug!(" > got a Signer from pending certificate");

            if let Some(protocol_initializer) = self
                .services
                .protocol_initializer_store
                .get_protocol_initializer(
                    pending_certificate
                        .epoch
                        .offset_to_signer_retrieval_epoch()?,
                )
                .await?
            {
                debug!(
                    " > got protocol initializer for this epoch ({})",
                    pending_certificate.epoch
                );

                if signer.verification_key == protocol_initializer.verification_key().into() {
                    debug!("verification keys match, we can sign");

                    return Ok(true);
                }
                debug!(" > verification key do not match, can NOT sign");
            } else {
                warn!(
                    " > NO protocol initializer found for this epoch ({})",
                    pending_certificate.epoch
                );
            }
        } else {
            debug!(" > Signer not found in the certificate pending");
        }

        Ok(false)
    }

    // Register epoch settings information
    async fn inform_epoch_settings(&self, epoch_settings: EpochSettings) -> StdResult<()> {
        debug!("RUNNER: register_epoch");
        self.services
            .epoch_service
            .write()
            .await
            .inform_epoch_settings(epoch_settings)
            .await
    }

    async fn compute_message(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<ProtocolMessage> {
        debug!("RUNNER: compute_message");

        let next_signers = self
            .get_next_signers_with_stake()
            .await
            .with_context(|| "Runner can not not retrieve next signers")?;

        // 1 compute the signed entity type part of the message
        let mut message = self
            .services
            .signable_builder_service
            .compute_protocol_message(signed_entity_type.to_owned())
            .await
            .with_context(|| format!("Runner can not compute protocol message for signed entity type: '{signed_entity_type}'"))?;

        // 2 set the next signers keys and stakes in the message
        let epoch = signed_entity_type.get_epoch();
        let next_signer_retrieval_epoch = epoch.offset_to_next_signer_retrieval_epoch();
        let next_protocol_initializer = self
            .services
            .protocol_initializer_store
            .get_protocol_initializer(next_signer_retrieval_epoch)
            .await?
            .ok_or_else(|| {
                RunnerError::NoValueError(format!(
                    "protocol_initializer at epoch {next_signer_retrieval_epoch}"
                ))
            })?;

        let avk = self
            .services
            .single_signer
            .compute_aggregate_verification_key(&next_signers, &next_protocol_initializer)?
            .ok_or_else(|| RunnerError::NoValueError("next_signers avk".to_string()))?;
        message.set_message_part(ProtocolMessagePartKey::NextAggregateVerificationKey, avk);

        Ok(message)
    }

    async fn compute_single_signature(
        &self,
        epoch: Epoch,
        message: &ProtocolMessage,
    ) -> StdResult<Option<SingleSignatures>> {
        debug!("RUNNER: compute_single_signature");

        let signers = self
            .get_current_signers_with_stake()
            .await
            .with_context(|| "Runner can not not retrieve signers")?;

        let signer_retrieval_epoch = epoch.offset_to_signer_retrieval_epoch()?;
        let protocol_initializer = self
            .services
            .protocol_initializer_store
            .get_protocol_initializer(signer_retrieval_epoch)
            .await?
            .ok_or_else(|| {
                RunnerError::NoValueError(format!(
                    "protocol_initializer at epoch {signer_retrieval_epoch}"
                ))
            })?;
        let signature = self.services.single_signer.compute_single_signatures(
            message,
            &signers,
            &protocol_initializer,
        )?;
        info!(
            " > {}",
            if signature.is_some() {
                "could compute a single signature!"
            } else {
                "NO single signature was computed."
            }
        );

        Ok(signature)
    }

    async fn send_single_signature(
        &self,
        signed_entity_type: &SignedEntityType,
        maybe_signature: Option<SingleSignatures>,
    ) -> StdResult<()> {
        debug!("RUNNER: send_single_signature");

        if let Some(single_signatures) = maybe_signature {
            debug!(" > there is a single signature to send");

            self.services
                .certificate_handler
                .register_signatures(signed_entity_type, &single_signatures)
                .await?;

            Ok(())
        } else {
            debug!(" > NO single signature to send, doing nothing");

            Ok(())
        }
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

    async fn upkeep(&self) -> StdResult<()> {
        debug!("RUNNER: upkeep");
        self.services.upkeep_service.run().await?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
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
        entities::{BlockNumber, BlockRange, CardanoDbBeacon, Epoch},
        era::{adapters::EraReaderBootstrapAdapter, EraChecker, EraReader},
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
    use mockall::mock;
    use std::{path::Path, sync::Arc};

    use crate::metrics::MetricsService;
    use crate::services::{
        CardanoTransactionsImporter, DumbAggregatorClient, MithrilEpochService,
        MithrilSingleSigner, MockAggregatorClient, MockTransactionStore, MockUpkeepService,
        SingleSigner,
    };
    use crate::store::ProtocolInitializerStore;

    use tokio::sync::RwLock;

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
        let adapter: MemoryAdapter<Epoch, ProtocolInitializer> = MemoryAdapter::new(None).unwrap();
        let stake_distribution_signers = fake_data::signers_with_stakes(2);
        let party_id = stake_distribution_signers[1].party_id.clone();
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
        let signable_builder_service = Arc::new(MithrilSignableBuilderService::new(
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
        let epoch_service = Arc::new(RwLock::new(MithrilEpochService::new(stake_store.clone())));

        SignerDependencyContainer {
            stake_store,
            certificate_handler: Arc::new(DumbAggregatorClient::default()),
            chain_observer,
            digester,
            single_signer: Arc::new(MithrilSingleSigner::new(party_id)),
            ticker_service,
            protocol_initializer_store: Arc::new(ProtocolInitializerStore::new(
                Box::new(adapter),
                None,
            )),
            era_checker,
            era_reader,
            api_version_provider,
            signable_builder_service,
            metrics_service,
            signed_entity_type_lock,
            cardano_transactions_preloader,
            upkeep_service,
            epoch_service,
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
        let epoch_settings = EpochSettings {
            epoch: current_epoch,
            current_signers: fixture.signers(),
            next_signers: fixture.signers(),
            ..fake_data::epoch_settings().clone()
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
    async fn test_can_i_sign() {
        let mut pending_certificate = fake_data::certificate_pending();
        let epoch = pending_certificate.epoch;
        let signer = &mut pending_certificate.signers[0];
        // All signed entities are available for signing.
        let signed_entity_type_lock = Arc::new(SignedEntityTypeLock::new());
        let mut services = init_services().await;
        let protocol_initializer_store = services.protocol_initializer_store.clone();
        services.single_signer = Arc::new(MithrilSingleSigner::new(signer.party_id.to_owned()));
        services.signed_entity_type_lock = signed_entity_type_lock.clone();
        let runner = init_runner(Some(services), None).await;

        let protocol_initializer = MithrilProtocolInitializerBuilder::build(
            &100,
            &fake_data::protocol_parameters(),
            None,
            None,
        )
        .expect("build protocol initializer should not fail");
        signer.verification_key = protocol_initializer.verification_key().into();
        protocol_initializer_store
            .save_protocol_initializer(
                epoch
                    .offset_to_signer_retrieval_epoch()
                    .expect("offset_to_signer_retrieval_epoch should not fail"),
                protocol_initializer,
            )
            .await
            .expect("save_protocol_initializer should not fail");

        let can_i_sign_result = runner.can_i_sign(&pending_certificate).await.unwrap();
        assert!(can_i_sign_result);

        // Lock the pending certificate signed entity type, the signer should not be able to sign.
        signed_entity_type_lock
            .lock(&pending_certificate.signed_entity_type)
            .await;

        let can_i_sign_result = runner.can_i_sign(&pending_certificate).await.unwrap();
        assert!(
            !can_i_sign_result,
            "The signer should not be able to sign when the signed entity type is locked."
        );
    }

    #[tokio::test]
    async fn test_compute_message() {
        let mut services = init_services().await;
        let current_time_point = services
            .ticker_service
            .get_current_time_point()
            .await
            .expect("get_current_time_point should not fail");
        let signed_entity_type = SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::new(
            "whatever",
            *current_time_point.epoch,
            current_time_point.immutable_file_number,
        ));
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let signer_with_stake = fixture.signers_fixture()[0].signer_with_stake.clone();
        let protocol_initializer = fixture.signers_fixture()[0].protocol_initializer.clone();
        let single_signer = Arc::new(MithrilSingleSigner::new(
            signer_with_stake.party_id.to_owned(),
        ));
        services.single_signer = single_signer.clone();
        services
            .protocol_initializer_store
            .save_protocol_initializer(
                current_time_point
                    .epoch
                    .offset_to_next_signer_retrieval_epoch(),
                protocol_initializer.clone(),
            )
            .await
            .expect("save_protocol_initializer should not fail");

        services
            .stake_store
            .save_stakes(
                current_time_point
                    .epoch
                    .offset_to_next_signer_retrieval_epoch(),
                fixture.stake_distribution(),
            )
            .await
            .expect("save_stakes should not fail");

        let next_signers_with_stake = &fixture.signers_with_stake()[3..5];
        let next_signers = &fixture.signers()[3..5];

        let mut expected = ProtocolMessage::new();
        expected.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            DIGESTER_RESULT.to_string(),
        );
        let avk = services
            .single_signer
            .compute_aggregate_verification_key(next_signers_with_stake, &protocol_initializer)
            .expect("compute_aggregate_verification_key should not fail")
            .expect("an avk should have been computed");
        expected.set_message_part(ProtocolMessagePartKey::NextAggregateVerificationKey, avk);

        let runner = init_runner(Some(services), None).await;

        // inform epoch settings
        let epoch_settings = EpochSettings {
            epoch: current_time_point.epoch,
            current_signers: fixture.signers(),
            next_signers: next_signers.to_vec(),
            ..fake_data::epoch_settings().clone()
        };
        runner.inform_epoch_settings(epoch_settings).await.unwrap();

        let message = runner
            .compute_message(&signed_entity_type)
            .await
            .expect("compute_message should not fail");

        assert_eq!(expected, message);
    }

    #[tokio::test]
    async fn test_compute_single_signature() {
        let mut services = init_services().await;
        let current_time_point = services
            .ticker_service
            .get_current_time_point()
            .await
            .expect("get_current_time_point should not fail");
        let fixture = MithrilFixtureBuilder::default().with_signers(5).build();
        let signer_with_stake = fixture.signers_fixture()[0].signer_with_stake.clone();
        let protocol_initializer = fixture.signers_fixture()[0].protocol_initializer.clone();
        let single_signer = Arc::new(MithrilSingleSigner::new(
            signer_with_stake.party_id.to_string(),
        ));
        services.single_signer = single_signer.clone();
        services
            .protocol_initializer_store
            .save_protocol_initializer(
                current_time_point
                    .epoch
                    .offset_to_signer_retrieval_epoch()
                    .expect("offset_to_signer_retrieval_epoch should not fail"),
                protocol_initializer.clone(),
            )
            .await
            .expect("save_protocol_initializer should not fail");

        services
            .stake_store
            .save_stakes(
                current_time_point
                    .epoch
                    .offset_to_signer_retrieval_epoch()
                    .expect("offset_to_signer_retrieval_epoch should not fail"),
                fixture.stake_distribution(),
            )
            .await
            .expect("save_stakes should not fail");

        let signers_with_stake = &fixture.signers_with_stake()[0..3];
        let signers = &fixture.signers()[0..3];

        let mut message = ProtocolMessage::new();
        message.set_message_part(
            ProtocolMessagePartKey::SnapshotDigest,
            "a message".to_string(),
        );
        message.set_message_part(
            ProtocolMessagePartKey::NextAggregateVerificationKey,
            "an avk".to_string(),
        );

        let expected = single_signer
            .compute_single_signatures(&message, &signers_with_stake, &protocol_initializer)
            .expect("compute_single_signatures should not fail");

        let runner = init_runner(Some(services), None).await;

        // inform epoch settings
        let epoch_settings = EpochSettings {
            epoch: current_time_point.epoch,
            current_signers: signers.to_vec(),
            next_signers: fixture.signers(),
            ..fake_data::epoch_settings().clone()
        };
        runner.inform_epoch_settings(epoch_settings).await.unwrap();

        let single_signature = runner
            .compute_single_signature(current_time_point.epoch, &message)
            .await
            .expect("compute_message should not fail");
        assert_eq!(expected, single_signature);
    }

    #[tokio::test]
    async fn test_send_single_signature() {
        let mut services = init_services().await;
        let mut certificate_handler = MockAggregatorClient::new();
        certificate_handler
            .expect_register_signatures()
            .once()
            .returning(|_, _| Ok(()));
        services.certificate_handler = Arc::new(certificate_handler);
        let runner = init_runner(Some(services), None).await;

        runner
            .send_single_signature(
                &SignedEntityType::dummy(),
                Some(fake_data::single_signatures(vec![2, 5, 12])),
            )
            .await
            .expect("send_single_signature should not fail");
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
        upkeep_service_mock.expect_run().returning(|| Ok(())).once();
        services.upkeep_service = Arc::new(upkeep_service_mock);

        let runner = init_runner(Some(services), None).await;
        runner.upkeep().await.expect("upkeep should not fail");
    }
}

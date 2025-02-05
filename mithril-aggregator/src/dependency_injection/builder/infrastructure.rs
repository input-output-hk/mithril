use anyhow::Context;
use slog::Logger;
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::mpsc::{UnboundedReceiver, UnboundedSender};
use tokio::sync::Mutex;

use mithril_common::{
    api_version::APIVersionProvider,
    cardano_block_scanner::{BlockScanner, CardanoBlockScanner},
    cardano_transactions_preloader::{
        CardanoTransactionsPreloader, CardanoTransactionsPreloaderActivation,
    },
    certificate_chain::{CertificateVerifier, MithrilCertificateVerifier},
    chain_observer::{CardanoCliRunner, ChainObserver, ChainObserverBuilder, FakeObserver},
    chain_reader::{ChainBlockReader, PallasChainReader},
    crypto_helper::{
        ProtocolGenesisSigner, ProtocolGenesisVerificationKey, ProtocolGenesisVerifier,
    },
    digesters::{
        CardanoImmutableDigester, DumbImmutableFileObserver, ImmutableDigester,
        ImmutableFileObserver, ImmutableFileSystemObserver,
    },
    entities::{Epoch, SignedEntityTypeDiscriminants},
    era::{
        adapters::{EraReaderAdapterBuilder, EraReaderDummyAdapter},
        EraChecker, EraMarker, EraReader, EraReaderAdapter, SupportedEra,
    },
    signable_builder::{SignableSeedBuilder, TransactionsImporter},
    signed_entity_type_lock::SignedEntityTypeLock,
};

use crate::{
    database::repository::ImmutableFileDigestRepository,
    dependency_injection::{DependenciesBuilder, DependenciesBuilderError, Result},
    event_store::{EventMessage, EventStore},
    services::{AggregatorSignableSeedBuilder, CardanoTransactionsImporter},
    CExplorerSignerRetriever, ExecutionEnvironment, ImmutableFileDigestMapper,
    MithrilSignerRegisterer, MultiSigner, MultiSignerImpl, SignersImporter,
    SingleSignatureAuthenticator,
};

impl DependenciesBuilder {
    /// Return a copy of the root logger.
    pub fn root_logger(&self) -> Logger {
        self.root_logger.clone()
    }

    async fn build_multi_signer(&mut self) -> Result<Arc<dyn MultiSigner>> {
        let multi_signer =
            MultiSignerImpl::new(self.get_epoch_service().await?, self.root_logger());

        Ok(Arc::new(multi_signer))
    }

    /// Get the [MultiSigner] instance
    pub async fn get_multi_signer(&mut self) -> Result<Arc<dyn MultiSigner>> {
        if self.multi_signer.is_none() {
            self.multi_signer = Some(self.build_multi_signer().await?);
        }

        Ok(self.multi_signer.as_ref().cloned().unwrap())
    }

    async fn build_chain_observer(&mut self) -> Result<Arc<dyn ChainObserver>> {
        let chain_observer: Arc<dyn ChainObserver> = match self.configuration.environment {
            ExecutionEnvironment::Production => {
                let cardano_cli_runner = &self.get_cardano_cli_runner().await?;
                let chain_observer_type = &self.configuration.chain_observer_type;
                let cardano_node_socket_path = &self.configuration.cardano_node_socket_path;
                let cardano_network = &self
                    .configuration
                    .get_network()
                    .with_context(|| "Dependencies Builder can not get Cardano network while building the chain observer")?;
                let chain_observer_builder = ChainObserverBuilder::new(
                    chain_observer_type,
                    cardano_node_socket_path,
                    cardano_network,
                    Some(cardano_cli_runner),
                );

                chain_observer_builder
                    .build()
                    .with_context(|| "Dependencies Builder can not build chain observer")?
            }
            _ => Arc::new(FakeObserver::default()),
        };

        Ok(chain_observer)
    }

    /// Get the [ChainObserver] instance
    pub async fn get_chain_observer(&mut self) -> Result<Arc<dyn ChainObserver>> {
        if self.chain_observer.is_none() {
            self.chain_observer = Some(self.build_chain_observer().await?);
        }

        Ok(self.chain_observer.as_ref().cloned().unwrap())
    }

    async fn build_cardano_cli_runner(&mut self) -> Result<Box<CardanoCliRunner>> {
        let cli_runner = CardanoCliRunner::new(
            self.configuration.cardano_cli_path.clone(),
            self.configuration.cardano_node_socket_path.clone(),
            self.configuration.get_network().with_context(|| {
                "Dependencies Builder can not get Cardano network while building cardano cli runner"
            })?,
        );

        Ok(Box::new(cli_runner))
    }

    /// Get the [CardanoCliRunner] instance
    pub async fn get_cardano_cli_runner(&mut self) -> Result<Box<CardanoCliRunner>> {
        if self.cardano_cli_runner.is_none() {
            self.cardano_cli_runner = Some(self.build_cardano_cli_runner().await?);
        }

        Ok(self.cardano_cli_runner.as_ref().cloned().unwrap())
    }

    async fn build_immutable_file_observer(&mut self) -> Result<Arc<dyn ImmutableFileObserver>> {
        let immutable_file_observer: Arc<dyn ImmutableFileObserver> =
            match self.configuration.environment {
                ExecutionEnvironment::Production => Arc::new(ImmutableFileSystemObserver::new(
                    &self.configuration.db_directory,
                )),
                _ => Arc::new(DumbImmutableFileObserver::default()),
            };

        Ok(immutable_file_observer)
    }

    /// Get the [ImmutableFileObserver] instance
    pub async fn get_immutable_file_observer(&mut self) -> Result<Arc<dyn ImmutableFileObserver>> {
        if self.immutable_file_observer.is_none() {
            self.immutable_file_observer = Some(self.build_immutable_file_observer().await?);
        }

        Ok(self.immutable_file_observer.as_ref().cloned().unwrap())
    }

    async fn build_chain_block_reader(&mut self) -> Result<Arc<Mutex<dyn ChainBlockReader>>> {
        let chain_block_reader = PallasChainReader::new(
            &self.configuration.cardano_node_socket_path,
            self.configuration.get_network()?,
            self.root_logger(),
        );

        Ok(Arc::new(Mutex::new(chain_block_reader)))
    }

    /// Get the [ChainBlockReader] instance
    pub async fn get_chain_block_reader(&mut self) -> Result<Arc<Mutex<dyn ChainBlockReader>>> {
        if self.chain_block_reader.is_none() {
            self.chain_block_reader = Some(self.build_chain_block_reader().await?);
        }

        Ok(self.chain_block_reader.as_ref().cloned().unwrap())
    }

    async fn build_block_scanner(&mut self) -> Result<Arc<dyn BlockScanner>> {
        let block_scanner = CardanoBlockScanner::new(
            self.get_chain_block_reader().await?,
            self.configuration
                .cardano_transactions_block_streamer_max_roll_forwards_per_poll,
            self.root_logger(),
        );

        Ok(Arc::new(block_scanner))
    }

    /// Get the [BlockScanner] instance
    pub async fn get_block_scanner(&mut self) -> Result<Arc<dyn BlockScanner>> {
        if self.block_scanner.is_none() {
            self.block_scanner = Some(self.build_block_scanner().await?);
        }

        Ok(self.block_scanner.as_ref().cloned().unwrap())
    }

    async fn build_immutable_digester(&mut self) -> Result<Arc<dyn ImmutableDigester>> {
        let immutable_digester_cache = match self.configuration.environment {
            ExecutionEnvironment::Production => Some(self.get_immutable_cache_provider().await?),
            _ => None,
        };
        let digester = CardanoImmutableDigester::new(
            self.configuration.get_network()?.to_string(),
            immutable_digester_cache,
            self.root_logger(),
        );

        Ok(Arc::new(digester))
    }

    /// Get the [ImmutableDigester] instance
    pub async fn get_immutable_digester(&mut self) -> Result<Arc<dyn ImmutableDigester>> {
        if self.immutable_digester.is_none() {
            self.immutable_digester = Some(self.build_immutable_digester().await?);
        }

        Ok(self.immutable_digester.as_ref().cloned().unwrap())
    }

    async fn build_immutable_file_digest_mapper(
        &mut self,
    ) -> Result<Arc<dyn ImmutableFileDigestMapper>> {
        let mapper = ImmutableFileDigestRepository::new(self.get_sqlite_connection().await?);

        Ok(Arc::new(mapper))
    }

    /// Get the [ImmutableFileDigestMapper] instance
    pub async fn get_immutable_file_digest_mapper(
        &mut self,
    ) -> Result<Arc<dyn ImmutableFileDigestMapper>> {
        if self.immutable_file_digest_mapper.is_none() {
            self.immutable_file_digest_mapper =
                Some(self.build_immutable_file_digest_mapper().await?);
        }

        Ok(self.immutable_file_digest_mapper.as_ref().cloned().unwrap())
    }

    async fn build_certificate_verifier(&mut self) -> Result<Arc<dyn CertificateVerifier>> {
        let verifier = Arc::new(MithrilCertificateVerifier::new(
            self.root_logger(),
            self.get_certificate_repository().await?,
        ));

        Ok(verifier)
    }

    /// Get the [CertificateVerifier] instance
    pub async fn get_certificate_verifier(&mut self) -> Result<Arc<dyn CertificateVerifier>> {
        if self.certificate_verifier.is_none() {
            self.certificate_verifier = Some(self.build_certificate_verifier().await?);
        }

        Ok(self.certificate_verifier.as_ref().cloned().unwrap())
    }

    async fn build_genesis_verifier(&mut self) -> Result<Arc<ProtocolGenesisVerifier>> {
        let genesis_verifier: ProtocolGenesisVerifier = match self.configuration.environment {
            ExecutionEnvironment::Production => ProtocolGenesisVerifier::from_verification_key(
                ProtocolGenesisVerificationKey::from_json_hex(
                    &self.configuration.genesis_verification_key,
                )
                .map_err(|e| DependenciesBuilderError::Initialization {
                    message: format!(
                        "Could not decode hex key to build genesis verifier: '{}'",
                        self.configuration.genesis_verification_key
                    ),
                    error: Some(e),
                })?,
            ),
            _ => ProtocolGenesisSigner::create_deterministic_genesis_signer()
                .create_genesis_verifier(),
        };

        Ok(Arc::new(genesis_verifier))
    }

    /// Get the [ProtocolGenesisVerifier] instance
    pub async fn get_genesis_verifier(&mut self) -> Result<Arc<ProtocolGenesisVerifier>> {
        if self.genesis_verifier.is_none() {
            self.genesis_verifier = Some(self.build_genesis_verifier().await?);
        }

        Ok(self.genesis_verifier.as_ref().cloned().unwrap())
    }

    async fn build_mithril_registerer(&mut self) -> Result<Arc<MithrilSignerRegisterer>> {
        let registerer = MithrilSignerRegisterer::new(
            self.get_chain_observer().await?,
            self.get_verification_key_store().await?,
            self.get_signer_store().await?,
            self.configuration.safe_epoch_retention_limit(),
        );

        Ok(Arc::new(registerer))
    }

    /// Get the [MithrilSignerRegisterer] instance
    pub async fn get_mithril_registerer(&mut self) -> Result<Arc<MithrilSignerRegisterer>> {
        if self.mithril_registerer.is_none() {
            self.mithril_registerer = Some(self.build_mithril_registerer().await?);
        }

        Ok(self.mithril_registerer.as_ref().cloned().unwrap())
    }

    async fn build_era_reader(&mut self) -> Result<Arc<EraReader>> {
        let era_adapter: Arc<dyn EraReaderAdapter> = match self.configuration.environment {
            ExecutionEnvironment::Production => EraReaderAdapterBuilder::new(
                &self.configuration.era_reader_adapter_type,
                &self.configuration.era_reader_adapter_params,
            )
            .build(self.get_chain_observer().await?)
            .map_err(|e| DependenciesBuilderError::Initialization {
                message: "Could not build EraReader as dependency.".to_string(),
                error: Some(e.into()),
            })?,
            _ => Arc::new(EraReaderDummyAdapter::from_markers(vec![EraMarker::new(
                &SupportedEra::dummy().to_string(),
                Some(Epoch(0)),
            )])),
        };

        Ok(Arc::new(EraReader::new(era_adapter)))
    }

    /// Get the [EraReader] instance
    pub async fn get_era_reader(&mut self) -> Result<Arc<EraReader>> {
        if self.era_reader.is_none() {
            self.era_reader = Some(self.build_era_reader().await?);
        }

        Ok(self.era_reader.as_ref().cloned().unwrap())
    }

    async fn build_era_checker(&mut self) -> Result<Arc<EraChecker>> {
        let current_epoch = self
            .get_ticker_service()
            .await?
            .get_current_epoch()
            .await
            .map_err(|e| DependenciesBuilderError::Initialization {
                message: "Error while building EraChecker".to_string(),
                error: Some(e),
            })?;
        let era_epoch_token = self
            .get_era_reader()
            .await?
            .read_era_epoch_token(current_epoch)
            .await
            .map_err(|e| DependenciesBuilderError::Initialization {
                message: "Error while building EraChecker".to_string(),
                error: Some(e.into()),
            })?;
        let era_checker = Arc::new(EraChecker::new(
            era_epoch_token.get_current_supported_era().map_err(|e| {
                DependenciesBuilderError::Initialization {
                    message: "Error while building EraChecker".to_string(),
                    error: Some(e),
                }
            })?,
            era_epoch_token.get_current_epoch(),
        ));

        Ok(era_checker)
    }

    /// Get the [EraChecker] instance
    pub async fn get_era_checker(&mut self) -> Result<Arc<EraChecker>> {
        if self.era_checker.is_none() {
            self.era_checker = Some(self.build_era_checker().await?);
        }

        Ok(self.era_checker.as_ref().cloned().unwrap())
    }

    async fn build_event_transmitter_channel(
        &mut self,
    ) -> Result<(
        UnboundedReceiver<EventMessage>,
        UnboundedSender<EventMessage>,
    )> {
        let (tx, rx) = tokio::sync::mpsc::unbounded_channel::<EventMessage>();

        Ok((rx, tx))
    }

    /// Return the EventMessage channel sender.
    pub async fn get_event_transmitter_sender(&mut self) -> Result<UnboundedSender<EventMessage>> {
        if let (_, None) = self.event_transmitter_channel {
            let (rx, tx) = self.build_event_transmitter_channel().await?;
            self.event_transmitter_channel = (Some(rx), Some(tx));
        }

        Ok(self
            .event_transmitter_channel
            .1
            .as_ref()
            .cloned()
            .expect("Transmitter<EventMessage> should be set."))
    }

    /// Return the channel receiver setup for the [EventStore]. Since this
    /// receiver is not clonable, it must be called only once.
    pub async fn get_event_transmitter_receiver(
        &mut self,
    ) -> Result<UnboundedReceiver<EventMessage>> {
        if let (_, None) = self.event_transmitter_channel {
            let (rx, tx) = self.build_event_transmitter_channel().await?;
            self.event_transmitter_channel = (Some(rx), Some(tx));
        }
        let mut receiver: Option<UnboundedReceiver<EventMessage>> = None;
        std::mem::swap(&mut self.event_transmitter_channel.0, &mut receiver);

        receiver.ok_or_else(|| {
            DependenciesBuilderError::InconsistentState(
                "Receiver<EventMessage> has already been given and is not clonable.".to_string(),
            )
        })
    }

    async fn build_api_version_provider(&mut self) -> Result<Arc<APIVersionProvider>> {
        let api_version_provider = Arc::new(APIVersionProvider::new(self.get_era_checker().await?));

        Ok(api_version_provider)
    }

    /// Get the [APIVersionProvider] instance
    pub async fn get_api_version_provider(&mut self) -> Result<Arc<APIVersionProvider>> {
        if self.api_version_provider.is_none() {
            self.api_version_provider = Some(self.build_api_version_provider().await?);
        }

        Ok(self.api_version_provider.as_ref().cloned().unwrap())
    }

    async fn build_signable_seed_builder(&mut self) -> Result<Arc<dyn SignableSeedBuilder>> {
        let signable_seed_builder_service = Arc::new(AggregatorSignableSeedBuilder::new(
            self.get_epoch_service().await?,
        ));

        Ok(signable_seed_builder_service)
    }

    /// Get the [SignableSeedBuilder] instance
    pub async fn get_signable_seed_builder(&mut self) -> Result<Arc<dyn SignableSeedBuilder>> {
        if self.signable_seed_builder.is_none() {
            self.signable_seed_builder = Some(self.build_signable_seed_builder().await?);
        }

        Ok(self.signable_seed_builder.as_ref().cloned().unwrap())
    }

    async fn build_signed_entity_lock(&mut self) -> Result<Arc<SignedEntityTypeLock>> {
        let signed_entity_type_lock = Arc::new(SignedEntityTypeLock::default());
        Ok(signed_entity_type_lock)
    }

    /// Get the [SignedEntityTypeLock] instance
    pub async fn get_signed_entity_lock(&mut self) -> Result<Arc<SignedEntityTypeLock>> {
        if self.signed_entity_type_lock.is_none() {
            self.signed_entity_type_lock = Some(self.build_signed_entity_lock().await?);
        }

        Ok(self.signed_entity_type_lock.as_ref().cloned().unwrap())
    }

    async fn build_transactions_importer(&mut self) -> Result<Arc<dyn TransactionsImporter>> {
        let transactions_importer = Arc::new(CardanoTransactionsImporter::new(
            self.get_block_scanner().await?,
            self.get_transaction_repository().await?,
            self.root_logger(),
        ));

        Ok(transactions_importer)
    }

    /// Get the [TransactionsImporter] instance
    pub async fn get_transactions_importer(&mut self) -> Result<Arc<dyn TransactionsImporter>> {
        if self.transactions_importer.is_none() {
            self.transactions_importer = Some(self.build_transactions_importer().await?);
        }

        Ok(self.transactions_importer.as_ref().cloned().unwrap())
    }

    async fn build_single_signature_authenticator(
        &mut self,
    ) -> Result<Arc<SingleSignatureAuthenticator>> {
        let authenticator =
            SingleSignatureAuthenticator::new(self.get_multi_signer().await?, self.root_logger());

        Ok(Arc::new(authenticator))
    }

    /// Get the [SingleSignatureAuthenticator] instance
    pub async fn get_single_signature_authenticator(
        &mut self,
    ) -> Result<Arc<SingleSignatureAuthenticator>> {
        if self.single_signer_authenticator.is_none() {
            self.single_signer_authenticator =
                Some(self.build_single_signature_authenticator().await?);
        }

        Ok(self.single_signer_authenticator.as_ref().cloned().unwrap())
    }

    /// Create dependencies for the [EventStore] task.
    pub async fn create_event_store(&mut self) -> Result<EventStore> {
        let event_store = EventStore::new(
            self.get_event_transmitter_receiver().await?,
            self.get_event_store_sqlite_connection().await?,
            self.root_logger(),
        );

        Ok(event_store)
    }

    /// Create a [CardanoTransactionsPreloader] instance.
    pub async fn create_cardano_transactions_preloader(
        &mut self,
    ) -> Result<Arc<CardanoTransactionsPreloader>> {
        let activation = self
            .get_allowed_signed_entity_types_discriminants()?
            .contains(&SignedEntityTypeDiscriminants::CardanoTransactions);
        let cardano_transactions_preloader = CardanoTransactionsPreloader::new(
            self.get_signed_entity_lock().await?,
            self.get_transactions_importer().await?,
            self.configuration
                .cardano_transactions_signing_config
                .security_parameter,
            self.get_chain_observer().await?,
            self.root_logger(),
            Arc::new(CardanoTransactionsPreloaderActivation::new(activation)),
        );

        Ok(Arc::new(cardano_transactions_preloader))
    }

    /// Create a [SignersImporter] instance.
    pub async fn create_signer_importer(
        &mut self,
        cexplorer_pools_url: &str,
    ) -> Result<SignersImporter> {
        let retriever = CExplorerSignerRetriever::new(
            cexplorer_pools_url,
            Some(Duration::from_secs(30)),
            self.root_logger(),
        )?;
        let persister = self.get_signer_store().await?;

        Ok(SignersImporter::new(
            Arc::new(retriever),
            persister,
            self.root_logger(),
        ))
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::SignedEntityTypeDiscriminants;

    use crate::Configuration;

    use super::*;

    #[tokio::test]
    async fn cardano_transactions_preloader_activated_with_cardano_transactions_signed_entity_type_in_configuration(
    ) {
        assert_cardano_transactions_preloader_activation(
            SignedEntityTypeDiscriminants::CardanoTransactions.to_string(),
            true,
        )
        .await;
        assert_cardano_transactions_preloader_activation(
            SignedEntityTypeDiscriminants::MithrilStakeDistribution.to_string(),
            false,
        )
        .await;
    }

    async fn assert_cardano_transactions_preloader_activation(
        signed_entity_types: String,
        expected_activation: bool,
    ) {
        let configuration = Configuration {
            signed_entity_types: Some(signed_entity_types),
            ..Configuration::new_sample()
        };
        let mut dep_builder = DependenciesBuilder::new_with_stdout_logger(configuration);

        let cardano_transactions_preloader = dep_builder
            .create_cardano_transactions_preloader()
            .await
            .unwrap();

        let is_activated = cardano_transactions_preloader.is_activated().await.unwrap();
        assert_eq!(
            expected_activation, is_activated,
            "'is_activated' expected {}, but was {}",
            expected_activation, is_activated
        );
    }
}

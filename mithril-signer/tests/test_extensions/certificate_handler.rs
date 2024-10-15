use anyhow::anyhow;
use async_trait::async_trait;
use std::collections::BTreeSet;
use std::{collections::HashMap, sync::Arc};
use tokio::sync::RwLock;

use mithril_common::{
    entities::{
        CardanoTransactionsSigningConfig, Epoch, ProtocolMessage, SignedEntityConfig,
        SignedEntityType, SignedEntityTypeDiscriminants, Signer, SingleSignatures, TimePoint,
    },
    messages::AggregatorFeaturesMessage,
    test_utils::fake_data,
    MithrilTickerService, TickerService,
};

use mithril_signer::{
    entities::SignerEpochSettings,
    services::{AggregatorClient, AggregatorClientError},
};

pub struct FakeAggregator {
    signed_entity_config: RwLock<SignedEntityConfig>,
    registered_signers: RwLock<HashMap<Epoch, Vec<Signer>>>,
    ticker_service: Arc<MithrilTickerService>,
    withhold_epoch_settings: RwLock<bool>,
}

impl FakeAggregator {
    pub fn new(
        signed_entity_config: SignedEntityConfig,
        ticker_service: Arc<MithrilTickerService>,
    ) -> Self {
        Self {
            signed_entity_config: RwLock::new(signed_entity_config),
            registered_signers: RwLock::new(HashMap::new()),
            ticker_service,
            withhold_epoch_settings: RwLock::new(true),
        }
    }

    pub async fn get_registered_signers(&self, epoch: &Epoch) -> Option<Vec<Signer>> {
        let store = self.registered_signers.read().await;
        store.get(epoch).cloned()
    }

    pub async fn release_epoch_settings(&self) {
        let mut settings = self.withhold_epoch_settings.write().await;
        *settings = false;
    }

    pub async fn change_allowed_discriminants(
        &self,
        discriminants: &BTreeSet<SignedEntityTypeDiscriminants>,
    ) {
        let mut signed_entity_config = self.signed_entity_config.write().await;
        signed_entity_config.allowed_discriminants = discriminants.clone();
    }

    pub async fn change_transaction_signing_config(
        &self,
        transaction_signing_config: &CardanoTransactionsSigningConfig,
    ) {
        let mut signed_entity_config = self.signed_entity_config.write().await;
        signed_entity_config.cardano_transactions_signing_config =
            transaction_signing_config.clone();
    }

    async fn get_time_point(&self) -> Result<TimePoint, AggregatorClientError> {
        let time_point = self
            .ticker_service
            .get_current_time_point()
            .await
            .map_err(|e| AggregatorClientError::RemoteServerTechnical(anyhow!(e)))?;

        Ok(time_point)
    }

    async fn get_current_signers(
        &self,
        store: &HashMap<Epoch, Vec<Signer>>,
    ) -> Result<Vec<Signer>, AggregatorClientError> {
        let time_point = self.get_time_point().await?;
        let epoch = time_point
            .epoch
            .offset_to_signer_retrieval_epoch()
            .map_err(|e| AggregatorClientError::RemoteServerTechnical(anyhow!(e)))?;

        Ok(store.get(&epoch).cloned().unwrap_or_default())
    }

    async fn get_next_signers(
        &self,
        store: &HashMap<Epoch, Vec<Signer>>,
    ) -> Result<Vec<Signer>, AggregatorClientError> {
        let time_point = self.get_time_point().await?;
        let epoch = time_point.epoch.offset_to_next_signer_retrieval_epoch();

        Ok(store.get(&epoch).cloned().unwrap_or_default())
    }
}

#[async_trait]
impl AggregatorClient for FakeAggregator {
    async fn retrieve_epoch_settings(
        &self,
    ) -> Result<Option<SignerEpochSettings>, AggregatorClientError> {
        if *self.withhold_epoch_settings.read().await {
            Ok(None)
        } else {
            let store = self.registered_signers.read().await;
            let signed_entity_config = self.signed_entity_config.read().await;
            let time_point = self.get_time_point().await?;
            let current_signers = self.get_current_signers(&store).await?;
            let next_signers = self.get_next_signers(&store).await?;

            Ok(Some(SignerEpochSettings {
                epoch: time_point.epoch,
                current_signers,
                next_signers,
                protocol_parameters: fake_data::protocol_parameters(),
                next_protocol_parameters: fake_data::protocol_parameters(),
                cardano_transactions_signing_config: Some(
                    signed_entity_config
                        .cardano_transactions_signing_config
                        .clone(),
                ),
                next_cardano_transactions_signing_config: None,
            }))
        }
    }

    /// Registers signer with the aggregator
    async fn register_signer(
        &self,
        epoch: Epoch,
        signer: &Signer,
    ) -> Result<(), AggregatorClientError> {
        let mut store = self.registered_signers.write().await;
        let mut signers = store.get(&epoch).cloned().unwrap_or_default();
        signers.push(signer.clone());
        let _ = store.insert(epoch, signers);

        Ok(())
    }

    /// Registers single signatures with the aggregator
    async fn register_signatures(
        &self,
        _signed_entity_type: &SignedEntityType,
        _signatures: &SingleSignatures,
        _protocol_message: &ProtocolMessage,
    ) -> Result<(), AggregatorClientError> {
        Ok(())
    }

    async fn retrieve_aggregator_features(
        &self,
    ) -> Result<AggregatorFeaturesMessage, AggregatorClientError> {
        let signed_entity_config = self.signed_entity_config.read().await;

        let mut message = AggregatorFeaturesMessage::dummy();
        message.capabilities.signed_entity_types =
            signed_entity_config.allowed_discriminants.clone();
        message.capabilities.cardano_transactions_signing_config = Some(
            signed_entity_config
                .cardano_transactions_signing_config
                .clone(),
        );

        Ok(message)
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::chain_observer::{ChainObserver, FakeObserver};
    use mithril_common::digesters::DumbImmutableFileObserver;
    use mithril_common::entities::{BlockNumber, ChainPoint};
    use mithril_common::test_utils::fake_data;

    use super::*;

    async fn init() -> (Arc<FakeObserver>, FakeAggregator) {
        let immutable_observer = Arc::new(DumbImmutableFileObserver::new());
        immutable_observer.shall_return(Some(1)).await;
        let chain_observer = Arc::new(FakeObserver::new(Some(TimePoint {
            epoch: Epoch(1),
            immutable_file_number: 1,
            chain_point: ChainPoint::dummy(),
        })));
        let ticker_service = Arc::new(MithrilTickerService::new(
            chain_observer.clone(),
            immutable_observer.clone(),
        ));

        (
            chain_observer,
            FakeAggregator::new(SignedEntityConfig::dummy(), ticker_service),
        )
    }

    #[tokio::test]
    async fn register_signer() {
        let (chain_observer, fake_aggregator) = init().await;
        let fake_signers = fake_data::signers(2);
        let epoch = chain_observer.get_current_epoch().await.unwrap().unwrap();
        let registration_epoch = Epoch(2);
        assert_eq!(2, fake_signers.len());

        fake_aggregator
            .register_signer(
                epoch.offset_to_recording_epoch(),
                &fake_signers.as_slice()[0],
            )
            .await
            .expect("aggregator client should not fail while registering a user");
        let signers = fake_aggregator
            .get_registered_signers(&registration_epoch)
            .await
            .expect("we should have a result, None found!");

        assert_eq!(1, signers.len());

        fake_aggregator
            .register_signer(
                epoch.offset_to_recording_epoch(),
                &fake_signers.as_slice()[1],
            )
            .await
            .expect("aggregator client should not fail while registering a user");
        let signers = fake_aggregator
            .get_registered_signers(&registration_epoch)
            .await
            .expect("we should have a result, None found!");

        assert_eq!(2, signers.len());
    }

    #[tokio::test]
    async fn retrieve_epoch_settings() {
        let (chain_observer, fake_aggregator) = init().await;
        let fake_signers = fake_data::signers(3);
        let epoch = chain_observer.get_current_epoch().await.unwrap().unwrap();

        fake_aggregator.release_epoch_settings().await;

        fake_aggregator
            .register_signer(epoch, &fake_signers.as_slice()[0])
            .await
            .expect("aggregator client should not fail while registering a user");
        let epoch_settings = fake_aggregator
            .retrieve_epoch_settings()
            .await
            .expect("we should have a result, None found!")
            .expect("we should have an EpochSettings, None found!");

        assert_eq!(0, epoch_settings.current_signers.len());
        assert_eq!(1, epoch_settings.next_signers.len());

        fake_aggregator
            .register_signer(epoch, &fake_signers.as_slice()[1])
            .await
            .expect("aggregator client should not fail while registering a user");
        let epoch_settings = fake_aggregator
            .retrieve_epoch_settings()
            .await
            .expect("we should have a result, None found!")
            .expect("we should have an EpochSettings, None found!");

        assert_eq!(0, epoch_settings.current_signers.len());
        assert_eq!(2, epoch_settings.next_signers.len());

        let epoch = chain_observer.next_epoch().await.unwrap();
        fake_aggregator
            .register_signer(epoch, &fake_signers.as_slice()[2])
            .await
            .expect("aggregator client should not fail while registering a user");
        let epoch_settings = fake_aggregator
            .retrieve_epoch_settings()
            .await
            .expect("we should have a result, None found!")
            .expect("we should have an EpochSettings, None found!");

        assert_eq!(2, epoch_settings.current_signers.len());
        assert_eq!(1, epoch_settings.next_signers.len());
    }

    #[tokio::test]
    async fn retrieve_aggregator_features() {
        let (_chain_observer, fake_aggregator) = init().await;

        {
            let mut signing_config = fake_aggregator.signed_entity_config.write().await;
            signing_config.allowed_discriminants = SignedEntityTypeDiscriminants::all();
            signing_config.cardano_transactions_signing_config =
                CardanoTransactionsSigningConfig::dummy();
        }

        let features = fake_aggregator
            .retrieve_aggregator_features()
            .await
            .unwrap();
        assert_eq!(
            &SignedEntityTypeDiscriminants::all(),
            &features.capabilities.signed_entity_types,
        );

        let new_discriminants = BTreeSet::from([
            SignedEntityTypeDiscriminants::CardanoTransactions,
            SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
        ]);
        let new_transaction_signing_config = CardanoTransactionsSigningConfig {
            security_parameter: BlockNumber(70),
            step: BlockNumber(20),
        };

        fake_aggregator
            .change_allowed_discriminants(&new_discriminants)
            .await;
        fake_aggregator
            .change_transaction_signing_config(&new_transaction_signing_config)
            .await;

        let updated_features = fake_aggregator
            .retrieve_aggregator_features()
            .await
            .unwrap();
        assert_eq!(
            &new_discriminants,
            &updated_features.capabilities.signed_entity_types,
        );
        assert_eq!(
            &Some(new_transaction_signing_config),
            &updated_features
                .capabilities
                .cardano_transactions_signing_config,
        );
    }
}

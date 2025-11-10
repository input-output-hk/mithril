use async_trait::async_trait;
use std::{collections::HashMap, sync::Arc};
use tokio::sync::RwLock;

use mithril_common::{
    StdResult,
    entities::{Epoch, ProtocolMessage, SignedEntityType, Signer, SingleSignature, TimePoint},
    test::double::Dummy,
};
use mithril_ticker::{MithrilTickerService, TickerService};

use mithril_signer::services::{SignaturePublisher, SignerRegistrationPublisher};
use mithril_signer::{entities::RegisteredSigners, services::AggregatorClient};

pub struct FakeAggregator {
    registered_signers: RwLock<HashMap<Epoch, Vec<Signer>>>,
    ticker_service: Arc<MithrilTickerService>,
    withhold_epoch_settings: RwLock<bool>,
}

impl FakeAggregator {
    pub fn new(ticker_service: Arc<MithrilTickerService>) -> Self {
        Self {
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

    async fn get_time_point(&self) -> StdResult<TimePoint> {
        let time_point = self.ticker_service.get_current_time_point().await?;
        Ok(time_point)
    }

    async fn get_current_signers(
        &self,
        store: &HashMap<Epoch, Vec<Signer>>,
    ) -> StdResult<Vec<Signer>> {
        let time_point = self.get_time_point().await?;
        let epoch = time_point.epoch.offset_to_signer_retrieval_epoch()?;

        Ok(store.get(&epoch).cloned().unwrap_or_default())
    }

    async fn get_next_signers(
        &self,
        store: &HashMap<Epoch, Vec<Signer>>,
    ) -> StdResult<Vec<Signer>> {
        let time_point = self.get_time_point().await?;
        let epoch = time_point.epoch.offset_to_next_signer_retrieval_epoch();

        Ok(store.get(&epoch).cloned().unwrap_or_default())
    }
}

#[async_trait]
impl SignaturePublisher for FakeAggregator {
    async fn publish(
        &self,
        _signed_entity_type: &SignedEntityType,
        _signature: &SingleSignature,
        _protocol_message: &ProtocolMessage,
    ) -> StdResult<()> {
        Ok(())
    }
}

#[async_trait]
impl SignerRegistrationPublisher for FakeAggregator {
    async fn register_signer(&self, epoch: Epoch, signer: &Signer) -> StdResult<()> {
        let mut store = self.registered_signers.write().await;
        let mut signers = store.get(&epoch).cloned().unwrap_or_default();
        signers.push(signer.clone());
        let _ = store.insert(epoch, signers);

        Ok(())
    }
}

#[async_trait]
impl AggregatorClient for FakeAggregator {
    async fn retrieve_all_signer_registrations(&self) -> StdResult<Option<RegisteredSigners>> {
        if *self.withhold_epoch_settings.read().await {
            Ok(None)
        } else {
            let store = self.registered_signers.read().await;
            let time_point = self.get_time_point().await?;
            let current_signers = self.get_current_signers(&store).await?;
            let next_signers = self.get_next_signers(&store).await?;

            Ok(Some(RegisteredSigners {
                epoch: time_point.epoch,
                current_signers,
                next_signers,
            }))
        }
    }
}

#[cfg(test)]
mod tests {
    use mithril_cardano_node_chain::chain_observer::ChainObserver;
    use mithril_cardano_node_chain::test::double::FakeChainObserver;
    use mithril_cardano_node_internal_database::test::double::DumbImmutableFileObserver;
    use mithril_common::entities::ChainPoint;
    use mithril_common::test::double::fake_data;

    use super::*;

    async fn init() -> (Arc<FakeChainObserver>, FakeAggregator) {
        let immutable_observer = Arc::new(DumbImmutableFileObserver::new());
        immutable_observer.shall_return(Some(1)).await;
        let chain_observer = Arc::new(FakeChainObserver::new(Some(TimePoint {
            epoch: Epoch(1),
            immutable_file_number: 1,
            chain_point: ChainPoint::dummy(),
        })));
        let ticker_service = Arc::new(MithrilTickerService::new(
            chain_observer.clone(),
            immutable_observer.clone(),
        ));

        (chain_observer, FakeAggregator::new(ticker_service))
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
            .retrieve_all_signer_registrations()
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
            .retrieve_all_signer_registrations()
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
            .retrieve_all_signer_registrations()
            .await
            .expect("we should have a result, None found!")
            .expect("we should have an EpochSettings, None found!");

        assert_eq!(2, epoch_settings.current_signers.len());
        assert_eq!(1, epoch_settings.next_signers.len());
    }
}

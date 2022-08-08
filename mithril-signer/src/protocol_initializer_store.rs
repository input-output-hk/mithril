use async_trait::async_trait;
use thiserror::Error;
use tokio::sync::RwLock;

use mithril_common::crypto_helper::ProtocolInitializer;
use mithril_common::entities::Epoch;
use mithril_common::store::adapter::{AdapterError, StoreAdapter};

type Adapter = Box<dyn StoreAdapter<Key = Epoch, Record = ProtocolInitializer>>;

#[derive(Debug, Error)]
pub enum ProtocolInitializerStoreError {
    #[error("adapter error {0}")]
    AdapterError(#[from] AdapterError),
}

#[async_trait]
pub trait ProtocolInitializerStorer: Sync + Send {
    async fn save_protocol_initializer(
        &self,
        epoch: Epoch,
        protocol_initializer: ProtocolInitializer,
    ) -> Result<Option<ProtocolInitializer>, ProtocolInitializerStoreError>;

    async fn get_protocol_initializer(
        &self,
        epoch: Epoch,
    ) -> Result<Option<ProtocolInitializer>, ProtocolInitializerStoreError>;
}
pub struct ProtocolInitializerStore {
    adapter: RwLock<Adapter>,
}

impl ProtocolInitializerStore {
    pub fn new(adapter: Adapter) -> Self {
        Self {
            adapter: RwLock::new(adapter),
        }
    }
}

#[async_trait]
impl ProtocolInitializerStorer for ProtocolInitializerStore {
    async fn save_protocol_initializer(
        &self,
        epoch: Epoch,
        protocol_initializer: ProtocolInitializer,
    ) -> Result<Option<ProtocolInitializer>, ProtocolInitializerStoreError> {
        let previous_protocol_initializer = self.adapter.read().await.get_record(&epoch).await?;
        self.adapter
            .write()
            .await
            .store_record(&epoch, &protocol_initializer)
            .await?;

        Ok(previous_protocol_initializer)
    }

    async fn get_protocol_initializer(
        &self,
        epoch: Epoch,
    ) -> Result<Option<ProtocolInitializer>, ProtocolInitializerStoreError> {
        let record = self.adapter.read().await.get_record(&epoch).await?;
        Ok(record)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use mithril_common::{fake_data, store::adapter::MemoryAdapter};

    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    fn setup_protocol_initializers(nb_epoch: u64) -> Vec<(Epoch, ProtocolInitializer)> {
        let mut values: Vec<(Epoch, ProtocolInitializer)> = Vec::new();
        for epoch in 1..=nb_epoch {
            let protocol_parameters = fake_data::protocol_parameters();
            let party_id = format!("{:<032}", 1);
            let stake = (epoch + 1) * 100;
            let seed: [u8; 32] = party_id.as_bytes()[..32].try_into().unwrap();
            let mut rng = ChaCha20Rng::from_seed(seed);
            let protocol_initializer =
                ProtocolInitializer::setup(protocol_parameters.into(), stake, &mut rng);
            values.push((Epoch(epoch), protocol_initializer));
        }
        values
    }

    fn init_store(nb_epoch: u64) -> ProtocolInitializerStore {
        let values = setup_protocol_initializers(nb_epoch);

        let values = if !values.is_empty() {
            Some(values)
        } else {
            None
        };
        let adapter: MemoryAdapter<Epoch, ProtocolInitializer> =
            MemoryAdapter::new(values).unwrap();
        ProtocolInitializerStore::new(Box::new(adapter))
    }

    #[tokio::test]
    async fn save_key_in_empty_store() {
        let protocol_initializers = setup_protocol_initializers(1);
        let store = init_store(0);
        let res = store
            .save_protocol_initializer(
                protocol_initializers[0].0.clone(),
                (&protocol_initializers[0].1).to_owned(),
            )
            .await
            .unwrap();

        assert!(res.is_none());
    }

    #[tokio::test]
    async fn update_protocol_initializer_in_store() {
        let protocol_initializers = setup_protocol_initializers(2);
        let store = init_store(1);
        let res = store
            .save_protocol_initializer(
                protocol_initializers[0].0.clone(),
                (&protocol_initializers[1].1).to_owned(),
            )
            .await
            .unwrap();

        assert!(res.is_some());
        assert_eq!(protocol_initializers[0].1.stake, res.unwrap().stake);
    }

    #[tokio::test]
    async fn get_protocol_initializer_for_empty_epoch() {
        let store = init_store(2);
        let res = store.get_protocol_initializer(Epoch(0)).await.unwrap();

        assert!(res.is_none());
    }

    #[tokio::test]
    async fn get_protocol_initializer_for_existing_epoch() {
        let store = init_store(2);
        let res = store.get_protocol_initializer(Epoch(1)).await.unwrap();

        assert!(res.is_some());
    }
}

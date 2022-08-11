use async_trait::async_trait;
use thiserror::Error;
use tokio::sync::RwLock;

use mithril_common::entities::{Epoch, ProtocolParameters};
use mithril_common::store::adapter::{AdapterError, StoreAdapter};

type Adapter = Box<dyn StoreAdapter<Key = Epoch, Record = ProtocolParameters>>;

#[derive(Debug, Error)]
pub enum ProtocolParametersStoreError {
    #[error("adapter error {0}")]
    AdapterError(#[from] AdapterError),
}

#[async_trait]
pub trait ProtocolParametersStorer {
    async fn save_protocol_parameters(
        &self,
        epoch: Epoch,
        protocol_parameters: ProtocolParameters,
    ) -> Result<Option<ProtocolParameters>, ProtocolParametersStoreError>;

    async fn get_protocol_parameters(
        &self,
        epoch: Epoch,
    ) -> Result<Option<ProtocolParameters>, ProtocolParametersStoreError>;
}
pub struct ProtocolParametersStore {
    adapter: RwLock<Adapter>,
}

impl ProtocolParametersStore {
    pub fn new(adapter: Adapter) -> Self {
        Self {
            adapter: RwLock::new(adapter),
        }
    }
}

#[async_trait]
impl ProtocolParametersStorer for ProtocolParametersStore {
    async fn save_protocol_parameters(
        &self,
        epoch: Epoch,
        protocol_parameters: ProtocolParameters,
    ) -> Result<Option<ProtocolParameters>, ProtocolParametersStoreError> {
        let previous_protocol_parameters = self.adapter.read().await.get_record(&epoch).await?;
        self.adapter
            .write()
            .await
            .store_record(&epoch, &protocol_parameters)
            .await?;

        Ok(previous_protocol_parameters)
    }

    async fn get_protocol_parameters(
        &self,
        epoch: Epoch,
    ) -> Result<Option<ProtocolParameters>, ProtocolParametersStoreError> {
        let record = self.adapter.read().await.get_record(&epoch).await?;
        Ok(record)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use mithril_common::{fake_data, store::adapter::MemoryAdapter};

    fn setup_protocol_parameters(nb_epoch: u64) -> Vec<(Epoch, ProtocolParameters)> {
        let mut values: Vec<(Epoch, ProtocolParameters)> = Vec::new();
        for epoch in 1..=nb_epoch {
            let mut protocol_parameters = fake_data::protocol_parameters();
            protocol_parameters.k += epoch;
            values.push((Epoch(epoch), protocol_parameters));
        }
        values
    }

    fn init_store(nb_epoch: u64) -> ProtocolParametersStore {
        let values = setup_protocol_parameters(nb_epoch);

        let values = if !values.is_empty() {
            Some(values)
        } else {
            None
        };
        let adapter: MemoryAdapter<Epoch, ProtocolParameters> = MemoryAdapter::new(values).unwrap();
        ProtocolParametersStore::new(Box::new(adapter))
    }

    #[tokio::test]
    async fn save_key_in_empty_store() {
        let protocol_parameters = setup_protocol_parameters(1);
        let store = init_store(0);
        let res = store
            .save_protocol_parameters(
                protocol_parameters[0].0.clone(),
                (&protocol_parameters[0].1).to_owned(),
            )
            .await
            .unwrap();

        assert!(res.is_none());
    }

    #[tokio::test]
    async fn update_protocol_parameters_in_store() {
        let protocol_parameters = setup_protocol_parameters(2);
        let store = init_store(1);
        let res = store
            .save_protocol_parameters(
                protocol_parameters[0].0.clone(),
                (&protocol_parameters[1].1).to_owned(),
            )
            .await
            .unwrap();

        assert_eq!(protocol_parameters[0].1, res.unwrap());
    }

    #[tokio::test]
    async fn get_protocol_parameters_for_empty_epoch() {
        let store = init_store(2);
        let res = store.get_protocol_parameters(Epoch(0)).await.unwrap();

        assert!(res.is_none());
    }

    #[tokio::test]
    async fn get_protocol_parameters_for_existing_epoch() {
        let store = init_store(2);
        let res = store.get_protocol_parameters(Epoch(1)).await.unwrap();

        assert!(res.is_some());
    }
}

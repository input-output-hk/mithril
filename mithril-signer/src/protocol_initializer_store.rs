use async_trait::async_trait;
use mithril_common::store::{StoreError, StorePruner};
use tokio::sync::RwLock;

use mithril_common::crypto_helper::ProtocolInitializer;
use mithril_common::entities::Epoch;
use mithril_common::store::adapter::StoreAdapter;

type Adapter = Box<dyn StoreAdapter<Key = Epoch, Record = ProtocolInitializer>>;

#[async_trait]
/// Store the ProtocolInitializer used for each Epoch. This is useful because
/// protocol parameters and stake distribution change over time.
pub trait ProtocolInitializerStorer: Sync + Send {
    /// Save a protocol initializer for the given Epoch.
    async fn save_protocol_initializer(
        &self,
        epoch: Epoch,
        protocol_initializer: ProtocolInitializer,
    ) -> Result<Option<ProtocolInitializer>, StoreError>;

    /// Fetch a protocol initializer if any saved for the given Epoch.
    async fn get_protocol_initializer(
        &self,
        epoch: Epoch,
    ) -> Result<Option<ProtocolInitializer>, StoreError>;

    /// Return the list of the N last saved protocol initializers if any.
    async fn get_last_protocol_initializer(
        &self,
        last: usize,
    ) -> Result<Vec<(Epoch, ProtocolInitializer)>, StoreError>;
}
/// Implementation of the ProtocolInitializerStorer
pub struct ProtocolInitializerStore {
    adapter: RwLock<Adapter>,
    retention_limit: Option<usize>,
}

impl ProtocolInitializerStore {
    /// Create a new ProtocolInitializerStore.
    pub fn new(adapter: Adapter, retention_limit: Option<usize>) -> Self {
        Self {
            adapter: RwLock::new(adapter),
            retention_limit,
        }
    }
}

#[async_trait]
impl StorePruner for ProtocolInitializerStore {
    type Key = Epoch;
    type Record = ProtocolInitializer;

    fn get_adapter(
        &self,
    ) -> &RwLock<Box<dyn StoreAdapter<Key = Self::Key, Record = Self::Record>>> {
        &self.adapter
    }

    fn get_max_records(&self) -> Option<usize> {
        self.retention_limit
    }
}

#[async_trait]
impl ProtocolInitializerStorer for ProtocolInitializerStore {
    async fn save_protocol_initializer(
        &self,
        epoch: Epoch,
        protocol_initializer: ProtocolInitializer,
    ) -> Result<Option<ProtocolInitializer>, StoreError> {
        let previous_protocol_initializer = self.adapter.read().await.get_record(&epoch).await?;
        self.adapter
            .write()
            .await
            .store_record(&epoch, &protocol_initializer)
            .await?;
        self.prune().await?;

        Ok(previous_protocol_initializer)
    }

    async fn get_protocol_initializer(
        &self,
        epoch: Epoch,
    ) -> Result<Option<ProtocolInitializer>, StoreError> {
        let record = self.adapter.read().await.get_record(&epoch).await?;
        Ok(record)
    }

    async fn get_last_protocol_initializer(
        &self,
        last: usize,
    ) -> Result<Vec<(Epoch, ProtocolInitializer)>, StoreError> {
        let records = self.adapter.read().await.get_last_n_records(last).await?;

        Ok(records)
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;

    use mithril_common::{store::adapter::MemoryAdapter, test_utils::fake_data};

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
            let kes_secret_key_path: Option<PathBuf> = None;
            let kes_period = Some(0);
            let protocol_initializer = ProtocolInitializer::setup(
                protocol_parameters.into(),
                kes_secret_key_path,
                kes_period,
                stake,
                &mut rng,
            )
            .expect("protocol initializer should not fail");
            values.push((Epoch(epoch), protocol_initializer));
        }
        values
    }

    fn init_store(nb_epoch: u64, retention_limit: Option<usize>) -> ProtocolInitializerStore {
        let values = setup_protocol_initializers(nb_epoch);

        let values = if !values.is_empty() {
            Some(values)
        } else {
            None
        };
        let adapter: MemoryAdapter<Epoch, ProtocolInitializer> =
            MemoryAdapter::new(values).unwrap();
        ProtocolInitializerStore::new(Box::new(adapter), retention_limit)
    }

    #[tokio::test]
    async fn save_key_in_empty_store() {
        let protocol_initializers = setup_protocol_initializers(1);
        let store = init_store(0, None);
        let res = store
            .save_protocol_initializer(
                protocol_initializers[0].0,
                protocol_initializers[0].1.clone(),
            )
            .await
            .unwrap();

        assert!(res.is_none());
    }

    #[tokio::test]
    async fn update_protocol_initializer_in_store() {
        let protocol_initializers = setup_protocol_initializers(2);
        let store = init_store(1, None);
        let res = store
            .save_protocol_initializer(
                protocol_initializers[0].0,
                protocol_initializers[1].1.clone(),
            )
            .await
            .unwrap();

        assert!(res.is_some());
        assert_eq!(
            protocol_initializers[0].1.get_stake(),
            res.unwrap().get_stake()
        );
    }

    #[tokio::test]
    async fn get_protocol_initializer_for_empty_epoch() {
        let store = init_store(2, None);
        let res = store.get_protocol_initializer(Epoch(0)).await.unwrap();

        assert!(res.is_none());
    }

    #[tokio::test]
    async fn get_protocol_initializer_for_existing_epoch() {
        let store = init_store(2, None);
        let res = store.get_protocol_initializer(Epoch(1)).await.unwrap();

        assert!(res.is_some());
    }

    #[tokio::test]
    async fn check_retention_limit() {
        let store = init_store(4, Some(2));
        let protocol_initializers = setup_protocol_initializers(1);
        let _ = store
            .save_protocol_initializer(
                protocol_initializers[0].0,
                protocol_initializers[0].1.clone(),
            )
            .await
            .unwrap();

        assert!(store
            .get_protocol_initializer(Epoch(1))
            .await
            .unwrap()
            .is_none());
    }
}

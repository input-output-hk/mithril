use async_trait::async_trait;
use std::collections::HashMap;
use thiserror::Error;

use crate::entities::{Epoch, PartyId};

use super::super::entities::SignerWithStake;
use super::adapter::{AdapterError, StoreAdapter};

type Adapter = Box<dyn StoreAdapter<Key = Epoch, Record = HashMap<PartyId, SignerWithStake>>>;

/// [StakeStorer] related errors.
#[derive(Debug, Error)]
pub enum StakeStoreError {
    /// Error raised when the underlying [adapter][StoreAdapter] raise an error.
    #[error("adapter error {0}")]
    AdapterError(#[from] AdapterError),
}

/// Represent a way to store the stake of mithril party members.
#[async_trait]
pub trait StakeStorer {
    /// Save the stake of a party in the store for a given `epoch`.
    async fn save_stake(
        &mut self,
        epoch: Epoch,
        signer: SignerWithStake,
    ) -> Result<Option<SignerWithStake>, StakeStoreError>;

    /// Get the stakes of all party at a given `epoch`.
    async fn get_stakes(
        &self,
        epoch: Epoch,
    ) -> Result<Option<HashMap<PartyId, SignerWithStake>>, StakeStoreError>;
}

/// A [StakeStorer] that use a [StoreAdapter] to store data.
pub struct StakeStore {
    adapter: Adapter,
}

impl StakeStore {
    /// StakeStore factory
    pub fn new(adapter: Adapter) -> Self {
        Self { adapter }
    }
}

#[async_trait]
impl StakeStorer for StakeStore {
    async fn save_stake(
        &mut self,
        epoch: Epoch,
        signer: SignerWithStake,
    ) -> Result<Option<SignerWithStake>, StakeStoreError> {
        let mut signers = match self.adapter.get_record(&epoch).await? {
            Some(s) => s,
            None => HashMap::new(),
        };
        let prev_signer = signers.insert(signer.party_id.clone(), signer);
        let _ = self.adapter.store_record(&epoch, &signers).await?;

        Ok(prev_signer)
    }

    async fn get_stakes(
        &self,
        epoch: Epoch,
    ) -> Result<Option<HashMap<PartyId, SignerWithStake>>, StakeStoreError> {
        Ok(self.adapter.get_record(&epoch).await?)
    }
}

#[cfg(test)]
mod tests {
    use super::super::adapter::MemoryAdapter;
    use super::*;

    fn init_store(nb_epoch: u64, signers_per_epoch: u64) -> StakeStore {
        let mut values: Vec<(Epoch, HashMap<PartyId, SignerWithStake>)> = Vec::new();

        for epoch in 1..=nb_epoch {
            let mut signers: HashMap<PartyId, SignerWithStake> = HashMap::new();

            for party_idx in 1..=signers_per_epoch {
                let party_id = format!("{}", party_idx);
                let _ = signers.insert(
                    party_id.clone(),
                    SignerWithStake {
                        party_id,
                        verification_key: "".to_string(),
                        stake: 100 * party_idx + 1,
                    },
                );
            }
            values.push((epoch, signers));
        }

        let values = if !values.is_empty() { Some(values) } else { None };
        let adapter: MemoryAdapter<u64, HashMap<PartyId, SignerWithStake>> =
            MemoryAdapter::new(values).unwrap();
        

        StakeStore::new(Box::new(adapter))
    }

    #[tokio::test]
    async fn save_key_in_empty_store() {
        let mut store = init_store(0, 0);
        let res = store
            .save_stake(
                0,
                SignerWithStake {
                    party_id: "0".to_string(),
                    verification_key: "".to_string(),
                    stake: 123,
                },
            )
            .await
            .unwrap();

        assert!(res.is_none());
    }

    #[tokio::test]
    async fn update_signer_in_store() {
        let mut store = init_store(1, 1);
        let res = store
            .save_stake(
                1,
                SignerWithStake {
                    party_id: "1".to_string(),
                    verification_key: "".to_string(),
                    stake: 123,
                },
            )
            .await
            .unwrap();

        assert!(res.is_some());
        assert_eq!(
            SignerWithStake {
                party_id: "1".to_string(),
                verification_key: "".to_string(),
                stake: 101,
            },
            res.unwrap(),
        );
    }

    #[tokio::test]
    async fn get_stakes_for_empty_epoch() {
        let store = init_store(2, 1);
        let res = store.get_stakes(0).await.unwrap();

        assert!(res.is_none());
    }

    #[tokio::test]
    async fn get_stakes_for_existing_epoch() {
        let store = init_store(2, 2);
        let res = store.get_stakes(1).await.unwrap();

        assert!(res.is_some());
        assert_eq!(2, res.unwrap().len());
    }
}

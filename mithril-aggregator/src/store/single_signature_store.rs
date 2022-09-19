use async_trait::async_trait;
use mithril_common::store::LimitKeyStore;
use std::collections::HashMap;
use tokio::sync::RwLock;

use mithril_common::entities::{Beacon, PartyId, SingleSignatures};
use mithril_common::store::{adapter::StoreAdapter, StoreError};

type Adapter = Box<dyn StoreAdapter<Key = Beacon, Record = HashMap<PartyId, SingleSignatures>>>;

/// Trait for mocking [SingleSignatureStore].
#[async_trait]
pub trait SingleSignatureStorer {
    /// Save the given [SingleSignatures] for the given [Beacon].
    async fn save_single_signatures(
        &self,
        beacon: &Beacon,
        single_signature: &SingleSignatures,
    ) -> Result<Option<SingleSignatures>, StoreError>;

    /// Get the [SingleSignatures] for the given [Beacon] if any.
    async fn get_single_signatures(
        &self,
        beacon: &Beacon,
    ) -> Result<Option<HashMap<PartyId, SingleSignatures>>, StoreError>;
}

/// Store for [SingleSignatures].
pub struct SingleSignatureStore {
    adapter: RwLock<Adapter>,
    retention_limit: Option<usize>,
}

impl SingleSignatureStore {
    /// Create a new instance.
    pub fn new(adapter: Adapter, retention_limit: Option<usize>) -> Self {
        Self {
            adapter: RwLock::new(adapter),
            retention_limit,
        }
    }
}

#[async_trait]
impl LimitKeyStore for SingleSignatureStore {
    type Key = Beacon;
    type Record = HashMap<PartyId, SingleSignatures>;

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
impl SingleSignatureStorer for SingleSignatureStore {
    async fn save_single_signatures(
        &self,
        beacon: &Beacon,
        single_signatures: &SingleSignatures,
    ) -> Result<Option<SingleSignatures>, StoreError> {
        let mut single_signatures_per_party_id = self
            .adapter
            .read()
            .await
            .get_record(beacon)
            .await?
            .unwrap_or_default();

        let prev_single_signature = single_signatures_per_party_id.insert(
            single_signatures.party_id.clone(),
            single_signatures.to_owned(),
        );
        self.adapter
            .write()
            .await
            .store_record(beacon, &single_signatures_per_party_id)
            .await?;
        self.apply_retention().await?;

        Ok(prev_single_signature)
    }

    async fn get_single_signatures(
        &self,
        beacon: &Beacon,
    ) -> Result<Option<HashMap<PartyId, SingleSignatures>>, StoreError> {
        let record = self.adapter.read().await.get_record(beacon).await?;
        Ok(record)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use mithril_common::store::adapter::MemoryAdapter;

    fn init_store(
        nb_immutable_file_numbers: u64,
        single_signers_per_epoch: u64,
        single_signatures_per_signers: u64,
        retention_limit: Option<usize>,
    ) -> SingleSignatureStore {
        let mut values: Vec<(Beacon, HashMap<PartyId, SingleSignatures>)> = Vec::new();

        for immutable_file_number in 1..=nb_immutable_file_numbers {
            let mut single_signatures: HashMap<PartyId, SingleSignatures> = HashMap::new();
            for party_idx in 1..=single_signers_per_epoch {
                let party_id = format!("{}", party_idx);
                let lottery_idx = (1..=single_signatures_per_signers).collect();
                single_signatures.insert(
                    party_id.clone(),
                    SingleSignatures {
                        party_id,
                        signature: format!("single-signature {} {:?}", party_idx, lottery_idx),
                        won_indexes: lottery_idx,
                    },
                );
            }
            values.push((
                Beacon::new("devnet".to_string(), 1, immutable_file_number),
                single_signatures,
            ));
        }

        let values = if !values.is_empty() {
            Some(values)
        } else {
            None
        };
        let adapter: MemoryAdapter<Beacon, HashMap<PartyId, SingleSignatures>> =
            MemoryAdapter::new(values).unwrap();
        SingleSignatureStore::new(Box::new(adapter), retention_limit)
    }

    #[tokio::test]
    async fn save_key_in_empty_store() {
        let store = init_store(0, 0, 0, None);
        let res = store
            .save_single_signatures(
                &Beacon::new("devnet".to_string(), 1, 0),
                &SingleSignatures {
                    party_id: "0".to_string(),
                    signature: "OK".to_string(),
                    won_indexes: vec![1, 5],
                },
            )
            .await
            .unwrap();

        assert!(res.is_none());
    }

    #[tokio::test]
    async fn add_signature_for_new_party_id() {
        let store = init_store(1, 1, 5, None);
        let beacon = Beacon::new("devnet".to_string(), 1, 1);

        assert_eq!(
            None,
            store
                .save_single_signatures(
                    &beacon,
                    &SingleSignatures {
                        party_id: "10".to_string(),
                        signature: "test".to_string(),
                        won_indexes: (1..5).collect(),
                    },
                )
                .await
                .unwrap()
        );
        assert_eq!(
            Some(&SingleSignatures {
                party_id: "10".to_string(),
                signature: "test".to_string(),
                won_indexes: (1..5).collect(),
            }),
            store
                .get_single_signatures(&beacon)
                .await
                .unwrap()
                .unwrap()
                .get("10")
        );
    }

    #[tokio::test]
    async fn get_single_signatures_for_empty_epoch() {
        let store = init_store(2, 1, 1, None);
        let res = store
            .get_single_signatures(&Beacon::new("devnet".to_string(), 1, 0))
            .await
            .unwrap();

        assert!(res.is_none());
    }

    #[tokio::test]
    async fn get_single_signatures_for_existing_epoch() {
        let store = init_store(2, 2, 2, None);
        let res = store
            .get_single_signatures(&Beacon::new("devnet".to_string(), 1, 1))
            .await
            .unwrap();

        assert!(res.is_some());
        assert_eq!(2, res.unwrap().len());
    }

    #[tokio::test]
    async fn using_retention_limit() {
        let store = init_store(2, 2, 2, Some(2));
        let beacon = Beacon::new("devnet".to_string(), 1, 1);

        assert!(store
            .get_single_signatures(&beacon)
            .await
            .unwrap()
            .is_some());
        let _ = store
            .save_single_signatures(
                &Beacon::new("devnet".to_string(), 2, 3),
                &SingleSignatures {
                    party_id: "0".to_string(),
                    signature: "OK".to_string(),
                    won_indexes: vec![1, 5],
                },
            )
            .await
            .unwrap();

        assert!(store
            .get_single_signatures(&beacon)
            .await
            .unwrap()
            .is_none());
    }
}

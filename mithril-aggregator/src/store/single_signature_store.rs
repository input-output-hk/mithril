use async_trait::async_trait;
use std::collections::HashMap;
use tokio::sync::RwLock;

use mithril_common::entities::{Beacon, PartyId, SingleSignatures};
use mithril_common::store::{adapter::StoreAdapter, StoreError};

type Adapter = Box<dyn StoreAdapter<Key = Beacon, Record = HashMap<PartyId, SingleSignatures>>>;

#[async_trait]
pub trait SingleSignatureStorer {
    async fn save_single_signatures(
        &self,
        beacon: &Beacon,
        single_signature: &SingleSignatures,
    ) -> Result<Option<SingleSignatures>, StoreError>;

    async fn get_single_signatures(
        &self,
        beacon: &Beacon,
    ) -> Result<Option<HashMap<PartyId, SingleSignatures>>, StoreError>;
}

pub struct SingleSignatureStore {
    adapter: RwLock<Adapter>,
}

impl SingleSignatureStore {
    pub fn new(adapter: Adapter) -> Self {
        Self {
            adapter: RwLock::new(adapter),
        }
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
        SingleSignatureStore::new(Box::new(adapter))
    }

    #[tokio::test]
    async fn save_key_in_empty_store() {
        let store = init_store(0, 0, 0);
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
        let store = init_store(1, 1, 5);
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
        let store = init_store(2, 1, 1);
        let res = store
            .get_single_signatures(&Beacon::new("devnet".to_string(), 1, 0))
            .await
            .unwrap();

        assert!(res.is_none());
    }

    #[tokio::test]
    async fn get_single_signatures_for_existing_epoch() {
        let store = init_store(2, 2, 2);
        let res = store
            .get_single_signatures(&Beacon::new("devnet".to_string(), 1, 1))
            .await
            .unwrap();

        assert!(res.is_some());
        assert_eq!(2, res.unwrap().len());
    }
}

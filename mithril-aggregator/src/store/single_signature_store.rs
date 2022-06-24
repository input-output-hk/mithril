use async_trait::async_trait;
use std::collections::HashMap;
use thiserror::Error;

use mithril_common::entities::{
    Beacon, ImmutableFileNumber, LotteryIndex, PartyId, SingleSignature,
};
use mithril_common::store::adapter::{AdapterError, StoreAdapter};

type SingleSignaturesMap = HashMap<LotteryIndex, SingleSignature>;

type Adapter = Box<
    dyn StoreAdapter<Key = ImmutableFileNumber, Record = HashMap<PartyId, SingleSignaturesMap>>,
>;

#[derive(Debug, Error)]
pub enum SingleSignatureStoreError {
    #[error("adapter error {0}")]
    AdapterError(#[from] AdapterError),
}

#[async_trait]
pub trait SingleSignatureStorer {
    async fn save_single_signature(
        &mut self,
        beacon: &Beacon,
        single_signature: &SingleSignature,
    ) -> Result<Option<SingleSignature>, SingleSignatureStoreError>;

    async fn get_single_signatures(
        &self,
        beacon: &Beacon,
    ) -> Result<Option<HashMap<PartyId, SingleSignaturesMap>>, SingleSignatureStoreError>;
}
pub struct SingleSignatureStore {
    adapter: Adapter,
}

impl SingleSignatureStore {
    pub fn new(adapter: Adapter) -> Self {
        Self { adapter }
    }
}

#[async_trait]
impl SingleSignatureStorer for SingleSignatureStore {
    async fn save_single_signature(
        &mut self,
        beacon: &Beacon,
        single_signature: &SingleSignature,
    ) -> Result<Option<SingleSignature>, SingleSignatureStoreError> {
        let mut single_signatures = match self
            .adapter
            .get_record(&beacon.immutable_file_number)
            .await?
        {
            Some(s) => s,
            None => HashMap::new(),
        };

        let single_signatures_map = single_signatures
            .entry(single_signature.party_id.clone())
            .or_insert_with(HashMap::new);
        let prev_single_signature =
            single_signatures_map.insert(single_signature.index, single_signature.to_owned());
        let _ = self
            .adapter
            .store_record(&beacon.immutable_file_number, &single_signatures)
            .await?;

        Ok(prev_single_signature)
    }

    async fn get_single_signatures(
        &self,
        beacon: &Beacon,
    ) -> Result<Option<HashMap<PartyId, SingleSignaturesMap>>, SingleSignatureStoreError> {
        Ok(self
            .adapter
            .get_record(&beacon.immutable_file_number)
            .await?)
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
        let mut values: Vec<(ImmutableFileNumber, HashMap<PartyId, SingleSignaturesMap>)> =
            Vec::new();

        for immutable_file_number in 1..=nb_immutable_file_numbers {
            let mut single_signatures: HashMap<PartyId, SingleSignaturesMap> = HashMap::new();
            for party_idx in 1..=single_signers_per_epoch {
                let mut single_signatures_map: SingleSignaturesMap = HashMap::new();
                let party_id = format!("{}", party_idx);
                for lottery_idx in 1..=single_signatures_per_signers {
                    let _ = single_signatures_map.insert(
                        lottery_idx,
                        SingleSignature {
                            party_id: party_id.clone(),
                            index: lottery_idx,
                            signature: format!("single-signature {} {}", party_idx, lottery_idx),
                        },
                    );
                }
                single_signatures.insert(party_id, single_signatures_map);
            }
            values.push((immutable_file_number, single_signatures));
        }

        let values = if values.len() > 0 { Some(values) } else { None };
        let adapter: MemoryAdapter<ImmutableFileNumber, HashMap<PartyId, SingleSignaturesMap>> =
            MemoryAdapter::new(values).unwrap();
        let store = SingleSignatureStore::new(Box::new(adapter));

        store
    }

    #[tokio::test]
    async fn save_key_in_empty_store() {
        let mut store = init_store(0, 0, 0);
        let res = store
            .save_single_signature(
                &Beacon::new("devnet".to_string(), 1, 0),
                &SingleSignature {
                    party_id: "0".to_string(),
                    index: 0,
                    signature: "OK".to_string(),
                },
            )
            .await
            .unwrap();

        assert!(res.is_none());
    }

    #[tokio::test]
    async fn update_single_signature_in_store() {
        let mut store = init_store(1, 1, 5);
        let res = store
            .save_single_signature(
                &Beacon::new("devnet".to_string(), 1, 1),
                &SingleSignature {
                    party_id: "1".to_string(),
                    index: 1,
                    signature: "test".to_string(),
                },
            )
            .await
            .unwrap();

        assert!(res.is_some());
        assert_eq!(
            SingleSignature {
                party_id: "1".to_string(),
                index: 1,
                signature: "single-signature 1 1".to_string(),
            },
            res.unwrap(),
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

use async_trait::async_trait;
use mithril_common::store::StorePruner;
use std::collections::HashMap;
use tokio::sync::RwLock;

use mithril_common::entities::{Epoch, PartyId, Signer, SignerWithStake};
use mithril_common::store::{adapter::StoreAdapter, StoreError};

type Adapter = Box<dyn StoreAdapter<Key = Epoch, Record = HashMap<PartyId, SignerWithStake>>>;

/// Mocking trait for `VerificationKeyStore`.
#[async_trait]
pub trait VerificationKeyStorer: Sync + Send {
    /// Save the verification key, for the given [Signer] for the given [Epoch].
    async fn save_verification_key(
        &self,
        epoch: Epoch,
        signer: SignerWithStake,
    ) -> Result<Option<SignerWithStake>, StoreError>;

    /// Returns a HashMap of [Signer] indexed by [PartyId] for the given `Beacon`.
    async fn get_verification_keys(
        &self,
        epoch: Epoch,
    ) -> Result<Option<HashMap<PartyId, Signer>>, StoreError>;
}

/// Store for the `VerificationKey`.
pub struct VerificationKeyStore {
    adapter: RwLock<Adapter>,
    retention_limit: Option<usize>,
}

impl VerificationKeyStore {
    /// Create a new instance.
    pub fn new(adapter: Adapter, retention_limit: Option<usize>) -> Self {
        Self {
            adapter: RwLock::new(adapter),
            retention_limit,
        }
    }
}

#[async_trait]
impl StorePruner for VerificationKeyStore {
    type Key = Epoch;
    type Record = HashMap<PartyId, SignerWithStake>;

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
impl VerificationKeyStorer for VerificationKeyStore {
    async fn save_verification_key(
        &self,
        epoch: Epoch,
        signer: SignerWithStake,
    ) -> Result<Option<SignerWithStake>, StoreError> {
        let mut signers = match self.adapter.read().await.get_record(&epoch).await? {
            Some(s) => s,
            None => HashMap::new(),
        };
        let prev_signer = signers.insert(signer.party_id.to_owned(), signer.clone());
        self.adapter
            .write()
            .await
            .store_record(&epoch, &signers)
            .await?;
        self.prune().await?;

        Ok(prev_signer)
    }

    async fn get_verification_keys(
        &self,
        epoch: Epoch,
    ) -> Result<Option<HashMap<PartyId, Signer>>, StoreError> {
        let record = self.adapter.read().await.get_record(&epoch).await?;
        Ok(record.map(|h| h.into_iter().map(|(k, v)| (k, v.into())).collect()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use mithril_common::store::adapter::MemoryAdapter;

    fn init_store(
        nb_epoch: u64,
        signers_per_epoch: u64,
        retention_limit: Option<usize>,
    ) -> VerificationKeyStore {
        let mut values: Vec<(Epoch, HashMap<PartyId, SignerWithStake>)> = Vec::new();

        for epoch in 1..=nb_epoch {
            let mut signers: HashMap<PartyId, SignerWithStake> = HashMap::new();

            for party_idx in 1..=signers_per_epoch {
                let party_id = format!("{party_idx}");
                signers.insert(
                    party_id.clone(),
                    SignerWithStake {
                        party_id: party_id.clone(),
                        verification_key: format!("vkey {party_id}"),
                        verification_key_signature: None,
                        operational_certificate: None,
                        kes_period: None,
                        stake: 10,
                    },
                );
            }
            values.push((Epoch(epoch), signers));
        }

        let values = if !values.is_empty() {
            Some(values)
        } else {
            None
        };
        let adapter: MemoryAdapter<Epoch, HashMap<PartyId, SignerWithStake>> =
            MemoryAdapter::new(values).unwrap();
        VerificationKeyStore::new(Box::new(adapter), retention_limit)
    }

    #[tokio::test]
    async fn save_key_in_empty_store() {
        let store = init_store(0, 0, None);
        let res = store
            .save_verification_key(
                Epoch(0),
                SignerWithStake {
                    party_id: "0".to_string(),
                    verification_key: "OK".to_string(),
                    verification_key_signature: None,
                    operational_certificate: None,
                    kes_period: None,
                    stake: 10,
                },
            )
            .await
            .unwrap();

        assert!(res.is_none());
    }

    #[tokio::test]
    async fn update_signer_in_store() {
        let store = init_store(1, 1, None);
        let res = store
            .save_verification_key(
                Epoch(1),
                SignerWithStake {
                    party_id: "1".to_string(),
                    verification_key: "test".to_string(),
                    verification_key_signature: None,
                    operational_certificate: None,
                    kes_period: None,
                    stake: 10,
                },
            )
            .await
            .unwrap();

        assert!(res.is_some());
        assert_eq!(
            SignerWithStake {
                party_id: "1".to_string(),
                verification_key: "vkey 1".to_string(),
                verification_key_signature: None,
                operational_certificate: None,
                kes_period: None,
                stake: 10,
            },
            res.unwrap(),
        );
    }

    #[tokio::test]
    async fn get_verification_keys_for_empty_epoch() {
        let store = init_store(2, 1, None);
        let res = store.get_verification_keys(Epoch(0)).await.unwrap();

        assert!(res.is_none());
    }

    #[tokio::test]
    async fn get_verification_keys_for_existing_epoch() {
        let store = init_store(2, 2, None);
        let res = store.get_verification_keys(Epoch(1)).await.unwrap();

        assert!(res.is_some());
        assert_eq!(2, res.unwrap().len());
    }

    #[tokio::test]
    async fn check_retention_limit() {
        let store = init_store(2, 2, Some(2));
        assert!(store
            .get_verification_keys(Epoch(1))
            .await
            .unwrap()
            .is_some());
        let _ = store
            .save_verification_key(
                Epoch(3),
                SignerWithStake {
                    party_id: "party_id".to_string(),
                    verification_key: "whatever".to_string(),
                    verification_key_signature: None,
                    operational_certificate: None,
                    kes_period: None,
                    stake: 10,
                },
            )
            .await
            .unwrap();
        assert!(store
            .get_verification_keys(Epoch(1))
            .await
            .unwrap()
            .is_none());
    }
}

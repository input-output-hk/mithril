use anyhow::Context;
use async_trait::async_trait;
use mithril_common::StdResult;
use std::collections::HashMap;
use tokio::sync::RwLock;

use mithril_common::entities::{Epoch, PartyId, Signer, SignerWithStake};
use mithril_persistence::store::adapter::StoreAdapter;

#[cfg(test)]
use mockall::automock;

type Adapter = Box<dyn StoreAdapter<Key = Epoch, Record = HashMap<PartyId, SignerWithStake>>>;

/// Store and get signers verification keys for given epoch.
///
/// Important note: This store works on the **recording** epoch, the epoch at which the signers
/// are signed into a certificate so they can sign single signatures at the next epoch.
#[cfg_attr(test, automock)]
#[async_trait]
pub trait VerificationKeyStorer: Sync + Send {
    /// Save the verification key, for the given [Signer] for the given [Epoch], returns the
    /// previous values if one already existed.
    async fn save_verification_key(
        &self,
        epoch: Epoch,
        signer: SignerWithStake,
    ) -> StdResult<Option<SignerWithStake>>;

    /// Returns a HashMap of [Signer] indexed by [PartyId] for the given `epoch`.
    async fn get_verification_keys(
        &self,
        epoch: Epoch,
    ) -> StdResult<Option<HashMap<PartyId, Signer>>>;

    /// Returns the list of signers for the given `epoch`.
    async fn get_signers(&self, epoch: Epoch) -> StdResult<Option<Vec<SignerWithStake>>>;

    /// Prune all verification keys that are at or below the given epoch.
    async fn prune_verification_keys(&self, max_epoch_to_prune: Epoch) -> StdResult<()>;
}

/// Store for the `VerificationKey`.
pub struct VerificationKeyStore {
    adapter: RwLock<Adapter>,
}

impl VerificationKeyStore {
    /// Create a new instance.
    pub fn new(adapter: Adapter) -> Self {
        Self {
            adapter: RwLock::new(adapter),
        }
    }
}

#[async_trait]
impl VerificationKeyStorer for VerificationKeyStore {
    async fn save_verification_key(
        &self,
        epoch: Epoch,
        signer: SignerWithStake,
    ) -> StdResult<Option<SignerWithStake>> {
        let mut signers = self
            .adapter
            .read()
            .await
            .get_record(&epoch)
            .await?
            .unwrap_or_default();
        let prev_signer = signers.insert(signer.party_id.to_owned(), signer.clone());
        self.adapter
            .write()
            .await
            .store_record(&epoch, &signers)
            .await?;

        Ok(prev_signer)
    }

    async fn get_verification_keys(
        &self,
        epoch: Epoch,
    ) -> StdResult<Option<HashMap<PartyId, Signer>>> {
        let record = self
            .adapter
            .read()
            .await
            .get_record(&epoch)
            .await
            .with_context(|| format!("Could not get verification keys for epoch {epoch}."))?;

        Ok(record.map(|h| h.into_iter().map(|(k, v)| (k, v.into())).collect()))
    }

    async fn get_signers(&self, epoch: Epoch) -> StdResult<Option<Vec<SignerWithStake>>> {
        let record = self
            .adapter
            .read()
            .await
            .get_record(&epoch)
            .await
            .with_context(|| format!("Could not get signers for epoch {epoch}."))?;

        Ok(record.map(|h| h.into_values().collect()))
    }

    async fn prune_verification_keys(&self, max_epoch_to_prune: Epoch) -> StdResult<()> {
        let mut adapter = self.adapter.write().await;

        for (epoch, _record) in adapter
            .get_last_n_records(usize::MAX)
            .await
            .with_context(|| {
                "Pruning verification keys: could not read last records from database".to_string()
            })?
            .into_iter()
            .filter(|(e, _)| e <= &max_epoch_to_prune)
        {
            adapter.remove(&epoch).await
                .with_context(|| format!("Pruning verification keys: could not remove record for epoch '{epoch}' from the database."))?;
        }

        Ok(())
    }
}

/// Macro that generate tests that a [VerificationKeyStorer] must pass
#[cfg(test)]
macro_rules! test_verification_key_storer {
    ($suit_name:ident => $store_builder:expr) => {
        #[cfg(test)]
        mod $suit_name {
            use crate::store::verification_key_store_test_suite as test_suite;

            #[tokio::test]
            async fn save_key_in_empty_store() {
                test_suite::save_key_in_empty_store(&$store_builder).await;
            }

            #[tokio::test]
            async fn update_signer_in_store() {
                test_suite::update_signer_in_store(&$store_builder).await;
            }

            #[tokio::test]
            async fn get_verification_keys_for_empty_epoch() {
                test_suite::get_verification_keys_for_empty_epoch(&$store_builder).await;
            }

            #[tokio::test]
            async fn get_signers_for_empty_epoch() {
                test_suite::get_signers_for_empty_epoch(&$store_builder).await;
            }

            #[tokio::test]
            async fn get_verification_keys_for_existing_epoch() {
                test_suite::get_verification_keys_for_existing_epoch(&$store_builder).await;
            }

            #[tokio::test]
            async fn get_signers_for_existing_epoch() {
                test_suite::get_signers_for_existing_epoch(&$store_builder).await;
            }

            #[tokio::test]
            async fn can_prune_keys_from_given_epoch_retention_limit() {
                test_suite::can_prune_keys_from_given_epoch_retention_limit(&$store_builder).await;
            }
        }
    };
}

#[cfg(test)]
pub(crate) use test_verification_key_storer;

#[macro_use]
#[cfg(test)]
pub mod test_suite {
    use mithril_common::entities::{Epoch, PartyId, Signer, SignerWithStake};
    use mithril_common::test_utils::fake_keys;
    use std::collections::{BTreeMap, BTreeSet, HashMap};
    use std::sync::Arc;

    use crate::VerificationKeyStorer;

    /// A builder of [VerificationKeyStorer], the arguments are:
    /// * initial_data
    type StoreBuilder =
        dyn Fn(Vec<(Epoch, HashMap<PartyId, SignerWithStake>)>) -> Arc<dyn VerificationKeyStorer>;

    fn build_signers(
        nb_epoch: u64,
        signers_per_epoch: usize,
    ) -> Vec<(Epoch, HashMap<PartyId, SignerWithStake>)> {
        let mut values = vec![];

        for epoch in 1..=nb_epoch {
            let mut signers: HashMap<PartyId, SignerWithStake> =
                HashMap::with_capacity(signers_per_epoch);

            for party_idx in 1..=signers_per_epoch {
                let party_id = format!("party_id:e{epoch}:{party_idx}");
                signers.insert(
                    party_id.clone(),
                    SignerWithStake {
                        party_id: party_id.clone(),
                        verification_key: fake_keys::signer_verification_key()[0]
                            .try_into()
                            .unwrap(),
                        verification_key_signature: None,
                        operational_certificate: None,
                        kes_period: None,
                        stake: 10,
                    },
                );
            }
            values.push((Epoch(epoch), signers));
        }

        values
    }

    pub async fn save_key_in_empty_store(store_builder: &StoreBuilder) {
        let signers = build_signers(0, 0);
        let store = store_builder(signers);
        let res = store
            .save_verification_key(
                Epoch(0),
                SignerWithStake {
                    party_id: "0".to_string(),
                    verification_key: fake_keys::signer_verification_key()[0].try_into().unwrap(),
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

    pub async fn update_signer_in_store(store_builder: &StoreBuilder) {
        let signers = build_signers(1, 1);
        let store = store_builder(signers);
        let res = store
            .save_verification_key(
                Epoch(1),
                SignerWithStake {
                    party_id: "party_id:e1:1".to_string(),
                    verification_key: fake_keys::signer_verification_key()[2].try_into().unwrap(),
                    verification_key_signature: None,
                    operational_certificate: None,
                    kes_period: None,
                    stake: 10,
                },
            )
            .await
            .unwrap();

        assert_eq!(
            Some(SignerWithStake {
                party_id: "party_id:e1:1".to_string(),
                verification_key: fake_keys::signer_verification_key()[2].try_into().unwrap(),
                verification_key_signature: None,
                operational_certificate: None,
                kes_period: None,
                stake: 10,
            }),
            res,
        );
    }

    pub async fn get_verification_keys_for_empty_epoch(store_builder: &StoreBuilder) {
        let signers = build_signers(2, 1);
        let store = store_builder(signers);
        let res = store.get_verification_keys(Epoch(0)).await.unwrap();

        assert!(res.is_none());
    }

    pub async fn get_signers_for_empty_epoch(store_builder: &StoreBuilder) {
        let signers = build_signers(2, 1);
        let store = store_builder(signers);
        let res = store.get_signers(Epoch(0)).await.unwrap();

        assert!(res.is_none());
    }

    pub async fn get_verification_keys_for_existing_epoch(store_builder: &StoreBuilder) {
        let signers = build_signers(2, 2);
        let store = store_builder(signers.clone());

        let expected_signers: Option<BTreeMap<PartyId, Signer>> = signers
            .into_iter()
            .filter(|(e, _)| e == 1)
            .map(|(_, signers)| {
                BTreeMap::from_iter(signers.into_iter().map(|(p, s)| (p, s.into())))
            })
            .next();
        let res = store
            .get_verification_keys(Epoch(1))
            .await
            .unwrap()
            .map(|x| BTreeMap::from_iter(x.into_iter()));

        assert_eq!(expected_signers, res);
    }

    pub async fn get_signers_for_existing_epoch(store_builder: &StoreBuilder) {
        let signers = build_signers(2, 2);
        let store = store_builder(signers.clone());

        let expected_signers: Option<BTreeSet<SignerWithStake>> = signers
            .into_iter()
            .filter(|(e, _)| e == 1)
            .map(|(_, signers)| BTreeSet::from_iter(signers.into_values()))
            .next();
        let res = store
            .get_signers(Epoch(1))
            .await
            .unwrap()
            .map(|x| BTreeSet::from_iter(x.into_iter()));

        assert_eq!(expected_signers, res);
    }

    pub async fn can_prune_keys_from_given_epoch_retention_limit(store_builder: &StoreBuilder) {
        let signers = build_signers(6, 2);
        let store = store_builder(signers);

        for epoch in 1..6 {
            assert!(
                store
                    .get_verification_keys(Epoch(epoch))
                    .await
                    .unwrap()
                    .is_some(),
                "Keys should exist before pruning"
            );
            store
                .prune_verification_keys(Epoch(epoch))
                .await
                .expect("Pruning should not fail");

            let pruned_epoch_keys = store.get_verification_keys(Epoch(epoch)).await.unwrap();
            assert_eq!(None, pruned_epoch_keys);
        }
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::{Epoch, PartyId, SignerWithStake};
    use mithril_persistence::store::adapter::MemoryAdapter;
    use std::{collections::HashMap, sync::Arc};

    use crate::{VerificationKeyStore, VerificationKeyStorer};

    pub fn init_store(
        initial_data: Vec<(Epoch, HashMap<PartyId, SignerWithStake>)>,
    ) -> Arc<dyn VerificationKeyStorer> {
        let values = if initial_data.is_empty() {
            None
        } else {
            Some(initial_data)
        };

        let adapter: MemoryAdapter<Epoch, HashMap<PartyId, SignerWithStake>> =
            MemoryAdapter::new(values).unwrap();

        Arc::new(VerificationKeyStore::new(Box::new(adapter)))
    }

    test_verification_key_storer!(
        test_verification_key_store =>
        crate::store::verification_key_store::tests::init_store
    );
}

use async_trait::async_trait;
use std::collections::HashMap;
use tokio::sync::RwLock;

use mithril_common::entities::{Epoch, PartyId, Signer, SignerWithStake};
use mithril_common::store::{adapter::StoreAdapter, StoreError};

type Adapter = Box<dyn StoreAdapter<Key = Epoch, Record = HashMap<PartyId, SignerWithStake>>>;

/// Store and get signers verification keys for given epoch.
#[async_trait]
pub trait VerificationKeyStorer: Sync + Send {
    /// Save the verification key, for the given [Signer] for the given [Epoch], returns the
    /// previous values if one already existed.
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

    /// Prune all verification keys that are at or below the given epoch.
    async fn prune_verification_keys(&self, max_epoch_to_prune: Epoch) -> Result<(), StoreError>;
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

        Ok(prev_signer)
    }

    async fn get_verification_keys(
        &self,
        epoch: Epoch,
    ) -> Result<Option<HashMap<PartyId, Signer>>, StoreError> {
        let record = self.adapter.read().await.get_record(&epoch).await?;
        Ok(record.map(|h| h.into_iter().map(|(k, v)| (k, v.into())).collect()))
    }

    async fn prune_verification_keys(&self, max_epoch_to_prune: Epoch) -> Result<(), StoreError> {
        let mut adapter = self.adapter.write().await;

        for (epoch, _record) in adapter
            .get_last_n_records(usize::MAX)
            .await?
            .into_iter()
            .filter(|(e, _)| e <= &max_epoch_to_prune)
        {
            adapter.remove(&epoch).await?;
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
            async fn get_verification_keys_for_existing_epoch() {
                test_suite::get_verification_keys_for_existing_epoch(&$store_builder).await;
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
    use std::collections::{BTreeMap, HashMap};
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
                        verification_key: "7b22766b223a5b3134352c32332c3135382c31322c3138332c3230392c33322c3134302c33372c3132342c3136362c3231352c3136302c3231352c3235302c3133342c3135342c3235302c3234312c3230362c3139342c3232322c382c35392c33332c392c35382c322c3235312c31302c33322c3135352c3232372c3134332c3232362c35372c3135312c37342c3139392c3131372c37352c3136382c3134302c34362c3233392c3134352c37322c31362c32312c3138312c3139332c3134362c38362c3231332c3230342c3139332c3232332c32352c3135372c33342c33332c3232372c35312c3132362c3132362c3135362c36342c3232302c3139392c3231332c31362c34352c3131302c3234332c33352c3134382c37312c3231382c3132342c3132332c31362c3132312c3135322c31382c32362c3231322c3231342c3230312c3139302c3137342c3131352c39372c3234392c3235342c3131362c3234335d2c22706f70223a5b3138332c3134352c3133392c3234322c3132302c3136302c35362c3131382c3234322c3230342c39312c38392c32312c3138342c382c34372c3231332c3130352c36332c3135302c32312c3231372c352c382c3231392c3138382c3131342c3230352c3136362c31362c3234302c3234302c3231342c31362c3230342c3231382c3139332c3138312c32342c35362c34352c39392c3234342c38312c32352c35322c3232342c36372c3136382c3136392c3130392c3132322c38372c34392c3137302c3138312c3135312c31352c3235322c3139352c3231312c3233342c3139352c34392c39312c31392c35312c3234312c33332c35382c3134302c3235322c3234322c362c342c34302c32312c3136372c3234392c3235312c33362c38372c36302c39362c36392c3135322c3231302c39382c3136352c352c362c34312c39362c3233352c37352c3138335d7d".try_into().unwrap(),
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
                    verification_key: "7b22766b223a5b3134352c32332c3135382c31322c3138332c3230392c33322c3134302c33372c3132342c3136362c3231352c3136302c3231352c3235302c3133342c3135342c3235302c3234312c3230362c3139342c3232322c382c35392c33332c392c35382c322c3235312c31302c33322c3135352c3232372c3134332c3232362c35372c3135312c37342c3139392c3131372c37352c3136382c3134302c34362c3233392c3134352c37322c31362c32312c3138312c3139332c3134362c38362c3231332c3230342c3139332c3232332c32352c3135372c33342c33332c3232372c35312c3132362c3132362c3135362c36342c3232302c3139392c3231332c31362c34352c3131302c3234332c33352c3134382c37312c3231382c3132342c3132332c31362c3132312c3135322c31382c32362c3231322c3231342c3230312c3139302c3137342c3131352c39372c3234392c3235342c3131362c3234335d2c22706f70223a5b3138332c3134352c3133392c3234322c3132302c3136302c35362c3131382c3234322c3230342c39312c38392c32312c3138342c382c34372c3231332c3130352c36332c3135302c32312c3231372c352c382c3231392c3138382c3131342c3230352c3136362c31362c3234302c3234302c3231342c31362c3230342c3231382c3139332c3138312c32342c35362c34352c39392c3234342c38312c32352c35322c3232342c36372c3136382c3136392c3130392c3132322c38372c34392c3137302c3138312c3135312c31352c3235322c3139352c3231312c3233342c3139352c34392c39312c31392c35312c3234312c33332c35382c3134302c3235322c3234322c362c342c34302c32312c3136372c3234392c3235312c33362c38372c36302c39362c36392c3135322c3231302c39382c3136352c352c362c34312c39362c3233352c37352c3138335d7d".try_into().unwrap(),
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
                    verification_key: "7b22766b223a5b3134352c35362c3137352c33322c3132322c3138372c3231342c3232362c3235312c3134382c38382c392c312c3130332c3135392c3134362c38302c3136362c3130372c3234332c3235312c3233362c34312c32382c3131312c3132382c3230372c3136342c3133322c3134372c3232382c38332c3234362c3232382c3137302c36382c38392c37382c36302c32382c3132332c3133302c38382c3233342c33382c39372c34322c36352c312c3130302c35332c31382c37382c3133312c382c36312c3132322c3133312c3233382c38342c3233332c3232332c3135342c3131382c3131382c37332c32382c32372c3130312c37382c38302c3233332c3132332c3230362c3232302c3137342c3133342c3230352c37312c3131302c3131322c3138302c39372c39382c302c3131332c36392c3134352c3233312c3136382c34332c3137332c3137322c35362c3130342c3230385d2c22706f70223a5b3133372c3231342c37352c37352c3134342c3136312c3133372c37392c39342c3134302c3138312c34372c33312c38312c3231332c33312c3137312c3231362c32342c3137342c37382c3234382c3133302c37352c3235352c31312c3134352c3132342c36312c38302c3139302c32372c3231362c3130352c3130362c3234382c39312c3134332c3230342c3130322c3230332c3136322c37362c3130372c31352c35322c36312c38322c3134362c3133302c3132342c37342c382c33342c3136342c3138372c3230332c38322c36342c3130382c3139312c3138352c3138382c37372c3132322c352c3234362c3235352c3130322c3131392c3234372c3139392c3131372c36372c3234312c3134332c32392c3136382c36372c39342c3135312c37382c3132392c3133312c33302c3130312c3137332c31302c36392c36382c3137352c39382c33372c3233392c3139342c32395d7d".try_into().unwrap(),
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
                verification_key: "7b22766b223a5b3134352c35362c3137352c33322c3132322c3138372c3231342c3232362c3235312c3134382c38382c392c312c3130332c3135392c3134362c38302c3136362c3130372c3234332c3235312c3233362c34312c32382c3131312c3132382c3230372c3136342c3133322c3134372c3232382c38332c3234362c3232382c3137302c36382c38392c37382c36302c32382c3132332c3133302c38382c3233342c33382c39372c34322c36352c312c3130302c35332c31382c37382c3133312c382c36312c3132322c3133312c3233382c38342c3233332c3232332c3135342c3131382c3131382c37332c32382c32372c3130312c37382c38302c3233332c3132332c3230362c3232302c3137342c3133342c3230352c37312c3131302c3131322c3138302c39372c39382c302c3131332c36392c3134352c3233312c3136382c34332c3137332c3137322c35362c3130342c3230385d2c22706f70223a5b3133372c3231342c37352c37352c3134342c3136312c3133372c37392c39342c3134302c3138312c34372c33312c38312c3231332c33312c3137312c3231362c32342c3137342c37382c3234382c3133302c37352c3235352c31312c3134352c3132342c36312c38302c3139302c32372c3231362c3130352c3130362c3234382c39312c3134332c3230342c3130322c3230332c3136322c37362c3130372c31352c35322c36312c38322c3134362c3133302c3132342c37342c382c33342c3136342c3138372c3230332c38322c36342c3130382c3139312c3138352c3138382c37372c3132322c352c3234362c3235352c3130322c3131392c3234372c3139392c3131372c36372c3234312c3134332c32392c3136382c36372c39342c3135312c37382c3132392c3133312c33302c3130312c3137332c31302c36392c36382c3137352c39382c33372c3233392c3139342c32395d7d".try_into().unwrap(),
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
    use mithril_common::{
        entities::{Epoch, PartyId, SignerWithStake},
        store::adapter::MemoryAdapter,
    };
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

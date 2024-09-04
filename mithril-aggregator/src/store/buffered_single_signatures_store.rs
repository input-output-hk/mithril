use async_trait::async_trait;
use std::collections::BTreeMap;
use tokio::sync::RwLock;

use mithril_common::entities::{SignedEntityTypeDiscriminants, SingleSignatures};
use mithril_common::StdResult;

use crate::services::BufferedSingleSignatureStore;

/// An in-memory implementation of [BufferedSingleSignatureStore].
pub struct InMemoryBufferedSingleSignatureStore {
    store: RwLock<BTreeMap<SignedEntityTypeDiscriminants, Vec<SingleSignatures>>>,
}

#[cfg(test)]
impl InMemoryBufferedSingleSignatureStore {
    pub(crate) fn with_data(
        initial_data: BTreeMap<SignedEntityTypeDiscriminants, Vec<SingleSignatures>>,
    ) -> Self {
        Self {
            store: RwLock::new(initial_data),
        }
    }
}

impl Default for InMemoryBufferedSingleSignatureStore {
    fn default() -> Self {
        Self {
            store: RwLock::new(BTreeMap::new()),
        }
    }
}

#[async_trait]
impl BufferedSingleSignatureStore for InMemoryBufferedSingleSignatureStore {
    async fn buffer_signature(
        &self,
        signed_entity_type_discriminants: SignedEntityTypeDiscriminants,
        signature: &SingleSignatures,
    ) -> StdResult<()> {
        let mut store = self.store.write().await;
        let signatures = store
            .entry(signed_entity_type_discriminants)
            .or_insert_with(Vec::new);
        signatures.push(signature.clone());
        Ok(())
    }

    async fn get_buffered_signatures(
        &self,
        signed_entity_type_discriminants: SignedEntityTypeDiscriminants,
    ) -> StdResult<Vec<SingleSignatures>> {
        let store = self.store.read().await;
        Ok(store
            .get(&signed_entity_type_discriminants)
            .cloned()
            .unwrap_or_default())
    }

    async fn remove_buffered_signatures(
        &self,
        signed_entity_type_discriminants: SignedEntityTypeDiscriminants,
        single_signatures: Vec<SingleSignatures>,
    ) -> StdResult<()> {
        let mut store = self.store.write().await;

        for signature in single_signatures {
            if let Some(signatures) = store.get_mut(&signed_entity_type_discriminants) {
                signatures.retain(|s| s != &signature);
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::test_utils::fake_data;

    use super::*;

    #[tokio::test]
    async fn store_and_retrieve_signatures() {
        let store = InMemoryBufferedSingleSignatureStore::default();

        let ctx = SignedEntityTypeDiscriminants::CardanoTransactions;
        store
            .buffer_signature(ctx, &fake_data::single_signatures(vec![1]))
            .await
            .unwrap();
        store
            .buffer_signature(ctx, &fake_data::single_signatures(vec![2]))
            .await
            .unwrap();

        // Different signed entity type to test that the store is able to differentiate between them
        let msd = SignedEntityTypeDiscriminants::MithrilStakeDistribution;
        store
            .buffer_signature(msd, &fake_data::single_signatures(vec![3]))
            .await
            .unwrap();

        let buffered_signatures_ctx = store.get_buffered_signatures(ctx).await.unwrap();
        assert_eq!(
            vec![
                fake_data::single_signatures(vec![1]),
                fake_data::single_signatures(vec![2])
            ],
            buffered_signatures_ctx
        );

        let buffered_signatures_msd = store.get_buffered_signatures(msd).await.unwrap();
        assert_eq!(
            vec![fake_data::single_signatures(vec![3])],
            buffered_signatures_msd
        );
    }

    #[tokio::test]
    async fn remove_buffered_signatures() {
        let store = InMemoryBufferedSingleSignatureStore::with_data(BTreeMap::from([
            (
                SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                vec![
                    fake_data::single_signatures(vec![1]),
                    fake_data::single_signatures(vec![2]),
                    fake_data::single_signatures(vec![3]),
                ],
            ),
            (
                SignedEntityTypeDiscriminants::CardanoTransactions,
                vec![fake_data::single_signatures(vec![10])],
            ),
        ]));

        store
            .remove_buffered_signatures(
                SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                vec![
                    fake_data::single_signatures(vec![1]),
                    fake_data::single_signatures(vec![3]),
                ],
            )
            .await
            .unwrap();

        let remaining_msd_sigs = store
            .get_buffered_signatures(SignedEntityTypeDiscriminants::MithrilStakeDistribution)
            .await
            .unwrap();
        assert_eq!(
            vec![fake_data::single_signatures(vec![2])],
            remaining_msd_sigs
        );

        let remaining_ctx_sigs = store
            .get_buffered_signatures(SignedEntityTypeDiscriminants::CardanoTransactions)
            .await
            .unwrap();
        assert_eq!(
            vec![fake_data::single_signatures(vec![10])],
            remaining_ctx_sigs,
            "CardanoTransactions signatures should have been left untouched"
        );
    }
}

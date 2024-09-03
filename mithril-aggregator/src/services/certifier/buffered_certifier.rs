use async_trait::async_trait;
use slog::{debug, Logger};
use std::collections::BTreeMap;
use std::sync::Arc;
use tokio::sync::RwLock;

use mithril_common::entities::{
    Certificate, Epoch, ProtocolMessage, SignedEntityType, SignedEntityTypeDiscriminants,
    SingleSignatures,
};
use mithril_common::StdResult;

use crate::entities::OpenMessage;
use crate::services::{BufferedSingleSignatureStore, CertifierService, CertifierServiceError};

/// A decorator of [CertifierService] that buffers that can buffer registration of single signatures
/// when the open message is not yet created.
///
/// When an open message is created, buffered single signatures for the open message type are
/// registered.
pub struct BufferedCertifierService {
    certifier_service: Arc<dyn CertifierService>,
    buffered_single_signature_store: Arc<dyn BufferedSingleSignatureStore>,
    logger: Logger,
}

impl BufferedCertifierService {
    /// Create a new instance of `BufferedCertifierService`.
    pub fn new(
        certifier_service: Arc<dyn CertifierService>,
        buffered_single_signature_store: Arc<dyn BufferedSingleSignatureStore>,
        logger: Logger,
    ) -> Self {
        Self {
            certifier_service,
            buffered_single_signature_store,
            logger,
        }
    }
}

#[async_trait]
impl CertifierService for BufferedCertifierService {
    async fn inform_epoch(&self, epoch: Epoch) -> StdResult<()> {
        self.certifier_service.inform_epoch(epoch).await
    }

    async fn register_single_signature(
        &self,
        signed_entity_type: &SignedEntityType,
        signature: &SingleSignatures,
    ) -> StdResult<()> {
        match self
            .certifier_service
            .register_single_signature(signed_entity_type, signature)
            .await
        {
            Ok(res) => Ok(res),
            Err(error) => match error.downcast_ref::<CertifierServiceError>() {
                Some(CertifierServiceError::NotFound(..)) => {
                    debug!(
                        self.logger,
                        "No OpenMessage available for signed entity - Buffering single signature";
                        "signed_entity_type" => ?signed_entity_type,
                        "party_id" => &signature.party_id
                    );

                    self.buffered_single_signature_store
                        .buffer_signature(signed_entity_type.into(), signature)
                        .await?;
                    Ok(())
                }
                _ => Err(error),
            },
        }
    }

    async fn create_open_message(
        &self,
        signed_entity_type: &SignedEntityType,
        protocol_message: &ProtocolMessage,
    ) -> StdResult<OpenMessage> {
        self.certifier_service
            .create_open_message(signed_entity_type, protocol_message)
            .await
    }

    async fn get_open_message(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<Option<OpenMessage>> {
        self.certifier_service
            .get_open_message(signed_entity_type)
            .await
    }

    async fn mark_open_message_if_expired(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<Option<OpenMessage>> {
        self.certifier_service
            .mark_open_message_if_expired(signed_entity_type)
            .await
    }

    async fn create_certificate(
        &self,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<Option<Certificate>> {
        self.certifier_service
            .create_certificate(signed_entity_type)
            .await
    }

    async fn get_certificate_by_hash(&self, hash: &str) -> StdResult<Option<Certificate>> {
        self.certifier_service.get_certificate_by_hash(hash).await
    }

    async fn get_latest_certificates(&self, last_n: usize) -> StdResult<Vec<Certificate>> {
        self.certifier_service.get_latest_certificates(last_n).await
    }

    async fn verify_certificate_chain(&self, epoch: Epoch) -> StdResult<()> {
        self.certifier_service.verify_certificate_chain(epoch).await
    }
}

/// An in-memory implementation of [BufferedSingleSignatureStore].
pub struct InMemoryBufferedSingleSignatureStore {
    store: RwLock<BTreeMap<SignedEntityTypeDiscriminants, Vec<SingleSignatures>>>,
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
}

#[cfg(test)]
mod tests {
    use mithril_common::test_utils::fake_data;

    use crate::services::{CertifierServiceError, MockCertifierService};
    use crate::test_tools::TestLogger;

    use super::*;

    #[tokio::test]
    async fn store_and_retrieve_signatures_in_buffered_store() {
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
    async fn buffer_signature_if_decorated_certifier_as_no_opened_message() {
        let mut inner_certifier = MockCertifierService::new();
        inner_certifier
            .expect_register_single_signature()
            .returning(|_, _| {
                Err(
                    CertifierServiceError::NotFound(SignedEntityType::MithrilStakeDistribution(
                        Epoch(5),
                    ))
                    .into(),
                )
            });

        let store = Arc::new(InMemoryBufferedSingleSignatureStore::default());
        let certifier = BufferedCertifierService::new(
            Arc::new(inner_certifier),
            store.clone(),
            TestLogger::stdout(),
        );
        certifier
            .register_single_signature(
                &SignedEntityType::MithrilStakeDistribution(Epoch(5)),
                &fake_data::single_signatures(vec![1]),
            )
            .await
            .unwrap();

        let buffered_signatures = store
            .get_buffered_signatures(SignedEntityTypeDiscriminants::MithrilStakeDistribution)
            .await
            .unwrap();
        assert_eq!(
            buffered_signatures,
            vec![fake_data::single_signatures(vec![1])]
        );
    }
}

use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;

use mithril_common::{
    StdResult,
    entities::{SignedEntityType, SingleSignature},
    messages::RegisterSignatureMessageDmq,
};

use mithril_dmq::DmqConsumerClient;

use super::SignatureConsumer;

/// DMQ implementation of the [SignatureConsumer] trait.
pub struct SignatureConsumerDmq {
    dmq_consumer: Arc<dyn DmqConsumerClient<RegisterSignatureMessageDmq>>,
}

impl SignatureConsumerDmq {
    /// Creates a new instance of [SignatureConsumerDmq].
    pub fn new(dmq_consumer: Arc<dyn DmqConsumerClient<RegisterSignatureMessageDmq>>) -> Self {
        Self { dmq_consumer }
    }
}

#[async_trait]
impl SignatureConsumer for SignatureConsumerDmq {
    async fn get_signatures(&self) -> StdResult<Vec<(SingleSignature, SignedEntityType)>> {
        self.dmq_consumer
            .consume_messages()
            .await
            .map(|messages| {
                messages
                    .into_iter()
                    .filter_map(|(message, party_id)| {
                        message.signed_entity_type.into_entity().map(|signed_entity_type| {
                            (message.signature, signed_entity_type, party_id)
                        })
                    })
                    .map(|(signature, signed_entity_type, party_id)| {
                        let won_indexes = signature.get_concatenation_signature_indices();
                        let single_signature =
                            SingleSignature::new(party_id, signature, won_indexes);

                        (single_signature, signed_entity_type)
                    })
                    .collect()
            })
            .with_context(|| "Failed to get signatures from DMQ")
    }

    fn get_origin_tag(&self) -> String {
        "DMQ".to_string()
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::{
        crypto_helper::ProtocolSingleSignature, entities::Epoch, messages::SignedEntityTypeMessage,
        test::double::fake_keys,
    };
    use mithril_dmq::test::double::DmqConsumerFake;

    use super::*;

    #[tokio::test]
    async fn get_signatures_success_and_filter_out_unknown_signed_entity() {
        let single_signature: ProtocolSingleSignature =
            fake_keys::single_signature()[0].try_into().unwrap();
        let dmq_consumer = Arc::new(DmqConsumerFake::new(vec![Ok(vec![
            (
                RegisterSignatureMessageDmq {
                    signature: single_signature.clone(),
                    signed_entity_type: SignedEntityTypeMessage::Known(
                        SignedEntityType::MithrilStakeDistribution(Epoch(3)),
                    ),
                },
                "pool-id-1".to_string(),
            ),
            (
                RegisterSignatureMessageDmq {
                    signature: single_signature.clone(),
                    signed_entity_type: SignedEntityTypeMessage::Unknown,
                },
                "pool-id-2".to_string(),
            ),
        ])]));
        let consumer = SignatureConsumerDmq::new(dmq_consumer);

        let signatures = consumer.get_signatures().await.unwrap();

        assert_eq!(
            vec![(
                SingleSignature::new(
                    "pool-id-1".to_string(),
                    single_signature.clone(),
                    single_signature.get_concatenation_signature_indices(),
                ),
                SignedEntityType::MithrilStakeDistribution(Epoch(3))
            )],
            signatures
        );
    }

    #[tokio::test]
    async fn get_signatures_failure() {
        let dmq_consumer = Arc::new(DmqConsumerFake::new(vec![Err(anyhow::anyhow!(
            "Test error"
        ))]));
        let consumer = SignatureConsumerDmq::new(dmq_consumer);

        consumer
            .get_signatures()
            .await
            .expect_err("SignatureConsumerDmq should return an error");
    }
}

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
                    .map(|(message, party_id)| {
                        let signature = message.signature;
                        let won_indexes = signature.concatenation_signature.indexes.clone();
                        let single_signature =
                            SingleSignature::new(party_id, signature, won_indexes);
                        let signed_entity_type = message.signed_entity_type;

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
        crypto_helper::ProtocolSingleSignature,
        test::double::{Dummy, fake_keys},
    };
    use mithril_dmq::test::double::DmqConsumerFake;

    use super::*;

    #[tokio::test]
    async fn get_signatures_success() {
        let signed_entity_type = SignedEntityType::dummy();
        let single_signature: ProtocolSingleSignature =
            fake_keys::single_signature()[0].try_into().unwrap();
        let dmq_consumer = Arc::new(DmqConsumerFake::new(vec![Ok(vec![(
            RegisterSignatureMessageDmq {
                signature: single_signature.clone(),
                signed_entity_type: signed_entity_type.to_owned(),
            },
            "pool-id-1".to_string(),
        )])]));
        let consumer = SignatureConsumerDmq::new(dmq_consumer);

        let signatures = consumer.get_signatures().await.unwrap();

        assert_eq!(
            vec![(
                SingleSignature::new(
                    "pool-id-1".to_string(),
                    single_signature.clone(),
                    single_signature.concatenation_signature.indexes.clone()
                ),
                signed_entity_type
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

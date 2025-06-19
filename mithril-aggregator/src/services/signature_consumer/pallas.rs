use anyhow::Context;
use async_trait::async_trait;

use mithril_common::{
    entities::{SignedEntityType, SingleSignature},
    messages::RegisterSignatureMessageDmq,
    StdResult,
};

use mithril_dmq_node::{DmqConsumer, DmqConsumerPallas};

use super::SignatureConsumer;

#[async_trait]
impl SignatureConsumer for DmqConsumerPallas<RegisterSignatureMessageDmq> {
    async fn get_signatures(&self) -> StdResult<Vec<(SingleSignature, SignedEntityType)>> {
        self.consume_messages()
            .await
            .map(|messages| {
                messages
                    .into_iter()
                    .map(|(message, party_id)| {
                        let signature = message.signature;
                        let won_indexes = signature.indexes.clone();
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

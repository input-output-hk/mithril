use anyhow::Context;
use mithril_common::entities::{ProtocolMessage, SignedEntityType, SingleSignatures};
use mithril_common::messages::{RegisterSignatureMessage, TryToMessageAdapter};
use mithril_common::protocol::ToMessage;
use mithril_common::StdResult;

pub struct ToRegisterSignatureMessageAdapter;

impl
    TryToMessageAdapter<
        (SignedEntityType, SingleSignatures, &ProtocolMessage),
        RegisterSignatureMessage,
    > for ToRegisterSignatureMessageAdapter
{
    fn try_adapt(
        (signed_entity_type, single_signature, protocol_message): (
            SignedEntityType,
            SingleSignatures,
            &ProtocolMessage,
        ),
    ) -> StdResult<RegisterSignatureMessage> {
        let message = RegisterSignatureMessage {
            signed_entity_type,
            party_id: single_signature.party_id,
            signature: single_signature.signature.try_into().with_context(|| {
                "'ToRegisterSignatureMessageAdapter' can not convert the single signature"
            })?,
            won_indexes: single_signature.won_indexes,
            signed_message: Some(protocol_message.to_message()),
        };

        Ok(message)
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::test_utils::fake_data;

    use super::*;

    #[test]
    fn adapt_ok() {
        let message: RegisterSignatureMessage = ToRegisterSignatureMessageAdapter::try_adapt((
            SignedEntityType::dummy(),
            fake_data::single_signatures([1, 3].to_vec()),
            &ProtocolMessage::default(),
        ))
        .unwrap();

        assert_eq!("party_id".to_string(), message.party_id);
        assert_eq!(
            Some(ProtocolMessage::default().to_message()),
            message.signed_message
        );
    }
}

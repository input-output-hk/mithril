use anyhow::Context;

use mithril_common::entities::{ProtocolMessage, SignedEntityType, SingleSignature};
use mithril_common::messages::{RegisterSignatureMessageHttp, TryToMessageAdapter};
use mithril_common::protocol::ToMessage;
use mithril_common::StdResult;

pub struct ToRegisterSignatureMessageAdapter;

impl
    TryToMessageAdapter<
        (SignedEntityType, SingleSignature, &ProtocolMessage),
        RegisterSignatureMessageHttp,
    > for ToRegisterSignatureMessageAdapter
{
    fn try_adapt(
        (signed_entity_type, single_signature, protocol_message): (
            SignedEntityType,
            SingleSignature,
            &ProtocolMessage,
        ),
    ) -> StdResult<RegisterSignatureMessageHttp> {
        let message = RegisterSignatureMessageHttp {
            signed_entity_type,
            party_id: single_signature.party_id,
            signature: single_signature.signature.try_into().with_context(|| {
                "'ToRegisterSignatureMessageAdapter' can not convert the single signature"
            })?,
            won_indexes: single_signature.won_indexes,
            signed_message: protocol_message.to_message(),
        };

        Ok(message)
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::{CardanoDbBeacon, Epoch};
    use mithril_common::test_utils::fake_data;

    use super::*;

    #[test]
    fn adapt_ok() {
        let message: RegisterSignatureMessageHttp = ToRegisterSignatureMessageAdapter::try_adapt((
            SignedEntityType::MithrilStakeDistribution(Epoch(6)),
            fake_data::single_signature([1, 3].to_vec()),
            &ProtocolMessage::default(),
        ))
        .unwrap();

        assert_eq!("party_id".to_string(), message.party_id);
        assert_eq!(
            ProtocolMessage::default().to_message(),
            message.signed_message
        );
        assert_eq!(
            SignedEntityType::MithrilStakeDistribution(Epoch(6)),
            message.signed_entity_type
        );
    }

    #[test]
    fn adapt_cardano_immutable_files_full_message() {
        let signed_entity_type =
            SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::new(6, 54));
        let message: RegisterSignatureMessageHttp = ToRegisterSignatureMessageAdapter::try_adapt((
            signed_entity_type.clone(),
            fake_data::single_signature([1, 3].to_vec()),
            &ProtocolMessage::default(),
        ))
        .unwrap();

        assert_eq!(signed_entity_type, message.signed_entity_type);
    }
}

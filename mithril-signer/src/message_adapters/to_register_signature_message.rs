use anyhow::Context;

use mithril_common::entities::{ProtocolMessage, SignedEntityType, SingleSignatures};
use mithril_common::messages::{RegisterSignatureMessage, TryToMessageAdapter};
use mithril_common::protocol::ToMessage;
use mithril_common::{CardanoNetwork, StdResult};

pub struct ToRegisterSignatureMessageAdapter;

impl
    TryToMessageAdapter<
        (
            SignedEntityType,
            SingleSignatures,
            &ProtocolMessage,
            CardanoNetwork,
        ),
        RegisterSignatureMessage,
    > for ToRegisterSignatureMessageAdapter
{
    fn try_adapt(
        (signed_entity_type, single_signature, protocol_message, cardano_network): (
            SignedEntityType,
            SingleSignatures,
            &ProtocolMessage,
            CardanoNetwork,
        ),
    ) -> StdResult<RegisterSignatureMessage> {
        let message = RegisterSignatureMessage {
            signed_entity_type: (signed_entity_type, cardano_network).into(),
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
    use mithril_common::entities::{CardanoDbBeacon, Epoch};
    use mithril_common::messages::SignedEntityTypeMessagePart;
    use mithril_common::test_utils::fake_data;

    use super::*;

    #[test]
    fn adapt_ok() {
        let message: RegisterSignatureMessage = ToRegisterSignatureMessageAdapter::try_adapt((
            SignedEntityType::MithrilStakeDistribution(Epoch(6)),
            fake_data::single_signatures([1, 3].to_vec()),
            &ProtocolMessage::default(),
            CardanoNetwork::MainNet,
        ))
        .unwrap();

        assert_eq!("party_id".to_string(), message.party_id);
        assert_eq!(
            Some(ProtocolMessage::default().to_message()),
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
            SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::new("unused", 6, 54));
        let message: RegisterSignatureMessage = ToRegisterSignatureMessageAdapter::try_adapt((
            signed_entity_type.clone(),
            fake_data::single_signatures([1, 3].to_vec()),
            &ProtocolMessage::default(),
            CardanoNetwork::MainNet,
        ))
        .unwrap();

        assert_eq!(
            SignedEntityTypeMessagePart::from((signed_entity_type, CardanoNetwork::MainNet)),
            message.signed_entity_type
        );
    }
}

use mithril_common::entities::{SignedEntityType, SingleSignatures};
use mithril_common::messages::{RegisterSignatureMessage, ToMessageAdapter};

pub struct ToRegisterSignatureMessageAdapter;

impl ToMessageAdapter<(SignedEntityType, SingleSignatures), RegisterSignatureMessage>
    for ToRegisterSignatureMessageAdapter
{
    fn adapt(
        (signed_entity_type, single_signature): (SignedEntityType, SingleSignatures),
    ) -> RegisterSignatureMessage {
        RegisterSignatureMessage {
            signed_entity_type: Some(signed_entity_type),
            party_id: single_signature.party_id,
            signature: single_signature.signature.try_into().unwrap(),
            won_indexes: single_signature.won_indexes,
        }
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::test_utils::fake_data;

    use super::*;

    #[test]
    fn adapt_ok() {
        let single_signature = fake_data::single_signatures([1, 3].to_vec());
        let message: RegisterSignatureMessage =
            ToRegisterSignatureMessageAdapter::adapt((SignedEntityType::dummy(), single_signature));

        assert_eq!("party_id".to_string(), message.party_id);
    }
}

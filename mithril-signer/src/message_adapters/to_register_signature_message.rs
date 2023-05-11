use mithril_common::entities::{SignedEntityType, SingleSignatures};
use mithril_common::messages::RegisterSignatureMessage;

pub struct ToRegisterSignatureMessageAdapter;

impl ToRegisterSignatureMessageAdapter {
    pub fn adapt(
        signed_entity_type: SignedEntityType,
        single_signature: SingleSignatures,
    ) -> RegisterSignatureMessage {
        RegisterSignatureMessage {
            signed_entity_type: Some(signed_entity_type),
            party_id: single_signature.party_id,
            signature: single_signature.signature,
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
        let message =
            ToRegisterSignatureMessageAdapter::adapt(SignedEntityType::dummy(), single_signature);

        assert_eq!("party_id".to_string(), message.party_id);
    }
}

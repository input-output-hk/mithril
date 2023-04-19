use mithril_common::entities::SingleSignatures;
use mithril_common::messages::RegisterSignatureMessage;

pub struct ToRegisterSignatureMessageAdapter;

impl ToRegisterSignatureMessageAdapter {
    pub fn adapt(single_signature: SingleSignatures) -> RegisterSignatureMessage {
        RegisterSignatureMessage {
            signed_entity_type: None,
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
        let message = ToRegisterSignatureMessageAdapter::adapt(single_signature);

        assert_eq!("party_id".to_string(), message.party_id);
    }
}

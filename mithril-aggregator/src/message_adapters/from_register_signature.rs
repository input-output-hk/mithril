use mithril_common::{entities::SingleSignatures, messages::RegisterSignatureMessage};

pub struct FromRegisterSingleSignatureAdapter;

impl FromRegisterSingleSignatureAdapter {
    pub fn adapt(register_single_signature_message: RegisterSignatureMessage) -> SingleSignatures {
        SingleSignatures {
            party_id: register_single_signature_message.party_id,
            signature: register_single_signature_message.signature,
            won_indexes: register_single_signature_message.won_indexes,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_message() {
        let message = RegisterSignatureMessage::dummy();
        let signatures = FromRegisterSingleSignatureAdapter::adapt(message);

        assert_eq!("party_id".to_string(), signatures.party_id);
    }
}

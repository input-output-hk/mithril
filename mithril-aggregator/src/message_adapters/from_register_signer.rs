use mithril_common::{entities::Signer, messages::RegisterSignerMessage};

struct FromRegisterSignerAdapter;

impl FromRegisterSignerAdapter {
    pub fn adapt(register_signer_message: RegisterSignerMessage) -> Signer {
        Signer {
            party_id: register_signer_message.party_id,
            verification_key: register_signer_message.verification_key,
            verification_key_signature: register_signer_message.verification_key_signature,
            operational_certificate: register_signer_message.operational_certificate,
            kes_period: register_signer_message.kes_period,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_message() {
        let register_signer_message = RegisterSignerMessage {
            party_id: "one".to_string(),
            ..Default::default()
        };

        let signer = FromRegisterSignerAdapter::adapt(register_signer_message);

        assert_eq!("one".to_string(), signer.party_id);
    }
}

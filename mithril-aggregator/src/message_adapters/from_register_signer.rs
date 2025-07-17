use anyhow::Context;
use mithril_common::{
    StdResult,
    entities::Signer,
    messages::{RegisterSignerMessage, TryFromMessageAdapter},
};

/// Adapter to convert [RegisterSignerMessage] to [Signer] instances.
pub struct FromRegisterSignerAdapter;

impl TryFromMessageAdapter<RegisterSignerMessage, Signer> for FromRegisterSignerAdapter {
    /// Method to trigger the conversion.
    fn try_adapt(register_signer_message: RegisterSignerMessage) -> StdResult<Signer> {
        Ok(Signer {
            party_id: register_signer_message.party_id,
            verification_key: register_signer_message.verification_key.try_into().with_context(
                || "'FromRegisterSignerAdapter' can not convert the verification key",
            )?,
            verification_key_signature: match register_signer_message.verification_key_signature {
                Some(verification_key_signature) => {
                    Some(verification_key_signature.try_into().with_context(|| {
                        "'FromRegisterSignerAdapter' can not convert the verification key signature"
                    })?)
                }
                _ => None,
            },
            operational_certificate: match register_signer_message.operational_certificate {
                Some(operational_certificate) => {
                    Some(operational_certificate.try_into().with_context(|| {
                        "'FromRegisterSignerAdapter' can not convert the operational certificate"
                    })?)
                }
                _ => None,
            },
            kes_period: register_signer_message.kes_period,
        })
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::test_utils::double::Dummy;

    use super::*;

    #[test]
    fn try_adapt_signer_registration_message_to_entity() {
        let register_signer_message = RegisterSignerMessage {
            party_id: "one".to_string(),
            ..RegisterSignerMessage::dummy()
        };

        let signer = FromRegisterSignerAdapter::try_adapt(register_signer_message)
            .expect("Converting message to signer should not fail");

        assert_eq!("one".to_string(), signer.party_id);
    }
}

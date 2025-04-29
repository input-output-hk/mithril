use anyhow::Context;
use mithril_common::{
    entities::{SingleSignature, SingleSignatureAuthenticationStatus},
    messages::{RegisterSignatureMessage, TryFromMessageAdapter},
    StdResult,
};

pub struct FromRegisterSingleSignatureAdapter;

impl TryFromMessageAdapter<RegisterSignatureMessage, SingleSignature>
    for FromRegisterSingleSignatureAdapter
{
    fn try_adapt(
        register_single_signature_message: RegisterSignatureMessage,
    ) -> StdResult<SingleSignature> {
        let signature = SingleSignature {
            party_id: register_single_signature_message.party_id,
            signature: register_single_signature_message
                .signature
                .try_into()
                .with_context(|| {
                    "'FromRegisterSingleSignatureAdapter' can not convert the single signature"
                })?,
            won_indexes: register_single_signature_message.won_indexes,
            authentication_status: SingleSignatureAuthenticationStatus::Unauthenticated,
        };

        Ok(signature)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn try_adapt_single_signature_message_to_entity() {
        let message = RegisterSignatureMessage::dummy();
        let signature = FromRegisterSingleSignatureAdapter::try_adapt(message.clone()).unwrap();

        assert_eq!(message.party_id, signature.party_id);
        assert_eq!(
            SingleSignatureAuthenticationStatus::Unauthenticated,
            signature.authentication_status
        );
    }
}

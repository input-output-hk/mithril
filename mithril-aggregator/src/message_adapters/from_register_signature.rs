use anyhow::Context;
use mithril_common::{
    entities::{SingleSignatureAuthenticationStatus, SingleSignatures},
    messages::{RegisterSignatureMessage, TryFromMessageAdapter},
    StdResult,
};

pub struct FromRegisterSingleSignatureAdapter;

impl TryFromMessageAdapter<RegisterSignatureMessage, SingleSignatures>
    for FromRegisterSingleSignatureAdapter
{
    fn try_adapt(
        register_single_signature_message: RegisterSignatureMessage,
    ) -> StdResult<SingleSignatures> {
        let signatures = SingleSignatures {
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

        Ok(signatures)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn try_adapt_single_signatures_message_to_entity() {
        let message = RegisterSignatureMessage::dummy();
        let signatures = FromRegisterSingleSignatureAdapter::try_adapt(message.clone()).unwrap();

        assert_eq!(message.party_id, signatures.party_id);
        assert_eq!(
            SingleSignatureAuthenticationStatus::Unauthenticated,
            signatures.authentication_status
        );
    }
}

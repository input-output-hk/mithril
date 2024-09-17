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
            signed_message: register_single_signature_message.signed_message,
            authentication_status: SingleSignatureAuthenticationStatus::Unauthenticated,
        };

        Ok(signatures)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_message() {
        let signatures = FromRegisterSingleSignatureAdapter::try_adapt(RegisterSignatureMessage {
            signed_message: Some("signed_message".to_string()),
            ..RegisterSignatureMessage::dummy()
        })
        .unwrap();

        assert_eq!("party_id".to_string(), signatures.party_id);
        assert_eq!(
            Some("signed_message".to_string()),
            signatures.signed_message
        );
    }
}

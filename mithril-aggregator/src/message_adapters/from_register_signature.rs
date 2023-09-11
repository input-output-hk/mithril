use anyhow::Context;
use mithril_common::{
    entities::SingleSignatures,
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
        };

        Ok(signatures)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_message() {
        let message = RegisterSignatureMessage::dummy();
        let signatures = FromRegisterSingleSignatureAdapter::try_adapt(message).unwrap();

        assert_eq!("party_id".to_string(), signatures.party_id);
    }
}

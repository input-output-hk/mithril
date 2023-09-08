use anyhow::Context;
use mithril_common::entities::{SignedEntityType, SingleSignatures};
use mithril_common::messages::{RegisterSignatureMessage, TryToMessageAdapter};
use mithril_common::StdResult;

pub struct ToRegisterSignatureMessageAdapter;

impl TryToMessageAdapter<(SignedEntityType, SingleSignatures), RegisterSignatureMessage>
    for ToRegisterSignatureMessageAdapter
{
    fn try_adapt(
        (signed_entity_type, single_signature): (SignedEntityType, SingleSignatures),
    ) -> StdResult<RegisterSignatureMessage> {
        let message = RegisterSignatureMessage {
            signed_entity_type: Some(signed_entity_type),
            party_id: single_signature.party_id,
            signature: single_signature.signature.try_into().with_context(|| {
                "'ToRegisterSignatureMessageAdapter' can not convert the single signature"
            })?,
            won_indexes: single_signature.won_indexes,
        };

        Ok(message)
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::test_utils::fake_data;

    use super::*;

    #[test]
    fn adapt_ok() {
        let single_signature = fake_data::single_signatures([1, 3].to_vec());
        let message: RegisterSignatureMessage = ToRegisterSignatureMessageAdapter::try_adapt((
            SignedEntityType::dummy(),
            single_signature,
        ))
        .unwrap();

        assert_eq!("party_id".to_string(), message.party_id);
    }
}

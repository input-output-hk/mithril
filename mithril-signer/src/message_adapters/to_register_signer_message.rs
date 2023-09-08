use anyhow::Context;
use mithril_common::{
    entities::{Epoch, Signer},
    messages::{RegisterSignerMessage, TryToMessageAdapter},
    StdResult,
};

/// Adapter to create [RegisterSignerMessage] from [Signer] instance.
pub struct ToRegisterSignerMessageAdapter;

impl TryToMessageAdapter<(Epoch, Signer), RegisterSignerMessage>
    for ToRegisterSignerMessageAdapter
{
    /// Method to trigger the conversion.
    fn try_adapt((epoch, signer): (Epoch, Signer)) -> StdResult<RegisterSignerMessage> {
        let message = RegisterSignerMessage {
            epoch: Some(epoch),
            party_id: signer.party_id,
            verification_key: signer.verification_key.try_into().with_context(|| {
                format!(
                    "'ToRegisterSignerMessageAdapter' can not convert the verification key: '{:?}'",
                    signer.verification_key
                )
            })?,
            verification_key_signature: match signer.verification_key_signature {
                Some(k) => Some(k.try_into().with_context(|| {
                    format!(
                        "'ToRegisterSignerMessageAdapter' can not convert the verification key signature: '{:?}'",
                        signer.verification_key_signature
                    )
                })?),
                None => None,
            },
            operational_certificate: match signer.operational_certificate {
                Some(o) => Some(o.try_into().with_context(|| {
                    "'ToRegisterSignerMessageAdapter' can not convert the operational certificate"
                })?),
                None => None,
            },
            kes_period: signer.kes_period,
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
        let epoch = Epoch(1);
        let mut signer = fake_data::signers(1)[0].to_owned();
        signer.party_id = "0".to_string();
        let message = ToRegisterSignerMessageAdapter::try_adapt((epoch, signer)).unwrap();

        assert_eq!("0".to_string(), message.party_id);
    }
}

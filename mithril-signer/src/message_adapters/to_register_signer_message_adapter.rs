use mithril_common::{entities::Signer, messages::RegisterSignerMessage};

/// Adapter to create [RegisterSignerMessage] from [Signer] instance.
pub struct ToRegisterSignerMessageAdapter;

impl ToRegisterSignerMessageAdapter {
    /// Method to trigger the conversion.
    pub fn adapt(signer: Signer) -> RegisterSignerMessage {
        RegisterSignerMessage {
            party_id: signer.party_id,
            verification_key: signer.verification_key,
            verification_key_signature: signer.verification_key_signature,
            operational_certificate: signer.operational_certificate,
            kes_period: signer.kes_period,
        }
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::test_utils::fake_data;

    use super::*;

    #[test]
    fn adapt_ok() {
        let signer = fake_data::signers(1)[0].to_owned();
        let message = ToRegisterSignerMessageAdapter::adapt(signer);

        assert_eq!("0".to_string(), message.party_id);
    }
}

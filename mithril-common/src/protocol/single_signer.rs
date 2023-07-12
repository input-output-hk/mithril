use anyhow::{anyhow, Context, Result};

use crate::{
    crypto_helper::{key_encode_hex, ProtocolSigner},
    entities::{PartyId, ProtocolMessage, SingleSignatures},
};

/// The SingleSigner is the structure responsible for issuing SingleSignatures.
#[cfg_attr(test, derive(Debug))]
pub struct SingleSigner {
    party_id: PartyId,
    protocol_signer: ProtocolSigner,
}

impl SingleSigner {
    pub(super) fn new(party_id: PartyId, protocol_signer: ProtocolSigner) -> Self {
        Self {
            party_id,
            protocol_signer,
        }
    }

    /// Issue a single signature for the given message.
    ///
    /// If no lottery are won None will be returned.
    pub fn sign(&self, message: &ProtocolMessage) -> Result<Option<SingleSignatures>> {
        match self.protocol_signer.sign(message.compute_hash().as_bytes()) {
            Some(signature) => {
                let won_indexes = signature.indexes.clone();
                let encoded_signature = key_encode_hex(signature)
                    .map_err(|err| anyhow!(err))
                    .with_context(|| {
                        format!(
                            "Could not encode protocol signature issued by party: {}",
                            self.party_id
                        )
                    })?;

                Ok(Some(SingleSignatures::new(
                    self.party_id.to_owned(),
                    encoded_signature,
                    won_indexes,
                )))
            }
            None => Ok(None),
        }
    }

    /// Return the partyId associated with this Signer.
    pub fn get_party_id(&self) -> PartyId {
        self.party_id.clone()
    }
}

#[cfg(test)]
mod test {
    use crate::{
        entities::ProtocolMessage, protocol::SignerBuilder, test_utils::MithrilFixtureBuilder,
    };

    #[test]
    fn single_signer_should_be_able_to_issue_single_signature() {
        let fixture = MithrilFixtureBuilder::default().with_signers(3).build();
        let signers = fixture.signers_fixture();
        let signer = signers.first().unwrap();

        let (single_signer, _) = SignerBuilder::new(
            &fixture.signers_with_stake(),
            &fixture.protocol_parameters(),
        )
        .unwrap()
        .build_test_single_signer(
            signer.signer_with_stake.clone(),
            signer.kes_secret_key_path(),
        )
        .unwrap();

        let signature = single_signer
            .sign(&ProtocolMessage::default())
            .expect("Single signer should be able to issue single signature");

        assert!(signature.is_some());
    }
}

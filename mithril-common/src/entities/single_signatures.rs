use crate::crypto_helper::{key_decode_hex, ProtocolSingleSignature};
use crate::entities::{HexEncodedSingleSignature, LotteryIndex, PartyId};
use serde::{Deserialize, Serialize};

/// SingleSignatures represent single signatures originating from a participant in the network
/// for a digest at won lottery indexes
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct SingleSignatures {
    /// The unique identifier of the signer
    pub party_id: PartyId,

    /// The single signature of the digest
    pub signature: HexEncodedSingleSignature,

    /// The indexes of the won lotteries that lead to the single signatures
    #[serde(rename = "indexes")]
    pub won_indexes: Vec<LotteryIndex>,
}

impl SingleSignatures {
    /// SingleSignature factory
    pub fn new(
        party_id: PartyId,
        signature: HexEncodedSingleSignature,
        won_indexes: Vec<LotteryIndex>,
    ) -> SingleSignatures {
        SingleSignatures {
            party_id,
            signature,
            won_indexes,
        }
    }

    /// Convert this [SingleSignatures] to its corresponding [MithrilStm Signature][ProtocolSingleSignature].
    pub fn to_protocol_signature(&self) -> Result<ProtocolSingleSignature, String> {
        match key_decode_hex::<ProtocolSingleSignature>(&self.signature) {
            Ok(signature) => Ok(signature),
            Err(error) => Err(format!(
                "Could not decode signature: {}, signature: {}",
                error, self.signature
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        crypto_helper::{key_encode_hex, tests_setup::setup_message},
        test_utils::MithrilFixtureBuilder,
    };

    #[test]
    fn single_signatures_should_convert_to_protocol_signatures() {
        let message = setup_message();
        let fixture = MithrilFixtureBuilder::default().with_signers(1).build();
        let signer = &fixture.signers_fixture()[0];
        let message_hash = either::for_both!(message, m => m.compute_hash());
        let protocol_sigs = signer
            .protocol_signer
            .sign(message_hash.as_bytes())
            .unwrap();

        let signature = SingleSignatures::new(
            signer.signer_with_stake.party_id.to_owned(),
            key_encode_hex(&protocol_sigs).unwrap(),
            protocol_sigs.indexes.clone(),
        );

        assert_eq!(protocol_sigs, signature.to_protocol_signature().unwrap());
    }
}

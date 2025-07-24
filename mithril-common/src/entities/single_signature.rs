use mithril_stm::SingleSignature as StmSingleSignature;
use serde::{Deserialize, Serialize};
use std::fmt::{Debug, Formatter};

use crate::{
    crypto_helper::ProtocolSingleSignature,
    entities::{LotteryIndex, PartyId},
};

/// SingleSignatures represent single signatures originating from a participant in the network
/// for a digest at won lottery indexes
#[derive(Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SingleSignature {
    /// The unique identifier of the signer
    pub party_id: PartyId,

    /// The single signature of the digest
    pub signature: ProtocolSingleSignature,

    /// The indexes of the won lotteries that lead to the single signatures
    #[serde(rename = "indexes")]
    pub won_indexes: Vec<LotteryIndex>,

    /// Status of the authentication of the signer that emitted the signature
    #[serde(skip)]
    pub authentication_status: SingleSignatureAuthenticationStatus,
}

/// Status of the authentication of the signer that emitted the signature
#[derive(Debug, Copy, Clone, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum SingleSignatureAuthenticationStatus {
    /// The signer that emitted the signature is authenticated
    Authenticated,
    /// The signer that emitted the signature is not authenticated
    #[default]
    Unauthenticated,
}

impl SingleSignature {
    /// `SingleSignatures` factory
    pub fn new<T: Into<PartyId>>(
        party_id: T,
        signature: ProtocolSingleSignature,
        won_indexes: Vec<LotteryIndex>,
    ) -> SingleSignature {
        SingleSignature {
            party_id: party_id.into(),
            signature,
            won_indexes,
            authentication_status: SingleSignatureAuthenticationStatus::Unauthenticated,
        }
    }

    /// Convert this [SingleSignature] to its corresponding [MithrilStm Signature][SingleSignature].
    pub fn to_protocol_signature(&self) -> StmSingleSignature {
        self.signature.clone().into()
    }

    /// Check that the signer that emitted the signature is authenticated
    pub fn is_authenticated(&self) -> bool {
        self.authentication_status == SingleSignatureAuthenticationStatus::Authenticated
    }
}

cfg_test_tools! {
impl SingleSignature {
    /// Create a fake [SingleSignature] with valid cryptographic data for testing purposes.
    pub fn fake<TPartyId: Into<String>, TMessage: Into<String>>(party_id: TPartyId, message: TMessage) -> Self {
        use crate::entities::{ProtocolParameters};
        use crate::test::builder::{MithrilFixtureBuilder, StakeDistributionGenerationMethod};

        let party_id = party_id.into();
        let message = message.into();

        let fixture = MithrilFixtureBuilder::default()
            .with_stake_distribution(StakeDistributionGenerationMethod::Custom(
                std::collections::BTreeMap::from([(party_id.to_string(), 100)]),
            ))
            .with_protocol_parameters(ProtocolParameters::new(1, 1, 1.0))
            .build();
        let signature = fixture.signers_fixture()[0].sign(&message).unwrap();

        Self {
            party_id,
            signature: signature.signature,
            won_indexes: vec![10, 15],
            authentication_status: SingleSignatureAuthenticationStatus::Unauthenticated,
        }
    }
}
}

impl Debug for SingleSignature {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let is_pretty_printing = f.alternate();
        let mut debug = f.debug_struct("SingleSignatures");
        debug
            .field("party_id", &self.party_id)
            .field("won_indexes", &format_args!("{:?}", self.won_indexes));

        match is_pretty_printing {
            true => debug
                .field("signature", &format_args!("{:?}", self.signature))
                .finish(),
            false => debug.finish_non_exhaustive(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::test::{builder::MithrilFixtureBuilder, crypto_helper::setup_message};

    use super::*;

    #[test]
    fn single_signatures_should_convert_to_protocol_signatures() {
        let message = setup_message();
        let fixture = MithrilFixtureBuilder::default().with_signers(1).build();
        let signer = &fixture.signers_fixture()[0];
        let protocol_sigs = signer
            .protocol_signer
            .sign(message.compute_hash().as_bytes())
            .unwrap();

        let signature = SingleSignature::new(
            signer.signer_with_stake.party_id.to_owned(),
            protocol_sigs.clone().into(),
            protocol_sigs.indexes.clone(),
        );

        assert_eq!(protocol_sigs, signature.to_protocol_signature());
    }
}
